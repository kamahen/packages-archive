/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008-2009, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <archive.h>
#include <archive_entry.h>
#include <assert.h>
#include <string.h>
#include <errno.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		Design of the libarchive interface

An archive is represented by a   symbol (blob). The C-structure contains
the archive, the current header and some state information.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ARCHIVE_MAGIC 348184378

typedef enum ar_status
{ AR_VIRGIN = 0,
  AR_OPENED,
  AR_NEW_ENTRY,
  AR_OPENED_ENTRY,
  AR_CLOSED_ENTRY
} ar_status;

typedef struct archive_wrapper
{ atom_t		symbol;		/* Associated symbol */
  IOSTREAM *		data;		/* Input data */
  unsigned short	type;		/* Type of format supported */
  int			magic;		/* magic code */
  ar_status		status;		/* Current status */
  int			close_parent;	/* Close the parent handle */
  struct archive *	archive;	/* Actual archive handle */
  struct archive_entry *entry;		/* Current entry */
} archive_wrapper;

static void free_archive(archive_wrapper *ar);


		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

static atom_t ATOM_close_parent;
static atom_t ATOM_compression;
static atom_t ATOM_format;
static atom_t ATOM_all;
static atom_t ATOM_bzip2;
static atom_t ATOM_compress;
static atom_t ATOM_gzip;
static atom_t ATOM_lzma;
static atom_t ATOM_none;
static atom_t ATOM_xz;
static atom_t ATOM_ar;
static atom_t ATOM_cpio;
static atom_t ATOM_empty;
static atom_t ATOM_iso9960;
static atom_t ATOM_mtree;
static atom_t ATOM_raw;
static atom_t ATOM_tar;
static atom_t ATOM_zip;

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_archive_error2;


		 /*******************************
		 *	  SYMBOL WRAPPER	*
		 *******************************/

static void
acquire_archive(atom_t symbol)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);
  ar->symbol = symbol;
}

static int
release_archive(atom_t symbol)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  free_archive(ar);
  PL_free(ar);

  return TRUE;
}

static int
compare_archives(atom_t a, atom_t b)
{ archive_wrapper *ara = PL_blob_data(a, NULL, NULL);
  archive_wrapper *arb = PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   arb < ara ? -1 : 0
	 );
}

static int
write_archive(IOSTREAM *s, atom_t symbol, int flags)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<archive>(%p)", ar);

  return TRUE;
}

static PL_blob_t archive_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "archive",
  release_archive,
  compare_archives,
  write_archive,
  acquire_archive
};


static int
get_archive(term_t t, archive_wrapper **arp)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &archive_blob)
  { archive_wrapper *ar = data;

    assert(ar->magic == ARCHIVE_MAGIC);

    if ( ar->symbol )
    { *arp = ar;

      return TRUE;
    }

    PL_permission_error("access", "closed_archive", t);
    return FALSE;
  }

  return FALSE;
}



		 /*******************************
		 *	      CALLBACKS		*
		 *******************************/

static int
ar_open(struct archive *a, void *cdata)
{ return ARCHIVE_OK;
}

static int
ar_close(struct archive *a, void *cdata)
{ archive_wrapper *ar = cdata;

  if ( ar->close_parent )
  { if ( Sclose(ar->data) != 0 )
    { archive_set_error(ar->archive, errno, "Close failed");
      ar->data = NULL;
      return ARCHIVE_FATAL;
    }
    ar->data = NULL;
  }

  return ARCHIVE_OK;				/* TBD: close_parent */
}

static ssize_t
ar_read(struct archive *a, void *cdata, const void **buffer)
{ archive_wrapper *ar = cdata;

  if ( Sfeof(ar->data) )
  { return 0;
  } else
  { ssize_t bytes = ar->data->limitp - ar->data->bufp;

    *buffer = ar->data->bufp;
    ar->data->bufp = ar->data->limitp;
    ar->data->position->byteno += bytes;

    return bytes;
  }
}

static off_t
ar_skip(struct archive *a, void *cdata, off_t request)
{ archive_wrapper *ar = cdata;

  if ( Sseek64(ar->data, request, SIO_SEEK_CUR) == 0 )
    return request;

  return 0;				/* cannot skip; library will read */
}



		 /*******************************
		 *	      PROLOG		*
		 *******************************/

static void
free_archive(archive_wrapper *ar)
{
}

static int
archive_error(archive_wrapper *ar)
{ int eno = archive_errno(ar->archive);

  if ( eno != 0 )
  { const char *s = archive_error_string(ar->archive);
    term_t ex = PL_new_term_ref();

    if ( PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
		         PL_FUNCTOR, FUNCTOR_archive_error2,
		           PL_INT, errno,
		           PL_CHARS, s,
		         PL_VARIABLE) )
      return PL_raise_exception(ex);

    return FALSE;
  }

  return TRUE;
}


#define	COMPRESS_ALL	  0x00ff
#define	COMPRESS_BZIP2	  0x0001
#define	COMPRESS_COMPRESS 0x0002
#define	COMPRESS_GZIP	  0x0004
#define	COMPRESS_LZMA	  0x0008
#define	COMPRESS_NONE	  0x0010
#define	COMPRESS_XZ	  0x0020

#define FORMAT_ALL	  0xff00
#define FORMAT_AR	  0x0100
#define FORMAT_CPIO	  0x0200
#define FORMAT_EMPTY	  0x0400
#define FORMAT_ISO9960	  0x0800
#define FORMAT_MTREE	  0x1000
#define FORMAT_RAW	  0x2000
#define FORMAT_TAR	  0x4000
#define FORMAT_ZIP	  0x8000

static void
enable_type(archive_wrapper *ar, int type,
	    int (*f)(struct archive *ar))
{ if ( (ar->type & type) )
  { if ( (*f)(ar->archive) != ARCHIVE_OK )
      ar->type &= ~type;
  }
}

static foreign_t
archive_open(term_t data, term_t handle, term_t options)
{ IOSTREAM *datas;
  archive_wrapper *ar;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();

  if ( !PL_get_stream_handle(data, &datas) )
    return FALSE;
  if ( !(datas->flags & SIO_INPUT) )
  { PL_release_stream(datas);
    return PL_domain_error("input_stream", data);
  }

  ar = PL_malloc(sizeof(*ar));
  memset(ar, 0, sizeof(*ar));
  ar->data = datas;
  ar->magic = ARCHIVE_MAGIC;
  if ( !PL_unify_blob(handle, ar, sizeof(*ar), &archive_blob) )
    return FALSE;

  while( PL_get_list_ex(tail, head, tail) )
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) ||
	 !PL_get_arg(1, head, arg) )
      return PL_type_error("option", head);
    if ( name == ATOM_compression )
    { atom_t c;

      if ( !PL_get_atom_ex(arg, &c) )
	return FALSE;

      if ( c == ATOM_all )
	ar->type |= COMPRESS_ALL;
      else if ( c == ATOM_bzip2 )
	ar->type |= COMPRESS_BZIP2;
      else if ( c == ATOM_compress )
	ar->type |= COMPRESS_COMPRESS;
      else if ( c == ATOM_gzip )
	ar->type |= COMPRESS_GZIP;
      else if ( c == ATOM_lzma )
	ar->type |= COMPRESS_LZMA;
      else if ( c == ATOM_none )
	ar->type |= COMPRESS_NONE;
      else if ( c == ATOM_xz )
	ar->type |= COMPRESS_XZ;
      else
	return PL_domain_error("compression", arg);
    } else if ( name == ATOM_format )
    { atom_t f;

      if ( !PL_get_atom_ex(arg, &f) )
	return FALSE;

      if ( f == ATOM_all )
	ar->type |= FORMAT_ALL;
      else if ( f == ATOM_ar )
	ar->type |= FORMAT_AR;
      else if ( f == ATOM_cpio )
	ar->type |= FORMAT_CPIO;
      else if ( f == ATOM_empty )
	ar->type |= FORMAT_EMPTY;
      else if ( f == ATOM_iso9960 )
	ar->type |= FORMAT_ISO9960;
      else if ( f == ATOM_mtree )
	ar->type |= FORMAT_MTREE;
      else if ( f == ATOM_raw )
	ar->type |= FORMAT_RAW;
      else if ( f == ATOM_tar )
	ar->type |= FORMAT_TAR;
      else if ( f == ATOM_zip )
	ar->type |= FORMAT_ZIP;
      else
	return PL_domain_error("format", arg);
    } else if ( name == ATOM_close_parent )
    { if ( !PL_get_bool_ex(arg, &ar->close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( !(ar->type & COMPRESS_ALL) )
    ar->type |= COMPRESS_ALL;
  if ( !(ar->type & FORMAT_ALL) )
    ar->type |= FORMAT_ALL;

  if ( !(ar->archive = archive_read_new()) )
    return PL_resource_error("memory");

  if ( (ar->type & COMPRESS_ALL) == COMPRESS_ALL )
  { archive_read_support_compression_all(ar->archive);
  } else
  { enable_type(ar, COMPRESS_BZIP2,    archive_read_support_compression_bzip2);
    enable_type(ar, COMPRESS_COMPRESS, archive_read_support_compression_compress);
    enable_type(ar, COMPRESS_GZIP,     archive_read_support_compression_gzip);
    enable_type(ar, COMPRESS_LZMA,     archive_read_support_compression_lzma);
    enable_type(ar, COMPRESS_NONE,     archive_read_support_compression_none);
    enable_type(ar, COMPRESS_XZ,       archive_read_support_compression_xz);
  }

  if ( (ar->type & FORMAT_ALL) == FORMAT_ALL )
  { archive_read_support_format_all(ar->archive);
  } else
  { enable_type(ar, FORMAT_AR,      archive_read_support_format_ar);
    enable_type(ar, FORMAT_CPIO,    archive_read_support_format_cpio);
    enable_type(ar, FORMAT_EMPTY,   archive_read_support_format_empty);
    enable_type(ar, FORMAT_ISO9960, archive_read_support_format_iso9660);
    enable_type(ar, FORMAT_MTREE,   archive_read_support_format_mtree);
    enable_type(ar, FORMAT_RAW,     archive_read_support_format_raw);
    enable_type(ar, FORMAT_TAR,     archive_read_support_format_tar);
    enable_type(ar, FORMAT_ZIP,     archive_read_support_format_zip);
  }

  if ( archive_read_open2(ar->archive, ar,
			  ar_open, ar_read, ar_skip, ar_close) == ARCHIVE_OK )

  { ar->status = AR_OPENED;
    return TRUE;
  }

  return archive_error(ar);
}




static foreign_t
archive_next_header(term_t archive, term_t name)
{ archive_wrapper *ar;
  int rc;

  if ( !get_archive(archive, &ar) )
    return FALSE;
  if ( ar->status == AR_NEW_ENTRY )
    archive_read_data_skip(ar->archive);

  while ( (rc=archive_read_next_header(ar->archive, &ar->entry)) == ARCHIVE_OK )
  { if ( PL_unify_atom_chars(name, archive_entry_pathname(ar->entry)) )
    { ar->status = AR_NEW_ENTRY;
      return TRUE;
    }
    if ( PL_exception(0) )
      return FALSE;
  }

  if ( rc == ARCHIVE_EOF )
    return FALSE;			/* simply at the end */

  return archive_error(ar);
}


static foreign_t
archive_close(term_t archive)
{ archive_wrapper *ar;
  int rc;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( (rc=archive_read_finish(ar->archive)) == ARCHIVE_OK )
  { ar->entry = NULL;
    ar->archive = NULL;
    ar->symbol = 0;

    return TRUE;
  }

  return archive_error(ar);
}


		 /*******************************
		 *	    READ MEMBERS	*
		 *******************************/

static ssize_t
ar_read_entry(void *handle, char *buf, size_t size)
{ archive_wrapper *ar = handle;

  return archive_read_data(ar->archive, buf, size);
}

static int
ar_close_entry(void *handle)
{ archive_wrapper *ar = handle;

  ar->status = AR_CLOSED_ENTRY;
  return 0;
}

static int
ar_control_entry(void *handle, int op, void *data)
{ archive_wrapper *ar = handle;

  (void)ar;

  switch(op)
  { case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
      return -1;
  }
}

static IOFUNCTIONS ar_entry_functions =
{ ar_read_entry,
  NULL,
  NULL,					/* seek */
  ar_close_entry,
  ar_control_entry,			/* control */
  NULL,					/* seek64 */
};

static foreign_t
archive_open_entry(term_t archive, term_t stream)
{ archive_wrapper *ar;
  IOSTREAM *s;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( (s=Snew(ar, SIO_INPUT, &ar_entry_functions)) )
    return PL_unify_stream(stream, s);

  return PL_resource_error("memory");
}



		 /*******************************
		 *	      INSTALL		*
		 *******************************/

#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)

install_t
install_archive4pl(void)
{ MKATOM(close_parent);
  MKATOM(compression);
  MKATOM(format);
  MKATOM(all);
  MKATOM(bzip2);
  MKATOM(compress);
  MKATOM(gzip);
  MKATOM(lzma);
  MKATOM(none);
  MKATOM(xz);
  MKATOM(ar);
  MKATOM(cpio);
  MKATOM(empty);
  MKATOM(iso9960);
  MKATOM(mtree);
  MKATOM(raw);
  MKATOM(tar);
  MKATOM(zip);

  MKFUNCTOR(error, 2);
  MKFUNCTOR(archive_error, 2);

  PL_register_foreign("archive_open",        3, archive_open,        0);
  PL_register_foreign("archive_close",       1, archive_close,       0);
  PL_register_foreign("archive_next_header", 2, archive_next_header, 0);
  PL_register_foreign("archive_open_entry",  2, archive_open_entry,  0);
}
