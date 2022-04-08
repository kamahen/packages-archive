/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2022, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_archive,
	  [ test_archive/0
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).

:- use_module(library(plunit)).
:- use_module(library(archive)).
:- use_module(library(readutil), [read_line_to_string/2]).

/* This is a very minimal test suite, which was written when fixing
   some memory leak issues. */

test_archive :-
    run_tests([ archive
              ]).

:- begin_tests(archive).

% The following is derived from check_installation/0 for archive:

test(smoke_test_open) :-
    tmp_file_stream(utf8, ArchivePath, Out),
    close(Out),
    % archive_open should error because the file is empty.
    catch(archive_open(ArchivePath, A, []), E, true),
    (   var(E)
    ->  archive_close(A)
    ;   true
    ),
    delete_file(ArchivePath).

test(create_and_entries,
     [FilesOut == Entries,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, FilesOut),
    archive_entries(ArchivePath, Entries).

test(create_and_open_named,
     [Line1 == "/*  Part of SWI-Prolog", % 1st line of this file */
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _),
    archive_open_named(ArchivePath, 'test_archive.pl', TestArchiveStream),
    read_line_to_string(TestArchiveStream, Line1),
    close(TestArchiveStream).

test(create_and_open_named_no_close, % same as above but without close/1
     [Line1 == "/*  Part of SWI-Prolog", % 1st line of this file */
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _),
    archive_open_named(ArchivePath, 'test_archive.pl', TestArchiveStream),
    read_line_to_string(TestArchiveStream, Line1).

test(create_and_open_named_twice_no_close,
     [Line1 == "/*  Part of SWI-Prolog", % 1st line of this file */
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _),
    archive_open_named(ArchivePath, 'archive.pl', _Stream0),
    archive_open_named(ArchivePath, 'test_archive.pl', TestArchiveStream),
    read_line_to_string(TestArchiveStream, Line1).

% TODO: following test causes memory leak:
test(create_and_open_named_fail, % Same as above but with bad EntryName
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _),
    archive_open_named(ArchivePath, 'XXX', _TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry,
     [Line1 == "/*  Part of SWI-Prolog", % 1st line of this file */
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _),
    open_archive_entry(ArchivePath, 'test_archive.pl', TestArchiveStream),
    read_line_to_string(TestArchiveStream, Line1),
    close(TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry_no_close, % same as above but without close/1
     [Line1 == "/*  Part of SWI-Prolog", % 1st line of this file */
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _),
    open_archive_entry(ArchivePath, 'test_archive.pl', TestArchiveStream),
    read_line_to_string(TestArchiveStream, Line1).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry_no_close, % same as above but bad EntryName
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _),
    open_archive_entry(ArchivePath, 'XXXl', _TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_entries_error,
     [error(existence_error(file, 'foobar-qqsv'), _),
      setup((tmp_file_stream(utf8, ArchivePath, Out),
             close(Out))),
      cleanup(delete_file(ArchivePath))]) :-
    FilesOut = ['foobar-qqsv'], % doesn't exist
    % TODO: what if zip doesn't exist on the system?
    archive_create(ArchivePath, FilesOut, [format(zip)]),
    archive_entries(ArchivePath, _Entries).

:- end_tests(archive).

create_tmp_file(Path) :-
    tmp_file_stream(utf8, Path, Out),
    close(Out).

create_archive_file(ArchivePath, FilesOut) :-
    source_dir(ArchiveSourceDir),
    FilesOut = ['archive4pl.c', 'archive.doc', 'archive.pl', 'CMakeLists.txt', 'test_archive.pl'],
    % TODO: what if zip doesn't exist on the system?
    archive_create(ArchivePath, FilesOut, [format(zip), directory(ArchiveSourceDir)]).

source_dir(ArchiveSourceDir) :-
    % ARCHIVE_SOURCE_DIR is set in CMakeLists.txt
    (   getenv('ARCHIVE_SOURCE_DIR', ArchiveSourceDir)
    ->  true
    ;   absolute_file_name('.', ArchiveSourceDir)
    ).

% Code from documentation of archive_close/1.
archive_open_named(ArchiveFile, EntryName, Stream) :-
    archive_open(ArchiveFile, Archive, []),
    archive_next_header(Archive, EntryName),
    archive_open_entry(Archive, Stream),
    archive_close(Archive).

% Code from documentation of archive_close/1.
open_archive_entry(ArchiveFile, EntryName, Stream) :-
    open(ArchiveFile, read, In, [type(binary)]),
    archive_open(In, Archive, [close_parent(true)]),
    archive_next_header(Archive, EntryName).
