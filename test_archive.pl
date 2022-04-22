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
:- use_module(library(plunit)).
:- use_module(library(archive)).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(apply), [maplist/3, maplist/2]).
:- use_module(library(filesex), [directory_file_path/3, relative_file_name/3]).
:- use_module(library(lists), [nth1/3]).

/* This is a very minimal test suite, which was written when fixing
   some memory leak issues. */

test_archive :-
    run_tests([ archive
              ]).

:- begin_tests(archive,
               [ condition(archive_has_format(zip))
               ]).

% The following is derived from check_installation/0 for archive:

test(smoke_test_open) :-
    create_tmp_file(ArchivePath),
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
    create_archive_file(ArchivePath, _, FilesOut, _),
    archive_entries(ArchivePath, Entries).

test(create_and_open_named,
     [LineRead1 == Line1,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_first_line(SrcDir, ExampleSourceFile, Line1),
    archive_open_named(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_line_to_string(TestArchiveStream, LineRead1),
    close(TestArchiveStream).

test(create_and_open_named_no_close, % same as above but without close/1
     [LineRead1 == Line1,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_first_line(SrcDir, ExampleSourceFile, Line1),
    archive_open_named(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_line_to_string(TestArchiveStream, LineRead1).

test(create_and_open_named_twice_no_close,
     [LineRead1 == Line1,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_first_line(SrcDir, ExampleSourceFile, Line1),
    archive_open_named(ArchivePath, 'swipl.rc', _Stream0),
    archive_open_named(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_line_to_string(TestArchiveStream, LineRead1).

% TODO: following test causes memory leak:
test(create_and_open_named_fail, % Same as above but with bad EntryName
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _, _),
    archive_open_named(ArchivePath, 'XXX', _TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry,
     [LineRead1 == Line1,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_first_line(SrcDir, ExampleSourceFile, Line1),
    open_archive_entry(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_line_to_string(TestArchiveStream, LineRead1),
    close(TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry_no_close, % same as above but without close/1
     [LineRead1 == Line1,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, SrcDir, _, ExampleSourceFile),
    file_first_line(SrcDir, ExampleSourceFile, Line1),
    open_archive_entry(ArchivePath, ExampleSourceFile, TestArchiveStream),
    read_line_to_string(TestArchiveStream, LineRead1).

% TODO: following test causes memory leak:
test(create_and_open_archive_entry_no_close, % same as above but bad EntryName
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _, _),
    open_archive_entry(ArchivePath, 'XXXl', _TestArchiveStream).

% TODO: following test causes memory leak:
test(create_and_entries_error,
     [error(existence_error(file, 'foobar-qqsv'), _),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    FilesOut = ['foobar-qqsv'], % doesn't exist
    archive_create(ArchivePath, FilesOut, [format(zip)]).

test(bad_unify_blob,
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    archive_open(ArchivePath, read, not_an_archive_blob, []).

test(bad_mode,
     [error(domain_error(io_mode, neither_read_nor_write), _),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    archive_open(ArchivePath, neither_read_nor_write, _Archive, []).

test(double_open_write,
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    % This should fail because blob doesn't have PL_BLOB_UNIQUE
    archive_open(ArchivePath, write, Archive, [format(zip)]),
    archive_open(ArchivePath, write, Archive, [format(zip)]).

test(double_open_entry_write,
     [error(permission_error(access,archive_entry,Archive)),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    archive_open(ArchivePath, write, Archive, [format(zip)]),
    archive_next_header(Archive, item1),
    archive_open_entry(Archive, _Stream1),
    archive_open_entry(Archive, _Stream2).

test(double_next_header_write,
     [error(permission_error(next_header,archive,Archive)),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    archive_open(ArchivePath, write, Archive, [format(zip)]),
    archive_next_header(Archive, item1),
    archive_open_entry(Archive, _Stream1),
    archive_next_header(Archive, item2),
    archive_open_entry(Archive, _Stream2).

test(double_open_read,
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    % This should fail because blob doesn't have PL_BLOB_UNIQUE
    create_archive_file(ArchivePath, _, _, _),
    archive_open(ArchivePath, read, Archive, []),
    archive_open(ArchivePath, read, Archive, []).

test(double_open_read2,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _, _),
    % It's OK to open an archive twice for input with 2 different streams
    archive_open(ArchivePath, read, Archive1, []),
    archive_open(ArchivePath, read, Archive2, []),
    archive_close(Archive1),
    archive_close(Archive2).

test(double_open_entry_read,
     [error(permission_error(access,archive_entry,Archive)),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _, ExampleSourceFile),
    archive_open(ArchivePath, read, Archive, []),
    archive_next_header(Archive, ExampleSourceFile),
    archive_open_entry(Archive, _Stream1),
    archive_open_entry(Archive, _Stream2).

test(double_next_header_read,
     [error(permission_error(next_header,archive,Archive)),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, FilesOut, _),
    archive_open(ArchivePath, read, Archive, []),
    FilesOut = [Item1, Item2 | _],
    archive_next_header(Archive, Item1),
    archive_open_entry(Archive, _Stream1),
    archive_next_header(Archive, Item2),
    archive_open_entry(Archive, _Stream2).

test(next_header_order1,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, FilesOut, _),
    archive_open(ArchivePath, read, Archive, []),
    archive_next_header(Archive, Item1),
    archive_next_header(Archive, Item2),
    assertion(ground(FilesOut)), % Ensure it's safe to use =/2 for next assertion
    assertion(FilesOut = [Item1, Item2|_]),
    archive_close(Archive).

test(next_header_order2,
     [fail,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, FilesOut, _),
    FilesOut = [Item1, Item2|_],
    archive_open(ArchivePath, read, Archive, []),
    archive_next_header(Archive, Item2),
    archive_next_header(Archive, Item1). % Can only go forward

test(next_header_order3,
     [setup(create_tmp_file(ArchivePath)), 
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, FilesOut, _),
    FilesOut = [Item1, _Item2, Item3|_],
    archive_open(ArchivePath, read, Archive, []),
    archive_next_header(Archive, Item1),
    archive_next_header(Archive, Item3), % Can skip forward
    archive_close(Archive).

test(next_header_order4,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, FilesOut, _),
    FilesOut = [_Item1, Item2, Item3|_],
    archive_open(ArchivePath, read, Archive, []),
    archive_next_header(Archive, Item2),
    archive_next_header(Archive, NextItem3),
    assertion(NextItem3 == Item3),
    archive_close(Archive).

test(close_parent1,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _FilesOut, Example),
    open(ArchivePath, read, Stream, [type(binary)]),
    assertion(is_stream(Stream)),
    archive_open(Stream, read, Archive, [close_parent(false)]),
    archive_next_header(Archive, Example),
    archive_close(Archive),
    assertion(is_stream(Stream)),
    close(Stream),
    assertion(\+ is_stream(Stream)).

test(close_parent2,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _FilesOut, Example),
    open(ArchivePath, read, Stream, [type(binary)]),
    assertion(is_stream(Stream)),
    archive_open(Stream, read, Archive, [close_parent(true)]),
    archive_next_header(Archive, Example),
    archive_close(Archive),
    assertion(\+ is_stream(Stream)).

test(close_parent3,
     [error(archive_error(_,fatal,Archive,archive_free)),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _FilesOut, Example),
    % Same as close_parent3, but has close(Stream) before archive_close(Archive).
    open(ArchivePath, read, Stream, [type(binary)]),
    assertion(is_stream(Stream)),
    archive_open(Stream, read, Archive, [close_parent(true)]),
    archive_next_header(Archive, Example),
    close(Stream),
    assertion(\+ is_stream(Stream)),
    archive_close(Archive).

test(close_entry1,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _FilesOut, Example),
    open(ArchivePath, read, Stream, [type(binary)]),
    archive_open(Stream, read, Archive, [close_parent(false)]),
    archive_next_header(Archive, Example),
    archive_open_entry(Archive, ExampleStream),
    read_line_to_string(ExampleStream, _LineRead1),
    close(ExampleStream),
    archive_close(Archive),
    close(Stream).

test(close_entry2,
     [setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _FilesOut, Example),
    % Like close_entry1, but ExampleStream is closed after archive_close/2
    open(ArchivePath, read, Stream, [type(binary)]),
    archive_open(Stream, read, Archive, [close_parent(false)]),
    archive_next_header(Archive, Example),
    archive_open_entry(Archive, ExampleStream),
    read_line_to_string(ExampleStream, _LineRead1),
    archive_close(Archive),
    close(ExampleStream),
    close(Stream).

test(close_entry3,
     [LineRead1 == Line1,
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, SrcDir, _FilesOut, ExampleSourceFile),
    file_first_line(SrcDir, ExampleSourceFile, Line1),
    % Like close_entry1, but archive_close/2 is called while entry stream still open
    open(ArchivePath, read, Stream, [type(binary)]),
    archive_open(Stream, read, Archive, [close_parent(false)]),
    archive_next_header(Archive, ExampleSourceFile),
    archive_open_entry(Archive, ExampleStream),
    archive_close(Archive),
    read_line_to_string(ExampleStream, LineRead1).

test(close_entry4,
     [error(archive_error(_,fatal,Archive,archive_read_next_header)),
      setup(create_tmp_file(ArchivePath)),
      cleanup(delete_file(ArchivePath))]) :-
    create_archive_file(ArchivePath, _, _FilesOut, Example),
    % Like close_entry1, but Stream is closed before any archive actions
    open(ArchivePath, read, Stream, [type(binary)]),
    archive_open(Stream, read, Archive, [close_parent(false)]),
    close(Stream),
    archive_next_header(Archive, Example),
    archive_open_entry(Archive, ExampleStream),
    read_line_to_string(ExampleStream, _LineRead1),
    close(ExampleStream),
    archive_close(Archive).

test(gc) :- % Run this last, to isolate PL_cleanup() problems TODO: remove this test
    garbage_collect,
    garbage_collect_atoms,
    garbage_collect_atoms.

:- end_tests(archive).

:- begin_tests(debug, % TODO: Remove this once asan memory bugs have
                      %       been fixed Also remove
                      %       archive_open_named_debug/3.
               [setup(disable_gc)]).

test(memleak) :-
    ArchivePath= '/tmp/ar_test.zip', % requires setting up manually
    format(user_error, '~n', []),
    (   archive_open_named_debug(ArchivePath, 'XXX', _TestArchiveStream)
    *-> format(user_error, '*** memleak test - Should have failed!!!~n', [])
    ;   format(user_error, '*** memleak test - about to garbage collect(1)~n', []),
        garbage_collect,
        garbage_collect_atoms,
        format(user_error, '*** memleak test - about to garbage collect(2)~n', []),
        garbage_collect_atoms
    ).

:- end_tests(debug).

disable_gc :-
    set_prolog_flag(agc_margin,0), % turn off gc
    set_prolog_flag(trace_gc, true),
    set_prolog_flag(gc_thread, false),
    set_prolog_gc_thread(false),
    trim_stacks,
    garbage_collect,
    garbage_collect_atoms.

create_tmp_file(Path) :-
    tmp_file_stream(utf8, Path, Out),
    close(Out).

%!  create_archive_file(+ArchiveFile, -RootDir, -Files, -Example) is det.
%
%   Create  a  `zip`  archive  using  three  files  from  the  installed
%   SWI-Prolog tree.

create_archive_file(ArchivePath, ArchiveSourceDir, FilesOut, ExampleSourceFile) :-
    Files = [swi('include/SWI-Prolog.h'), library('archive.pl'), swi('swipl.rc')],
    absolute_file_name(swi(.), ArchiveSourceDir, [file_type(directory), access(read)]),
    maplist(ar_input(ArchiveSourceDir), Files, FilesOut),
    nth1(2, FilesOut, ExampleSourceFile),
    archive_create(ArchivePath, FilesOut,
                   [ format(zip),
                     directory(ArchiveSourceDir)
                   ]).

ar_input(Dir, Spec, File) :-
    directory_file_path(Dir, dummy, RelTo),
    absolute_file_name(Spec, AbsFile, [access(read)]),
    relative_file_name(AbsFile, RelTo, File).

archive_has_format(Format) :-
    create_tmp_file(Path),
    catch(archive_open(Path, A, [format(Format)]), E, true),
    (   var(E)
    ->  archive_close(A),
        delete_file(Path)
    ;   true
    ),
    \+ subsumes_term(error(domain_error(format, _),_), E).

file_first_line(SrcDir, File, Line) :-
    directory_file_path(SrcDir, File, Path),
    setup_call_cleanup(
        open(Path, read, In),
        read_line_to_string(In, Line),
        close(In)).

% TODO: delete this when debug test casesa are removed
% (This is archive_open_named/3 with extra debug statements)
archive_open_named_debug(ArchiveFile, EntryName, Stream) :-
    format(user_error, '*** ~q~n', [archive_open_named_debug_1(ArchiveFile, EntryName, Stream)]),
    archive_open(ArchiveFile, Archive, []),
    format(user_error, '*** ~q~n', [archive_open_named_debug_2(ArchiveFile, EntryName, Stream, Archive)]),
    archive_next_header(Archive, EntryName),
    format(user_error, '*** ~q~n', [archive_open_named_debug_3(ArchiveFile, EntryName, Stream, Archive)]),
    archive_open_entry(Archive, Stream),
    format(user_error, '*** ~q~n', [archive_open_named_debug_4(ArchiveFile, EntryName, Stream, Archive)]),
    archive_close(Archive),
    format(user_error, '*** ~q~n', [archive_open_named_debug_5(ArchiveFile, EntryName, Stream, Archive)]).

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
    archive_next_header(Archive, EntryName),
    archive_open_entry(Archive, Stream).

% Code from documentation of module (1)

list_archive(File) :-
    setup_call_cleanup(
        archive_open(File, Archive, []),
        (   repeat,
            (   archive_next_header(Archive, Path)
            ->  format('~w~n', [Path]),
                fail
            ;   !
            )
        ),
        archive_close(Archive)).

% Code from documentation of module (2)

list_archive2(File) :-
    list_archive2(File, Headers),
    maplist(writeln, Headers).

list_archive2(File, Headers) :-
    archive_foldl(add_header, File, Headers, []).

add_header(Path, _, [Path|Paths], Paths).

% Code from documentation of module (3)

print_entry(Path, Handle, Cnt0, Cnt1) :-
    archive_header_property(Handle, filetype(Type)),
    format('File ~w is of type ~w~n', [Path, Type]),
    Cnt1 is Cnt0 + 1.

list_archive_headers(File) :-
    archive_foldl(print_entry, File, 0, FileCount),
    format('We have ~w files', [FileCount]).