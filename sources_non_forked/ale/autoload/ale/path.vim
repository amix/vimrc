" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for working with paths in the filesystem.

" simplify a path, and fix annoying issues with paths on Windows.
"
" Forward slashes are changed to back slashes so path equality works better.
"
" Paths starting with more than one forward slash are changed to only one
" forward slash, to prevent the paths being treated as special MSYS paths.
function! ale#path#Simplify(path) abort
    if has('unix')
        return substitute(simplify(a:path), '^//\+', '/', 'g') " no-custom-checks
    endif

    let l:win_path = substitute(a:path, '/', '\\', 'g')

    return substitute(simplify(l:win_path), '^\\\+', '\', 'g') " no-custom-checks
endfunction

" Given a buffer and a filename, find the nearest file by searching upwards
" through the paths relative to the given buffer.
function! ale#path#FindNearestFile(buffer, filename) abort
    let l:buffer_filename = fnamemodify(bufname(a:buffer), ':p')
    let l:buffer_filename = fnameescape(l:buffer_filename)

    let l:relative_path = findfile(a:filename, l:buffer_filename . ';')

    if !empty(l:relative_path)
        return fnamemodify(l:relative_path, ':p')
    endif

    return ''
endfunction

" Given a buffer and a directory name, find the nearest directory by searching upwards
" through the paths relative to the given buffer.
function! ale#path#FindNearestDirectory(buffer, directory_name) abort
    let l:buffer_filename = fnamemodify(bufname(a:buffer), ':p')
    let l:buffer_filename = fnameescape(l:buffer_filename)

    let l:relative_path = finddir(a:directory_name, l:buffer_filename . ';')

    if !empty(l:relative_path)
        return fnamemodify(l:relative_path, ':p')
    endif

    return ''
endfunction

" Given a buffer, a string to search for, an a global fallback for when
" the search fails, look for a file in parent paths, and if that fails,
" use the global fallback path instead.
function! ale#path#ResolveLocalPath(buffer, search_string, global_fallback) abort
    " Search for a locally installed file first.
    let l:path = ale#path#FindNearestFile(a:buffer, a:search_string)

    " If the serach fails, try the global executable instead.
    if empty(l:path)
        let l:path = a:global_fallback
    endif

    return l:path
endfunction

" Output 'cd <directory> && '
" This function can be used changing the directory for a linter command.
function! ale#path#CdString(directory) abort
    return 'cd ' . ale#Escape(a:directory) . ' && '
endfunction

" Output 'cd <buffer_filename_directory> && '
" This function can be used changing the directory for a linter command.
function! ale#path#BufferCdString(buffer) abort
    return ale#path#CdString(fnamemodify(bufname(a:buffer), ':p:h'))
endfunction

" Return 1 if a path is an absolute path.
function! ale#path#IsAbsolute(filename) abort
    if has('win32') && a:filename[:0] is# '\'
        return 1
    endif

    " Check for /foo and C:\foo, etc.
    return a:filename[:0] is# '/' || a:filename[1:2] is# ':\'
endfunction

let s:temp_dir = ale#path#Simplify(fnamemodify(tempname(), ':h'))

" Given a filename, return 1 if the file represents some temporary file
" created by Vim.
function! ale#path#IsTempName(filename) abort
    return ale#path#Simplify(a:filename)[:len(s:temp_dir) - 1] is# s:temp_dir
endfunction

" Given a base directory, which must not have a trailing slash, and a
" filename, which may have an absolute path a path relative to the base
" directory, return the absolute path to the file.
function! ale#path#GetAbsPath(base_directory, filename) abort
    if ale#path#IsAbsolute(a:filename)
        return ale#path#Simplify(a:filename)
    endif

    let l:sep = has('win32') ? '\' : '/'

    return ale#path#Simplify(a:base_directory . l:sep . a:filename)
endfunction

" Given a buffer number and a relative or absolute path, return 1 if the
" two paths represent the same file on disk.
function! ale#path#IsBufferPath(buffer, complex_filename) abort
    " If the path is one of many different names for stdin, we have a match.
    if a:complex_filename is# '-'
    \|| a:complex_filename is# 'stdin'
    \|| a:complex_filename[:0] is# '<'
        return 1
    endif

    let l:test_filename = ale#path#Simplify(a:complex_filename)

    if l:test_filename[:1] is# './'
        let l:test_filename = l:test_filename[2:]
    endif

    if l:test_filename[:1] is# '..'
        " Remove ../../ etc. from the front of the path.
        let l:test_filename = substitute(l:test_filename, '\v^(\.\.[/\\])+', '/', '')
    endif

    " Use the basename for temporary files, as they are likely our files.
    if ale#path#IsTempName(l:test_filename)
        let l:test_filename = fnamemodify(l:test_filename, ':t')
    endif

    let l:buffer_filename = expand('#' . a:buffer . ':p')

    return l:buffer_filename is# l:test_filename
    \   || l:buffer_filename[-len(l:test_filename):] is# l:test_filename
endfunction

" Given a path, return every component of the path, moving upwards.
function! ale#path#Upwards(path) abort
    let l:pattern = has('win32') ? '\v/+|\\+' : '\v/+'
    let l:sep = has('win32') ? '\' : '/'
    let l:parts = split(ale#path#Simplify(a:path), l:pattern)
    let l:path_list = []

    while !empty(l:parts)
        call add(l:path_list, join(l:parts, l:sep))
        let l:parts = l:parts[:-2]
    endwhile

    if has('win32') && a:path =~# '^[a-zA-z]:\'
        " Add \ to C: for C:\, etc.
        let l:path_list[-1] .= '\'
    elseif a:path[0] is# '/'
        " If the path starts with /, even on Windows, add / and / to all paths.
        call map(l:path_list, '''/'' . v:val')
        call add(l:path_list, '/')
    endif

    return l:path_list
endfunction

" Convert a filesystem path to a file:// URI
" relatives paths will not be prefixed with the protocol.
" For Windows paths, the `:` in C:\ etc. will not be percent-encoded.
function! ale#path#ToURI(path) abort
    let l:has_drive_letter = a:path[1:2] is# ':\'

    return substitute(
    \   ((l:has_drive_letter || a:path[:0] is# '/') ? 'file://' : '')
    \       . (l:has_drive_letter ? '/' . a:path[:2] : '')
    \       . ale#uri#Encode(l:has_drive_letter ? a:path[3:] : a:path),
    \   '\\',
    \   '/',
    \   'g',
    \)
endfunction

function! ale#path#FromURI(uri) abort
    let l:i = len('file://')
    let l:encoded_path = a:uri[: l:i - 1] is# 'file://' ? a:uri[l:i :] : a:uri

    let l:path = ale#uri#Decode(l:encoded_path)

    " If the path is like /C:/foo/bar, it should be C:\foo\bar instead.
    if l:path =~# '^/[a-zA-Z]:'
        let l:path = substitute(l:path[1:], '/', '\\', 'g')
    endif

    return l:path
endfunction
