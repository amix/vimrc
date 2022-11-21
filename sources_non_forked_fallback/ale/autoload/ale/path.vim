" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for working with paths in the filesystem.

" simplify a path, and fix annoying issues with paths on Windows.
"
" Forward slashes are changed to back slashes so path equality works better
" on Windows. Back slashes are changed to forward slashes on Unix.
"
" Unix paths can technically contain back slashes, but in practice no path
" should, and replacing back slashes with forward slashes makes linters work
" in environments like MSYS.
"
" Paths starting with more than one forward slash are changed to only one
" forward slash, to prevent the paths being treated as special MSYS paths.
function! ale#path#Simplify(path) abort
    if has('unix')
        let l:unix_path = substitute(a:path, '\\', '/', 'g')

        return substitute(simplify(l:unix_path), '^//\+', '/', 'g') " no-custom-checks
    endif

    let l:win_path = substitute(a:path, '/', '\\', 'g')

    return substitute(simplify(l:win_path), '^\\\+', '\', 'g') " no-custom-checks
endfunction

" Simplify a path without a Windows drive letter.
" This function can be used for checking if paths are equal.
function! ale#path#RemoveDriveLetter(path) abort
    return has('win32') && a:path[1:2] is# ':\'
    \   ? ale#path#Simplify(a:path[2:])
    \   : ale#path#Simplify(a:path)
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

" Given a buffer, a string to search for, and a global fallback for when
" the search fails, look for a file in parent paths, and if that fails,
" use the global fallback path instead.
function! ale#path#ResolveLocalPath(buffer, search_string, global_fallback) abort
    " Search for a locally installed file first.
    let l:path = ale#path#FindNearestFile(a:buffer, a:search_string)

    " If the search fails, try the global executable instead.
    if empty(l:path)
        let l:path = a:global_fallback
    endif

    return l:path
endfunction

" Given a buffer number, a base variable name, and a list of paths to search
" for in ancestor directories, detect the executable path for a program.
function! ale#path#FindNearestExecutable(buffer, path_list) abort
    for l:path in a:path_list
        if ale#path#IsAbsolute(l:path)
            let l:executable = filereadable(l:path) ? l:path : ''
        else
            let l:executable = ale#path#FindNearestFile(a:buffer, l:path)
        endif

        if !empty(l:executable)
            return l:executable
        endif
    endfor

    return ''
endfunction

" Given a buffer number, a base variable name, and a list of paths to search
" for in ancestor directories, detect the executable path for a program.
"
" The use_global and executable options for the relevant program will be used.
function! ale#path#FindExecutable(buffer, base_var_name, path_list) abort
    if ale#Var(a:buffer, a:base_var_name . '_use_global')
        return ale#Var(a:buffer, a:base_var_name . '_executable')
    endif

    let l:nearest = ale#path#FindNearestExecutable(a:buffer, a:path_list)

    if !empty(l:nearest)
        return l:nearest
    endif

    return ale#Var(a:buffer, a:base_var_name . '_executable')
endfunction

" Return 1 if a path is an absolute path.
function! ale#path#IsAbsolute(filename) abort
    if has('win32') && a:filename[:0] is# '\'
        return 1
    endif

    " Check for /foo and C:\foo, etc.
    return a:filename[:0] is# '/' || a:filename[1:2] is# ':\'
endfunction

let s:temp_dir = ale#path#Simplify(fnamemodify(ale#util#Tempname(), ':h:h'))

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

" Given a path, return the directory name for that path, with no trailing
" slashes. If the argument is empty(), return an empty string.
function! ale#path#Dirname(path) abort
    if empty(a:path)
        return ''
    endif

    " For /foo/bar/ we need :h:h to get /foo
    if a:path[-1:] is# '/' || (has('win32') && a:path[-1:] is# '\')
        return fnamemodify(a:path, ':h:h')
    endif

    return fnamemodify(a:path, ':h')
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
function! ale#path#ToFileURI(path) abort
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

function! ale#path#FromFileURI(uri) abort
    if a:uri[:6] is? 'file://'
        let l:encoded_path = a:uri[7:]
    elseif a:uri[:4] is? 'file:'
        let l:encoded_path = a:uri[5:]
    else
        let l:encoded_path = a:uri
    endif

    let l:path = ale#uri#Decode(l:encoded_path)

    " If the path is like /C:/foo/bar, it should be C:\foo\bar instead.
    if has('win32') && l:path =~# '^/[a-zA-Z][:|]'
        let l:path = substitute(l:path[1:], '/', '\\', 'g')
        let l:path = l:path[0] . ':' . l:path[2:]
    endif

    return l:path
endfunction
