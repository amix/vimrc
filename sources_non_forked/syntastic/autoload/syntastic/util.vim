if exists("g:loaded_syntastic_util_autoload")
    finish
endif
let g:loaded_syntastic_util_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists("g:syntastic_debug")
    let g:syntastic_debug = 0
endif

let s:deprecationNoticesIssued = []

function! syntastic#util#DevNull()
    if has('win32')
        return 'NUL'
    endif
    return '/dev/null'
endfunction

"search the first 5 lines of the file for a magic number and return a map
"containing the args and the executable
"
"e.g.
"
"#!/usr/bin/perl -f -bar
"
"returns
"
"{'exe': '/usr/bin/perl', 'args': ['-f', '-bar']}
function! syntastic#util#parseShebang()
    for lnum in range(1,5)
        let line = getline(lnum)

        if line =~ '^#!'
            let exe = matchstr(line, '^#!\s*\zs[^ \t]*')
            let args = split(matchstr(line, '^#!\s*[^ \t]*\zs.*'))
            return {'exe': exe, 'args': args}
        endif
    endfor

    return {'exe': '', 'args': []}
endfunction

" Run 'command' in a shell and parse output as a version string.
" Returns an array of version components.
function! syntastic#util#parseVersion(command)
    return split(matchstr( system(a:command), '\v^\D*\zs\d+(\.\d+)+\ze' ), '\.')
endfunction

" Verify that the 'installed' version is at least the 'required' version.
"
" 'installed' and 'required' must be arrays. If they have different lengths,
" the "missing" elements will be assumed to be 0 for the purposes of checking.
"
" See http://semver.org for info about version numbers.
function! syntastic#util#versionIsAtLeast(installed, required)
    for index in range(max([len(a:installed), len(a:required)]))
        if len(a:installed) <= index
            let installed_element = 0
        else
            let installed_element = a:installed[index]
        endif
        if len(a:required) <= index
            let required_element = 0
        else
            let required_element = a:required[index]
        endif
        if installed_element != required_element
            return installed_element > required_element
        endif
    endfor
    " Everything matched, so it is at least the required version.
    return 1
endfunction

"print as much of a:msg as possible without "Press Enter" prompt appearing
function! syntastic#util#wideMsg(msg)
    let old_ruler = &ruler
    let old_showcmd = &showcmd

    "convert tabs to spaces so that the tabs count towards the window width
    "as the proper amount of characters
    let msg = substitute(a:msg, "\t", repeat(" ", &tabstop), "g")
    let msg = strpart(msg, 0, winwidth(0)-1)

    "This is here because it is possible for some error messages to begin with
    "\n which will cause a "press enter" prompt. I have noticed this in the
    "javascript:jshint checker and have been unable to figure out why it
    "happens
    let msg = substitute(msg, "\n", "", "g")

    set noruler noshowcmd
    redraw

    echo msg

    let &ruler=old_ruler
    let &showcmd=old_showcmd
endfunction

" Check whether a buffer is loaded, listed, and not hidden
function! syntastic#util#bufIsActive(buffer)
    " convert to number, or hell breaks loose
    let buf = str2nr(a:buffer)

    if !bufloaded(buf) || !buflisted(buf)
        return 0
    endif

    " get rid of hidden buffers
    for tab in range(1, tabpagenr('$'))
        if index(tabpagebuflist(tab), buf) >= 0
            return 1
        endif
    endfor

    return 0
endfunction

" start in directory a:where and walk up the parent folders until it
" finds a file matching a:what; return path to that file
function! syntastic#util#findInParent(what, where)
    let here = fnamemodify(a:where, ':p')

    while !empty(here)
        let p = split(globpath(here, a:what), '\n')

        if !empty(p)
            return fnamemodify(p[0], ':p')
        elseif here == '/'
            break
        endif

        " we use ':h:h' rather than ':h' since ':p' adds a trailing '/'
        " if 'here' is a directory
        let here = fnamemodify(here, ':p:h:h')
    endwhile

    return ''
endfunction

" Returns unique elements in a list
function! syntastic#util#unique(list)
    let seen = {}
    let uniques = []
    for e in a:list
        if !has_key(seen, e)
            let seen[e] = 1
            call add(uniques, e)
        endif
    endfor
    return uniques
endfunction

" A less noisy shellescape()
function! syntastic#util#shescape(string)
    return a:string =~ '\m^[A-Za-z0-9_/.-]\+$' ? a:string : shellescape(a:string, 1)
endfunction

" A less noisy shellescape(expand())
function! syntastic#util#shexpand(string)
    return syntastic#util#shescape(escape(expand(a:string), '|'))
endfunction

function! syntastic#util#debug(msg)
    if g:syntastic_debug
        echomsg "syntastic: debug: " . a:msg
    endif
endfunction

function! syntastic#util#info(msg)
    echomsg "syntastic: info: " . a:msg
endfunction

function! syntastic#util#warn(msg)
    echohl WarningMsg
    echomsg "syntastic: warning: " . a:msg
    echohl None
endfunction

function! syntastic#util#error(msg)
    execute "normal \<Esc>"
    echohl ErrorMsg
    echomsg "syntastic: error: " . a:msg
    echohl None
endfunction

function! syntastic#util#deprecationWarn(msg)
    if index(s:deprecationNoticesIssued, a:msg) >= 0
        return
    endif

    call add(s:deprecationNoticesIssued, a:msg)
    call syntastic#util#warn(a:msg)
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: set et sts=4 sw=4:
