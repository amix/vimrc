if exists('g:loaded_syntastic_util_autoload')
    finish
endif
let g:loaded_syntastic_util_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" strwidth() was added in Vim 7.3; if it doesn't exist, we use strlen()
" and hope for the best :)
let s:width = function(exists('*strwidth') ? 'strwidth' : 'strlen')

" Public functions {{{1

function! syntastic#util#isRunningWindows()
    return has('win16') || has('win32') || has('win64')
endfunction

function! syntastic#util#DevNull()
    if syntastic#util#isRunningWindows()
        return 'NUL'
    endif
    return '/dev/null'
endfunction

" Get directory separator
function! syntastic#util#Slash() abort
    return (!exists("+shellslash") || &shellslash) ? '/' : '\'
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
            let exe = matchstr(line, '\m^#!\s*\zs[^ \t]*')
            let args = split(matchstr(line, '\m^#!\s*[^ \t]*\zs.*'))
            return { 'exe': exe, 'args': args }
        endif
    endfor

    return { 'exe': '', 'args': [] }
endfunction

" Parse a version string.  Return an array of version components.
function! syntastic#util#parseVersion(version)
    return split(matchstr( a:version, '\v^\D*\zs\d+(\.\d+)+\ze' ), '\m\.')
endfunction

" Run 'command' in a shell and parse output as a version string.
" Returns an array of version components.
function! syntastic#util#getVersion(command)
    return syntastic#util#parseVersion(system(a:command))
endfunction

" Verify that the 'installed' version is at least the 'required' version.
"
" 'installed' and 'required' must be arrays. If they have different lengths,
" the "missing" elements will be assumed to be 0 for the purposes of checking.
"
" See http://semver.org for info about version numbers.
function! syntastic#util#versionIsAtLeast(installed, required)
    for idx in range(max([len(a:installed), len(a:required)]))
        let installed_element = get(a:installed, idx, 0)
        let required_element = get(a:required, idx, 0)
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

    "This is here because it is possible for some error messages to
    "begin with \n which will cause a "press enter" prompt.
    let msg = substitute(a:msg, "\n", "", "g")

    "convert tabs to spaces so that the tabs count towards the window
    "width as the proper amount of characters
    let chunks = split(msg, "\t", 1)
    let msg = join(map(chunks[:-2], 'v:val . repeat(" ", &ts - s:width(v:val) % &ts)'), '') . chunks[-1]
    let msg = strpart(msg, 0, winwidth(0) - 1)

    set noruler noshowcmd
    call syntastic#util#redraw(0)

    echo msg

    let &ruler = old_ruler
    let &showcmd = old_showcmd
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

    let root = syntastic#util#Slash()
    if syntastic#util#isRunningWindows() && here[1] == ':'
        " The drive letter is an ever-green source of fun.  That's because
        " we don't care about running syntastic on Amiga these days. ;)
        let root = fnamemodify(root, ':p')
        let root = here[0] . root[1:]
    endif

    let old = ''
    while here != ''
        let p = split(globpath(here, a:what), '\n')

        if !empty(p)
            return fnamemodify(p[0], ':p')
        elseif here ==? root || here ==? old
            break
        endif

        let old = here

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
    return a:string =~ '\m^[A-Za-z0-9_/.-]\+$' ? a:string : shellescape(a:string)
endfunction

" A less noisy shellescape(expand())
function! syntastic#util#shexpand(string)
    return syntastic#util#shescape(expand(a:string))
endfunction

" decode XML entities
function! syntastic#util#decodeXMLEntities(string)
    let str = a:string
    let str = substitute(str, '\m&lt;', '<', 'g')
    let str = substitute(str, '\m&gt;', '>', 'g')
    let str = substitute(str, '\m&quot;', '"', 'g')
    let str = substitute(str, '\m&apos;', "'", 'g')
    let str = substitute(str, '\m&amp;', '\&', 'g')
    return str
endfunction

function! syntastic#util#redraw(full)
    if a:full
        redraw!
    else
        redraw
    endif
endfunction

function! syntastic#util#dictFilter(errors, filter)
    let rules = s:translateFilter(a:filter)
    " call syntastic#log#debug(g:SyntasticDebugFilters, "applying filter:", rules)
    try
        call filter(a:errors, rules)
    catch /\m^Vim\%((\a\+)\)\=:E/
        let msg = matchstr(v:exception, '\m^Vim\%((\a\+)\)\=:\zs.*')
        call syntastic#log#error('quiet_messages: ' . msg)
    endtry
endfunction

" Private functions {{{1

function! s:translateFilter(filters)
    let conditions = []
    for [k, v] in items(a:filters)
        if type(v) == type([])
            call extend(conditions, map(copy(v), 's:translateElement(k, v:val)'))
        else
            call add(conditions, s:translateElement(k, v))
        endif
    endfor
    return len(conditions) == 1 ? conditions[0] : join(map(conditions, '"(" . v:val . ")"'), ' && ')
endfunction

function! s:translateElement(key, term)
    if a:key ==? 'level'
        let ret = 'v:val["type"] !=? ' . string(a:term[0])
    elseif a:key ==? 'type'
        let ret = a:term ==? 'style' ? 'get(v:val, "subtype", "") !=? "style"' : 'has_key(v:val, "subtype")'
    elseif a:key ==? 'regex'
        let ret = 'v:val["text"] !~? ' . string(a:term)
    elseif a:key ==? 'file'
        let ret = 'bufname(str2nr(v:val["bufnr"])) !~# ' . string(a:term)
    else
        let ret = "1"
    endif
    return ret
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: set et sts=4 sw=4 fdm=marker:
