if exists('g:loaded_syntastic_util_autoload') || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_util_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" Public functions {{{1

function! syntastic#util#isRunningWindows() " {{{2
    return has('win16') || has('win32') || has('win64')
endfunction " }}}2

function! syntastic#util#DevNull() " {{{2
    if syntastic#util#isRunningWindows()
        return 'NUL'
    endif
    return '/dev/null'
endfunction " }}}2

" Get directory separator
function! syntastic#util#Slash() abort " {{{2
    return (!exists("+shellslash") || &shellslash) ? '/' : '\'
endfunction " }}}2

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
function! syntastic#util#parseShebang() " {{{2
    for lnum in range(1, 5)
        let line = getline(lnum)
        if line =~ '^#!'
            let line = substitute(line, '\v^#!\s*(\S+/env(\s+-\S+)*\s+)?', '', '')
            let exe = matchstr(line, '\m^\S*\ze')
            let args = split(matchstr(line, '\m^\S*\zs.*'))
            return { 'exe': exe, 'args': args }
        endif
    endfor

    return { 'exe': '', 'args': [] }
endfunction " }}}2

" Get the value of a variable.  Allow local variables to override global ones.
function! syntastic#util#var(name, ...) " {{{2
    return
        \ exists('b:syntastic_' . a:name) ? b:syntastic_{a:name} :
        \ exists('g:syntastic_' . a:name) ? g:syntastic_{a:name} :
        \ a:0 > 0 ? a:1 : ''
endfunction " }}}2

" Parse a version string.  Return an array of version components.
function! syntastic#util#parseVersion(version) " {{{2
    return map(split(matchstr( a:version, '\v^\D*\zs\d+(\.\d+)+\ze' ), '\m\.'), 'str2nr(v:val)')
endfunction " }}}2

" Run 'command' in a shell and parse output as a version string.
" Returns an array of version components.
function! syntastic#util#getVersion(command) " {{{2
    return syntastic#util#parseVersion(system(a:command))
endfunction " }}}2

" Verify that the 'installed' version is at least the 'required' version.
"
" 'installed' and 'required' must be arrays. If they have different lengths,
" the "missing" elements will be assumed to be 0 for the purposes of checking.
"
" See http://semver.org for info about version numbers.
function! syntastic#util#versionIsAtLeast(installed, required) " {{{2
    return syntastic#util#compareLexi(a:installed, a:required) >= 0
endfunction " }}}2

" Almost lexicographic comparison of two lists of integers. :) If lists
" have different lengths, the "missing" elements are assumed to be 0.
function! syntastic#util#compareLexi(a, b) " {{{2
    for idx in range(max([len(a:a), len(a:b)]))
        let a_element = str2nr(get(a:a, idx, 0))
        let b_element = str2nr(get(a:b, idx, 0))
        if a_element != b_element
            return a_element > b_element ? 1 : -1
        endif
    endfor
    " Everything matched, so it is at least the required version.
    return 0
endfunction " }}}2

" strwidth() was added in Vim 7.3; if it doesn't exist, we use strlen()
" and hope for the best :)
let s:width = function(exists('*strwidth') ? 'strwidth' : 'strlen')
lockvar s:width

function! syntastic#util#screenWidth(str, tabstop) " {{{2
    let chunks = split(a:str, "\t", 1)
    let width = s:width(chunks[-1])
    for c in chunks[:-2]
        let cwidth = s:width(c)
        let width += cwidth + a:tabstop - cwidth % a:tabstop
    endfor
    return width
endfunction " }}}2

"print as much of a:msg as possible without "Press Enter" prompt appearing
function! syntastic#util#wideMsg(msg) " {{{2
    let old_ruler = &ruler
    let old_showcmd = &showcmd

    "This is here because it is possible for some error messages to
    "begin with \n which will cause a "press enter" prompt.
    let msg = substitute(a:msg, "\n", "", "g")

    "convert tabs to spaces so that the tabs count towards the window
    "width as the proper amount of characters
    let chunks = split(msg, "\t", 1)
    let msg = join(map(chunks[:-2], 'v:val . repeat(" ", &tabstop - s:width(v:val) % &tabstop)'), '') . chunks[-1]
    let msg = strpart(msg, 0, &columns - 1)

    set noruler noshowcmd
    call syntastic#util#redraw(0)

    echo msg

    let &ruler = old_ruler
    let &showcmd = old_showcmd
endfunction " }}}2

" Check whether a buffer is loaded, listed, and not hidden
function! syntastic#util#bufIsActive(buffer) " {{{2
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
endfunction " }}}2

" start in directory a:where and walk up the parent folders until it
" finds a file matching a:what; return path to that file
function! syntastic#util#findInParent(what, where) " {{{2
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
endfunction " }}}2

" Returns unique elements in a list
function! syntastic#util#unique(list) " {{{2
    let seen = {}
    let uniques = []
    for e in a:list
        if !has_key(seen, e)
            let seen[e] = 1
            call add(uniques, e)
        endif
    endfor
    return uniques
endfunction " }}}2

" A less noisy shellescape()
function! syntastic#util#shescape(string) " {{{2
    return a:string =~ '\m^[A-Za-z0-9_/.-]\+$' ? a:string : shellescape(a:string)
endfunction " }}}2

" A less noisy shellescape(expand())
function! syntastic#util#shexpand(string) " {{{2
    return syntastic#util#shescape(expand(a:string))
endfunction " }}}2

" decode XML entities
function! syntastic#util#decodeXMLEntities(string) " {{{2
    let str = a:string
    let str = substitute(str, '\m&lt;', '<', 'g')
    let str = substitute(str, '\m&gt;', '>', 'g')
    let str = substitute(str, '\m&quot;', '"', 'g')
    let str = substitute(str, '\m&apos;', "'", 'g')
    let str = substitute(str, '\m&amp;', '\&', 'g')
    return str
endfunction " }}}2

function! syntastic#util#redraw(full) " {{{2
    if a:full
        redraw!
    else
        redraw
    endif
endfunction " }}}2

function! syntastic#util#dictFilter(errors, filter) " {{{2
    let rules = s:_translateFilter(a:filter)
    " call syntastic#log#debug(g:SyntasticDebugFilters, "applying filter:", rules)
    try
        call filter(a:errors, rules)
    catch /\m^Vim\%((\a\+)\)\=:E/
        let msg = matchstr(v:exception, '\m^Vim\%((\a\+)\)\=:\zs.*')
        call syntastic#log#error('quiet_messages: ' . msg)
    endtry
endfunction " }}}2

" Return a [high, low] list of integers, representing the time
" (hopefully high resolution) since program start
" TODO: This assumes reltime() returns a list of integers.
function! syntastic#util#stamp() " {{{2
    return reltime(g:syntastic_start)
endfunction " }}}2

" }}}1

" Private functions {{{1

function! s:_translateFilter(filters) " {{{2
    let conditions = []
    for k in keys(a:filters)
        if type(a:filters[k]) == type([])
            call extend(conditions, map(copy(a:filters[k]), 's:_translateElement(k, v:val)'))
        else
            call add(conditions, s:_translateElement(k, a:filters[k]))
        endif
    endfor

    if conditions == []
        let conditions = ["1"]
    endif
    return len(conditions) == 1 ? conditions[0] : join(map(conditions, '"(" . v:val . ")"'), ' && ')
endfunction " }}}2

function! s:_translateElement(key, term) " {{{2
    if a:key ==? 'level'
        let ret = 'v:val["type"] !=? ' . string(a:term[0])
    elseif a:key ==? 'type'
        let ret = a:term ==? 'style' ? 'get(v:val, "subtype", "") !=? "style"' : 'has_key(v:val, "subtype")'
    elseif a:key ==? 'regex'
        let ret = 'v:val["text"] !~? ' . string(a:term)
    elseif a:key ==? 'file'
        let ret = 'bufname(str2nr(v:val["bufnr"])) !~# ' . string(a:term)
    else
        call syntastic#log#warn('quiet_messages: ignoring invalid key ' . strtrans(string(a:key)))
        let ret = "1"
    endif
    return ret
endfunction " }}}2

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
