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

" Create a temporary directory
function! syntastic#util#tmpdir() " {{{2
    let tempdir = ''

    if (has('unix') || has('mac')) && executable('mktemp')
        " TODO: option "-t" to mktemp(1) is not portable
        let tmp = $TMPDIR != '' ? $TMPDIR : $TMP != '' ? $TMP : '/tmp'
        let out = split(system('mktemp -q -d ' . tmp . '/vim-syntastic-' . getpid() . '-XXXXXXXX'), "\n")
        if v:shell_error == 0 && len(out) == 1
            let tempdir = out[0]
        endif
    endif

    if tempdir == ''
        if has('win32') || has('win64')
            let tempdir = $TEMP . syntastic#util#Slash() . 'vim-syntastic-' . getpid()
        elseif has('win32unix')
            let tempdir = s:CygwinPath('/tmp/vim-syntastic-'  . getpid())
        elseif $TMPDIR != ''
            let tempdir = $TMPDIR . '/vim-syntastic-' . getpid()
        else
            let tempdir = '/tmp/vim-syntastic-' . getpid()
        endif

        try
            call mkdir(tempdir, 'p', 0700)
        catch /\m^Vim\%((\a\+)\)\=:E739/
            call syntastic#log#error(v:exception)
            let tempdir = '.'
        endtry
    endif

    return tempdir
endfunction " }}}2

" Recursively remove a directory
function! syntastic#util#rmrf(what) " {{{2
    " try to make sure we don't delete directories we didn't create
    if a:what !~? 'vim-syntastic-'
        return
    endif

    if  getftype(a:what) ==# 'dir'
        if !exists('s:rmrf')
            let s:rmrf =
                \ has('unix') || has('mac') ? 'rm -rf' :
                \ has('win32') || has('win64') ? 'rmdir /S /Q' :
                \ has('win16') || has('win95') || has('dos16') || has('dos32') ? 'deltree /Y' : ''
        endif

        if s:rmrf != ''
            silent! call system(s:rmrf . ' ' . syntastic#util#shescape(a:what))
        else
            call s:_rmrf(a:what)
        endif
    else
        silent! call delete(a:what)
    endif
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
let s:_width = function(exists('*strwidth') ? 'strwidth' : 'strlen')
lockvar s:_width

function! syntastic#util#screenWidth(str, tabstop) " {{{2
    let chunks = split(a:str, "\t", 1)
    let width = s:_width(chunks[-1])
    for c in chunks[:-2]
        let cwidth = s:_width(c)
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
    let msg = join(map(chunks[:-2], 'v:val . repeat(" ", &tabstop - s:_width(v:val) % &tabstop)'), '') . chunks[-1]
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
        let p = split(globpath(here, a:what, 1), '\n')

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
function! syntastic#util#shexpand(string, ...) " {{{2
    return syntastic#util#shescape(a:0 ? expand(a:string, a:1) : expand(a:string, 1))
endfunction " }}}2

" Escape arguments
function! syntastic#util#argsescape(opt) " {{{2
    if type(a:opt) == type('') && a:opt != ''
        return [a:opt]
    elseif type(a:opt) == type([])
        return map(copy(a:opt), 'syntastic#util#shescape(v:val)')
    endif

    return []
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
    " call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, "applying filter:", rules)
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
    return reltime(g:_SYNTASTIC_START)
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
    let fkey = a:key
    if fkey[0] == '!'
        let fkey = fkey[1:]
        let not = 1
    else
        let not = 0
    endif

    if fkey ==? 'level'
        let op = not ? ' ==? ' : ' !=? '
        let ret = 'v:val["type"]' . op . string(a:term[0])
    elseif fkey ==? 'type'
        if a:term ==? 'style'
            let op = not ? ' ==? ' : ' !=? '
            let ret = 'get(v:val, "subtype", "")' . op . '"style"'
        else
            let op = not ? '!' : ''
            let ret = op . 'has_key(v:val, "subtype")'
        endif
    elseif fkey ==? 'regex'
        let op = not ? ' =~? ' : ' !~? '
        let ret = 'v:val["text"]' . op . string(a:term)
    elseif fkey ==? 'file' || fkey[:4] ==? 'file:'
        let op = not ? ' =~# ' : ' !~# '
        let ret = 'bufname(str2nr(v:val["bufnr"]))'
        let mod = fkey[4:]
        if mod != ''
            let ret = 'fnamemodify(' . ret . ', ' . string(mod) . ')'
        endif
        let ret .= op . string(a:term)
    else
        call syntastic#log#warn('quiet_messages: ignoring invalid key ' . strtrans(string(fkey)))
        let ret = "1"
    endif
    return ret
endfunction " }}}2

function! s:_rmrf(what) " {{{2
    if !exists('s:rmdir')
        let s:rmdir = syntastic#util#shescape(get(g:, 'netrw_localrmdir', 'rmdir'))
    endif

    if getftype(a:what) ==# 'dir'
        if filewritable(a:what) != 2
            return
        endif

        for f in split(globpath(a:what, '*', 1), "\n")
            call s:_rmrf(f)
        endfor
        silent! call system(s:rmdir . ' ' . syntastic#util#shescape(a:what))
    else
        silent! call delete(a:what)
    endif
endfunction " }}}2

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
