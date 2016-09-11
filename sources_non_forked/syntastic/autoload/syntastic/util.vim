if exists('g:loaded_syntastic_util_autoload') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_util_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

" Public functions {{{1

function! syntastic#util#isRunningWindows() abort " {{{2
    return has('win16') || has('win32') || has('win64')
endfunction " }}}2

function! syntastic#util#DevNull() abort " {{{2
    if syntastic#util#isRunningWindows()
        return 'NUL'
    endif
    return '/dev/null'
endfunction " }}}2

" Get directory separator
function! syntastic#util#Slash() abort " {{{2
    return (!exists('+shellslash') || &shellslash) ? '/' : '\'
endfunction " }}}2

function! syntastic#util#CygwinPath(path) abort " {{{2
    return substitute(syntastic#util#system('cygpath -m ' . syntastic#util#shescape(a:path)), "\n", '', 'g')
endfunction " }}}2

function! syntastic#util#system(command) abort " {{{2
    let old_shell = &shell
    let old_lc_messages = $LC_MESSAGES
    let old_lc_all = $LC_ALL

    let &shell = syntastic#util#var('shell')
    let $LC_MESSAGES = 'C'
    let $LC_ALL = ''

    let cmd_start = reltime()
    let out = system(a:command)
    let cmd_time = split(reltimestr(reltime(cmd_start)))[0]

    let $LC_ALL = old_lc_all
    let $LC_MESSAGES = old_lc_messages

    let &shell = old_shell

    if exists('g:_SYNTASTIC_DEBUG_TRACE')
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'system: command run in ' . cmd_time . 's')
    endif

    return out
endfunction " }}}2

" Create a temporary directory
function! syntastic#util#tmpdir() abort " {{{2
    let tempdir = ''

    if (has('unix') || has('mac')) && executable('mktemp') && !has('win32unix')
        " TODO: option "-t" to mktemp(1) is not portable
        let tmp = $TMPDIR !=# '' ? $TMPDIR : $TMP !=# '' ? $TMP : '/tmp'
        let out = split(syntastic#util#system('mktemp -q -d ' . tmp . '/vim-syntastic-' . getpid() . '-XXXXXXXX'), "\n")
        if v:shell_error == 0 && len(out) == 1
            let tempdir = out[0]
        endif
    endif

    if tempdir ==# ''
        if has('win32') || has('win64')
            let tempdir = $TEMP . syntastic#util#Slash() . 'vim-syntastic-' . getpid()
        elseif has('win32unix')
            let tempdir = syntastic#util#CygwinPath('/tmp/vim-syntastic-'  . getpid())
        elseif $TMPDIR !=# ''
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
function! syntastic#util#rmrf(what) abort " {{{2
    " try to make sure we don't delete directories we didn't create
    if a:what !~? 'vim-syntastic-'
        return
    endif

    if  getftype(a:what) ==# 'dir'
        call s:_delete(a:what, 'rf')
    else
        silent! call delete(a:what)
    endif
endfunction " }}}2

" Search the first 5 lines of the file for a magic number and return a map
" containing the args and the executable
"
" e.g.
"
" #!/usr/bin/perl -f -bar
"
" returns
"
" {'exe': '/usr/bin/perl', 'args': ['-f', '-bar']}
function! syntastic#util#parseShebang() abort " {{{2
    for lnum in range(1, 5)
        let line = getline(lnum)
        if line =~# '^#!'
            let line = substitute(line, '\v^#!\s*(\S+/env(\s+-\S+)*\s+)?', '', '')
            let exe = matchstr(line, '\m^\S*\ze')
            let args = split(matchstr(line, '\m^\S*\zs.*'))
            return { 'exe': exe, 'args': args }
        endif
    endfor

    return { 'exe': '', 'args': [] }
endfunction " }}}2

" Get the value of a Vim variable.  Allow local variables to override global ones.
function! syntastic#util#rawVar(name, ...) abort " {{{2
    return get(b:, a:name, get(g:, a:name, a:0 > 0 ? a:1 : ''))
endfunction " }}}2

" Get the value of a syntastic variable.  Allow local variables to override global ones.
function! syntastic#util#var(name, ...) abort " {{{2
    return call('syntastic#util#rawVar', ['syntastic_' . a:name] + a:000)
endfunction " }}}2

" Parse a version string.  Return an array of version components.
function! syntastic#util#parseVersion(version, ...) abort " {{{2
    return map(split(matchstr( a:version, a:0 ? a:1 : '\v^\D*\zs\d+(\.\d+)+\ze' ), '\m\.'), 'str2nr(v:val)')
endfunction " }}}2

" Verify that the 'installed' version is at least the 'required' version.
"
" 'installed' and 'required' must be arrays. If they have different lengths,
" the "missing" elements will be assumed to be 0 for the purposes of checking.
"
" See http://semver.org for info about version numbers.
function! syntastic#util#versionIsAtLeast(installed, required) abort " {{{2
    return syntastic#util#compareLexi(a:installed, a:required) >= 0
endfunction " }}}2

" Almost lexicographic comparison of two lists of integers. :) If lists
" have different lengths, the "missing" elements are assumed to be 0.
function! syntastic#util#compareLexi(a, b) abort " {{{2
    for idx in range(max([len(a:a), len(a:b)]))
        let a_element = str2nr(get(a:a, idx, 0))
        let b_element = str2nr(get(a:b, idx, 0))
        if a_element != b_element
            return a_element > b_element ? 1 : -1
        endif
    endfor
    " still here, thus everything matched
    return 0
endfunction " }}}2

" strwidth() was added in Vim 7.3; if it doesn't exist, we use strlen()
" and hope for the best :)
let s:_width = function(exists('*strwidth') ? 'strwidth' : 'strlen')
lockvar s:_width

function! syntastic#util#screenWidth(str, tabstop) abort " {{{2
    let chunks = split(a:str, "\t", 1)
    let width = s:_width(chunks[-1])
    for c in chunks[:-2]
        let cwidth = s:_width(c)
        let width += cwidth + a:tabstop - cwidth % a:tabstop
    endfor
    return width
endfunction " }}}2

" Print as much of a:msg as possible without "Press Enter" prompt appearing
function! syntastic#util#wideMsg(msg) abort " {{{2
    let old_ruler = &ruler
    let old_showcmd = &showcmd

    "This is here because it is possible for some error messages to
    "begin with \n which will cause a "press enter" prompt.
    let msg = substitute(a:msg, "\n", '', 'g')

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
function! syntastic#util#bufIsActive(buffer) abort " {{{2
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

" Start in directory a:where and walk up the parent folders until it finds a
" file named a:what; return path to that file
function! syntastic#util#findFileInParent(what, where) abort " {{{2
    let old_suffixesadd = &suffixesadd
    let &suffixesadd = ''
    let file = findfile(a:what, escape(a:where, ' ') . ';')
    let &suffixesadd = old_suffixesadd
    return file
endfunction " }}}2

" Start in directory a:where and walk up the parent folders until it finds a
" file matching a:what; return path to that file
function! syntastic#util#findGlobInParent(what, where) abort " {{{2
    let here = fnamemodify(a:where, ':p')

    let root = syntastic#util#Slash()
    if syntastic#util#isRunningWindows() && here[1] ==# ':'
        " The drive letter is an ever-green source of fun.  That's because
        " we don't care about running syntastic on Amiga these days. ;)
        let root = fnamemodify(root, ':p')
        let root = here[0] . root[1:]
    endif

    let old = ''
    while here !=# ''
        try
            " Vim 7.4.279 and later
            let p = globpath(here, a:what, 1, 1)
        catch /\m^Vim\%((\a\+)\)\=:E118/
            let p = split(globpath(here, a:what, 1), "\n")
        endtry

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
function! syntastic#util#unique(list) abort " {{{2
    let seen = {}
    let uniques = []
    for e in a:list
        let k = string(e)
        if !has_key(seen, k)
            let seen[k] = 1
            call add(uniques, e)
        endif
    endfor
    return uniques
endfunction " }}}2

" A less noisy shellescape()
function! syntastic#util#shescape(string) abort " {{{2
    return a:string =~# '\m^[A-Za-z0-9_/.-]\+$' ? a:string : shellescape(a:string)
endfunction " }}}2

" A less noisy shellescape(expand())
function! syntastic#util#shexpand(string, ...) abort " {{{2
    return syntastic#util#shescape(a:0 ? expand(a:string, a:1) : expand(a:string, 1))
endfunction " }}}2

" Escape arguments
function! syntastic#util#argsescape(opt) abort " {{{2
    if type(a:opt) == type('') && a:opt !=# ''
        return [a:opt]
    elseif type(a:opt) == type([])
        return map(copy(a:opt), 'syntastic#util#shescape(v:val)')
    endif

    return []
endfunction " }}}2

" Decode XML entities
function! syntastic#util#decodeXMLEntities(string) abort " {{{2
    let str = a:string
    let str = substitute(str, '\m&lt;', '<', 'g')
    let str = substitute(str, '\m&gt;', '>', 'g')
    let str = substitute(str, '\m&quot;', '"', 'g')
    let str = substitute(str, '\m&apos;', "'", 'g')
    let str = substitute(str, '\m&amp;', '\&', 'g')
    return str
endfunction " }}}2

function! syntastic#util#redraw(full) abort " {{{2
    if a:full
        redraw!
    else
        redraw
    endif
endfunction " }}}2

function! syntastic#util#dictFilter(errors, filter) abort " {{{2
    let rules = s:_translateFilter(a:filter)
    " call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, "applying filter:", rules)
    try
        call filter(a:errors, rules)
    catch /\m^Vim\%((\a\+)\)\=:E/
        let msg = matchstr(v:exception, '\m^Vim\%((\a\+)\)\=:\zs.*')
        call syntastic#log#error('quiet_messages: ' . msg)
    endtry
endfunction " }}}2

" Return a [seconds, fractions] list of strings, representing the
" (hopefully high resolution) time since program start
function! syntastic#util#stamp() abort " {{{2
    return split( split(reltimestr(reltime(g:_SYNTASTIC_START)))[0], '\.' )
endfunction " }}}2

function! syntastic#util#setChangedtick() abort " {{{2
    unlockvar! b:syntastic_changedtick
    let b:syntastic_changedtick = b:changedtick
    lockvar! b:syntastic_changedtick
endfunction " }}}2

let s:_wid_base = 'syntastic_' . getpid() . '_' . reltimestr(g:_SYNTASTIC_START) . '_'
let s:_wid_pool = 0

" Add unique IDs to windows
function! syntastic#util#setWids() abort " {{{2
    for tab in range(1, tabpagenr('$'))
        for win in range(1, tabpagewinnr(tab, '$'))
            if gettabwinvar(tab, win, 'syntastic_wid') ==# ''
                call settabwinvar(tab, win, 'syntastic_wid', s:_wid_base . s:_wid_pool)
                let s:_wid_pool += 1
            endif
        endfor
    endfor
endfunction " }}}2

let s:_str2float = function(exists('*str2float') ? 'str2float' : 'str2nr')
lockvar s:_str2float

function! syntastic#util#str2float(val) abort " {{{2
    return s:_str2float(a:val)
endfunction " }}}2

function! syntastic#util#float2str(val) abort " {{{2
    return s:_float2str(a:val)
endfunction " }}}2

" Crude printf()-like width formatter.  Handles wide characters.
function! syntastic#util#wformat(format, str) abort " {{{2
    if a:format ==# ''
        return a:str
    endif

 echomsg string(a:format) . ', ' . string(a:str)
    let specs = matchlist(a:format, '\v^(-?)(0?)(%([1-9]\d*))?%(\.(\d+))?$')
    if len(specs) < 5
        return a:str
    endif

    let flushleft = specs[1] ==# '-'
    let lpad = specs[2] ==# '0' ? '0' : ' '
    let minlen = str2nr(specs[3])
    let maxlen = str2nr(specs[4])
    let out = substitute(a:str, "\t", ' ', 'g')

    if maxlen && s:_width(out) > maxlen
        let chars = filter(split(out, '\zs\ze', 1), 'v:val !=# ""')
        let out = ''

        if flushleft
            for c in chars
                if s:_width(out . c) < maxlen
                    let out .= c
                else
                    let out .= &encoding ==# 'utf-8' && &termencoding ==# 'utf-8' ? "\u2026" : '>'
                    break
                endif
            endfor
        else
            call reverse(chars)
            for c in chars
                if s:_width(c . out) < maxlen
                    let out = c . out
                else
                    let out = (&encoding ==# 'utf-8' && &termencoding ==# 'utf-8' ? "\u2026" : '<') . out
                    break
                endif
            endfor
        endif
    endif

    if minlen && s:_width(out) < minlen
        if flushleft
            let out .= repeat(' ', minlen - s:_width(out))
        else
            let out = repeat(lpad, minlen - s:_width(out)) . out
        endif
    endif

    return out
endfunction " }}}2

" }}}1

" Private functions {{{1

function! s:_translateFilter(filters) abort " {{{2
    let conditions = []
    for k in keys(a:filters)
        if type(a:filters[k]) == type([])
            call extend(conditions, map(copy(a:filters[k]), 's:_translateElement(k, v:val)'))
        else
            call add(conditions, s:_translateElement(k, a:filters[k]))
        endif
    endfor

    if conditions == []
        let conditions = ['1']
    endif
    return len(conditions) == 1 ? conditions[0] : join(map(conditions, '"(" . v:val . ")"'), ' && ')
endfunction " }}}2

function! s:_translateElement(key, term) abort " {{{2
    let fkey = a:key
    if fkey[0] ==# '!'
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
        if mod !=# ''
            let ret = 'fnamemodify(' . ret . ', ' . string(mod) . ')'
        endif
        let ret .= op . string(a:term)
    else
        call syntastic#log#warn('quiet_messages: ignoring invalid key ' . strtrans(string(fkey)))
        let ret = '1'
    endif
    return ret
endfunction " }}}2

" @vimlint(EVL103, 1, a:flags)
function! s:_delete_dumb(what, flags) abort " {{{2
    if !exists('s:rmrf')
        let s:rmrf =
            \ has('unix') || has('mac') ? 'rm -rf' :
            \ has('win32') || has('win64') ? 'rmdir /S /Q' :
            \ has('win16') || has('win95') || has('dos16') || has('dos32') ? 'deltree /Y' : ''
    endif

    if s:rmrf !=# ''
        silent! call syntastic#util#system(s:rmrf . ' ' . syntastic#util#shescape(a:what))
    else
        call s:_rmrf(a:what)
    endif
endfunction " }}}2
" @vimlint(EVL103, 0, a:flags)

" delete(dir, 'rf') was added in Vim 7.4.1107, but it didn't become usable until 7.4.1128
let s:_delete = function(v:version > 704 || (v:version == 704 && has('patch1128')) ? 'delete' : 's:_delete_dumb')
lockvar s:_delete

function! s:_rmrf(what) abort " {{{2
    if !exists('s:rmdir')
        let s:rmdir = syntastic#util#shescape(get(g:, 'netrw_localrmdir', 'rmdir'))
    endif

    if getftype(a:what) ==# 'dir'
        if filewritable(a:what) != 2
            return
        endif

        try
            " Vim 7.4.279 and later
            let entries = globpath(a:what, '*', 1, 1)
        catch /\m^Vim\%((\a\+)\)\=:E118/
            let entries = split(globpath(a:what, '*', 1), "\n")
        endtry
        for f in entries
            call s:_rmrf(f)
        endfor
        silent! call syntastic#util#system(s:rmdir . ' ' . syntastic#util#shescape(a:what))
    else
        silent! call delete(a:what)
    endif
endfunction " }}}2

function! s:_float2str_smart(val) abort " {{{2
    return printf('%.1f', a:val)
endfunction " }}}2

function! s:_float2str_dumb(val) abort " {{{2
    return a:val
endfunction " }}}2

let s:_float2str = function(has('float') ? 's:_float2str_smart' : 's:_float2str_dumb')
lockvar s:_float2str

" }}}1

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
