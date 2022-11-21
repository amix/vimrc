" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    225


if !exists('g:tlib#file#drop')
    " If true, use |:drop| to edit loaded buffers (only available with GUI).
    let g:tlib#file#drop = has('gui')   "{{{2
endif


if !exists('g:tlib#file#use_tabs')
    let g:tlib#file#use_tabs = 0   "{{{2
endif


if !exists('g:tlib#file#edit_cmds')
    let g:tlib#file#edit_cmds = g:tlib#file#use_tabs ? {'buffer': 'tab split | buffer', 'edit': 'tabedit'} : {}  "{{{2
endif


if !exists('g:tlib#file#absolute_filename_rx')
    let g:tlib#file#absolute_filename_rx = '^\~\?[\/]'   "{{{2
endif


if !exists('g:tlib#file#reject_rx')
    let g:tlib#file#reject_rx = '\%(^\|[\/]\)\%(tags\|Thumbs\.db\)$'   "{{{2
endif


""" File related {{{1
" For the following functions please see ../../test/tlib.vim for examples.


" EXAMPLES: >
"   tlib#file#Split('foo/bar/filename.txt')
"   => ['foo', 'bar', 'filename.txt']
function! tlib#file#Split(filename) abort "{{{3
    let prefix = matchstr(a:filename, '^\(\w\+:\)\?/\+')
    Tlibtrace 'tlib', prefix
    if !empty(prefix)
        let filename = a:filename[len(prefix) : -1]
    else
        let filename = a:filename
    endif
    let rv = split(filename, '[\/]')
    " let rv = split(filename, '[\/]', 1)
    if !empty(prefix)
        call insert(rv, prefix[0:-2])
    endif
    return rv
endf


" :display: tlib#file#Join(filename_parts, ?strip_slashes=1, ?maybe_absolute=0)
" EXAMPLES: >
"   tlib#file#Join(['foo', 'bar', 'filename.txt'])
"   => 'foo/bar/filename.txt'
function! tlib#file#Join(filename_parts, ...) abort "{{{3
    TVarArg ['strip_slashes', 1], 'maybe_absolute'
    Tlibtrace 'tlib', a:filename_parts, strip_slashes
    if maybe_absolute
        let filename_parts = []
        for part in a:filename_parts
            if part =~ g:tlib#file#absolute_filename_rx
                let filename_parts = []
            endif
            call add(filename_parts, part)
        endfor
    else
        let filename_parts = a:filename_parts
    endif
    if strip_slashes
        " let rx    = tlib#rx#Escape(g:tlib#dir#sep) .'$'
        let rx    = '[/\\]\+$'
        let parts = map(copy(filename_parts), 'substitute(v:val, rx, "", "")')
        Tlibtrace 'tlib', parts
        return join(parts, g:tlib#dir#sep)
    else
        return join(filename_parts, g:tlib#dir#sep)
    endif
endf


" EXAMPLES: >
"   tlib#file#Relative('foo/bar/filename.txt', 'foo')
"   => 'bar/filename.txt'
function! tlib#file#Relative(filename, basedir) abort "{{{3
    Tlibtrace 'tlib', a:filename, a:basedir
    " TLogDBG getcwd()
    " TLogDBG expand('%:p')
    let b0 = tlib#file#Absolute(a:basedir)
    let b  = tlib#file#Split(b0)
    Tlibtrace 'tlib', b
    let f0 = tlib#file#Absolute(a:filename)
    let fn = fnamemodify(f0, ':t')
    let fd = fnamemodify(f0, ':h')
    let f  = tlib#file#Split(fd)
    Tlibtrace 'tlib', f0, fn, fd, f
    if f[0] != b[0]
        let rv = f0
    else
        while !empty(f) && !empty(b)
            if f[0] != b[0]
                break
            endif
            call remove(f, 0)
            call remove(b, 0)
        endwh
        Tlibtrace 'tlib', f, b
        let rv = tlib#file#Join(repeat(['..'], len(b)) + f + [fn])
    endif
    Tlibtrace 'tlib', rv
    return rv
endf


function! tlib#file#IsAbsolute(filename) abort "{{{3
    return a:filename =~? '^\%(/\|\w\+:/\)'
endf


function! tlib#file#Absolute(filename, ...) abort "{{{3
    if filereadable(a:filename)
        let filename = fnamemodify(a:filename, ':p')
    elseif a:filename =~# '^\(/\|[^\/]\+:\)'
        let filename = a:filename
    else
        let cwd = a:0 >= 1 ? a:1 : getcwd()
        let filename = tlib#file#Join([cwd, a:filename])
    endif
    let filename = substitute(filename, '\(^\|[\/]\)\zs\.[\/]', '', 'g')
    let filename = substitute(filename, '[\/]\zs[^\/]\+[\/]\.\.[\/]', '', 'g')
    return filename
endf


function! tlib#file#Canonic(filename, ...) abort "{{{3
    TVarArg ['mode', '']
    if empty(mode)
        if a:filename =~# '^\\\\'
            let mode = 'windows'
        elseif a:filename =~# '^\(file\|ftp\|http\)s\?:'
            let mode = 'url'
        elseif (empty(mode) && g:tlib#sys#windows)
            let mode = 'windows'
        endif
    endif
    let filename = a:filename
    if mode ==# 'windows'
        let filename = substitute(filename, '/', '\\', 'g')
    else
        let filename = substitute(filename, '\\', '/', 'g')
    endif
    return filename
endf


function! s:SetScrollBind(world) abort "{{{3
    let sb = get(a:world, 'scrollbind', &scrollbind)
    if sb != &scrollbind
        let &scrollbind = sb
    endif
endf


" :def: function! tlib#file#With(fcmd, bcmd, files, ?world={}) abort
function! tlib#file#With(fcmd, bcmd, files, ...) abort "{{{3
    Tlibtrace 'tlib', a:fcmd, a:bcmd, a:files
    let world = a:0 >= 1 ? a:1 : {}
    let unset_switchbuf = a:0 >= 2 ? a:2 : 0
    exec tlib#arg#Let([['world', {}]])
    call tlib#autocmdgroup#Init()
    augroup TLibFileRead
        autocmd!
    augroup END
    if unset_switchbuf
        let switchbuf = &switchbuf
        set switchbuf&
    endif
    try
        for f in a:files
            try
                let bn = bufnr('^'.f.'$')
                Tlibtrace 'tlib', f, bn
                let bufloaded = bufloaded(bn)
                let ok = 0
                let s:bufread = ""
                if bn != -1 && buflisted(bn)
                    if !empty(a:bcmd)
                        let bcmd = a:bcmd .' '. bn
                        Tlibtrace 'tlib', bcmd
                        exec bcmd
                        let ok = 1
                        call s:SetScrollBind(world)
                    endif
                else
                    if filereadable(f)
                        if !empty(a:fcmd)
                            " TLogDBG a:fcmd .' '. tlib#arg#Ex(f)
                            exec 'autocmd TLibFileRead BufRead' escape(f, '\ ') 'let s:bufread=expand("<afile>:p")'
                            try 
                                let fcmd = a:fcmd .' '. tlib#arg#Ex(f)
                                Tlibtrace 'tlib', fcmd
                                exec fcmd
                            finally
                                exec 'autocmd! TLibFileRead BufRead'
                            endtry
                            let ok = 1
                            call s:SetScrollBind(world)
                        endif
                    else
                        echohl error
                        echom 'File not readable: '. f
                        echohl NONE
                    endif
                endif
                Tlibtrace 'tlib', ok, bufloaded, &filetype
                if empty(s:bufread) && ok && !bufloaded && empty(&filetype)
                    doautocmd BufRead
                endif
            catch /^Vim\%((\a\+)\)\=:E325/
                echohl ErrorMsg
                echom v:exception
                echohl NONE
            endtry
        endfor
    finally
        augroup! TLibFileRead
        if unset_switchbuf
            let &switchbuf = switchbuf
        endif
        unlet! s:bufread
    endtry
    " TLogDBG "done"
endf


" Return 0 if the file isn't readable/doesn't exist.
" Otherwise return 1.
function! tlib#file#Edit(fileid) abort "{{{3
    if type(a:fileid) == 0
        let bn = a:fileid
        let filename = fnamemodify(bufname(bn), ':p')
    else
        let filename = fnamemodify(a:fileid, ':p')
        let bn = bufnr(filename)
    endif
    if filename == expand('%:p')
        return 1
    else
        Tlibtrace 'tlib', a:fileid, bn, filename, g:tlib#file#drop, filereadable(filename), bufnr('%')
        if bn != -1 && buflisted(bn)
            if g:tlib#file#drop
                " echom "DBG" get(g:tlib#file#edit_cmds, 'drop', 'drop') fnameescape(filename)
                exec get(g:tlib#file#edit_cmds, 'drop', 'drop') fnameescape(filename)
                " echom "DBG" bufnr('%')
            else
                " echom "DBG" get(g:tlib#file#edit_cmds, 'buffer', 'buffer') bn
                exec get(g:tlib#file#edit_cmds, 'buffer', 'buffer') bn
                " echom "DBG" bufnr('%')
            endif
            return 1
        endif
        if !filereadable(filename) && exists('#TLibPrepareFile#User')
            exec 'doautocmd TLibPrepareFile User' filename
        endif
        if filereadable(filename)
            try
                " let file = tlib#arg#Ex(filename)
                " Tlibtrace 'tlib', file
                " echom "DBG" get(g:tlib#file#edit_cmds, 'edit', 'edit') fnameescape(filename)
                exec get(g:tlib#file#edit_cmds, 'edit', 'edit') fnameescape(filename)
            catch /E325/
                " swap file exists, let the user handle it
            catch
                echohl error
                echom v:exception
                echohl NONE
            endtry
            return 1
        else
            echom "TLIB: File not readable: " . filename
            if filename != a:fileid
                echom "TLIB: original filename: " . a:fileid
            endif
        endif
    endif
    return 0
endf


function! tlib#file#FilterFiles(files, options) abort "{{{3
    Tlibtrace 'tlib', a:files, a:options, g:tlib#file#reject_rx
    if !get(a:options, 'all', 0)
        call filter(a:files, 'v:val !~# g:tlib#file#reject_rx')
    endif
    Tlibtrace 'tlib', a:files
    let type = get(a:options, 'type', 'fd')
    Tlibtrace 'tlib', type
    if type !~# 'd' || type !~# 'f'
        call filter(a:files, 'isdirectory(v:val) ? type =~# "d" : type =~# "f"')
    endif
    Tlibtrace 'tlib', a:files
    return a:files
endf


if v:version > 704 || (v:version == 704 && has('patch279'))

    function! tlib#file#Glob(pattern, ...) abort "{{{3
        let all = a:0 >= 1 ? a:1 : 0
        let nosuf = a:0 >= 2 ? a:2 : 0
        return tlib#file#FilterFiles(glob(a:pattern, nosuf, 1), {'all': all})
    endf

    function! tlib#file#Globpath(path, pattern, ...) abort "{{{3
        let all = a:0 >= 1 ? a:1 : 0
        let nosuf = a:0 >= 2 ? a:2 : 0
        return tlib#file#FilterFiles(globpath(a:path, a:pattern, nosuf, 1), {'all': all})
    endf

else

    " :nodoc:
    function! tlib#file#Glob(pattern, ...) abort "{{{3
        let all = a:0 >= 1 ? a:1 : 0
        let nosuf = a:0 >= 2 ? a:2 : 0
        return tlib#file#FilterFiles(split(glob(a:pattern, nosuf), '\n'), {'all': all})
    endf

    " :nodoc:
    function! tlib#file#Globpath(path, pattern, ...) abort "{{{3
        let all = a:0 >= 1 ? a:1 : 0
        let nosuf = a:0 >= 2 ? a:2 : 0
        return tlib#file#FilterFiles(split(globpath(a:path, a:pattern), '\n'), {'all': all})
    endf

endif


let s:filereadable = {}

augroup TLib
	autocmd BufWritePost,FileWritePost,FocusLost * let s:filereadable = {}
augroup end

function! tlib#file#Filereadable(filename) abort "{{{3
    if !has_key(s:filereadable, a:filename)
        let s:filereadable[a:filename] = filereadable(a:filename)
    endif
    return s:filereadable[a:filename]
endf

