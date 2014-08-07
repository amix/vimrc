" file.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2014-07-07.
" @Revision:    0.0.150

if &cp || exists("loaded_tlib_file_autoload")
    finish
endif
let loaded_tlib_file_autoload = 1


""" File related {{{1
" For the following functions please see ../../test/tlib.vim for examples.


" EXAMPLES: >
"   tlib#file#Split('foo/bar/filename.txt')
"   => ['foo', 'bar', 'filename.txt']
function! tlib#file#Split(filename) "{{{3
    let prefix = matchstr(a:filename, '^\(\w\+:\)\?/\+')
    " TLogVAR prefix
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


" :display: tlib#file#Join(filename_parts, ?strip_slashes=1)
" EXAMPLES: >
"   tlib#file#Join(['foo', 'bar', 'filename.txt'])
"   => 'foo/bar/filename.txt'
function! tlib#file#Join(filename_parts, ...) "{{{3
    TVarArg ['strip_slashes', 1]
    " TLogVAR a:filename_parts, strip_slashes
    if strip_slashes
        " let rx    = tlib#rx#Escape(g:tlib#dir#sep) .'$'
        let rx    = '[/\\]\+$'
        let parts = map(copy(a:filename_parts), 'substitute(v:val, rx, "", "")')
        " TLogVAR parts
        return join(parts, g:tlib#dir#sep)
    else
        return join(a:filename_parts, g:tlib#dir#sep)
    endif
endf


" EXAMPLES: >
"   tlib#file#Relative('foo/bar/filename.txt', 'foo')
"   => 'bar/filename.txt'
function! tlib#file#Relative(filename, basedir) "{{{3
    " TLogVAR a:filename, a:basedir
    " TLogDBG getcwd()
    " TLogDBG expand('%:p')
    let b0 = tlib#file#Absolute(a:basedir)
    let b  = tlib#file#Split(b0)
    " TLogVAR b
    let f0 = tlib#file#Absolute(a:filename)
    let fn = fnamemodify(f0, ':t')
    let fd = fnamemodify(f0, ':h')
    let f  = tlib#file#Split(fd)
    " TLogVAR f0, fn, fd, f
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
        " TLogVAR f, b
        let rv = tlib#file#Join(repeat(['..'], len(b)) + f + [fn])
    endif
    " TLogVAR rv
    return rv
endf


function! tlib#file#Absolute(filename, ...) "{{{3
    if filereadable(a:filename)
        let filename = fnamemodify(a:filename, ':p')
    elseif a:filename =~ '^\(/\|[^\/]\+:\)'
        let filename = a:filename
    else
        let cwd = a:0 >= 1 ? a:1 : getcwd()
        let filename = tlib#file#Join([cwd, a:filename])
    endif
    let filename = substitute(filename, '\(^\|[\/]\)\zs\.[\/]', '', 'g')
    let filename = substitute(filename, '[\/]\zs[^\/]\+[\/]\.\.[\/]', '', 'g')
    return filename
endf


function! tlib#file#Canonic(filename, ...) "{{{3
    TVarArg ['mode', '']
    if a:filename =~ '^\\\\'
        let mode = 'windows'
    elseif a:filename =~ '^\(file\|ftp\|http\)s\?:'
        let mode = 'url'
    elseif (empty(mode) && g:tlib#sys#windows)
        let mode = 'windows'
    endif
    let filename = a:filename
    if mode == 'windows'
        let filename = substitute(filename, '/', '\\', 'g')
    else
        let filename = substitute(filename, '\\', '/', 'g')
    endif
    return filename
endf


function! s:SetScrollBind(world) "{{{3
    let sb = get(a:world, 'scrollbind', &scrollbind)
    if sb != &scrollbind
        let &scrollbind = sb
    endif
endf


" :def: function! tlib#file#With(fcmd, bcmd, files, ?world={})
function! tlib#file#With(fcmd, bcmd, files, ...) "{{{3
    " TLogVAR a:fcmd, a:bcmd, a:files
    exec tlib#arg#Let([['world', {}]])
    call tlib#autocmdgroup#Init()
    augroup TLibFileRead
        autocmd!
    augroup END
    for f in a:files
        let bn = bufnr('^'.f.'$')
        " TLogVAR f, bn
        let bufloaded = bufloaded(bn)
        let ok = 0
        let s:bufread = ""
        if bn != -1 && buflisted(bn)
            if !empty(a:bcmd)
                " TLogDBG a:bcmd .' '. bn
                exec a:bcmd .' '. bn
                let ok = 1
                call s:SetScrollBind(world)
            endif
        else
            if filereadable(f)
                if !empty(a:fcmd)
                    " TLogDBG a:fcmd .' '. tlib#arg#Ex(f)
                    exec 'autocmd TLibFileRead BufRead' escape(f, '\ ') 'let s:bufread=expand("<afile>:p")'
                    try 
                        exec a:fcmd .' '. tlib#arg#Ex(f)
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
        " TLogVAR ok, bufloaded, &filetype
        if empty(s:bufread) && ok && !bufloaded && empty(&filetype)
            doautocmd BufRead
        endif
    endfor
    augroup! TLibFileRead
    unlet! s:bufread
    " TLogDBG "done"
endf


