" file.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2010-04-03.
" @Revision:    0.0.74

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


" :display: tlib#file#Join(filename_parts, ?strip_slashes=0)
" EXAMPLES: >
"   tlib#file#Join(['foo', 'bar', 'filename.txt'])
"   => 'foo/bar/filename.txt'
function! tlib#file#Join(filename_parts, ...) "{{{3
    TVarArg 'strip_slashes'
    if strip_slashes
        " let rx    = tlib#rx#Escape(g:tlib_filename_sep) .'$'
        let rx    = '[/\\]$'
        let parts = map(copy(a:filename_parts), 'substitute(v:val, rx, "", "")')
        return join(parts, g:tlib_filename_sep)
    else
        return join(a:filename_parts, g:tlib_filename_sep)
    endif
endf


" EXAMPLES: >
"   tlib#file#Relative('foo/bar/filename.txt', 'foo')
"   => 'bar/filename.txt'
function! tlib#file#Relative(filename, basedir) "{{{3
    " TLogVAR a:filename, a:basedir
    " TLogDBG getcwd()
    " TLogDBG expand('%:p')
    let f0 = fnamemodify(a:filename, ':p')
    let fn = fnamemodify(f0, ':t')
    let fd = fnamemodify(f0, ':h')
    let f  = tlib#file#Split(fd)
    " TLogVAR f
    let b0 = fnamemodify(a:basedir, ':p')
    let b  = tlib#file#Split(b0)
    " TLogVAR b
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
        let rv = tlib#file#Join(repeat(['..'], len(b)) + f + [fn])
    endif
    " TLogVAR rv
    return rv
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
    for f in a:files
        let bn = bufnr('^'.f.'$')
        " TLogVAR f, bn
        if bn != -1 && buflisted(bn)
            if !empty(a:bcmd)
                " TLogDBG a:bcmd .' '. bn
                exec a:bcmd .' '. bn
                call s:SetScrollBind(world)
            endif
        else
            if filereadable(f)
                if !empty(a:fcmd)
                    " TLogDBG a:fcmd .' '. escape(f, '%#\ ')
                    " exec a:fcmd .' '. escape(f, '%#\ ')
                    " exec a:fcmd .' '. escape(f, '%# ')
                    exec a:fcmd .' '. tlib#arg#Ex(f)
                    call s:SetScrollBind(world)
                endif
            else
                echohl error
                echom 'File not readable: '. f
                echohl NONE
            endif
        endif
    endfor
endf


