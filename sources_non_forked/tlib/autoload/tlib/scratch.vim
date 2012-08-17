" scratch.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-07-18.
" @Last Change: 2011-03-10.
" @Revision:    0.0.166

if &cp || exists("loaded_tlib_scratch_autoload")
    finish
endif
let loaded_tlib_scratch_autoload = 1


" :def: function! tlib#scratch#UseScratch(?keyargs={})
" Display a scratch buffer (a buffer with no file). See :TScratch for an 
" example.
" Return the scratch buffer's number.
" Values for keyargs:
"   scratch_split ... 1: split, 0: window, -1: tab
function! tlib#scratch#UseScratch(...) "{{{3
    exec tlib#arg#Let([['keyargs', {}]])
    " TLogDBG string(keys(keyargs))
    let id = get(keyargs, 'scratch', '__Scratch__')
    " TLogVAR id
    " TLogDBG winnr()
    " TLogDBG bufnr(id)
    " TLogDBG bufwinnr(id)
    " TLogDBG bufnr('%')
    if id =~ '^\d\+$' && bufwinnr(id) != -1
        if bufnr('%') != id
            exec 'buffer! '. id
        endif
        " let ft = &ft
        let ft = '*'
    else
        let bn = bufnr(id)
        let wpos = get(keyargs, 'scratch_pos', g:tlib_scratch_pos)
        " TLogVAR keyargs.scratch_vertical
        if get(keyargs, 'scratch_vertical')
            let wpos .= ' vertical'
        endif
        " TLogVAR wpos
        let scratch_split = get(keyargs, 'scratch_split', 1)
        if bn != -1
            " TLogVAR bn
            let wn = bufwinnr(bn)
            if wn != -1
                " TLogVAR wn
                exec wn .'wincmd w'
            else
                if scratch_split == 1
                    let cmd = wpos.' sbuffer! '
                elseif scratch_split == -1
                    let cmd = wpos.' tab sbuffer! '
                else
                    let cmd = 'buffer! '
                endif
                " TLogVAR cmd
                silent exec cmd . bn
            endif
        else
            " TLogVAR id
            if scratch_split == 1
                let cmd = wpos.' split '
            elseif scratch_split == -1
                let cmd = wpos.' tab split '
            else
                let cmd = 'edit '
            endif
            " TLogVAR cmd
            silent exec cmd . escape(id, '%#\ ')
            " silent exec 'split '. id
        endif
        let ft = get(keyargs, 'scratch_filetype', '')
        " TLogVAR ft
    endif
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nobuflisted
    setlocal foldmethod=manual
    setlocal foldcolumn=0
    setlocal modifiable
    setlocal nospell
    if &ft != '*'
        let &ft = ft
    endif
    let keyargs.scratch = bufnr('%')
    return keyargs.scratch
endf


" Close a scratch buffer as defined in keyargs (usually a World).
" Return 1 if the scratch buffer is closed (or if it already was 
" closed).
function! tlib#scratch#CloseScratch(keyargs, ...) "{{{3
    TVarArg ['reset_scratch', 1]
    let scratch = get(a:keyargs, 'scratch', '')
    " TLogVAR scratch, reset_scratch
    " TLogDBG string(tlib#win#List())
    if !empty(scratch) && winnr('$') > 1
        let wn = bufwinnr(scratch)
        " TLogVAR wn
        try
            if wn != -1
                " TLogDBG winnr()
                let wb = tlib#win#Set(wn)
                wincmd c
                " exec wb 
                " redraw
                " TLogVAR winnr()
            endif
            return 1
        finally
            if reset_scratch
                let a:keyargs.scratch = ''
            endif
        endtry
    endif
    return 0
endf

