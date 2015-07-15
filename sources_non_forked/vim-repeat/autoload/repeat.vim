" repeat.vim - Let the repeat command repeat plugin maps
" Maintainer:   Tim Pope
" Version:      1.1
" GetLatestVimScripts: 2136 1 :AutoInstall: repeat.vim

" Installation:
" Place in either ~/.vim/plugin/repeat.vim (to load at start up) or
" ~/.vim/autoload/repeat.vim (to load automatically as needed).
"
" License:
" Copyright (c) Tim Pope.  Distributed under the same terms as Vim itself.
" See :help license
"
" Developers:
" Basic usage is as follows:
"
"   silent! call repeat#set("\<Plug>MappingToRepeatCommand",3)
"
" The first argument is the mapping that will be invoked when the |.| key is
" pressed.  Typically, it will be the same as the mapping the user invoked.
" This sequence will be stuffed into the input queue literally.  Thus you must
" encode special keys by prefixing them with a backslash inside double quotes.
"
" The second argument is the default count.  This is the number that will be
" prefixed to the mapping if no explicit numeric argument was given.  The
" value of the v:count variable is usually correct and it will be used if the
" second parameter is omitted.  If your mapping doesn't accept a numeric
" argument and you never want to receive one, pass a value of -1.
"
" Make sure to call the repeat#set function _after_ making changes to the
" file.
"
" For mappings that use a register and want the same register used on
" repetition, use:
"
"   silent! call repeat#setreg("\<Plug>MappingToRepeatCommand", v:register)
"
" This function can (and probably needs to be) called before making changes to
" the file (as those typically clear v:register).  Therefore, the call sequence
" in your mapping will look like this:
"
"   nnoremap <silent> <Plug>MyMap
"   \   :<C-U>execute 'silent! call repeat#setreg("\<lt>Plug>MyMap", v:register)'<Bar>
"   \   call <SID>MyFunction(v:register, ...)<Bar>
"   \   silent! call repeat#set("\<lt>Plug>MyMap")<CR>

if exists("g:loaded_repeat") || &cp || v:version < 700
    finish
endif
let g:loaded_repeat = 1

let g:repeat_tick = -1
let g:repeat_reg = ['', '']

" Special function to avoid spurious repeats in a related, naturally repeating
" mapping when your repeatable mapping doesn't increase b:changedtick.
function! repeat#invalidate()
    autocmd! repeat_custom_motion
    let g:repeat_tick = -1
endfunction

function! repeat#set(sequence,...)
    let g:repeat_sequence = a:sequence
    let g:repeat_count = a:0 ? a:1 : v:count
    let g:repeat_tick = b:changedtick
    augroup repeat_custom_motion
        autocmd!
        autocmd CursorMoved <buffer> let g:repeat_tick = b:changedtick | autocmd! repeat_custom_motion
    augroup END
endfunction

function! repeat#setreg(sequence,register)
    let g:repeat_reg = [a:sequence, a:register]
endfunction

function! repeat#run(count)
    try
        if g:repeat_tick == b:changedtick
            let r = ''
            if g:repeat_reg[0] ==# g:repeat_sequence && !empty(g:repeat_reg[1])
                if g:repeat_reg[1] ==# '='
                    " This causes a re-evaluation of the expression on repeat, which
                    " is what we want.
                    let r = '"=' . getreg('=', 1) . "\<CR>"
                else
                    let r = '"' . g:repeat_reg[1]
                endif
            endif

            let c = g:repeat_count
            let s = g:repeat_sequence
            let cnt = c == -1 ? "" : (a:count ? a:count : (c ? c : ''))
            if ((v:version == 703 && has('patch100')) || (v:version == 704 && !has('patch601')))
                exe 'norm ' . r . cnt . s
            else
                call feedkeys(s, 'i')
                call feedkeys(r . cnt, 'ni')
            endif
        else
            if ((v:version == 703 && has('patch100')) || (v:version == 704 && !has('patch601')))
                exe 'norm! '.(a:count ? a:count : '') . '.'
            else
                call feedkeys((a:count ? a:count : '') . '.', 'ni')
            endif
        endif
    catch /^Vim(normal):/
        return 'echoerr v:errmsg'
    endtry
    return ''
endfunction

function! repeat#wrap(command,count)
    let preserve = (g:repeat_tick == b:changedtick)
    exe 'norm! '.(a:count ? a:count : '').a:command . (&foldopen =~# 'undo\|all' ? 'zv' : '')
    if preserve
        let g:repeat_tick = b:changedtick
    endif
endfunction

nnoremap <silent> <Plug>(RepeatDot)      :<C-U>exe repeat#run(v:count)<CR>
nnoremap <silent> <Plug>(RepeatUndo)     :<C-U>call repeat#wrap('u',v:count)<CR>
nnoremap <silent> <Plug>(RepeatUndoLine) :<C-U>call repeat#wrap('U',v:count)<CR>
nnoremap <silent> <Plug>(RepeatRedo)     :<C-U>call repeat#wrap("\<Lt>C-R>",v:count)<CR>

if !hasmapto('<Plug>(RepeatDot)', 'n')
    nmap . <Plug>(RepeatDot)
endif
if !hasmapto('<Plug>(RepeatUndo)', 'n')
    nmap u <Plug>(RepeatUndo)
endif
if maparg('U','n') ==# '' && !hasmapto('<Plug>(RepeatUndoLine)', 'n')
    nmap U <Plug>(RepeatUndoLine)
endif
if !hasmapto('<Plug>(RepeatRedo)', 'n')
    nmap <C-R> <Plug>(RepeatRedo)
endif

augroup repeatPlugin
    autocmd!
    autocmd BufLeave,BufWritePre,BufReadPre * let g:repeat_tick = (g:repeat_tick == b:changedtick || g:repeat_tick == 0) ? 0 : -1
    autocmd BufEnter,BufWritePost * if g:repeat_tick == 0|let g:repeat_tick = b:changedtick|endif
augroup END

" vim:set ft=vim et sw=4 sts=4:
