"=============================================================================
" FILE: autoload/EasyMotion/command_line.vim
" AUTHOR: haya14busa
" Reference: https://github.com/osyo-manga/vim-over
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================
scriptencoding utf-8
" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

" CommandLine:
let s:V = vital#easymotion#new()
let s:cmdline = s:V.import('Over.Commandline.Base')
let s:modules = s:V.import("Over.Commandline.Modules")
let s:search = s:cmdline.make()
let s:search.highlights.prompt = 'Question'

" Add Module: {{{
call s:search.connect('Exit')
call s:search.connect('Cancel')
call s:search.connect('Redraw')
call s:search.connect('DrawCommandline')
call s:search.connect('Delete')
call s:search.connect('CursorMove')
call s:search.connect('Paste')
call s:search.connect('BufferComplete')
call s:search.connect('InsertRegister')
call s:search.connect('ExceptionExit')
call s:search.connect(s:modules.get('ExceptionMessage').make('EasyMotion: ', 'echom'))
call s:search.connect(s:modules.get('History').make('/'))
call s:search.connect(s:modules.get('NoInsert').make_special_chars())
call s:search.connect(s:modules.get('KeyMapping').make_emacs())
call s:search.connect(s:modules.get('Doautocmd').make('EMCommandLine'))

let s:module = {
\   "name" : "EasyMotion",
\}
function! s:module.on_char_pre(cmdline)
    if a:cmdline.is_input("<Over>(em-scroll-f)")
        call s:scroll(0)
        call a:cmdline.setchar('')
    elseif a:cmdline.is_input("<Over>(em-scroll-b)")
        call s:scroll(1)
        call a:cmdline.setchar('')
    elseif a:cmdline.is_input("<Over>(em-jumpback)")
        keepjumps call setpos('.', s:save_orig_pos)
        let s:orig_pos = s:save_orig_pos
        let s:orig_line_start = getpos('w0')
        let s:orig_line_end = getpos('w$')
        let s:direction = s:save_direction
        call a:cmdline.setchar('')
    elseif a:cmdline.is_input("<Over>(em-openallfold)")
        " TODO: better solution
        normal! zR
        call a:cmdline.setchar('')
    endif
endfunction
call s:search.connect(s:module)
"}}}

" CommandLine Keymap: {{{
" .keymapping() won't be remapped by user defined KeyMappings.
function! s:search.keymapping() "{{{
    return {
\       "\<CR>"   : {
\           "key" : "<Over>(exit)",
\           "noremap" : 1,
\           "lock" : 1,
\       },
\   }
endfunction "}}}

call s:search.cnoremap("\<C-l>", '<Over>(buffer-complete)')
call s:search.cnoremap("\<Tab>", '<Over>(em-scroll-f)')
call s:search.cnoremap("\<S-Tab>", '<Over>(em-scroll-b)')
call s:search.cnoremap("\<C-o>", '<Over>(em-jumpback)')
call s:search.cnoremap("\<C-z>", '<Over>(em-openallfold)')

" Fins Motion CommandLine Mapping Command: {{{
function! EasyMotion#command_line#cmap(args)
    let lhs = s:as_keymapping(a:args[0])
    let rhs = s:as_keymapping(a:args[1])
    call s:search.cmap(lhs, rhs)
endfunction
function! EasyMotion#command_line#cnoremap(args)
    let lhs = s:as_keymapping(a:args[0])
    let rhs = s:as_keymapping(a:args[1])
    call s:search.cnoremap(lhs, rhs)
endfunction
function! EasyMotion#command_line#cunmap(lhs)
    let lhs = s:as_keymapping(a:lhs)
    call s:search.cunmap(lhs)
endfunction
function! s:as_keymapping(key)
    execute 'let result = "' . substitute(a:key, '\(<.\{-}>\)', '\\\1', 'g') . '"'
    return result
endfunction
"}}}
"}}}

" Event: {{{
function! s:search.on_enter(cmdline) "{{{
    if s:num_strokes == -1
        call EasyMotion#highlight#delete_highlight()
        call EasyMotion#helper#VarReset('&scrolloff', 0)
        if g:EasyMotion_do_shade
            call EasyMotion#highlight#add_highlight('\_.*',
                                                \ g:EasyMotion_hl_group_shade)
        endif
    endif
    if g:EasyMotion_cursor_highlight
        call EasyMotion#highlight#add_highlight('\%#',
                                              \ g:EasyMotion_hl_inc_cursor)
    endif
endfunction "}}}
function! s:search.on_leave(cmdline) "{{{
    if s:num_strokes == -1
        call EasyMotion#highlight#delete_highlight(g:EasyMotion_hl_inc_search)
        if g:EasyMotion_do_shade
            call EasyMotion#highlight#delete_highlight(g:EasyMotion_hl_group_shade)
        endif
    endif
    if g:EasyMotion_cursor_highlight
        call EasyMotion#highlight#delete_highlight(g:EasyMotion_hl_inc_cursor)
    endif
endfunction "}}}
function! s:search.on_char(cmdline) "{{{
    if s:num_strokes == -1
        let re = s:search.getline()
        if EasyMotion#helper#should_case_sensitive(re, 1)
            let case_flag = '\c'
        else
            let case_flag = '\C'
        endif
        let re .= case_flag
        if g:EasyMotion_inc_highlight
            call s:inc_highlight(re)
        endif
        if g:EasyMotion_off_screen_search
            call s:off_screen_search(re)
        endif
    elseif s:search.line.length() >=  s:num_strokes
        call s:search.exit()
    endif
endfunction "}}}
"}}}

" Main:
function! EasyMotion#command_line#GetInput(num_strokes, prev, direction) "{{{
    let s:num_strokes = a:num_strokes

    let s:prompt_base = s:getPromptMessage(a:num_strokes)
    call s:search.set_prompt(s:prompt_base)

    " Screen: cursor position, first and last line
    let s:orig_pos = getpos('.')
    let s:orig_line_start = getpos('w0')
    let s:orig_line_end = getpos('w$')
    let s:save_orig_pos = deepcopy(s:orig_pos)

    " Direction:
    let s:direction = a:direction == 1 ? 'b' : ''
    let s:save_direction = deepcopy(s:direction)

    let input = s:search.get()
    if input == '' && ! s:search.exit_code()
        return a:prev
    elseif s:search.exit_code() == 1 || s:search.exit_code() == -1
        call s:Cancell()
        return ''
    else
        return input
    endif
endfunction "}}}

" Helper:
function! s:Cancell() " {{{
    call EasyMotion#highlight#delete_highlight()
    call EasyMotion#helper#VarReset('&scrolloff')
    keepjumps call setpos('.', s:save_orig_pos)
    if g:EasyMotion_verbose
        echo 'EasyMotion: Cancelled'
    endif
    return ''
endfunction " }}}
function! s:getPromptMessage(num_strokes) "{{{
    if a:num_strokes == 1
        let prompt = substitute(
            \ substitute(g:EasyMotion_prompt,'{n}', a:num_strokes, 'g'),
            \ '(s)', '', 'g')
    elseif a:num_strokes == -1
        let prompt = substitute(
            \ substitute(g:EasyMotion_prompt, '{n}\s\{0,1}', '', 'g'),
            \ '(s)', 's', 'g')
    else
        let prompt = substitute(
            \ substitute(g:EasyMotion_prompt,'{n}', a:num_strokes, 'g'),
            \ '(s)', 's', 'g')
    endif
    return prompt
endfunction "}}}

function! s:off_screen_search(re) "{{{
    " First: search within visible screen range
    call s:adjust_screen()
    " Error occur when '\zs' without '!'
    silent! let pos = searchpos(a:re, s:direction . 'n', s:orig_line_end[1])
    if pos != [0, 0]
        " Restore cursor posision
        keepjumps call setpos('.', s:orig_pos)
    else
        " Second: if there were no much, search off screen
        silent! let pos = searchpos(a:re, s:direction)
        if pos != [0, 0]
            " Match
            keepjumps call setpos('.', pos)
            " Move cursor
            if s:save_direction != 'b'
                normal! zzH0
            else
                normal! zzL0
            endif
        else
            " No much
            call s:adjust_screen()
            keepjumps call setpos('.', s:orig_pos)
        endif
    endif
    " redraw
endfunction "}}}
function! s:adjust_screen() "{{{
    if s:save_direction != 'b'
        " Forward
        keepjumps call setpos('.', s:orig_line_start)
        normal! zt
    else
        " Backward
        keepjumps call setpos('.', s:orig_line_end)
        normal! zb
    endif
endfunction "}}}
function! s:scroll(direction) "{{{
    " direction: 0 -> forward, 1 -> backward
    exec a:direction == 0 ? "normal! \<C-f>" : "normal! \<C-b>"
    let s:orig_pos = getpos('.')
    let s:orig_line_start = getpos('w0')
    let s:orig_line_end = getpos('w$')
    let s:direction = a:direction == 0 ? '' : 'b'
endfunction "}}}
function! s:inc_highlight(re) "{{{
    call EasyMotion#highlight#delete_highlight(g:EasyMotion_hl_inc_search)
    if s:search.line.length() > 0
        " Error occur when '\zs' without '!'
        silent! call EasyMotion#highlight#add_highlight(a:re, g:EasyMotion_hl_inc_search)
    endif
endfunction "}}}

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
unlet s:save_cpo
" }}}
" vim: fdm=marker:et:ts=4:sw=4:sts=4
