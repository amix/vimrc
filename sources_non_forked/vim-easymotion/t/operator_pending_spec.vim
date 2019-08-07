"=============================================================================
" FILE: t/operator_pending_spec.vim
" AUTHOR: haya14busa
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

" Avoid source test files {{{
if expand("%:p") ==# expand("<sfile>:p")
  finish
endif
"}}}

" Setup {{{
let s:root_dir = matchstr(system('git rev-parse --show-cdup'), '[^\n]\+')
execute 'set' 'rtp +=./'.s:root_dir
runtime! plugin/EasyMotion.vim
"}}}

" Functions for Test {{{
function! AddLine(str)
    put! =a:str
endfunction

function! CursorPos()
    return [line('.'), col('.'), getline('.')[col('.')-1]]
endfunction
"}}}


" NOTE:
"   I cannot test inclusive motion because mode() doesn't works well with
"   vim-vspec

" word motions {{{
describe 'word motions'
    before
        new
        let g:EasyMotion_keys = '123456789'
        omap f <Plug>(easymotion-f)
        omap w <Plug>(easymotion-w)
        omap b <Plug>(easymotion-b)
        call EasyMotion#init()
        call AddLine('vim deco vim deco vim deco')
        "             123456789012345678901234567890
    end

    after
        close!
    end

    it '<Plug>(easymotion-w)'
        " Default position
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'v']

        normal dw1
        Expect CursorPos() == [l,1,'d']
        normal! u
        normal! 0
        Expect CursorPos() == [l,1,'v']

        normal dw2
        Expect CursorPos() == [l,1,'v']
        normal! 0
        normal! u
        normal! 0
        Expect CursorPos() == [l,1,'v']
    end

    it '<Plug>(easymotion-b)'
        " Default position
        normal! $
        let l = line('.')
        Expect CursorPos() == [l,26,'o']

        normal db1
        Expect CursorPos() == [l,23,'o']
        normal! u
        normal! $
        Expect CursorPos() == [l,26,'o']

        normal db2
        Expect CursorPos() == [l,19,'o']
        normal! u
        normal! $
        Expect CursorPos() == [l,26,'o']
    end
end
"}}}

