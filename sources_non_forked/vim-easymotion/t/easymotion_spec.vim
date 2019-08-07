"=============================================================================
" FILE: t/easymotion_spec.vim
" AUTHOR: haya14busa
" Test: https://github.com/kana/vim-vspec
" Refer: https://github.com/rhysd/clever-f.vim
" Description: EasyMotion test with vim-vspec
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

let s:root_dir = matchstr(system('git rev-parse --show-cdup'), '[^\n]\+')
execute 'set' 'rtp +=./'.s:root_dir
runtime! plugin/EasyMotion.vim

" Default settings {{{
describe 'Default settings'
    it 'provide default <Plug> mappings for find motion'
        " Find Motion: {{{
        " s
        Expect maparg('<Plug>(easymotion-s)', 'n') ==# ':<C-U>call EasyMotion#S(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-s)', 'o') ==# ':<C-U>call EasyMotion#S(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-s)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(1,1,2)<CR>'
        " f
        Expect maparg('<Plug>(easymotion-f)', 'n') ==# ':<C-U>call EasyMotion#S(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-f)', 'o') ==# ':<C-U>call EasyMotion#S(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-f)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(1,1,0)<CR>'
        " F
        Expect maparg('<Plug>(easymotion-F)', 'n') ==# ':<C-U>call EasyMotion#S(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-F)', 'o') ==# ':<C-U>call EasyMotion#S(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-F)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(1,1,1)<CR>'
        " bd-f
        Expect maparg('<Plug>(easymotion-bd-f)', 'n') ==# maparg('<Plug>(easymotion-s)', 'n')
        Expect maparg('<Plug>(easymotion-bd-f)', 'o') ==# maparg('<Plug>(easymotion-s)', 'o')
        Expect maparg('<Plug>(easymotion-bd-f)', 'v') ==# maparg('<Plug>(easymotion-s)', 'v')
        " t
        Expect maparg('<Plug>(easymotion-t)', 'n') ==# ':<C-U>call EasyMotion#T(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-t)', 'o') ==# ':<C-U>call EasyMotion#T(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-t)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(1,1,0)<CR>'
        " T
        Expect maparg('<Plug>(easymotion-T)', 'n') ==# ':<C-U>call EasyMotion#T(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-T)', 'o') ==# ':<C-U>call EasyMotion#T(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-T)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(1,1,1)<CR>'
        " bd-t
        Expect maparg('<Plug>(easymotion-bd-t)', 'n') ==# ':<C-U>call EasyMotion#T(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-t)', 'o') ==# ':<C-U>call EasyMotion#T(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-t)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(1,1,2)<CR>'
        " sl
        Expect maparg('<Plug>(easymotion-sl)', 'n') ==# ':<C-U>call EasyMotion#SL(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sl)', 'o') ==# ':<C-U>call EasyMotion#SL(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(1,1,2)<CR>'
        " fl
        Expect maparg('<Plug>(easymotion-fl)', 'n') ==# ':<C-U>call EasyMotion#SL(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fl)', 'o') ==# ':<C-U>call EasyMotion#SL(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(1,1,0)<CR>'
        " Fl
        Expect maparg('<Plug>(easymotion-Fl)', 'n') ==# ':<C-U>call EasyMotion#SL(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fl)', 'o') ==# ':<C-U>call EasyMotion#SL(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(1,1,1)<CR>'
        " bd-fl
        Expect maparg('<Plug>(easymotion-bd-fl)', 'n') ==# maparg('<Plug>(easymotion-sl)', 'n')
        Expect maparg('<Plug>(easymotion-bd-fl)', 'o') ==# maparg('<Plug>(easymotion-sl)', 'o')
        Expect maparg('<Plug>(easymotion-bd-fl)', 'v') ==# maparg('<Plug>(easymotion-sl)', 'v')
        " tl
        Expect maparg('<Plug>(easymotion-tl)', 'n') ==# ':<C-U>call EasyMotion#TL(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tl)', 'o') ==# ':<C-U>call EasyMotion#TL(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(1,1,0)<CR>'
        " Tl
        Expect maparg('<Plug>(easymotion-Tl)', 'n') ==# ':<C-U>call EasyMotion#TL(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tl)', 'o') ==# ':<C-U>call EasyMotion#TL(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(1,1,1)<CR>'
        " bd-tl
        Expect maparg('<Plug>(easymotion-bd-tl)', 'n') ==# ':<C-U>call EasyMotion#TL(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tl)', 'o') ==# ':<C-U>call EasyMotion#TL(1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(1,1,2)<CR>'
        "}}}

        " Two Char Find Motion: {{{
        " s2
        Expect maparg('<Plug>(easymotion-s2)', 'n') ==# ':<C-U>call EasyMotion#S(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-s2)', 'o') ==# ':<C-U>call EasyMotion#S(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-s2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(2,1,2)<CR>'
        " f2
        Expect maparg('<Plug>(easymotion-f2)', 'n') ==# ':<C-U>call EasyMotion#S(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-f2)', 'o') ==# ':<C-U>call EasyMotion#S(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-f2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(2,1,0)<CR>'
        " F2
        Expect maparg('<Plug>(easymotion-F2)', 'n') ==# ':<C-U>call EasyMotion#S(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-F2)', 'o') ==# ':<C-U>call EasyMotion#S(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-F2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(2,1,1)<CR>'
        " bd-f2
        Expect maparg('<Plug>(easymotion-bd-f2)', 'n') ==# maparg('<Plug>(easymotion-s2)', 'n')
        Expect maparg('<Plug>(easymotion-bd-f2)', 'o') ==# maparg('<Plug>(easymotion-s2)', 'o')
        Expect maparg('<Plug>(easymotion-bd-f2)', 'v') ==# maparg('<Plug>(easymotion-s2)', 'v')
        " t2
        Expect maparg('<Plug>(easymotion-t2)', 'n') ==# ':<C-U>call EasyMotion#T(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-t2)', 'o') ==# ':<C-U>call EasyMotion#T(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-t2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(2,1,0)<CR>'
        " T2
        Expect maparg('<Plug>(easymotion-T2)', 'n') ==# ':<C-U>call EasyMotion#T(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-T2)', 'o') ==# ':<C-U>call EasyMotion#T(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-T2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(2,1,1)<CR>'
        " sl2
        Expect maparg('<Plug>(easymotion-sl2)', 'n') ==# ':<C-U>call EasyMotion#SL(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sl2)', 'o') ==# ':<C-U>call EasyMotion#SL(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sl2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(2,1,2)<CR>'
        " bd-t2
        Expect maparg('<Plug>(easymotion-bd-t2)', 'n') ==# ':<C-U>call EasyMotion#T(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-t2)', 'o') ==# ':<C-U>call EasyMotion#T(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-t2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(2,1,2)<CR>'
        " fl2
        Expect maparg('<Plug>(easymotion-fl2)', 'n') ==# ':<C-U>call EasyMotion#SL(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fl2)', 'o') ==# ':<C-U>call EasyMotion#SL(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fl2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(2,1,0)<CR>'
        " Fl2
        Expect maparg('<Plug>(easymotion-Fl2)', 'n') ==# ':<C-U>call EasyMotion#SL(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fl2)', 'o') ==# ':<C-U>call EasyMotion#SL(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fl2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(2,1,1)<CR>'
        " bd-fl2
        Expect maparg('<Plug>(easymotion-bd-fl2)', 'n') ==# maparg('<Plug>(easymotion-sl2)', 'n')
        Expect maparg('<Plug>(easymotion-bd-fl2)', 'o') ==# maparg('<Plug>(easymotion-sl2)', 'o')
        Expect maparg('<Plug>(easymotion-bd-fl2)', 'v') ==# maparg('<Plug>(easymotion-sl2)', 'v')
        " tl2
        Expect maparg('<Plug>(easymotion-tl2)', 'n') ==# ':<C-U>call EasyMotion#TL(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tl2)', 'o') ==# ':<C-U>call EasyMotion#TL(2,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tl2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(2,1,0)<CR>'
        " Tl2
        Expect maparg('<Plug>(easymotion-Tl2)', 'n') ==# ':<C-U>call EasyMotion#TL(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tl2)', 'o') ==# ':<C-U>call EasyMotion#TL(2,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tl2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(2,1,1)<CR>'
        " bd-tl2
        Expect maparg('<Plug>(easymotion-bd-tl2)', 'n') ==# ':<C-U>call EasyMotion#TL(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tl2)', 'o') ==# ':<C-U>call EasyMotion#TL(2,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tl2)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(2,1,2)<CR>'
        "}}}

        " Multi Char Find Motion: {{{
        " sn
        Expect maparg('<Plug>(easymotion-sn)', 'n') ==# ':<C-U>call EasyMotion#S(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sn)', 'o') ==# ':<C-U>call EasyMotion#S(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sn)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(-1,1,2)<CR>'
        " fn
        Expect maparg('<Plug>(easymotion-fn)', 'n') ==# ':<C-U>call EasyMotion#S(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fn)', 'o') ==# ':<C-U>call EasyMotion#S(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fn)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(-1,1,0)<CR>'
        " Fn
        Expect maparg('<Plug>(easymotion-Fn)', 'n') ==# ':<C-U>call EasyMotion#S(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fn)', 'o') ==# ':<C-U>call EasyMotion#S(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fn)', 'v') ==# '<Esc>:<C-U>call EasyMotion#S(-1,1,1)<CR>'
        " bd-fn
        Expect maparg('<Plug>(easymotion-bd-fn)', 'n') ==# maparg('<Plug>(easymotion-sn)', 'n')
        Expect maparg('<Plug>(easymotion-bd-fn)', 'o') ==# maparg('<Plug>(easymotion-sn)', 'o')
        Expect maparg('<Plug>(easymotion-bd-fn)', 'v') ==# maparg('<Plug>(easymotion-sn)', 'v')
        " tn
        Expect maparg('<Plug>(easymotion-tn)', 'n') ==# ':<C-U>call EasyMotion#T(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tn)', 'o') ==# ':<C-U>call EasyMotion#T(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tn)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(-1,1,0)<CR>'
        " Tn
        Expect maparg('<Plug>(easymotion-Tn)', 'n') ==# ':<C-U>call EasyMotion#T(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tn)', 'o') ==# ':<C-U>call EasyMotion#T(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tn)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(-1,1,1)<CR>'
        " bd-tn
        Expect maparg('<Plug>(easymotion-bd-tn)', 'n') ==# ':<C-U>call EasyMotion#T(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tn)', 'o') ==# ':<C-U>call EasyMotion#T(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tn)', 'v') ==# '<Esc>:<C-U>call EasyMotion#T(-1,1,2)<CR>'
        " sln
        Expect maparg('<Plug>(easymotion-sln)', 'n') ==# ':<C-U>call EasyMotion#SL(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sln)', 'o') ==# ':<C-U>call EasyMotion#SL(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sln)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(-1,1,2)<CR>'
        " fln
        Expect maparg('<Plug>(easymotion-fln)', 'n') ==# ':<C-U>call EasyMotion#SL(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fln)', 'o') ==# ':<C-U>call EasyMotion#SL(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-fln)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(-1,1,0)<CR>'
        " Fln
        Expect maparg('<Plug>(easymotion-Fln)', 'n') ==# ':<C-U>call EasyMotion#SL(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fln)', 'o') ==# ':<C-U>call EasyMotion#SL(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Fln)', 'v') ==# '<Esc>:<C-U>call EasyMotion#SL(-1,1,1)<CR>'
        " bd-fln
        Expect maparg('<Plug>(easymotion-bd-fln)', 'n') ==# maparg('<Plug>(easymotion-sln)', 'n')
        Expect maparg('<Plug>(easymotion-bd-fln)', 'o') ==# maparg('<Plug>(easymotion-sln)', 'o')
        Expect maparg('<Plug>(easymotion-bd-fln)', 'v') ==# maparg('<Plug>(easymotion-sln)', 'v')
        " tln
        Expect maparg('<Plug>(easymotion-tln)', 'n') ==# ':<C-U>call EasyMotion#TL(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tln)', 'o') ==# ':<C-U>call EasyMotion#TL(-1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-tln)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(-1,1,0)<CR>'
        " Tln
        Expect maparg('<Plug>(easymotion-Tln)', 'n') ==# ':<C-U>call EasyMotion#TL(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tln)', 'o') ==# ':<C-U>call EasyMotion#TL(-1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-Tln)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(-1,1,1)<CR>'
        " bd-tln
        Expect maparg('<Plug>(easymotion-bd-tln)', 'n') ==# ':<C-U>call EasyMotion#TL(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tln)', 'o') ==# ':<C-U>call EasyMotion#TL(-1,0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-tln)', 'v') ==# '<Esc>:<C-U>call EasyMotion#TL(-1,1,2)<CR>'
        "}}}
    end

    it 'provide default <Plug> mappings for regex motion'
        "(is_visual, direction)
        " direction:
        "   - 0: forward
        "   - 1: backward
        "   - 2: bi-direction
        " Word Motion: {{{
        Expect maparg('<Plug>(easymotion-w)', 'n') ==# ':<C-U>call EasyMotion#WB(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-w)', 'o') ==# ':<C-U>call EasyMotion#WB(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-w)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WB(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-b)', 'n') ==# ':<C-U>call EasyMotion#WB(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-b)', 'o') ==# ':<C-U>call EasyMotion#WB(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-b)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WB(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-bd-w)', 'n') ==# ':<C-U>call EasyMotion#WB(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-w)', 'o') ==# ':<C-U>call EasyMotion#WB(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-w)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WB(1,2)<CR>'
        " }}}

        " WORD Motion: {{{
        Expect maparg('<Plug>(easymotion-W)', 'n') ==# ':<C-U>call EasyMotion#WBW(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-W)', 'o') ==# ':<C-U>call EasyMotion#WBW(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-W)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBW(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-B)', 'n') ==# ':<C-U>call EasyMotion#WBW(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-B)', 'o') ==# ':<C-U>call EasyMotion#WBW(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-B)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBW(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-bd-W)', 'n') ==# ':<C-U>call EasyMotion#WBW(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-W)', 'o') ==# ':<C-U>call EasyMotion#WBW(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-W)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBW(1,2)<CR>'
        " }}}

        " Word Motion IsKeyWord: {{{
        Expect maparg('<Plug>(easymotion-iskeyword-w)', 'n') ==# ':<C-U>call EasyMotion#WBK(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-w)', 'o') ==# ':<C-U>call EasyMotion#WBK(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-w)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBK(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-b)', 'n') ==# ':<C-U>call EasyMotion#WBK(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-b)', 'o') ==# ':<C-U>call EasyMotion#WBK(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-b)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBK(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-bd-w)', 'n') ==# ':<C-U>call EasyMotion#WBK(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-bd-w)', 'o') ==# ':<C-U>call EasyMotion#WBK(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-bd-w)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBK(1,2)<CR>'
        " }}}

        " End Of Word Motion: {{{
        Expect maparg('<Plug>(easymotion-e)', 'n')    ==# ':<C-U>call EasyMotion#E(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-e)', 'o')    ==# ':<C-U>call EasyMotion#E(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-e)', 'v')    ==# '<Esc>:<C-U>call EasyMotion#E(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-ge)', 'n')   ==# ':<C-U>call EasyMotion#E(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-ge)', 'o')   ==# ':<C-U>call EasyMotion#E(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-ge)', 'v')   ==# '<Esc>:<C-U>call EasyMotion#E(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-bd-e)', 'n') ==# ':<C-U>call EasyMotion#E(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-e)', 'o') ==# ':<C-U>call EasyMotion#E(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-e)', 'v') ==# '<Esc>:<C-U>call EasyMotion#E(1,2)<CR>'
        " }}}

        " END OF WORD Motion: {{{
        Expect maparg('<Plug>(easymotion-E)', 'n')    ==# ':<C-U>call EasyMotion#EW(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-E)', 'o')    ==# ':<C-U>call EasyMotion#EW(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-E)', 'v')    ==# '<Esc>:<C-U>call EasyMotion#EW(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-gE)', 'n')   ==# ':<C-U>call EasyMotion#EW(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-gE)', 'o')   ==# ':<C-U>call EasyMotion#EW(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-gE)', 'v')   ==# '<Esc>:<C-U>call EasyMotion#EW(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-bd-E)', 'n') ==# ':<C-U>call EasyMotion#EW(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-E)', 'o') ==# ':<C-U>call EasyMotion#EW(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-E)', 'v') ==# '<Esc>:<C-U>call EasyMotion#EW(1,2)<CR>'
        " }}}

        " End Of Word Motion IsKeyWord: {{{
        Expect maparg('<Plug>(easymotion-iskeyword-e)', 'n')    ==# ':<C-U>call EasyMotion#EK(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-e)', 'o')    ==# ':<C-U>call EasyMotion#EK(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-e)', 'v')    ==# '<Esc>:<C-U>call EasyMotion#EK(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-ge)', 'n')   ==# ':<C-U>call EasyMotion#EK(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-ge)', 'o')   ==# ':<C-U>call EasyMotion#EK(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-ge)', 'v')   ==# '<Esc>:<C-U>call EasyMotion#EK(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-bd-e)', 'n') ==# ':<C-U>call EasyMotion#EK(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-bd-e)', 'o') ==# ':<C-U>call EasyMotion#EK(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-iskeyword-bd-e)', 'v') ==# '<Esc>:<C-U>call EasyMotion#EK(1,2)<CR>'
        " }}}

        " JK Motion: {{{
        " default
        Expect maparg('<Plug>(easymotion-j)', 'n') ==# ':<C-U>call EasyMotion#JK(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-j)', 'o') ==# ':<C-U>call EasyMotion#JK(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-j)', 'v') ==# '<Esc>:<C-U>call EasyMotion#JK(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-k)', 'n') ==# ':<C-U>call EasyMotion#JK(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-k)', 'o') ==# ':<C-U>call EasyMotion#JK(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-k)', 'v') ==# '<Esc>:<C-U>call EasyMotion#JK(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-bd-jk)', 'n') ==# ':<C-U>call EasyMotion#JK(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-jk)', 'o') ==# ':<C-U>call EasyMotion#JK(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-jk)', 'v') ==# '<Esc>:<C-U>call EasyMotion#JK(1,2)<CR>'
        " start of line
        Expect maparg('<Plug>(easymotion-sol-j)', 'n') ==# ':<C-U>call EasyMotion#Sol(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-sol-j)', 'o') ==# ':<C-U>call EasyMotion#Sol(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-sol-j)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Sol(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-sol-k)', 'n') ==# ':<C-U>call EasyMotion#Sol(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-sol-k)', 'o') ==# ':<C-U>call EasyMotion#Sol(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-sol-k)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Sol(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-sol-bd-jk)', 'n') ==# ':<C-U>call EasyMotion#Sol(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sol-bd-jk)', 'o') ==# ':<C-U>call EasyMotion#Sol(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-sol-bd-jk)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Sol(1,2)<CR>'
        " end of line
        Expect maparg('<Plug>(easymotion-eol-j)', 'n') ==# ':<C-U>call EasyMotion#Eol(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-eol-j)', 'o') ==# ':<C-U>call EasyMotion#Eol(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-eol-j)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Eol(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-eol-k)', 'n') ==# ':<C-U>call EasyMotion#Eol(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-eol-k)', 'o') ==# ':<C-U>call EasyMotion#Eol(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-eol-k)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Eol(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-eol-bd-jk)', 'n') ==# ':<C-U>call EasyMotion#Eol(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-eol-bd-jk)', 'o') ==# ':<C-U>call EasyMotion#Eol(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-eol-bd-jk)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Eol(1,2)<CR>'
        " }}}

        " Search Motion: {{{
        Expect maparg('<Plug>(easymotion-n)', 'n') ==# ':<C-U>call EasyMotion#Search(0,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-n)', 'o') ==# ':<C-U>call EasyMotion#Search(0,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-n)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Search(1,0,0)<CR>'
        Expect maparg('<Plug>(easymotion-N)', 'n') ==# ':<C-U>call EasyMotion#Search(0,1,0)<CR>'
        Expect maparg('<Plug>(easymotion-N)', 'o') ==# ':<C-U>call EasyMotion#Search(0,1,0)<CR>'
        Expect maparg('<Plug>(easymotion-N)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Search(1,1,0)<CR>'
        Expect maparg('<Plug>(easymotion-bd-n)', 'n') ==# ':<C-U>call EasyMotion#Search(0,2,0)<CR>'
        Expect maparg('<Plug>(easymotion-bd-n)', 'o') ==# ':<C-U>call EasyMotion#Search(0,2,0)<CR>'
        Expect maparg('<Plug>(easymotion-bd-n)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Search(1,2,0)<CR>'

        " respect previous direction
        Expect maparg('<Plug>(easymotion-vim-n)', 'n') ==# ':<C-U>call EasyMotion#Search(0,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-vim-n)', 'o') ==# ':<C-U>call EasyMotion#Search(0,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-vim-n)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Search(1,0,1)<CR>'
        Expect maparg('<Plug>(easymotion-vim-N)', 'n') ==# ':<C-U>call EasyMotion#Search(0,1,1)<CR>'
        Expect maparg('<Plug>(easymotion-vim-N)', 'o') ==# ':<C-U>call EasyMotion#Search(0,1,1)<CR>'
        Expect maparg('<Plug>(easymotion-vim-N)', 'v') ==# '<Esc>:<C-U>call EasyMotion#Search(1,1,1)<CR>'
        " }}}

        " JumpToAnywhere Motion: {{{
        Expect maparg('<Plug>(easymotion-jumptoanywhere)', 'n')
            \ ==# ':<C-U>call EasyMotion#JumpToAnywhere(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-jumptoanywhere)', 'o')
            \ ==# ':<C-U>call EasyMotion#JumpToAnywhere(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-jumptoanywhere)', 'v')
            \ ==# '<Esc>:<C-U>call EasyMotion#JumpToAnywhere(1,2)<CR>'
        " }}}

        " Repeat Motion: {{{
        " (is_visual)
        Expect maparg('<Plug>(easymotion-repeat)', 'n')
            \ ==# ':<C-U>call EasyMotion#Repeat(0)<CR>'
        Expect maparg('<Plug>(easymotion-repeat)', 'o')
            \ ==# ':<C-U>call EasyMotion#Repeat(0)<CR>'
        Expect maparg('<Plug>(easymotion-repeat)', 'v')
            \ ==# '<Esc>:<C-U>call EasyMotion#Repeat(1)<CR>'
        Expect maparg('<Plug>(easymotion-dotrepeat)', 'n')
            \ ==# ':<C-U>call EasyMotion#DotRepeat()<CR>'
        Expect maparg('<Plug>(easymotion-dotrepeat)', 'o')
            \ ==# ':<C-U>call EasyMotion#DotRepeat()<CR>'
        Expect maparg('<Plug>(easymotion-dotrepeat)', 'v')
            \ ==# ':<C-U>call EasyMotion#DotRepeat()<CR>'
        " }}}

        " Next, Previous motion {{{
        Expect maparg('<Plug>(easymotion-next)', 'n')
            \ ==# ':<C-U>call EasyMotion#NextPrevious(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-next)', 'o')
            \ ==# ':<C-U>call EasyMotion#NextPrevious(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-next)', 'v')
            \ ==# ':<C-U>call EasyMotion#NextPrevious(1,0)<CR>'

        Expect maparg('<Plug>(easymotion-prev)', 'n')
            \ ==# ':<C-U>call EasyMotion#NextPrevious(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-prev)', 'o')
            \ ==# ':<C-U>call EasyMotion#NextPrevious(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-prev)', 'v')
            \ ==# ':<C-U>call EasyMotion#NextPrevious(1,1)<CR>'
        " }}}

        " Line Motion: {{{
        " word
        Expect maparg('<Plug>(easymotion-wl)', 'n') ==# ':<C-U>call EasyMotion#WBL(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-wl)', 'o') ==# ':<C-U>call EasyMotion#WBL(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-wl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBL(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-bl)', 'n') ==# ':<C-U>call EasyMotion#WBL(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-bl)', 'o') ==# ':<C-U>call EasyMotion#WBL(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-bl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBL(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-bd-wl)', 'n') ==# ':<C-U>call EasyMotion#WBL(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-wl)', 'o') ==# ':<C-U>call EasyMotion#WBL(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-wl)', 'v') ==# '<Esc>:<C-U>call EasyMotion#WBL(1,2)<CR>'
        " end of word
        Expect maparg('<Plug>(easymotion-el)', 'n')    ==# ':<C-U>call EasyMotion#EL(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-el)', 'o')    ==# ':<C-U>call EasyMotion#EL(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-el)', 'v')    ==# '<Esc>:<C-U>call EasyMotion#EL(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-gel)', 'n')   ==# ':<C-U>call EasyMotion#EL(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-gel)', 'o')   ==# ':<C-U>call EasyMotion#EL(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-gel)', 'v')   ==# '<Esc>:<C-U>call EasyMotion#EL(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-bd-el)', 'n') ==# ':<C-U>call EasyMotion#EL(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-el)', 'o') ==# ':<C-U>call EasyMotion#EL(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-bd-el)', 'v') ==# '<Esc>:<C-U>call EasyMotion#EL(1,2)<CR>'
        " LineAnywhere
        Expect maparg('<Plug>(easymotion-lineforward)', 'n')
            \ ==# ':<C-U>call EasyMotion#LineAnywhere(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-lineforward)', 'o')
            \ ==# ':<C-U>call EasyMotion#LineAnywhere(0,0)<CR>'
        Expect maparg('<Plug>(easymotion-lineforward)', 'v')
            \ ==# '<Esc>:<C-U>call EasyMotion#LineAnywhere(1,0)<CR>'
        Expect maparg('<Plug>(easymotion-linebackward)', 'n')
            \ ==# ':<C-U>call EasyMotion#LineAnywhere(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-linebackward)', 'o')
            \ ==# ':<C-U>call EasyMotion#LineAnywhere(0,1)<CR>'
        Expect maparg('<Plug>(easymotion-linebackward)', 'v')
            \ ==# '<Esc>:<C-U>call EasyMotion#LineAnywhere(1,1)<CR>'
        Expect maparg('<Plug>(easymotion-lineanywhere)', 'n')
            \ ==# ':<C-U>call EasyMotion#LineAnywhere(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-lineanywhere)', 'o')
            \ ==# ':<C-U>call EasyMotion#LineAnywhere(0,2)<CR>'
        Expect maparg('<Plug>(easymotion-lineanywhere)', 'v')
            \ ==# '<Esc>:<C-U>call EasyMotion#LineAnywhere(1,2)<CR>'
        " }}}

        " Activate {{{
        Expect maparg('<Plug>(easymotion-activate)', 'n')
            \ ==# ':<C-U>call EasyMotion#activate(0)<CR>'
        Expect maparg('<Plug>(easymotion-activate)', 'o')
            \ ==# ':<C-U>call EasyMotion#activate(0)<CR>'
        Expect maparg('<Plug>(easymotion-activate)', 'v')
            \ ==# ':<C-U>call EasyMotion#activate(1)<CR>'
        " }}}
    end

    it 'provide autoload functions'
        try
            " load autoload functions
            runtime autoload/EasyMotion.vim
            runtime autoload/EasyMotion/helper.vim
            runtime autoload/EasyMotion/cmigemo.vim
            runtime autoload/EasyMotion/init.vim
            runtime autoload/EasyMotion/sticky_table.vim
        catch
        endtry
        " autoload/EasyMotion.vim {{{
        Expect exists('*EasyMotion#hoge') ==# 0
        Expect exists('*EasyMotion#S') ==# 1
        Expect exists('*EasyMotion#T') ==# 1
        Expect exists('*EasyMotion#WB') ==# 1
        Expect exists('*EasyMotion#WBW') ==# 1
        Expect exists('*EasyMotion#E') ==# 1
        Expect exists('*EasyMotion#EW') ==# 1
        Expect exists('*EasyMotion#JK') ==# 1
        Expect exists('*EasyMotion#Sol') ==# 1
        Expect exists('*EasyMotion#Eol') ==# 1
        Expect exists('*EasyMotion#Search') ==# 1
        Expect exists('*EasyMotion#JumpToAnywhere') ==# 1
        Expect exists('*EasyMotion#SL') ==# 1
        Expect exists('*EasyMotion#TL') ==# 1
        Expect exists('*EasyMotion#WBL') ==# 1
        Expect exists('*EasyMotion#EL') ==# 1
        Expect exists('*EasyMotion#LineAnywhere') ==# 1
        Expect exists('*EasyMotion#Repeat') ==# 1
        Expect exists('*EasyMotion#NextPrevious') ==# 1
        Expect exists('*EasyMotion#DotRepeat') ==# 1
        "}}}
    end

    it 'provide variables to customize EasyMotion'
        " Option {{{
        Expect g:EasyMotion_keys ==# 'asdghklqwertyuiopzxcvbnmfj;'
        " Expect g:EasyMotion_keys ==# 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        Expect g:EasyMotion_do_mapping         ==# 1
        Expect g:EasyMotion_do_shade           ==# 1
        Expect g:EasyMotion_grouping           ==# 1
        Expect g:EasyMotion_startofline        ==# 1
        Expect g:EasyMotion_smartcase          ==# 0
        Expect g:EasyMotion_skipfoldedline     ==# 1
        Expect g:EasyMotion_use_migemo         ==# 0
        Expect g:EasyMotion_use_upper          ==# 0
        Expect g:EasyMotion_enter_jump_first   ==# 0
        Expect g:EasyMotion_space_jump_first   ==# 0
        Expect g:EasyMotion_show_prompt        ==# 1
        Expect g:EasyMotion_inc_highlight      ==# 1
        Expect g:EasyMotion_move_highlight     ==# 1
        Expect g:EasyMotion_landing_highlight  ==# 0
        Expect g:EasyMotion_cursor_highlight   ==# 1
        Expect g:EasyMotion_use_regexp         ==# 1
        Expect g:EasyMotion_add_search_history ==# 1
        Expect g:EasyMotion_off_screen_search  ==# 1
        Expect g:EasyMotion_prompt             ==# 'Search for {n} character(s): '
        Expect g:EasyMotion_command_line_key_mappings ==# {}
        Expect g:EasyMotion_force_csapprox ==# 0
        Expect g:EasyMotion_disable_two_key_combo ==# 0
        " }}}

        " highlight {{{
        Expect g:EasyMotion_hl_group_target ==# 'EasyMotionTarget'
        Expect g:EasyMotion_hl2_first_group_target ==# 'EasyMotionTarget2First'
        Expect g:EasyMotion_hl2_second_group_target ==# 'EasyMotionTarget2Second'
        Expect g:EasyMotion_hl_group_shade ==# 'EasyMotionShade'
        " }}}
    end
end
"}}}

function! AddLine(str)
    put! =a:str
endfunction

function! CursorPos()
    return [line('.'), col('.'), getline('.')[col('.')-1]]
endfunction

" s, f, F, t and T mappings {{{
describe 's, f, F, t and T mappings'

    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('poge huga hiyo poyo')
    end

    after
        close!
    end

    " s {{{
    it 'provide improved forward search like builtin f & F'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal sha
        Expect CursorPos() == [l,6,'h']
        Expect CursorPos() != [l,1,'h']

        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal shb
        Expect CursorPos() == [l,11,'h']

        normal sh
        Expect CursorPos() == [l,6,'h']

        normal! $
        let l = line('.')
        Expect CursorPos() == [l,19,'o']

        normal spc
        Expect CursorPos() == [l,19,'o']

        normal spb
        Expect CursorPos() == [l,1,'p']
    end
    "}}}
    " f {{{
    it 'provide improved backward search like builtin f'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']

        normal fha
        Expect CursorPos() == [l,6,'h']
        Expect CursorPos() != [l,1,'h']

        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal fhb
        Expect CursorPos() == [l,11,'h']

        normal fh
        Expect CursorPos() == [l,11,'h']

    end
    " "}}}
    " F {{{
    it 'provide improved backward search like builtin F'
        normal! $
        let l = line('.')
        Expect CursorPos() == [l,19,'o']

        normal Fpa
        Expect CursorPos() == [l,16,'p']

        normal! $
        normal Fpc
        Expect CursorPos() == [l,19,'o']

        normal Fpb
        Expect CursorPos() == [l,1,'p']
    end
    " "}}}
    " t {{{
    it 'provide t mapping like builtin t'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        Expect CursorPos() != [l,10,'a']

        normal tha
        Expect CursorPos() == [l,5,' ']

        normal! 0
        normal thb
        Expect CursorPos() == [l,10,' ']

        normal! 0
        normal thc
        Expect CursorPos() == [l,1,'p']
    end
    " }}}
    " T {{{
    it 'provide T mapping like builtin T'
        normal! $
        let l = line('.')
        Expect CursorPos() == [l,19,'o']
        Expect CursorPos() != [l,18,'a']

        normal Toa
        Expect CursorPos() == [l,18,'y']

        normal! $
        normal Tob
        Expect CursorPos() == [l,15,' ']

        normal Toa
        Expect CursorPos() == [l,3,'g']

        normal! $
        normal Tod
        Expect CursorPos() == [l,19,'o']

        normal Toc
        Expect CursorPos() == [l,3,'g']
    end
    "}}}
    " visual
    it 'have different context in normal mode and visual mode'
        let l = line('.')
        Expect CursorPos() == [l, 1, 'p']

        normal foa
        Expect CursorPos() == [l, 2, 'o']

        normal vfha
        Expect CursorPos() == [l, 6, 'h']

        normal fha
        Expect CursorPos() == [l, 11, 'h']

        normal! d
        Expect getline('.') == "piyo poyo"
        Expect CursorPos() == [l, 2, 'i']

        normal! dfpa
        Expect getline('.') == "poyo"
        Expect CursorPos() == [l, 2, 'o']
    end
end
"}}}

" a non-existent char {{{
describe 'a non-existent char'

    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('poge huga hiyo poyo')
    end

    after
        close!
    end

    " makes no change {{{
    it 'makes no change'
        normal! 0
        let origin = CursorPos()

        normal fd
        Expect CursorPos() == origin
        normal f1
        Expect CursorPos() == origin
        normal f)
        Expect CursorPos() == origin
        normal f^
        Expect CursorPos() == origin
        normal fm
        Expect CursorPos() == origin
    end
    "}}}
end
"}}}

" when target is in other line, s, f and F mappings {{{
describe 'when target is in other line, s, f and F mappings'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('foo bar baz')         " L2
        call AddLine('poge huga hiyo poyo') " L1
        "            '1234567890123456789'
        normal! gg0
    end

    after
        close!
    end

    " move cursor forward across lines {{{
    it 'move cursor forward & backward across lines'
        normal! 0
        let l = line('.')
        Expect col('.') == 1

        normal saa
        Expect CursorPos() == [l, 9, 'a']

        normal saa
        Expect CursorPos() == [l+1, 6, 'a']

        normal sob
        Expect CursorPos() == [l+1, 2, 'o']

        normal! gg0
        let l = line('.')
        Expect col('.') == 1

        normal faa
        Expect CursorPos() == [l, 9, 'a']

        normal faa
        Expect CursorPos() == [l+1, 6, 'a']

        normal faa
        Expect CursorPos() == [l+1, 10, 'a']

        normal Faa
        Expect CursorPos() == [l+1, 6, 'a']

        normal Faa
        Expect CursorPos() == [l, 9, 'a']
    end
    " }}}
end
"}}}

" Multibyte characters {{{
describe 'Multibyte characters'

    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('ビムかわいいよzビムx')
        call AddLine('foo bar baz')
        normal! gg
    end

    after
        close!
    end

    " is supported "{{{
    it 'is supported'
        normal! gg0
        let l = line('.')

        normal fza
        Expect CursorPos() == [l, 11, 'z']
        Expect CursorPos() != [l, 1, '2']

        normal fz
        Expect CursorPos() == [l+1, 22, 'z']

        normal! h
        normal fxa
        Expect CursorPos() == [l+1, 29, 'x']
    end
    " }}}

end
"}}}

" EasyMotion#helper#include_multibyte_char {{{
describe 'EasyMotion#helper#include_multibyte_char'

    it 'return true when the argument includes multibyte char'
        Expect EasyMotion#helper#include_multibyte_char("あいうえお") ==# 1
        Expect EasyMotion#helper#include_multibyte_char("aiueoあ") ==# 1
        Expect EasyMotion#helper#include_multibyte_char("１２3ABC４5") ==# 1
    end

    it 'return false when the argument does not include multibyte char'
        Expect EasyMotion#helper#include_multibyte_char("aiueo") ==# 0
        Expect EasyMotion#helper#include_multibyte_char("this_is_a_pen.") ==# 0
        Expect EasyMotion#helper#include_multibyte_char("!#$%&'()'") ==# 0
        Expect EasyMotion#helper#include_multibyte_char("") ==# 0
    end

end
"}}}

" migemo support {{{
describe 'migemo support'

    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        let g:EasyMotion_use_migemo = 1
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('はー，ビムかわいいよビム')
        normal! gg0
    end

    after
        close!
        let g:clever_f_use_migemo = 0
    end

    " makes f and F mapping match multibyte characters {{{
    it 'makes f and F mapping match multibyte characters'
        normal fba
        Expect col('.') == 10
        normal fb
        Expect col('.') == 31
        normal Fb
        Expect col('.') == 10
        normal $
        normal Fba
        Expect col('.') == 31
        normal Fb
        Expect col('.') == 10
    end
    " }}}

    " makes t and T mapping match multibyte characters {{{
    it 'makes t and T mapping match multibyte characters'
        normal tba
        Expect col('.') == 7
        normal tb
        Expect col('.') == 28
        normal Tb
        Expect col('.') == 13
        normal $
        normal Tba
        Expect col('.') == 13
        normal tb
        Expect col('.') == 28
    end
    " }}}

end
"}}}

" g:EasyMotion_smartcase {{{
describe 'g:EasyMotion_smartcase'

    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('poHe huga Hiyo hoyo: poyo();')
                      1234567890123456789012345678
        normal! gg0
        let g:EasyMotion_smartcase = 1
    end

    after
        close!
        let g:EasyMotion_smartcase = 0
    end

    " makes f smart case {{{
    it 'makes f smart case'
        normal fha
        Expect col('.') == 3
        normal fha
        Expect col('.') == 6
        normal fha
        Expect col('.') == 11
        normal fh
        Expect col('.') == 16
        normal Fha
        Expect col('.') == 11

        normal! 0
        normal fHa
        Expect col('.') == 3
        normal fH
        Expect col('.') == 11
        normal fHa
        Expect col('.') == 11
        normal FH
        Expect col('.') == 3
    end
    "}}}

    " makes t smart case {{{
    it 'makes t smart case'
        normal! $
        normal Tha
        Expect col('.') == 17
        normal Tha
        Expect col('.') == 12
        normal Tha
        Expect col('.') == 7
        normal Th
        Expect col('.') == 4
        normal tha
        Expect col('.') == 5

        normal! $
        normal THa
        Expect col('.') == 12
        normal TH
        Expect col('.') == 4
        normal tH
        Expect col('.') == 10
    end
    " }}}

    " makes no effect on searching signs {{{
    it 'makes no effect on searching signs'
        normal! 0
        normal f;
        Expect col('.') == 28
        normal! 0
        normal f:
        Expect col('.') == 20
        normal f:
        Expect col('.') == 20
    end
    " }}}
end
"}}}

" g:EasyMotion_smartsign {{{
describe 'g:EasyMotion_smartsign'

    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('poHe huga Hiyo hoyo: poyo();')
        "             1234567890123456789012345678
        normal! gg0
        let g:EasyMotion_smartcase = 1
        let g:EasyMotion_use_smartsign_us = 1
    end

    after
        close!
        let g:EasyMotion_smartcase = 0
        let g:EasyMotion_use_smartsign_us = 0
    end

    " makes f smart sign {{{
    it 'makes f smart case'
        normal! 0
        normal f;
        Expect col('.') == 1
        normal f;a
        Expect col('.') == 20
        normal f;
        Expect col('.') == 28

        normal! 0
        normal f:
        Expect col('.') == 20
        normal f:
        Expect col('.') == 20
    end
    "}}}

    " makes t,T smart sign {{{
    it 'makes t,T smart case'
        normal! 0
        normal t;
        Expect col('.') == 1
        normal t;a
        Expect col('.') == 19
        normal t;
        Expect col('.') == 27
        normal! $
        Expect col('.') == 28
        normal T;
        Expect col('.') == 21
    end
    "}}}
end
"}}}

" Next & Previous {{{
describe '<Plug>(easymotion-next) & <Plug>(easymotion-prev)'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        map ; <Plug>(easymotion-next)
        map , <Plug>(easymotion-prev)
        set wrapscan
        call EasyMotion#init()
        call AddLine('poge huga hiyo poyo')
        "             1234567890123456789
    end

    after
        close!
    end

    " provide next & previous motion to replace `;`, `,` {{{
    it 'provide next & previous motion to replace `;`, `,`'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal sha
        Expect CursorPos() == [l,6,'h']

        normal ;
        Expect CursorPos() == [l,11,'h']

        normal ,
        Expect CursorPos() == [l,6,'h']

        " wrapscan
        normal ,
        Expect CursorPos() == [l,11,'h']
        normal ;
        Expect CursorPos() == [l,6,'h']

        normal! $
        let l = line('.')
        Expect CursorPos() == [l,19,'o']

        normal ,
        Expect CursorPos() == [l,11,'h']

    end
    "}}}

    " next & previous motion count {{{
    it 'next & previous motion count'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal sha
        Expect CursorPos() == [l,6,'h']

        normal ;
        Expect CursorPos() == [l,11,'h']

        normal 2,
        Expect CursorPos() == [l,11,'h']

        " wrapscan
        normal 4,
        Expect CursorPos() == [l,11,'h']
        normal 3;
        Expect CursorPos() == [l,6,'h']

        normal! $
        let l = line('.')
        Expect CursorPos() == [l,19,'o']

        normal ,
        Expect CursorPos() == [l,11,'h']

    end
    "}}}
end
"}}}

" Jumplist {{{
describe 'EasyMotion is jump motion'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map s <Plug>(easymotion-s)
        call EasyMotion#init()
        call AddLine('poge huga hiyo poyo')
        "             1234567890123456789
    end

    after
        close!
    end

    " <C-o> could jump back to previous location {{{
    it '<C-o> could jump back to previous location'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal sha
        Expect CursorPos() == [l,6,'h']

        exec "normal! \<C-o>"
        Expect CursorPos() == [l,1,'p']

        normal! ``
        Expect CursorPos() == [l,6,'h']
        normal! ``
        Expect CursorPos() == [l,1,'p']
        normal! ``
        Expect CursorPos() == [l,6,'h']

        normal! $
        Expect CursorPos() == [l,19,'o']
        normal spa
        Expect CursorPos() == [l,16,'p']
        exec "normal! \<C-o>"
        Expect CursorPos() == [l,19,'o']
    end
    "}}}
end
"}}}

" Regexp {{{
describe 'EasyMotion regexp'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        let g:EasyMotion_use_regexp = 1
        map s <Plug>(easymotion-sn)
        call EasyMotion#init()
        call AddLine('poge1 2huga 3hiyo 4poyo')
        "             12345678901234567890123
    end

    after
        let g:EasyMotion_use_regexp = 0
        close!
    end

    " <C-o> could jump back to previous location {{{
    it 'provide regexp motion'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        exe "normal s\\d\<CR>a"
        Expect CursorPos() == [l,5,'1']

        normal! 0
        Expect CursorPos() == [l,1,'p']
        exe "normal s\\d\<CR>c"
        Expect CursorPos() == [l,13,'3']

        exe "normal s\$\<CR>a"
        Expect CursorPos() == [l,23,'o']

        exe "normal s\^\<CR>b"
        Expect CursorPos() == [l,1,'p']
    end
    "}}}
end
"}}}

" bi-directional t motion {{{
describe 'bi-directional t motion'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        let g:EasyMotion_use_regexp = 1
        map t <Plug>(easymotion-bd-t)
        call EasyMotion#init()
        call AddLine('poge1 2huga 3hiyo 4poyo')
        "             12345678901234567890123
    end

    after
        let g:EasyMotion_use_regexp = 0
        close!
    end

    " provide bidirectional motion with one key mapping {{{
    it 'provide bidirectional motion with one key mapping'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal thb
        Expect CursorPos() == [l,13,'3']

        normal! h
        Expect CursorPos() == [l,12,' ']

        normal thb
        Expect CursorPos() == [l,9,'u']

    end
    "}}}
end
"}}}

" off-screen search {{{
describe 'off-screen search'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        let g:EasyMotion_off_screen_search = 1
        map s/ <Plug>(easymotion-sn)
        map f/ <Plug>(easymotion-fn)
        map F/ <Plug>(easymotion-Fn)
        map t/ <Plug>(easymotion-tn)
        map T/ <Plug>(easymotion-Tn)
        call EasyMotion#init()
        call AddLine('deco-chan deco-chan')
        call AddLine('vim')
        for i in range(50)
            call AddLine('poge1 2huga 3hiyo 4poyo')
        endfor
        "             12345678901234567890123
    end

    after
        let g:EasyMotion_off_screen_search = 0
        close!
    end

    " provide search with off-screen range {{{
    it 'provide search with off-screen range'
        normal! gg0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']

        exec "normal s/vim\<CR>"
        Expect CursorPos() == [51,1,'v']

        normal! gg0
        exec "normal f/im\<CR>"
        Expect CursorPos() == [51,2,'i']

        set wrapscan
        Expect &wrapscan == 1
        normal! gg0
        exec "normal F/im\<CR>"
        Expect CursorPos() == [51,2,'i']

        " Cancel
        normal! gg0
        exec "normal s/vim\<Esc>"
        Expect CursorPos() == [l,1,'p']

        " Label
        normal! gg0
        exec "normal s/deco-chan\<CR>\<Esc>"
        Expect CursorPos() == [l,1,'p']

        normal! gg0
        exec "normal s/deco-chan\<CR>a"
        Expect CursorPos() == [52,1,'d']

        normal! gg0
        exec "normal s/deco-chan\<CR>b"
        Expect CursorPos() == [52,11,'d']

        normal! gg0
        exec "normal t/chan\<CR>a"
        Expect CursorPos() == [52,5,'-']

        normal! gg0
        exec "normal t/chan\<CR>b"
        Expect CursorPos() == [52,15,'-']

    end
    "}}}
end

describe 'dot notoff-screen search'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        let g:EasyMotion_off_screen_search = 0
        map s/ <Plug>(easymotion-sn)
        call EasyMotion#init()
        call AddLine('deco-chan deco-chan')
        call AddLine('vim')
        for i in range(50)
            call AddLine('poge1 2huga 3hiyo 4poyo')
        endfor
        "             12345678901234567890123
    end

    after
        close!
    end

    " provide search with off-screen range {{{
    it 'provide search with off-screen range'
        normal! gg0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']

        exec "normal s/vim\<CR>"
        Expect CursorPos() != [51,1,'v']
        Expect CursorPos() == [l,1,'p']
    end
    "}}}
end
"}}}

" off-screen search scroll {{{
describe 'off-screen search scroll'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        let g:EasyMotion_off_screen_search = 1
        map / <Plug>(easymotion-sn)
        call EasyMotion#init()
        call AddLine('deco-chan deco-chan')
        call AddLine('vim')
        for i in range(500)
            call AddLine('poge1 2huga 3hiyo 4poyo')
        endfor
        call AddLine('deco-chan deco-chan')
        "             12345678901234567890123
    end

    after
        let g:EasyMotion_off_screen_search = 0
        close!
    end

    " provide scroll {{{
    it 'provide scroll'
        normal! gg0
        let l = line('.')
        Expect CursorPos() == [l,1,'d']

        normal! gg0
        exec "normal /deco-chan\<CR>"
        Expect CursorPos() == [l,11,'d']

        normal! gg0
        exec "normal /deco-chan\<Tab>\<CR>a"
        Expect CursorPos() == [503,1,'d']

    end
    "}}}
end
"}}}

"Word motion {{{
describe 'Word motion'
    before
        new
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        map w <Plug>(easymotion-w)
        map b <Plug>(easymotion-b)
        map <Leader>w <Plug>(easymotion-iskeyword-w)
        map <Leader>b <Plug>(easymotion-iskeyword-b)
        call EasyMotion#init()
        call AddLine('vim vim vim')
        call AddLine('poge1 2huga 3hiyo 4poyo 5:test')
        "             12345678901234567890123
        " 0        1         2         3
        " 123456789012345678901234567890
        " poge1 2huga 3hiyo 4poyo 5:test
        " vim vim vim
    end

    after
        close!
    end

    " Word motion {{
    it 'Word motion'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,'p']
        normal wc
        Expect CursorPos() == [l,19,'4']

        normal bb
        Expect CursorPos() == [l,7,'2']

        normal! 0
        Expect CursorPos() == [l,1,'p']

        normal wh
        Expect CursorPos() == [2,9,'v']
        normal bh
        Expect CursorPos() == [l,1,'p']
    end
    "}}
end

describe 'Verbose'
    before
        new
        map s <Plug>(easymotion-s)
        map f <Plug>(easymotion-f)
        map F <Plug>(easymotion-F)
        map t <Plug>(easymotion-t)
        map T <Plug>(easymotion-T)
        call EasyMotion#init()
        call AddLine('some words in the sentence')
    end

    after
        close!
    end

    it 'Verbose global variable'
        Expect g:EasyMotion_verbose ==# 1
    end

    it 'Turned On'
        let g:EasyMotion_verbose = 1
        let &verbosefile = tempname()
        normal sa
        " TODO: l:tmp_name_verbose should have one line
    end
    it 'Turned Off'
        let g:EasyMotion_verbose = 0
        let &verbosefile = &verbosefile
        normal s_
        " TODO: l:tmp_name_not_verbose should have no lines
    end
end
"}}}

" vim: fdm=marker:et:ts=4:sw=4:sts=4
