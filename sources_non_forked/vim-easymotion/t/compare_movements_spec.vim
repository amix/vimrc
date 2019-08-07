"=============================================================================
" FILE: t/compare_movements_spec.vim
" AUTHOR: YggdrasiI
" Test: https://github.com/kana/vim-vspec
" Description: EasyMotion keyword movement test with vim-vspec
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

" Setup {{{
let s:root_dir = matchstr(system('git rev-parse --show-cdup'), '[^\n]\+')

" The consumed time depends from the length of the text and could be really high
" on vimdoc pages. (See it 'Loop through Vim help buffer and compare movements')
" Reduce this value to stop CompareMovements(...) before it reached the end of the
" buffer.
let s:maximal_number_of_compared_movments = 10000
execute 'set' 'rtp +=./'.s:root_dir
runtime! plugin/EasyMotion.vim
" }}}

" Functions for Test {{{
function! AddLine(str)
    put =a:str
endfunction

function! CursorPos()
    return [line('.'), col('.'), getline('.')[col('.')-1]]
endfunction

" Nested normal to avoid throwing readonly errors. They abort the testing.
function TryNormal(str)
    try
        exec 'normal ' . a:str
    catch /^Vim\%((\a\+)\)\=:E21/
    endtry
    return 0
endfunction

let s:to_cursor = {}
function! s:to_cursor.match(actual, expected)
    return a:actual == a:expected
endfunction

" Add metadata about failure.
function! s:to_cursor.failure_message_for_should(actual, expected)
    Expect a:actual[0] > 0
    Expect a:expected[0] > 0
    Expect a:actual[0] <= getpos('$')[1]
    Expect a:expected[0] <= getpos('$')[1]
    Expect a:actual[1] > 0
    Expect a:expected[1] > 0

    let line1 = getline(a:actual[0])
    let line2 = getline(a:expected[0])
    " Change char on cursor to '█'. 
    let line1 = strpart(l:line1, 0, a:actual[1]-1)
                \ . '█'
                \ . strpart(l:line1, a:actual[1])
    let line2 = strpart(l:line2, 0, a:expected[1]-1)
                \ . '█'
                \ . strpart(l:line2, a:expected[1])
    " Separation of both cases with \n would be nice, but
    " vim-vspec allow oneliners as return string, only.
    let msg = 'Line ' . string(a:actual[0]) . ": '" . l:line1
                \ . "',\x09\x09 Line " . string(a:expected[0]) . ": '" . l:line2 . "'\x0a"
    return l:msg
endfunction

function! CompareMovements(movement1, movement2, backward)
    let jumpmarks = [
                \ [a:movement1, []],
                \ [a:movement2, []],
                \ ]

    " Loop through current buffer in both variants {{
    for [l:handler, l:list] in l:jumpmarks
        if a:backward == 1
            let last_line = line('$')
            let last_char = len(getline(l:last_line))
            call cursor(l:last_line, l:last_char)
        else
            call cursor([1,1])
        endif

        let lastpos = [0,0]

        " Centralize line. Otherwise, Easymotion functions aborts
        " at the end of the (virtual) window.
        call TryNormal('zz')
        call TryNormal(l:handler)
        let curpos = getpos(".")[1:2]

        while l:lastpos != l:curpos 
            let list += [l:curpos]
            let lastpos = l:curpos
            call TryNormal('zz')
            call TryNormal(l:handler)
            let curpos = getpos(".")[1:2]
            " Abort after a fixed number of steps.
            if len(l:list) > s:maximal_number_of_compared_movments
                break
            endif
        endwhile
    endfor
    " }}

    " The resulting lists are stored in l:jumpmarks[*][1], now.
    let [l:cursor_positions1, l:cursor_positions2] = [ l:jumpmarks[0][1], l:jumpmarks[1][1] ]

    if l:cursor_positions1 == l:cursor_positions2
        return 0
    endif

    " Search for first unmatching position. {{
    let index = 0
    let len = min([len(l:cursor_positions2), len(l:cursor_positions1)]) 
    while l:index < l:len
        Expect l:cursor_positions2[l:index] to_cursor l:cursor_positions1[l:index]
        let index += 1
    endwhile

    " Collision with begin or end of file or while loop aborts to early.
    if a:backward == 1
        Expect join([a:movement2, ': File begin reached after ', len(l:cursor_positions2), ' steps.'])
                    \ == join([a:movement1, ': File begin reached after ', len(l:cursor_positions1), ' steps.'])
    else
        Expect l:cursor_positions2[l:index-1] to_cursor l:cursor_positions1[l:index]
        Expect join([a:movement2, ': File end reached after ', len(l:cursor_positions2), ' steps.'])
                    \ == join([a:movement1, ': File end reached after ', len(l:cursor_positions1), ' steps.'])
    endif
    " }}

    return -1
endfunction

" Hand crafted text with rare cases
function! InsertTestText1()

    " Blanks at document begin
    call AddLine('')
    call AddLine(' ')
    call AddLine('')

    call AddLine('scriptencoding utf-8')
    
    " '^\s*[not-\k]'-case
    call AddLine('!foo')
    call AddLine('   !bar')

    call AddLine('<!{}>s!  ')

    " Blanks at document end
    call AddLine('')
    call AddLine(' ')
    call AddLine('')
endfunction

"}}}

"Keyword word motion {{{
describe 'Keyword word motion'
    before
        new
        resize 10
        nmap a <Nop>
        let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        let g:EasyMotion_maximal_jumpmarks  = 2 " Error for value 1 unanalyzed. 
        nmap <Leader>w <Plug>(easymotion-iskeyword-w)
        nmap <Leader>b <Plug>(easymotion-iskeyword-b)
        nmap <Leader>e <Plug>(easymotion-iskeyword-e)
        nmap <Leader>ge <Plug>(easymotion-iskeyword-ge)
        nmap <Leader>W <Plug>(easymotion-W)
        nmap <Leader>B <Plug>(easymotion-B)
        nmap <Leader>E <Plug>(easymotion-E)
        nmap <Leader>gE <Plug>(easymotion-gE)
        call EasyMotion#init()
        call vspec#customize_matcher('to_cursor', s:to_cursor)
    end

    after
        close!
    end

    it 'Simple test to check setup of this test'
        " Check if a is remapped to <Nop> to avoid start of insert mode.
        normal aa\<Esc>
        Expect getline(1) == ''

        call AddLine('word')
        Expect CompareMovements('w', 'w', 0) == 0
        Expect CompareMovements('w', '\wa', 0) == 0
        Expect CompareMovements('b', '\ba', 1) == 0
        Expect CompareMovements('e', '\ea', 0) == 0
        Expect CompareMovements('ge', '\gea', 1) == 0
        Expect CompareMovements('W', '\Wa', 0) == 0
        Expect CompareMovements('B', '\Ba', 1) == 0
        Expect CompareMovements('E', '\Ea', 0) == 0
        Expect CompareMovements('gE', '\gEa', 1) == 0
    end

    it 'w'
        call InsertTestText1()
        Expect CompareMovements('w', '\wa', 0) == 0
    end

    it 'b'
        call InsertTestText1()
        Expect CompareMovements('b', '\ba', 1) == 0
    end

    it 'e'
        call InsertTestText1()
        Expect CompareMovements('e', '\ea', 0) == 0
    end

    it 'ge'
        call InsertTestText1()
        Expect CompareMovements('ge', '\gea', 1) == 0
    end

    it 'W'
        call InsertTestText1()
        Expect CompareMovements('W', 'W', 0) == 0
    end

    it 'B'
        call InsertTestText1()
        Expect CompareMovements('B', 'B', 1) == 0
    end

    it 'E'
        call InsertTestText1()
        Expect CompareMovements('E', 'E', 0) == 0
    end

    it 'gE'
        call InsertTestText1()
        Expect CompareMovements('gE', 'gE', 1) == 0
    end

    " Really time consuming test...
    "it 'Loop through Vim help buffer and compare movements'
    "    help motion.txt
    "    Expect expand('%:t') ==# 'motion.txt'
    "    "Optional: Copy text into editable buffer
    "    exec "normal! Gygg\<C-W>cP"
    "    Expect CompareMovements('w', '\wa', 0) == 0
    "end

end
"}}}

" __END__  {{{
" vim: fdm=marker:et:ts=4:sw=4:sts=4
" }}}
