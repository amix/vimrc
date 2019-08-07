"=============================================================================
" FILE: autoload/EasyMotion/helper.vim
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
scriptencoding utf-8
" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

function! EasyMotion#helper#mode(flag) "{{{
    return mode(a:flag) == "\<C-v>" ? "C-v" : mode(a:flag)
endfunction "}}}

function! EasyMotion#helper#get_char_by_coord(coord) "{{{
    " @param coord: [lnum, col] or [bufnum, lnum, col, off]
    if len(a:coord) == 4
        let [line_num, col_num] = [a:coord[1], a:coord[2]]
    else
        let [line_num, col_num] = a:coord
    endif
    let target_col_regexp = '\%' . (col_num) . 'c.'
    return matchstr(getline(line_num), target_col_regexp)
endfunction "}}}

function! EasyMotion#helper#is_greater_coords(coords1, coords2) "{{{
    " [line_num, col_num] < [line_num, col_num]
    "
    " coords1 < coords2  : return 1
    " coords1 > coords2  : return -1
    " coords1 == coords2 : return 0
    if a:coords1 == a:coords2 | return 0 | endif

    if a:coords1[0] < a:coords2[0]
        return 1
    elseif a:coords1[0] > a:coords2[0]
        return -1
    endif

    " Same line
    if a:coords1[1] < a:coords2[1]
        return 1
    elseif a:coords1[1] > a:coords2[1]
        return -1
    endif
endfunction "}}}

function! EasyMotion#helper#is_folded(line) "{{{
    " Return false if g:EasyMotion_skipfoldedline == 1
    " and line is start of folded lines
    let _foldclosed = foldclosed(a:line)
    return _foldclosed != -1 &&
        \ (g:EasyMotion_skipfoldedline == 1 || a:line != _foldclosed)
endfunction "}}}
function! EasyMotion#helper#should_case_sensitive(input, is_search) "{{{
    if !a:is_search
        if g:EasyMotion_smartcase == 0
            return 0
        else
            " return 1 if input didn't match uppercase letter
            return match(a:input, '\u') == -1
        endif
    endif

    if (g:EasyMotion_smartcase == 1 && match(a:input, '\u') == -1) ||
    \  (&ignorecase && &smartcase && match(a:input, '\u') == -1) ||
    \  (&ignorecase && !&smartcase)
        return 1
    endif
    return 0
endfunction "}}}
function! EasyMotion#helper#silent_feedkeys(expr, name, ...) "{{{
    " Ref:
    " https://github.com/osyo-manga/vim-over/blob/d51b028c29661d4a5f5b79438ad6d69266753711/autoload/over.vim#L6
    let mode = get(a:, 1, "m")
    let name = "easymotion-" . a:name
    let map = printf("<Plug>(%s)", name)
    if mode == "n"
        let command = "nnoremap"
    else
        let command = "nmap"
    endif
    execute command "<silent>" map printf("%s:nunmap %s<CR>", a:expr, map)
    if mode(1) !=# 'ce'
        " FIXME: mode(1) !=# 'ce' exists only for the test
        "        :h feedkeys() doesn't work while runnning a test script
        "        https://github.com/kana/vim-vspec/issues/27
        call feedkeys(printf("\<Plug>(%s)", name))
    endif
endfunction "}}}
function! EasyMotion#helper#VarReset(var, ...) "{{{
    if ! exists('s:var_reset')
        let s:var_reset = {}
    endif

    if a:0 == 0 && has_key(s:var_reset, a:var)
        " Reset var to original value
        " setbufvar( or bufname): '' or '%' can be used for the current buffer
        call setbufvar('%', a:var, s:var_reset[a:var])
    elseif a:0 == 1
        " Save original value and set new var value

        let new_value = a:0 == 1 ? a:1 : ''

        " Store original value
        let s:var_reset[a:var] = getbufvar("", a:var)

        " Set new var value
        call setbufvar('%', a:var, new_value)
    endif
endfunction "}}}

" Migemo {{{
function! EasyMotion#helper#load_migemo_dict() "{{{
    let enc = &l:encoding
    if enc ==# 'utf-8'
        return EasyMotion#migemo#utf8#load_dict()
    elseif enc ==# 'cp932'
        return EasyMotion#migemo#cp932#load_dict()
    elseif enc ==# 'euc-jp'
        return EasyMotion#migemo#eucjp#load_dict()
    else
        let g:EasyMotion_use_migemo = 0
        throw "Error: ".enc." is not supported. Migemo is made disabled."
    endif
endfunction "}}}

" EasyMotion#helper#strchars() {{{
if exists('*strchars')
    function! EasyMotion#helper#strchars(str)
        return strchars(a:str)
    endfunction
else
    function! EasyMotion#helper#strchars(str)
        return strlen(substitute(a:str, ".", "x", "g"))
    endfunction
endif "}}}
function! EasyMotion#helper#include_multibyte_char(str) "{{{
    return strlen(a:str) != EasyMotion#helper#strchars(a:str)
endfunction "}}}

function! EasyMotion#helper#vcol(expr) abort
    let col_num = col(a:expr)
    let line = getline(a:expr)
    let before_line = col_num > 2 ? line[: col_num - 2]
    \   : col_num is# 2 ? line[0]
    \   : ''
    let vcol_num = 1
    for c in split(before_line, '\zs')
        let vcol_num += c is# "\t" ? s:_virtual_tab2spacelen(vcol_num) : len(c)
    endfor
    return vcol_num
endfunction

function! s:_virtual_tab2spacelen(col_num) abort
    return &tabstop - ((a:col_num - 1) % &tabstop)
endfunction

"}}}

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
unlet s:save_cpo
" }}}
" vim: fdm=marker:et:ts=4:sw=4:sts=4
