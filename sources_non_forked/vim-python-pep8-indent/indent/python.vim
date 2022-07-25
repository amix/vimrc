" PEP8 compatible Python indent file
" Language:         Python
" Maintainer:       Daniel Hahler <https://daniel.hahler.de/>
" Prev Maintainer:  Hynek Schlawack <hs@ox.cx>
" Prev Maintainer:  Eric Mc Sween <em@tomcom.de> (address invalid)
" Original Author:  David Bustos <bustos@caltech.edu> (address invalid)
" License:          CC0
"
" vim-python-pep8-indent - A nicer Python indentation style for vim.
" Written in 2004 by David Bustos <bustos@caltech.edu>
" Maintained from 2004-2005 by Eric Mc Sween <em@tomcom.de>
" Maintained from 2013 by Hynek Schlawack <hs@ox.cx>
" Maintained from 2017 by Daniel Hahler <https://daniel.hahler.de/>
"
" To the extent possible under law, the author(s) have dedicated all copyright
" and related and neighboring rights to this software to the public domain
" worldwide. This software is distributed without any warranty.
" You should have received a copy of the CC0 Public Domain Dedication along
" with this software. If not, see
" <http://creativecommons.org/publicdomain/zero/1.0/>.

" Only load this indent file when no other was loaded.
if exists('b:did_indent')
    finish
endif
let b:did_indent = 1

setlocal nolisp
setlocal autoindent
setlocal indentexpr=GetPythonPEPIndent(v:lnum)
setlocal indentkeys=!^F,o,O,<:>,0),0],0},=elif,=except

if !exists('g:python_pep8_indent_multiline_string')
    let g:python_pep8_indent_multiline_string = 0
endif

if !exists('g:python_pep8_indent_hang_closing')
    let g:python_pep8_indent_hang_closing = 0
endif

" TODO: check required patch for timeout argument, likely lower than 7.3.429 though.
if !exists('g:python_pep8_indent_searchpair_timeout')
    if has('patch-8.0.1483')
        let g:python_pep8_indent_searchpair_timeout = 150
    else
        let g:python_pep8_indent_searchpair_timeout = 0
    endif
endif

let s:block_rules = {
      \ '^\s*elif\>': [['if', 'elif'], ['else']],
      \ '^\s*except\>': [['try', 'except'], []],
      \ '^\s*finally\>': [['try', 'except', 'else'], []]
      \ }
let s:block_rules_multiple = {
      \ '^\s*else\>': [['if', 'elif', 'for', 'try', 'except'], []]
      \ }
" Pairs to look for when searching for opening parenthesis.
" The value is the maximum offset in lines.
let s:paren_pairs = {'()': 50, '[]': 100, '{}': 1000}

if &filetype ==# 'pyrex' || &filetype ==# 'cython'
    let b:control_statement = '\v^\s*(class|def|if|while|with|for|except|cdef|cpdef)>'
else
    let b:control_statement = '\v^\s*(class|def|if|while|with|for|except)>'
endif
let s:stop_statement = '^\s*\(break\|continue\|raise\|return\|pass\)\>'

let s:skip_after_opening_paren = 'synIDattr(synID(line("."), col("."), 0), "name") ' .
            \ '=~? "\\vcomment|jedi\\S"'

let s:special_chars_syn_pattern = "\\vstring|comment|^pythonbytes%(contents)=$|pythonTodo|jedi\\S"

if !get(g:, 'python_pep8_indent_skip_concealed', 0) || !has('conceal')
    " Skip strings and comments. Return 1 for chars to skip.
    " jedi* refers to syntax definitions from jedi-vim for call signatures, which
    " are inserted temporarily into the buffer.
    function! s:_skip_special_chars(line, col)
        return synIDattr(synID(a:line, a:col, 0), 'name')
              \ =~? s:special_chars_syn_pattern
    endfunction
else
    " Also ignore anything concealed.
    " TODO: doc; likely only necessary with jedi-vim, where a better version is
    " planned (https://github.com/Vimjas/vim-python-pep8-indent/pull/98).

    " Wrapper around synconcealed for older Vim (7.3.429, used on Travis CI).
    function! s:is_concealed(line, col)
        let concealed = synconcealed(a:line, a:col)
        return len(concealed) && concealed[0]
    endfunction

    function! s:_skip_special_chars(line, col)
        return synIDattr(synID(a:line, a:col, 0), 'name')
              \ =~? s:special_chars_syn_pattern
              \ || s:is_concealed(a:line, a:col)
    endfunction
endif

" Use 'shiftwidth()' instead of '&sw'.
" (Since Vim patch 7.3.629, 'shiftwidth' can be set to 0 to follow 'tabstop').
if exists('*shiftwidth')
    function! s:sw()
        return shiftwidth()
    endfunction
else
    function! s:sw()
        return &shiftwidth
    endfunction
endif

" Find backwards the closest open parenthesis/bracket/brace.
function! s:find_opening_paren(lnum, col)
    " Return if cursor is in a comment.
    if synIDattr(synID(a:lnum, a:col, 0), 'name') =~? 'comment'
        return [0, 0]
    endif

    call cursor(a:lnum, a:col)

    let nearest = [0, 0]
    let timeout = g:python_pep8_indent_searchpair_timeout
    let skip_special_chars = 's:_skip_special_chars(line("."), col("."))'
    for [p, maxoff] in items(s:paren_pairs)
        let stopline = max([0, line('.') - maxoff, nearest[0]])
        let next = searchpairpos(
           \ '\V'.p[0], '', '\V'.p[1], 'bnW', skip_special_chars, stopline, timeout)
        if next[0] && (next[0] > nearest[0] || (next[0] == nearest[0] && next[1] > nearest[1]))
            let nearest = next
        endif
    endfor
    return nearest
endfunction

" Find the start of a multi-line statement
function! s:find_start_of_multiline_statement(lnum)
    let lnum = a:lnum
    while lnum > 0
        if getline(lnum - 1) =~# '\\$'
            let lnum = prevnonblank(lnum - 1)
        else
            let [paren_lnum, _] = s:find_opening_paren(lnum, 1)
            if paren_lnum < 1
                return lnum
            else
                let lnum = paren_lnum
            endif
        endif
    endwhile
endfunction

" Find possible indent(s) of the block starter that matches the current line.
function! s:find_start_of_block(lnum, types, skip, multiple) abort
    let r = []
    let re = '\V\^\s\*\('.join(a:types, '\|').'\)\>'
    if !empty(a:skip)
      let re_skip = '\V\^\s\*\('.join(a:skip, '\|').'\)\>'
    else
      let re_skip = ''
    endif
    let last_indent = indent(a:lnum) + 1
    let lnum = a:lnum - 1
    while lnum > 0 && last_indent > 0
        let indent = indent(lnum)
        if indent < last_indent
            let line = getline(lnum)
            if !empty(re_skip) && line =~# re_skip
                let last_indent = indent
            elseif line =~# re
                if !a:multiple
                    return [indent]
                endif
                if index(r, indent) == -1
                    let r += [indent]
                endif
                let last_indent = indent
            endif
        endif
        let lnum = prevnonblank(lnum - 1)
    endwhile
    return r
endfunction

" Is "expr" true for every position in "lnum", beginning at "start"?
" (optionally up to a:1 / 4th argument)
function! s:match_expr_on_line(expr, lnum, start, ...)
    let text = getline(a:lnum)
    let end = a:0 ? a:1 : len(text)
    if a:start > end
        return 1
    endif
    let save_pos = getpos('.')
    let r = 1
    for i in range(a:start, end)
        call cursor(a:lnum, i)
        if !(eval(a:expr) || text[i-1] =~# '\s')
            let r = 0
            break
        endif
    endfor
    call setpos('.', save_pos)
    return r
endfunction

" Line up with open parenthesis/bracket/brace.
function! s:indent_like_opening_paren(lnum)
    let [paren_lnum, paren_col] = s:find_opening_paren(a:lnum, 1)
    if paren_lnum <= 0
        return -2
    endif
    let text = getline(paren_lnum)
    let base = indent(paren_lnum)

    let nothing_after_opening_paren = s:match_expr_on_line(
                \ s:skip_after_opening_paren, paren_lnum, paren_col+1)
    let starts_with_closing_paren = getline(a:lnum) =~# '^\s*[])}]'

    let hang_closing = get(b:, 'python_pep8_indent_hang_closing',
                \ get(g:, 'python_pep8_indent_hang_closing', 0))

    if nothing_after_opening_paren
        if starts_with_closing_paren && !hang_closing
            let res = base
        else
            let res = base + s:sw()

            " Special case for parenthesis.
            if text[paren_col-1] ==# '(' && getline(a:lnum) !~# '\v\)\s*:?\s*$'
                return res
            endif
        endif
    else
        " Indent to match position of opening paren.
        let res = paren_col
    endif

    " If this line is the continuation of a control statement
    " indent further to distinguish the continuation line
    " from the next logical line.
    if text =~# b:control_statement && res == base + s:sw()
        " But only if not inside parens itself (Flake's E127).
        let [paren_lnum, _] = s:find_opening_paren(paren_lnum, 1)
        if paren_lnum <= 0
            return res + s:sw()
        endif
    endif
    return res
endfunction

" Match indent of first block of this type.
function! s:indent_like_block(lnum)
    let text = getline(a:lnum)
    for [multiple, block_rules] in [
                \ [0, s:block_rules],
                \ [1, s:block_rules_multiple],
                \ ]
        for [line_re, blocks_ignore] in items(block_rules)
            if text !~# line_re
                continue
            endif

            let [blocks, skip] = blocks_ignore
            let indents = s:find_start_of_block(a:lnum, blocks, skip, multiple)
            if empty(indents)
                return -1
            endif
            if len(indents) == 1
                return indents[0]
            endif

            " Multiple valid indents, e.g. for 'else' with both try and if.
            let indent = indent(a:lnum)
            if index(indents, indent) != -1
                " The indent is valid, keep it.
                return indent
            endif
            " Fallback to the first/nearest one.
            return indents[0]
        endfor
    endfor
    return -2
endfunction

function! s:indent_like_previous_line(lnum)
    let lnum = prevnonblank(a:lnum - 1)

    " No previous line, keep current indent.
    if lnum < 1
      return -1
    endif

    let text = getline(lnum)
    let start = s:find_start_of_multiline_statement(lnum)
    let base = indent(start)
    let current = indent(a:lnum)

    " Ignore last character in previous line?
    let lastcol = len(text)
    let col = lastcol

    " Search for final colon that is not inside something to be ignored.
    while 1
        if col == 1 | break | endif
        if text[col-1] =~# '\s' || s:_skip_special_chars(lnum, col)
            let col = col - 1
            continue
        elseif text[col-1] ==# ':'
            return base + s:sw()
        endif
        break
    endwhile

    if text =~# '\\$' && !s:_skip_special_chars(lnum, lastcol)
        " If this line is the continuation of a control statement
        " indent further to distinguish the continuation line
        " from the next logical line.
        if getline(start) =~# b:control_statement
            return base + s:sw() * 2
        endif

        " Nest (other) explicit continuations only one level deeper.
        return base + s:sw()
    endif

    let empty = getline(a:lnum) =~# '^\s*$'

    " Current and prev line are empty, next is not -> indent like next.
    if empty && a:lnum > 1 &&
          \ (getline(a:lnum - 1) =~# '^\s*$') &&
          \ !(getline(a:lnum + 1) =~# '^\s*$')
      return indent(a:lnum + 1)
    endif

    " If the previous statement was a stop-execution statement or a pass
    if getline(start) =~# s:stop_statement
        " Remove one level of indentation if the user hasn't already dedented
        if empty || current > base - s:sw()
            return base - s:sw()
        endif
        " Otherwise, trust the user
        return -1
    endif

    if (current || !empty) && s:is_dedented_already(current, base)
        return -1
    endif

    " In all other cases, line up with the start of the previous statement.
    return base
endfunction

" If this line is dedented and the number of indent spaces is valid
" (multiple of the indentation size), trust the user.
function! s:is_dedented_already(current, base)
    let dedent_size = a:current - a:base
    return (dedent_size < 0 && a:current % s:sw() == 0) ? 1 : 0
endfunction

" Is the syntax at lnum (and optionally cnum) a python string?
function! s:is_python_string(lnum, ...)
    let line = getline(a:lnum)
    if a:0
      let cols = type(a:1) != type([]) ? [a:1] : a:1
    else
      let cols = range(1, max([1, len(line)]))
    endif
    for cnum in cols
        if match(map(synstack(a:lnum, cnum),
                    \ "synIDattr(v:val, 'name')"), 'python\S*String') == -1
            return 0
        end
    endfor
    return 1
endfunction

function! GetPythonPEPIndent(lnum)
    " First line has indent 0
    if a:lnum == 1
        return 0
    endif

    let line = getline(a:lnum)
    let prevline = getline(a:lnum-1)

    " Multilinestrings: continous, docstring or starting.
    if s:is_python_string(a:lnum-1, max([1, len(prevline)]))
                \ && (s:is_python_string(a:lnum, 1)
                \     || match(line, '^\%("""\|''''''\)') != -1)

        " Indent closing quotes as the line with the opening ones.
        let match_quotes = match(line, '^\s*\zs\%("""\|''''''\)')
        if match_quotes != -1
            " closing multiline string
            let quotes = line[match_quotes:(match_quotes+2)]
            call cursor(a:lnum, 1)
            let pairpos = searchpairpos(quotes, '', quotes, 'bW', '', 0, g:python_pep8_indent_searchpair_timeout)
            if pairpos[0] != 0
                return indent(pairpos[0])
            else
                return -1
            endif
        endif

        if s:is_python_string(a:lnum-1)
            " Previous line is (completely) a string: keep current indent.
            return -1
        endif

        if match(prevline, '^\s*\%("""\|''''''\)') != -1
            " docstring.
            return indent(a:lnum-1)
        endif

        let indent_multi = get(b:, 'python_pep8_indent_multiline_string',
                    \ get(g:, 'python_pep8_indent_multiline_string', 0))
        if match(prevline, '\v%("""|'''''')$') != -1
            " Opening multiline string, started in previous line.
            if (&autoindent && indent(a:lnum) == indent(a:lnum-1))
                        \ || match(line, '\v^\s+$') != -1
                " <CR> with empty line or to split up 'foo("""bar' into
                " 'foo("""' and 'bar'.
                if indent_multi == -2
                    return indent(a:lnum-1) + s:sw()
                endif
                return indent_multi
            endif
        endif

        " Keep existing indent.
        if match(line, '\v^\s*\S') != -1
            return -1
        endif

        if indent_multi != -2
            return indent_multi
        endif

        return s:indent_like_opening_paren(a:lnum)
    endif

    " Parens: If we can find an open parenthesis/bracket/brace, line up with it.
    let indent = s:indent_like_opening_paren(a:lnum)
    if indent >= -1
        return indent
    endif

    " Blocks: Match indent of first block of this type.
    let indent = s:indent_like_block(a:lnum)
    if indent >= -1
        return indent
    endif

    return s:indent_like_previous_line(a:lnum)
endfunction
