" vim:tabstop=2:shiftwidth=2:expandtab:foldmethod=marker:textwidth=79
" Vimwiki autoload plugin file
" Desc: Tables
" | Easily | manageable | text  | tables | !       |
" |--------+------------+-------+--------+---------|
" | Have   | fun!       | Drink | tea    | Period. |
"
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/

" Load only once {{{
if exists("g:loaded_vimwiki_tbl_auto") || &cp
  finish
endif
let g:loaded_vimwiki_tbl_auto = 1
"}}}

let s:textwidth = &tw

" Misc functions {{{
function! s:wide_len(str) "{{{
  " vim73 has new function that gives correct string width.
  if exists("*strdisplaywidth")
    return strdisplaywidth(a:str)
  endif

  " get str display width in vim ver < 7.2
  if !g:vimwiki_CJK_length
    let ret = strlen(substitute(a:str, '.', 'x', 'g'))
  else
    let savemodified = &modified
    let save_cursor = getpos('.')
    exe "norm! o\<esc>"
    call setline(line("."), a:str)
    let ret = virtcol("$") - 1
    d
    call setpos('.', save_cursor)
    let &modified = savemodified
  endif
  return ret
endfunction "}}}

function! s:is_table(line) "{{{
  return a:line =~ '^\s*\%(|[^|]\+\)\+|\s*$' || s:is_separator(a:line)
endfunction "}}}

function! s:is_separator(line) "{{{
  return a:line =~ '^\s*[|+]\s*--[-|+]\+'
endfunction "}}}

function! s:is_last_column(lnum, cnum) "{{{
  return strpart(getline(a:lnum), a:cnum - 1) =~ '^[^|]*|\s*$'
endfunction "}}}

function! s:is_first_column(lnum, cnum) "{{{
  let line = strpart(getline(a:lnum), 0, a:cnum - 1)
  return line =~ '^\s*|[^|]*$' || line =~ '^\s*$'
endfunction "}}}

function! s:count_separators_up(lnum) "{{{
  let lnum = a:lnum - 1
  while lnum > 1
    if !s:is_separator(getline(lnum))
      break
    endif
    let lnum -= 1
  endwhile

  return (a:lnum-lnum)
endfunction "}}}

function! s:count_separators_down(lnum) "{{{
  let lnum = a:lnum + 1
  while lnum < line('$')
    if !s:is_separator(getline(lnum))
      break
    endif
    let lnum += 1
  endwhile

  return (lnum-a:lnum)
endfunction "}}}

function! s:create_empty_row(cols) "{{{
  let first_cell = "|   |"
  let cell = "   |"
  let row = first_cell

  for c in range(a:cols - 1)
    let row .= cell
  endfor

  return row
endfunction "}}}

function! s:create_row_sep(cols) "{{{
  let first_cell = "|---+"
  let cell = "---+"
  let last_cell = "---|"

  if a:cols < 2
    return "|---|"
  endif

  let row = first_cell

  for c in range(a:cols - 2)
    let row .= cell
  endfor

  let row .= last_cell

  return row
endfunction "}}}

function! s:get_values(line) "{{{
  return split(a:line, '\s*|\s*', 1)[1:-2]
endfunction "}}}

function! s:col_count(lnum) "{{{
  let line = getline(a:lnum)
  if !s:is_separator(line)
    return len(split(line, '\s*|\s*', 1)[1:-2])
  else
    return len(split(line, '-+-', 1))
  endif
endfunction "}}}

function! s:get_indent(lnum) "{{{
  if !s:is_table(getline(a:lnum))
    return
  endif

  let indent = 0

  let lnum = a:lnum - 1
  while lnum > 1
    let line = getline(lnum)
    if !s:is_table(line)
      let indent = indent(lnum+1)
      break
    endif
    let lnum -= 1
  endwhile

  return indent
endfunction " }}}

function! s:get_rows(lnum) "{{{
  if !s:is_table(getline(a:lnum))
    return
  endif

  let upper_rows = []
  let lower_rows = []

  let lnum = a:lnum - 1
  while lnum > 1
    let line = getline(lnum)
    if s:is_table(line)
      call add(upper_rows, [lnum, line])
    else
      break
    endif
    let lnum -= 1
  endwhile
  call reverse(upper_rows)

  let lnum = a:lnum
  while lnum <= line('$')
    let line = getline(lnum)
    if s:is_table(line)
      call add(lower_rows, [lnum, line])
    else
      break
    endif
    let lnum += 1
  endwhile

  return upper_rows + lower_rows
endfunction "}}}

function! s:get_cell_max_lens(lnum) "{{{
  let max_lens = {}
  for [lnum, row] in s:get_rows(a:lnum)
    if s:is_separator(row)
      continue
    endif
    let cells = s:get_values(row)
    for idx in range(len(cells))
      let value = cells[idx]
      if has_key(max_lens, idx)
        let max_lens[idx] = max([s:wide_len(value), max_lens[idx]])
      else
        let max_lens[idx] = s:wide_len(value)
      endif
    endfor
  endfor
  return max_lens
endfunction "}}}

function! s:get_aligned_rows(lnum, col1, col2) "{{{
  let max_lens = s:get_cell_max_lens(a:lnum)
  let rows = []
  for [lnum, row] in s:get_rows(a:lnum)
    if s:is_separator(row)
      let new_row = s:fmt_sep(max_lens, a:col1, a:col2)
    else
      let new_row = s:fmt_row(row, max_lens, a:col1, a:col2)
    endif
    call add(rows, [lnum, new_row])
  endfor
  return rows
endfunction "}}}

" Number of the current column. Starts from 0.
function! s:cur_column() "{{{
  let line = getline('.')
  if !s:is_table(line)
    return -1
  endif
  if s:is_separator(line)
    let sep = '[+|]'
  else
    let sep = '|'
  endif

  let curs_pos = col('.')
  let mpos = match(line, '|', 0)
  let col = -1
  while mpos < curs_pos && mpos != -1
    let mpos = match(line, sep, mpos+1)
    if mpos != -1
      let col += 1
    endif
  endwhile
  return col
endfunction "}}}

" }}}

" Format functions {{{
function! s:fmt_cell(cell, max_len) "{{{
  let cell = ' '.a:cell.' '

  let diff = a:max_len - s:wide_len(a:cell)
  if diff == 0 && empty(a:cell)
    let diff = 1
  endif

  let cell .= repeat(' ', diff)
  return cell
endfunction "}}}

function! s:fmt_row(line, max_lens, col1, col2) "{{{
  let new_line = '|'
  let cells = s:get_values(a:line)
  for idx in range(len(cells))
    if idx == a:col1
      let idx = a:col2
    elseif idx == a:col2
      let idx = a:col1
    endif
    let value = cells[idx]
    let new_line .= s:fmt_cell(value, a:max_lens[idx]).'|'
  endfor

  let idx = len(cells)
  while idx < len(a:max_lens)
    let new_line .= s:fmt_cell('', a:max_lens[idx]).'|'
    let idx += 1
  endwhile
  return new_line
endfunction "}}}

function! s:fmt_cell_sep(max_len) "{{{
  if a:max_len == 0
    return repeat('-', 3)
  else
    return repeat('-', a:max_len+2)
  endif
endfunction "}}}

function! s:fmt_sep(max_lens, col1, col2) "{{{
  let sep = '|'
  for idx in range(len(a:max_lens))
    if idx == a:col1
      let idx = a:col2
    elseif idx == a:col2
      let idx = a:col1
    endif
    let sep .= s:fmt_cell_sep(a:max_lens[idx]).'+'
  endfor
  let sep = substitute(sep, '+$', '|', '')
  return sep
endfunction "}}}
"}}}

" Keyboard functions "{{{
function! s:kbd_create_new_row(cols, goto_first) "{{{
  let cmd = "\<ESC>o".s:create_empty_row(a:cols)
  let cmd .= "\<ESC>:call org#tbl#format(line('.'))\<CR>"
  if a:goto_first
    let cmd .= "\<ESC>0:call search('|', 'c', line('.'))\<CR>la"
  else
    let cmd .= "0".(col('.')-1)."lT|a"
  endif
  return cmd
endfunction "}}}

function! s:kbd_goto_next_row() "{{{
  let cmd = "\<ESC>jt|T|a"
  return cmd
endfunction "}}}

function! s:kbd_goto_prev_row() "{{{
  let cmd = "\<ESC>jt|T|a"
  return cmd
endfunction "}}}

function! org#tbl#next_col(last)
  return s:kbd_goto_next_col(a:last)
endfunction
function! s:kbd_goto_next_col(last) "{{{
  if a:last
    let seps = s:count_separators_down(line('.'))
    if mode() == 'n'
      let cmd = seps . "j0:call search('|', 'c', line('.'))\<CR>l"
    else
      let cmd = "\<ESC>".seps."j0:call search('|', 'c', line('.'))\<CR>la"
    endif
  else
    if mode() == 'n'
      let cmd = ":call search('|', 'c', line('.'))\<CR>l"
    else
      let cmd = "\<ESC>:call search('|', 'c', line('.'))\<CR>la"
    endif
  endif
  return cmd
endfunction "}}}

function! org#tbl#prev_col(first)
  return s:kbd_goto_prev_col(a:first)
endfunction
function! s:kbd_goto_prev_col(first) "{{{
  if a:first
    let seps = s:count_separators_up(line('.'))
    let cmd = "\<ESC>".seps."k$:call search('|', 'b', line('.'))\<CR>la"
  else
    let cmd = "\<ESC>2F|la"
  endif
  return cmd
endfunction "}}}

"}}}

" Global functions {{{
function! org#tbl#kbd_cr() "{{{
  let lnum = line('.')
  if !s:is_table(getline(lnum))
    return "\<CR>"
  endif

  if s:is_separator(getline(lnum+1)) || !s:is_table(getline(lnum+1))
    let cols = len(s:get_values(getline(lnum)))
    return s:kbd_create_new_row(cols, 0)
  else
    return s:kbd_goto_next_row()
  endif
endfunction "}}}

function! org#tbl#kbd_tab() "{{{
  let lnum = line('.')
  if !s:is_table(getline(lnum))
    return "\<Tab>"
  endif

  let last = s:is_last_column(lnum, col('.'))
  if last && !s:is_table(getline(lnum+1))
    let cols = len(s:get_values(getline(lnum)))
    return s:kbd_create_new_row(cols, 1)
  endif
  return s:kbd_goto_next_col(last)
endfunction "}}}

function! org#tbl#kbd_shift_tab() "{{{
  let lnum = line('.')
  if !s:is_table(getline(lnum))
    return "\<S-Tab>"
  endif

  let first = s:is_first_column(lnum, col('.'))
  if first && !s:is_table(getline(lnum-1))
    return ""
  endif
  return s:kbd_goto_prev_col(first)
endfunction "}}}

function! org#tbl#format(lnum, ...) "{{{
  let line = getline(a:lnum)
  if !s:is_table(line)
    return
  endif

  if a:0 == 2
    let col1 = a:1
    let col2 = a:2
  else
    let col1 = 0
    let col2 = 0
  endif

  let indent = s:get_indent(a:lnum)

  for [lnum, row] in s:get_aligned_rows(a:lnum, col1, col2)
    let row = repeat(' ', indent).row
    call setline(lnum, row)
  endfor
  
  let &tw = s:textwidth
endfunction "}}}

function! org#tbl#create(...) "{{{
  if a:0 > 1
    let cols = a:1
    let rows = a:2
  elseif a:0 == 1
    let cols = a:1
    let rows = 2
  elseif a:0 == 0
    let cols = 5
    let rows = 2
  endif

  if cols < 1
    let cols = 5
  endif

  if rows < 1
    let rows = 2
  endif

  let lines = []
  let row = s:create_empty_row(cols)

  call add(lines, row)
  if rows > 1
    call add(lines, s:create_row_sep(cols))
  endif

  for r in range(rows - 1)
    call add(lines, row)
  endfor
  
  call append(line('.'), lines)
endfunction "}}}

function! org#tbl#align_or_cmd(cmd) "{{{
  if s:is_table(getline('.'))
    call org#tbl#format(line('.'))
  else
    exe 'normal! '.a:cmd
  endif
endfunction "}}}

function! org#tbl#reset_tw(lnum) "{{{
  let line = getline(a:lnum)
  if !s:is_table(line)
    return
  endif
  
  let s:textwidth = &tw
  let &tw = 0
endfunction "}}}

" TODO: move_column_left and move_column_right are good candidates to be
" refactored.
function! org#tbl#move_column_left() "{{{
  if !s:is_table(getline('.'))
    return
  endif

  let cur_col = s:cur_column()
  if cur_col == -1
    return
  endif

  if cur_col > 0
    call org#tbl#format(line('.'), cur_col-1, cur_col)
    call cursor(line('.'), 1)
    if !s:is_separator(getline('.'))
      call search('\%(|[^|]\+\)\{'.(cur_col-1).'}| .', 'eW')
    else
      call search('|\%([^+]\++\)\{'.(cur_col-1).'}--', 'eW')
    endif
  endif
endfunction "}}}

function! org#tbl#move_column_right() "{{{
  if !s:is_table(getline('.'))
    return
  endif

  let cur_col = s:cur_column()
  if cur_col == -1
    return
  endif

  if cur_col < s:col_count(line('.'))-1
    call org#tbl#format(line('.'), cur_col, cur_col+1)
    call cursor(line('.'), 1)
    if !s:is_separator(getline('.'))
      call search('\%(|[^|]\+\)\{'.(cur_col+1).'}| .', 'eW')
    else
      call search('|\%([^+]\++\)\{'.(cur_col+1).'}--', 'eW')
    endif
  endif
endfunction "}}}

function! org#tbl#get_rows(lnum) "{{{
  return s:get_rows(a:lnum)
endfunction "}}}

"}}}

