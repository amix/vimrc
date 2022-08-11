scriptencoding utf-8
let s:is_vim = !has('nvim')

" first window id for bufnr
" builtin bufwinid returns window of current tab only
function! coc#compat#buf_win_id(bufnr) abort
  let info = filter(getwininfo(), 'v:val["bufnr"] =='.a:bufnr)
  if empty(info)
    return -1
  endif
  return info[0]['winid']
endfunction

function! coc#compat#buf_set_lines(bufnr, start, end, replacement) abort
  if s:is_vim
    call coc#api#exec('buf_set_lines', [a:bufnr, a:start, a:end, 0, a:replacement])
  else
    call nvim_buf_set_lines(a:bufnr, a:start, a:end, 0, a:replacement)
  endif
endfunction

function! coc#compat#buf_line_count(bufnr) abort
  if exists('*nvim_buf_line_count')
    return nvim_buf_line_count(a:bufnr)
  endif
  if bufnr('%') == a:bufnr
    return line('$')
  endif
  if exists('*getbufinfo')
    let info = getbufinfo(a:bufnr)
    if empty(info)
      return 0
    endif
    " vim 8.1 has getbufinfo but no linecount
    if has_key(info[0], 'linecount')
      return info[0]['linecount']
    endif
  endif
  if exists('*getbufline')
    let lines = getbufline(a:bufnr, 1, '$')
    return len(lines)
  endif
  let curr = bufnr('%')
  execute 'noa buffer '.a:bufnr
  let n = line('$')
  execute 'noa buffer '.curr
  return n
endfunction

function! coc#compat#prepend_lines(bufnr, replacement) abort
  if exists('*appendbufline')
    call appendbufline(a:bufnr, 0, a:replacement)
  elseif !s:is_vim
    call nvim_buf_set_lines(a:bufnr, 0, 0, 0, a:replacement)
  else
    throw 'appendbufline() required for prepend lines.'
  endif
endfunction

function! coc#compat#win_is_valid(winid) abort
  if exists('*nvim_win_is_valid')
    return nvim_win_is_valid(a:winid)
  endif
  return !empty(getwininfo(a:winid))
endfunction

" clear matches by window id, not throw on none exists window.
" may not work on vim < 8.1.1084 & neovim < 0.4.0
function! coc#compat#clear_matches(winid) abort
  if !coc#compat#win_is_valid(a:winid)
    return
  endif
  let curr = win_getid()
  if curr == a:winid
    call clearmatches()
    return
  endif
  if s:is_vim
    if has('patch-8.1.1084')
      call clearmatches(a:winid)
    endif
  else
    if exists('*nvim_set_current_win')
      noa call nvim_set_current_win(a:winid)
      call clearmatches()
      noa call nvim_set_current_win(curr)
    endif
  endif
endfunction

function! coc#compat#matchaddpos(group, pos, priority, winid) abort
  let curr = win_getid()
  if curr == a:winid
    call matchaddpos(a:group, a:pos, a:priority, -1)
  else
    if s:is_vim
      if has('patch-8.1.0218')
        call matchaddpos(a:group, a:pos, a:priority, -1, {'window': a:winid})
      endif
    else
      call matchaddpos(a:group, a:pos, a:priority, -1, {'window': a:winid})
    endif
  endif
endfunction

function! coc#compat#buf_del_var(bufnr, name) abort
  if !bufloaded(a:bufnr)
    return
  endif
  if exists('*nvim_buf_del_var')
    silent! call nvim_buf_del_var(a:bufnr, a:name)
  else
    if a:bufnr == bufnr('%')
      execute 'unlet! b:'.a:name
    elseif exists('*win_execute')
      let winid = coc#compat#buf_win_id(a:bufnr)
      if winid != -1
        call win_execute(winid, 'unlet! b:'.a:name)
      endif
    endif
  endif
endfunction

" hlGroup, pos, priority
function! coc#compat#matchaddgroups(winid, groups) abort
  for group in a:groups
    call matchaddpos(group['hlGroup'], [group['pos']], group['priority'], -1, {'window': a:winid})
  endfor
endfunction

function! coc#compat#del_var(name) abort
  if exists('*nvim_del_var')
    silent! call nvim_del_var(a:name)
  else
    execute 'unlet! '.a:name
  endif
endfunction

" remove keymap for specific buffer
function! coc#compat#buf_del_keymap(bufnr, mode, lhs) abort
  if !bufloaded(a:bufnr)
    return
  endif
  if exists('*nvim_buf_del_keymap')
    try
      call nvim_buf_del_keymap(a:bufnr, a:mode, a:lhs)
    catch /^Vim\%((\a\+)\)\=:E5555/
      " ignore keymap doesn't exist
    endtry
    return
  endif
  if bufnr == a:bufnr
    execute 'silent! '.a:mode.'unmap <buffer> '.a:lhs
    return
  endif
  if exists('*win_execute')
    let winid = coc#compat#buf_win_id(a:bufnr)
    if winid != -1
      call win_execute(winid, a:mode.'unmap <buffer> '.a:lhs, 'silent!')
    endif
  endif
endfunction

function! coc#compat#buf_add_keymap(bufnr, mode, lhs, rhs, opts) abort
  if !bufloaded(a:bufnr)
    return
  endif
  if exists('*nvim_buf_set_keymap')
    call nvim_buf_set_keymap(a:bufnr, a:mode, a:lhs, a:rhs, a:opts)
  else
    let cmd = a:mode . 'noremap '
    for key in keys(a:opts)
      if get(a:opts, key, 0)
        let cmd .= '<'.key.'>'
      endif
    endfor
    let cmd .= '<buffer> '.a:lhs.' '.a:rhs
    if bufnr('%') == a:bufnr
      execute cmd
    elseif exists('*win_execute')
      let winid = coc#compat#buf_win_id(a:bufnr)
      if winid != -1
        call win_execute(winid, cmd)
      endif
    endif
  endif
endfunction

" execute command or list of commands in window
function! coc#compat#execute(winid, command, ...) abort
  if exists('*win_execute')
    if type(a:command) == v:t_string
      keepalt call win_execute(a:winid, a:command, get(a:, 1, ''))
    elseif type(a:command) == v:t_list
      keepalt call win_execute(a:winid, join(a:command, "\n"), get(a:, 1, ''))
    endif
  elseif has('nvim')
    if !nvim_win_is_valid(a:winid)
      return
    endif
    let curr = nvim_get_current_win()
    noa keepalt call nvim_set_current_win(a:winid)
    if type(a:command) == v:t_string
      exe get(a:, 1, '').' '.a:command
    elseif type(a:command) == v:t_list
      for cmd in a:command
        exe get(a:, 1, '').' '.cmd
      endfor
    endif
    noa keepalt call nvim_set_current_win(curr)
  else
    throw 'win_execute does not exist, please upgrade vim.'
  endif
endfunc

function! coc#compat#trim(str)
  if exists('*trim')
    return trim(a:str)
  endif
  " TODO trim from beginning
  return substitute(a:str, '\s\+$', '', '')
endfunction
