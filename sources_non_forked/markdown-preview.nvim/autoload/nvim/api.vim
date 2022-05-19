if has('nvim') | finish | endif
let s:funcs = {}
let s:prop_id = 1000
let s:namespace_id = 1
let s:namespace_cache = {}

" helper {{
function! s:buf_line_count(bufnr) abort
  if bufnr('%') == a:bufnr
    return line('$')
  endif
  let lines = getbufline(a:bufnr, 1, '$')
  return len(lines)
endfunction

function! s:execute(cmd)
  if a:cmd =~# '^echo'
    execute a:cmd
  else
    silent! execute a:cmd
  endif
endfunction
" }}"

" nvim client methods {{
function! s:funcs.set_current_dir(dir) abort
  execute 'cd '.a:dir
endfunction

function! s:funcs.set_var(name, value) abort
  execute 'let g:'.a:name.'= a:value'
endfunction

function! s:funcs.del_var(name) abort
  execute 'unlet g:'.a:name
endfunction

function! s:funcs.set_option(name, value) abort
  execute 'let &'.a:name.' = a:value'
endfunction

function! s:funcs.set_current_buf(bufnr) abort
  if !bufexists(a:bufnr) | return | endif
  execute 'buffer '.a:bufnr
endfunction

function! s:funcs.set_current_win(win_id) abort
  let [tabnr, winnr] = win_id2tabwin(a:win_id)
  if tabnr == 0 | return | endif
  execute 'normal! '.tabnr.'gt'
  execute winnr.' wincmd w'
endfunction

function! s:funcs.set_current_tabpage(tabnr) abort
  execute 'normal! '.a:tabnr.'gt'
endfunction

function! s:funcs.list_wins() abort
  return map(getwininfo(), 'v:val["winid"]')
endfunction

function! s:funcs.call_atomic(calls)
  let res = []
  for [key, arglist] in a:calls
    let name = key[5:]
    try
      call add(res, call(s:funcs[name], arglist))
    catch /.*/
      return [res, v:exception]
    endtry
  endfor
  return [res, v:null]
endfunction

function! s:funcs.set_client_info(...) abort
endfunction

function! s:funcs.subscribe(...) abort
endfunction

function! s:funcs.unsubscribe(...) abort
endfunction

function! s:funcs.call_function(method, args) abort
  return call(a:method, a:args)
endfunction

function! s:funcs.call_dict_function(dict, method, args) abort
  return call(a:method, a:args, a:dict)
endfunction

function! s:funcs.command(command) abort
  " command that could cause cursor vanish
  if a:command =~# '^echo' || a:command =~# '^redraw' || a:command =~# '^sign place'
    call timer_start(0, {-> s:execute(a:command)})
  else
    execute a:command
  endif
endfunction

function! s:funcs.eval(expr) abort
  return eval(a:expr)
endfunction

function! s:funcs.get_api_info()
  let names = nvim#api#func_names()
  return [1, {'functions': map(names, '{"name": "nvim_".v:val}')}]
endfunction

function! s:funcs.list_bufs()
  return map(getbufinfo({'buflisted': 1}), 'v:val["bufnr"]')
endfunction

function! s:funcs.feedkeys(keys, mode, escape_csi)
  call feedkeys(a:keys, a:mode)
endfunction

function! s:funcs.list_runtime_paths()
  return split(&runtimepath, ',')
endfunction

function! s:funcs.command_output(cmd)
  return execute(a:cmd)
endfunction

function! s:funcs.get_current_line()
  return getline('.')
endfunction

function! s:funcs.set_current_line(line)
  call setline('.', a:line)
endfunction

function! s:funcs.del_current_line(line)
  execute 'normal! dd'
endfunction

function! s:funcs.get_var(var)
  return get(g:, a:var, v:null)
endfunction

function! s:funcs.get_vvar(var)
  return get(v:, a:var, v:null)
endfunction

function! s:funcs.get_option(name)
  return eval('&'.a:name)
endfunction

function! s:funcs.get_current_buf()
  return bufnr('%')
endfunction

function! s:funcs.get_current_win()
  return win_getid()
endfunction

function! s:funcs.get_current_tabpage()
  return tabpagenr()
endfunction

function! s:funcs.list_tabpages()
  return range(1, tabpagenr('$'))
endfunction

function! s:funcs.get_mode()
  return {'blocking': v:false, 'mode': mode()}
endfunction

function! s:funcs.strwidth(str)
  return strwidth(a:str)
endfunction

function! s:funcs.out_write(str)
  echon a:str
endfunction

function! s:funcs.err_write(str)
  echoerr a:str
endfunction

function! s:funcs.err_writeln(str)
  echoerr a:str
endfunction

function! s:funcs.create_namespace(name) abort
  if empty(a:name)
    let id = s:namespace_id
    let s:namespace_id = s:namespace_id + 1
    return id
  endif
  let id = get(s:namespace_cache, a:name, 0)
  if !id
    let id = s:namespace_id
    let s:namespace_id = s:namespace_id + 1
    let s:namespace_cache[a:name] = id
  endif
  return id
endfunction
" }}

" buffer methods {{
function! s:funcs.buf_set_option(bufnr, name, val)
  return setbufvar(a:bufnr, '&'.a:name, a:val)
endfunction

function! s:funcs.buf_get_changedtick(bufnr)
  return getbufvar(a:bufnr, 'changedtick')
endfunction

function! s:funcs.buf_is_valid(bufnr)
  return bufloaded(a:bufnr) ? v:true : v:false
endfunction

function! s:funcs.buf_get_mark(bufnr, name)
  let nr = bufnr('%')
  if a:bufnr != 0 || a:bufnr != nr
    throw 'buf_get_mark support current buffer only'
  endif
  return [line("'" . a:name), col("'" . a:name)]
endfunction

function! s:funcs.buf_add_highlight(bufnr, srcId, hlGroup, line, colStart, colEnd) abort
  if !has('textprop')
    return
  endif
  let key = 'Coc'.a:hlGroup
  if empty(prop_type_get(key))
    call prop_type_add(key, {'highlight': a:hlGroup, 'combine': 1})
  endif
  let end = a:colEnd
  if end == -1
    let end = strlen(getbufline(a:bufnr, a:line + 1)[0]) + 1
  endif
  let id = 0
  if a:srcId != 0
    let cached = getbufvar(a:bufnr, 'prop_namespace_'.a:srcId, [])
    let id = s:prop_id
    let s:prop_id = id + 1
    call add(cached, id)
    call setbufvar(a:bufnr, 'prop_namespace_'.a:srcId, cached)
  endif
  call prop_add(a:line + 1, a:colStart + 1, {'length': end - a:colStart, 'bufnr': a:bufnr, 'type': key, 'id': id})
endfunction

function! s:funcs.buf_clear_namespace(bufnr, srcId, startLine, endLine) abort
  if !has('textprop')
    return
  endif
  if a:srcId == 0
    if a:endLine == -1
      call prop_clear(a:startLine + 1, {'bufnr': a:bufnr})
    else
      call prop_clear(a:startLine + 1, a:endLine + 1, {'bufnr': a:bufnr})
    endif
  else
    let cached = getbufvar(a:bufnr, 'prop_namespace_'.a:srcId, [])
    if empty(cached)
      return
    endif
    for id in cached
      if a:endLine == -1
        if a:startLine == 0 && a:endLine == -1
          call prop_remove({'id':id, 'bufnr': a:bufnr})
        elseif a:endLine != -1
          call prop_remove({'id':id, 'bufnr': a:bufnr}, a:startLine, a:endLine)
        else
          let len = s:buf_line_count(a:bufnr)
          call prop_remove({'id':id, 'bufnr': a:bufnr}, a:startLine, len)
        endif
      else
      endif
    endfor
  endif
endfunction

function! s:funcs.buf_line_count(bufnr) abort
  return s:buf_line_count(a:bufnr)
endfunction

function! s:funcs.buf_attach(...)
  " not supported
  return 1
endfunction

function! s:funcs.buf_detach()
  " not supported
  return 1
endfunction

function! s:funcs.buf_get_lines(bufnr, start, end, strict) abort
  let lines = getbufline(a:bufnr, 1, '$')
  let start = a:start < 0 ? a:start + 1 : a:start
  let end = a:end < 0 ? a:end + 1 : a:end
  if a:strict && end > len(lines)
    throw 'line number out of range: '. end
  endif
  return lines[start : end - 1]
endfunction

function! s:funcs.buf_set_lines(bufnr, start, end, strict, ...) abort
  let replacement = get(a:, 1, [])
  let lineCount = s:buf_line_count(a:bufnr)
  let startLnum = a:start >= 0 ? a:start + 1 : lineCount + a:start + 1
  let end = a:end >= 0 ? a:end : lineCount + a:end + 1
  let delCount = end - (startLnum - 1)
  if a:bufnr == bufnr('%')
    " replace
    if delCount == len(replacement)
      call setline(startLnum, replacement)
    else
      if len(replacement)
        call append(startLnum - 1, replacement)
      endif
      if delCount
        let start = startLnum + len(replacement)
        silent execute start . ','.(start + delCount - 1).'d'
      endif
    endif
  else
    if exists('*setbufline')
      " replace
      if delCount == len(replacement)
        call setbufline(a:bufnr, startLnum, replacement)
      else
        if len(replacement)
          call appendbufline(a:bufnr, startLnum - 1, replacement)
        endif
        if delCount
          let start = startLnum + len(replacement)
          call deletebufline(a:bufnr, start, start + delCount - 1)
        endif
      endif
    endif
  endif
endfunction

function! s:funcs.buf_set_name(bufnr, name) abort
  let nr = bufnr('%')
  if a:bufnr != nr
    throw 'buf_set_name support current buffer only'
  else
    execute '0f'
    execute 'file '.fnameescape(a:name)
  endif
endfunction

function! s:funcs.buf_get_var(bufnr, name)
  return getbufvar(a:bufnr, a:name)
endfunction

function! s:funcs.buf_set_var(bufnr, name, val)
  if !bufloaded(a:bufnr) | return | endif
  call setbufvar(a:bufnr, a:name, a:val)
endfunction

function! s:funcs.buf_del_var(bufnr, name)
  call setbufvar(a:bufnr, a:name, v:null)
endfunction

function! s:funcs.buf_get_option(bufnr, name)
  return getbufvar(a:bufnr, '&'.a:name)
endfunction

function! s:funcs.buf_get_name(bufnr)
  return bufname(a:bufnr)
endfunction
" }}

" window methods {{
function! s:funcs.win_get_buf(winid)
  return winbufnr(a:winid)
endfunction

function! s:funcs.win_get_position(win_id) abort
  let [row, col] = win_screenpos(a:win_id)
  if row == 0 && col == 0
    throw 'Invalid window '.a:win_id
  endif
  return [row - 1, col - 1]
endfunction

function! s:funcs.win_get_height(win_id) abort
  return winheight(a:win_id)
endfunction

function! s:funcs.win_get_width(win_id) abort
  return winwidth(a:win_id)
endfunction

function! s:funcs.win_get_cursor(win_id) abort
  let winid = win_getid()
  call win_gotoid(a:win_id)
  let pos = [line('.'), col('.')]
  call win_gotoid(winid)
  return pos
endfunction

function! s:funcs.win_get_var(win_id, name) abort
  return gettabwinvar(0, a:win_id, a:name)
endfunction

function! s:funcs.win_set_width(win_id, width) abort
  let winid = win_getid()
  call win_gotoid(a:win_id)
  execute 'vertical resize '.a:width
  call win_gotoid(winid)
endfunction

function! s:funcs.win_get_option(win_id, name) abort
  return gettabwinvar(0, a:win_id, '&'.a:name)
endfunction

function! s:funcs.win_set_height(win_id, height) abort
  let winnr = win_id2win(a:win_id)
  if winnr != 0
    let curr = winnr()
    if winnr == curr
      execute 'resize '.a:height
    else
      execute winnr.'wincmd w'
      execute 'resize '.a:height
      wincmd p
    endif
  endif
endfunction

function! s:funcs.win_set_option(win_id, name, value) abort
  call setwinvar(a:win_id, '&'.a:name, a:value)
endfunction

function! s:funcs.win_set_var(win_id, name, value) abort
  call setwinvar(a:win_id, a:name, a:value)
endfunction

function! s:funcs.win_del_var(win_id, name) abort
  call settabwinvar(0, a:win_id, a:name, v:null)
endfunction

function! s:funcs.win_is_valid(win_id) abort
  let info = getwininfo(a:win_id)
  return !empty(info)
endfunction

function! s:funcs.win_get_number(win_id) abort
  let info = getwininfo(a:win_id)
  if !info
    throw 'Invalid window id '.a:win_id
  endif
  return info[0]['winnr']
endfunction

function! s:funcs.win_set_cursor(win_id, pos) abort
  let winnr = win_id2win(a:win_id)
  if winnr != 0
    let [line, col] = a:pos
    let curr = winnr()
    if winnr == curr
      call cursor(line, col + 1)
    else
      execute winnr.'wincmd w'
      call cursor(line, col + 1)
      execute curr.'wincmd w'
    endif
  endif
endfunction

function! s:funcs.win_get_tabpage(win_id) abort
  let info = getwininfo(a:win_id)
  if !info
    throw 'Invalid window id '.a:win_id
  endif
  return info[0]['tabnr']
endfunction
" }}

" tabpage methods {{
function! s:funcs.tabpage_get_number(id)
  return a:id
endfunction

function! s:funcs.tabpage_list_wins(tabnr)
  let info = getwininfo()
  return map(filter(info, 'v:val["tabnr"] == a:tabnr'), 'v:val["winid"]')
endfunction

function! s:funcs.tabpage_get_var(tabnr, name)
  return gettabvar(a:tabnr, a:name, v:null)
endfunction

function! s:funcs.tabpage_set_var(tabnr, name, value)
  call settabvar(a:tabnr, a:name, a:value)
endfunction

function! s:funcs.tabpage_del_var(tabnr, name)
  call settabvar(a:tabnr, a:name, v:null)
endfunction

function! s:funcs.tabpage_is_valid(tabnr)
  let max = tabpagenr('$')
  return a:tabnr <= max
endfunction

function! s:funcs.tabpage_get_win(tabnr)
  let wnr = tabpagewinnr(a:tabnr)
  return win_getid(wnr, a:tabnr)
endfunction
" }}

function! nvim#api#func_names() abort
  return keys(s:funcs)
endfunction

function! nvim#api#call(method, args) abort
  let err = v:null
  let res = v:null
  try
    let res = call(s:funcs[a:method], a:args)
  catch /.*/
    let err = v:exception
  endtry
  return [err, res]
endfunction

function! nvim#api#notify(method, args) abort
  call call(s:funcs[a:method], a:args)
endfunction
" vim: set sw=2 ts=2 sts=2 et tw=78 foldmarker={{,}} foldmethod=marker foldlevel=0:
