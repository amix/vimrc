scriptencoding utf-8
let s:is_vim = !has('nvim')
let s:utf = &encoding =~# '^utf'
let s:error_icon = get(g:, 'coc_notify_error_icon', s:utf ? '' : 'E')
let s:warning_icon = get(g:, 'coc_notify_warning_icon', s:utf ? '⚠' : 'W')
let s:info_icon = get(g:, 'coc_notify_info_icon', s:utf ? '' : 'I')
let s:interval = get(g:, 'coc_notify_interval', s:is_vim ? 50 : 20)
let s:phl = 'CocNotificationProgress'
let s:progress_char = '─'
let s:duration = 300.0
let s:winids = []

" Valid notify winids on current tab
function! coc#notify#win_list() abort
  call filter(s:winids, 'coc#float#valid(v:val)')
  return filter(copy(s:winids), '!empty(getwinvar(v:val,"float"))')
endfunction

function! coc#notify#close_all() abort
  for winid in coc#notify#win_list()
    call coc#notify#close(winid)
  endfor
endfunction

" Do action for winid or first notify window with actions.
function! coc#notify#do_action(...) abort
  let winids = a:0 > 0 ? a:000 : coc#notify#win_list()
  for winid in winids
    if coc#float#valid(winid) && getwinvar(winid, 'closing', 0) != 1
      let actions = getwinvar(winid, 'actions', [])
      if !empty(actions)
        let items = map(copy(actions), '(v:key + 1).". ".v:val')
        let msg = join(getbufline(winbufnr(winid), 1, '$'), ' ')
        call coc#ui#quickpick(msg, items, {err, res -> s:on_action(err, res, winid) })
        break
      endif
    endif
  endfor
endfunction

" Copy notification contents
function! coc#notify#copy() abort
  let lines = []
  for winid in coc#notify#win_list()
    let key = getwinvar(winid, 'key', v:null)
    if type(key) == v:t_string
      call extend(lines, json_decode(key)['lines'])
    endif
  endfor
  if empty(lines)
    echohl WarningMsg | echon 'No content to copy' | echohl None
    return
  endif
  call setreg('*', join(lines, "\n"))
endfunction

" Show source name in window
function! coc#notify#show_sources() abort
  if !exists('*getbufline') || !exists('*appendbufline')
    throw "getbufline and appendbufline functions required, please upgrade your vim."
  endif
  let winids = filter(coc#notify#win_list(), 'coc#window#get_var(v:val,"closing") != 1')
  for winid in winids
    let key = getwinvar(winid, 'key', v:null)
    if type(key) == v:t_string
      let bufnr = winbufnr(winid)
      let obj = json_decode(key)
      let sourcename = get(obj, 'source', '')
      let lnum = get(obj, 'kind', '') ==# 'progress' ? 1 : 0
      let content = get(getbufline(bufnr, lnum + 1), 0, '')
      if empty(sourcename) || content ==# sourcename
        continue
      endif
      call appendbufline(bufnr, lnum, sourcename)
      call coc#highlight#add_highlight(bufnr, -1, 'Title', lnum, 0, -1)
      call coc#float#scroll_win(winid, 0, 1)
    endif
  endfor
  redra
endfunction

function! coc#notify#close_by_source(source) abort
  let winids = filter(coc#notify#win_list(), 'coc#window#get_var(v:val,"closing") != 1')
  for winid in winids
    let key = getwinvar(winid, 'key', v:null)
    if type(key) == v:t_string
      let obj = json_decode(key)
      if get(obj, 'source', '') ==# a:source
        call coc#notify#close(winid)
      endif
    endif
  endfor
endfunction

" Cancel auto hide
function! coc#notify#keep() abort
  for winid in coc#notify#win_list()
    call s:cancel(winid, 'close_timer')
  endfor
endfunction

" borderhighlight - border highlight [string]
" maxWidth - max content width, default 60 [number]
" minWidth - minimal width [number]
" maxHeight - max content height, default 10 [number]
" highlight - default highlight [string]
" winblend - winblend [number]
" timeout - auto close timeout, default 5000 [number]
" title - title text
" marginRight - margin right, default 10 [number]
" focusable - focusable [number]
" source -  source name [string]
" kind - kind for create icon [string]
" actions - action names [string[]]
function! coc#notify#create(lines, config) abort
  let actions = get(a:config, 'actions', [])
  let key = json_encode(extend({'lines': a:lines}, a:config))
  let winid = s:find_win(key)
  let kind = get(a:config, 'kind', '')
  let row = 0
  if winid != -1
    let row = getwinvar(winid, 'top', 0)
    call filter(s:winids, 'v:val != '.winid)
    call coc#float#close(winid)
    let winid = v:null
  endif
  let opts = coc#dict#pick(a:config, ['highlight', 'borderhighlight', 'focusable', 'shadow'])
  let border = has_key(opts, 'borderhighlight') ? [1, 1, 1, 1] : []
  let icon = s:get_icon(kind, get(a:config, 'highlight', 'CocFloating'))
  let margin = get(a:config, 'marginRight', 10)
  let maxWidth = min([&columns - margin - 2,  get(a:config, 'maxWidth', 80)])
  if maxWidth <= 0
    throw 'No enough spaces for notification'
  endif
  let lines = map(copy(a:lines), 'tr(v:val, "\t", " ")')
  if has_key(a:config, 'title')
    if !empty(border)
      let opts['title'] = a:config['title']
    else
      let lines = [a:config['title']] + lines
    endif
  endif
  let width = max(map(copy(lines), 'strwidth(v:val)')) + (empty(icon) ? 1 : 3)
  if width > maxWidth
    let lines = coc#string#reflow(lines, maxWidth)
    let width = max(map(copy(lines), 'strwidth(v:val)')) + (empty(icon) ? 1 : 3)
  endif
  let highlights = []
  if !empty(icon)
    let ic = icon['text']
    if empty(lines)
      call add(lines, ic)
    else
      let lines[0] = ic.' '.lines[0]
    endif
    call add(highlights, {'lnum': 0, 'hlGroup': icon['hl'], 'colStart': 0, 'colEnd': strlen(ic)})
  endif
  let actionText = join(actions, ' ')
  call map(lines, 'v:key == 0 ? v:val : repeat(" ", '.(empty(icon) ? 0 : 2).').v:val')
  let minWidth = get(a:config, 'minWidth', kind ==# 'progress' ? 30 : 10)
  let width = max(extend(map(lines + [get(opts, 'title', '').'   '], 'strwidth(v:val)'), [minWidth, strwidth(actionText) + 1]))
  let width = min([maxWidth, width])
  let height = min([get(a:config, 'maxHeight', 3), len(lines)])
  if kind ==# 'progress'
    let lines = [repeat(s:progress_char, width)] + lines
    let height = height + 1
  endif
  if !empty(actions)
    let before = max([width - strwidth(actionText), 0])
    let lines = lines + [repeat(' ', before).actionText]
    let height = height + 1
    call s:add_action_highlights(before, height - 1, highlights, actions)
  endif
  if row == 0
    let wintop = coc#notify#get_top()
    let row = wintop - height - (empty(border) ? 0 : 2) - 1
    if !s:is_vim && !empty(border)
      let row = row + 1
    endif
  endif
  let col = &columns - margin - width
  if s:is_vim && !empty(border)
    let col = col - 2
  endif
  let winblend = 60
  " Avoid animate for transparent background.
  if get(a:config, 'winblend', 30) == 0 && empty(synIDattr(synIDtrans(hlID(get(opts, 'highlight', 'CocFloating'))), 'bg', 'gui'))
    let winblend = 0
  endif
  call extend(opts, {
      \ 'relative': 'editor',
      \ 'width': width,
      \ 'height': height,
      \ 'col': col,
      \ 'row': row + 1,
      \ 'lines': lines,
      \ 'rounded': 1,
      \ 'highlights': highlights,
      \ 'winblend': winblend,
      \ 'border': border,
      \ })
  let result = coc#float#create_float_win(0, 0, opts)
  if empty(result)
    throw 'Unable to create notify window'
  endif
  let winid = result[0]
  let bufnr = result[1]
  call setwinvar(winid, 'right', 1)
  call setwinvar(winid, 'kind', 'notification')
  call setwinvar(winid, 'top', row)
  call setwinvar(winid, 'key', key)
  call setwinvar(winid, 'actions', actions)
  call setwinvar(winid, 'source', get(a:config, 'source', ''))
  call setwinvar(winid, 'border', !empty(border))
  call coc#float#nvim_scrollbar(winid)
  call add(s:winids, winid)
  let from = {'row': opts['row'], 'winblend': opts['winblend']}
  let to = {'row': row, 'winblend': get(a:config, 'winblend', 30)}
  call timer_start(s:interval, { -> s:animate(winid, from, to, 0)})
  if kind ==# 'progress'
    call timer_start(s:interval, { -> s:progress(winid, width, 0, -1)})
  endif
  if !s:is_vim
    call coc#compat#buf_add_keymap(bufnr, 'n', '<LeftRelease>', ':call coc#notify#nvim_click('.winid.')<CR>', {
        \ 'silent': v:true,
        \ 'nowait': v:true
        \ })
  endif
  " Enable auto close
  if empty(actions) && kind !=# 'progress'
    let timer = timer_start(get(a:config, 'timeout', 10000), { -> coc#notify#close(winid)})
    call setwinvar(winid, 'close_timer', timer)
  endif
  return [winid, bufnr]
endfunction

function! coc#notify#nvim_click(winid) abort
  if getwinvar(a:winid, 'closing', 0)
    return
  endif
  call s:cancel(a:winid, 'close_timer')
  let actions = getwinvar(a:winid, 'actions', [])
  if !empty(actions)
    let character = strpart(getline('.'), col('.') - 1, 1)
    if character =~# '^\k'
      let word = expand('<cword>')
      let idx = index(actions, word)
      if idx != -1
        call coc#rpc#notify('FloatBtnClick', [winbufnr(a:winid), idx])
        call coc#notify#close(a:winid)
      endif
    endif
  endif
endfunction

function! coc#notify#on_close(winid) abort
  if index(s:winids, a:winid) >= 0
    call filter(s:winids, 'v:val != '.a:winid)
    call coc#notify#reflow()
  endif
endfunction

function! coc#notify#get_top() abort
  let mintop = min(map(coc#notify#win_list(), 'coc#notify#get_win_top(v:val)'))
  if mintop != 0
    return mintop
  endif
  return &lines - &cmdheight - (&laststatus == 0 ? 0 : 1 )
endfunction

function! coc#notify#get_win_top(winid) abort
  let row = getwinvar(a:winid, 'top', 0)
  if row == 0
    return row
  endif
  return row - (s:is_vim ? 0 : getwinvar(a:winid, 'border', 0))
endfunction

" Close with timer
function! coc#notify#close(winid) abort
  if !coc#float#valid(a:winid) || coc#window#get_var(a:winid, 'closing', 0) == 1
    return
  endif
  if !coc#window#visible(a:winid)
    call coc#float#close(a:winid)
    return
  endif
  let row = coc#window#get_var(a:winid, 'top')
  if type(row) != v:t_number
    call coc#float#close(a:winid)
    return
  endif
  call coc#window#set_var(a:winid, 'closing', 1)
  call s:cancel(a:winid)
  let winblend = coc#window#get_var(a:winid, 'winblend', 0)
  let curr = s:is_vim ? {'row': row} : {'winblend': winblend}
  let dest = s:is_vim ? {'row': row + 1} : {'winblend': winblend == 0 ? 0 : 60}
  call s:animate(a:winid, curr, dest, 0, 1)
endfunction

function! s:add_action_highlights(before, lnum, highlights, actions) abort
  let colStart = a:before
  for text in a:actions
    let w = strwidth(text)
    call add(a:highlights, {
        \ 'lnum': a:lnum,
        \ 'hlGroup': 'CocNotificationButton',
        \ 'colStart': colStart,
        \ 'colEnd': colStart + w
        \ })
    let colStart = colStart + w + 1
  endfor
endfunction

function! s:on_action(err, idx, winid) abort
  if !empty(a:err)
    throw a:err
  endif
  if a:idx > 0
    call coc#rpc#notify('FloatBtnClick', [winbufnr(a:winid), a:idx - 1])
    call coc#notify#close(a:winid)
  endif
endfunction

function! s:cancel(winid, ...) abort
  let name = get(a:, 1, 'timer')
  let timer = coc#window#get_var(a:winid, name)
  if !empty(timer)
    call timer_stop(timer)
    call coc#window#set_var(a:winid, name, v:null)
  endif
endfunction

function! s:progress(winid, total, curr, index) abort
  if !coc#float#valid(a:winid)
    return
  endif
  if coc#window#visible(a:winid)
    let total = a:total
    let idx = float2nr(a:curr/5.0)%total
    if idx != a:index
      " update percent
      let bufnr = winbufnr(a:winid)
      let percent = coc#window#get_var(a:winid, 'percent')
      if !empty(percent)
        let width = strchars(getbufline(bufnr, 1)[0])
        let line = repeat(s:progress_char, width - 4).printf('%4s', percent)
        let total = width - 4
        call setbufline(bufnr, 1, line)
      endif
      let message = coc#window#get_var(a:winid, 'message')
      if !empty(message)
        let linecount = coc#compat#buf_line_count(bufnr)
        let hasAction = !empty(coc#window#get_var(a:winid, 'actions', []))
        if getbufvar(bufnr, 'message', 0) == 0
          call appendbufline(bufnr, linecount - hasAction, message)
          call setbufvar(bufnr, 'message', 1)
          call coc#float#change_height(a:winid, 1)
          let tabnr = coc#window#tabnr(a:winid)
          call coc#notify#reflow(tabnr)
        else
          call setbufline(bufnr, linecount - hasAction, message)
        endif
      endif
      let bytes = strlen(s:progress_char)
      call coc#highlight#clear_highlight(bufnr, -1, 0, 1)
      let colStart = bytes * idx
      if idx + 4 <= total
        let colEnd = bytes * (idx + 4)
        call coc#highlight#add_highlight(bufnr, -1, s:phl, 0, colStart, colEnd)
      else
        let colEnd = bytes * total
        call coc#highlight#add_highlight(bufnr, -1, s:phl, 0, colStart, colEnd)
        call coc#highlight#add_highlight(bufnr, -1, s:phl, 0, 0, bytes * (idx + 4 - total))
      endif
    endif
    call timer_start(s:interval, { -> s:progress(a:winid, total, a:curr + 1, idx)})
  else
    " Not block CursorHold event
    call timer_start(&updatetime + 100, { -> s:progress(a:winid, a:total, a:curr, a:index)})
  endif
endfunction

" Optional row & winblend
function! s:config_win(winid, props) abort
  let change_row = has_key(a:props, 'row')
  if s:is_vim
    if change_row
      call popup_move(a:winid, {'line': a:props['row'] + 1})
    endif
  else
    if change_row
      let [row, column] = nvim_win_get_position(a:winid)
      call nvim_win_set_config(a:winid, {
          \ 'row': a:props['row'],
          \ 'col': column,
          \ 'relative': 'editor',
          \ })
      call s:nvim_move_related(a:winid, a:props['row'])
    endif
    call coc#float#nvim_set_winblend(a:winid, get(a:props, 'winblend', v:null))
    call coc#float#nvim_refresh_scrollbar(a:winid)
  endif
endfunction

function! s:nvim_move_related(winid, row) abort
  let winids = coc#window#get_var(a:winid, 'related')
  if empty(winids)
    return
  endif
  for winid in winids
    if nvim_win_is_valid(winid)
      let [row, column] = nvim_win_get_position(winid)
      let delta = coc#window#get_var(winid, 'delta', 0)
      call nvim_win_set_config(winid, {
          \ 'row': a:row + delta,
          \ 'col': column,
          \ 'relative': 'editor',
          \ })
    endif
  endfor
endfunction

function! s:animate(winid, from, to, prev, ...) abort
  if !coc#float#valid(a:winid)
    return
  endif
  let curr = a:prev + s:interval
  let percent = coc#math#min(curr / s:duration, 1)
  let props = s:get_props(a:from, a:to, percent)
  call s:config_win(a:winid, props)
  let close = get(a:, 1, 0)
  if percent < 1
    call timer_start(s:interval, { -> s:animate(a:winid, a:from, a:to, curr, close)})
  elseif close
    call filter(s:winids, 'v:val != '.a:winid)
    let tabnr = coc#window#tabnr(a:winid)
    if tabnr != -1
      call coc#float#close(a:winid)
      call coc#notify#reflow(tabnr)
    endif
  endif
endfunction

function! coc#notify#reflow(...) abort
  let tabnr = get(a:, 1, tabpagenr())
  let winids = filter(copy(s:winids), 'coc#window#tabnr(v:val) == '.tabnr.' && coc#window#get_var(v:val,"closing") != 1')
  if empty(winids)
    return
  endif
  let animate = tabnr == tabpagenr()
  let wins = map(copy(winids), {_, val -> {
        \ 'winid': val,
        \ 'row': coc#window#get_var(val,'top',0),
        \ 'top': coc#window#get_var(val,'top',0) - (s:is_vim ? 0 : coc#window#get_var(val, 'border', 0)),
        \ 'height': coc#float#get_height(val),
        \ }})
  call sort(wins, {a, b -> b['top'] - a['top']})
  let bottom = &lines - &cmdheight - (&laststatus == 0 ? 0 : 1 )
  let moved = 0
  for item in wins
    let winid = item['winid']
    let delta = bottom - (item['top'] + item['height'] + 1)
    if delta != 0
      call s:cancel(winid)
      let dest = item['row'] + delta
      call coc#window#set_var(winid, 'top', dest)
      if animate
        call s:move_win_timer(winid, {'row': item['row']}, {'row': dest}, 0)
      else
        call s:config_win(winid, {'row': dest})
      endif
      let moved = moved + delta
    endif
    let bottom = item['top'] + delta
  endfor
endfunction

function! s:move_win_timer(winid, from, to, curr) abort
  if !coc#float#valid(a:winid)
    return
  endif
  if coc#window#get_var(a:winid, 'closing', 0) == 1
    return
  endif
  let percent = coc#math#min(a:curr / s:duration, 1)
  let next = a:curr + s:interval
  if a:curr > 0
    call s:config_win(a:winid, s:get_props(a:from, a:to, percent))
  endif
  if percent < 1
    let timer = timer_start(s:interval, { -> s:move_win_timer(a:winid, a:from, a:to, next)})
    call coc#window#set_var(a:winid, 'timer', timer)
  endif
endfunction

function! s:find_win(key) abort
  for winid in coc#notify#win_list()
    if getwinvar(winid, 'key', '') ==# a:key
      return winid
    endif
  endfor
  return -1
endfunction

function! s:get_icon(kind, bg) abort
  if a:kind ==# 'info'
    return {'text': s:info_icon, 'hl': coc#highlight#compose_hlgroup('CocInfoSign', a:bg)}
  endif
  if a:kind ==# 'warning'
    return {'text': s:warning_icon, 'hl': coc#highlight#compose_hlgroup('CocWarningSign', a:bg)}
  endif
  if a:kind ==# 'error'
    return {'text': s:error_icon, 'hl': coc#highlight#compose_hlgroup('CocErrorSign', a:bg)}
  endif
  return v:null
endfunction

" percent should be float
function! s:get_props(from, to, percent) abort
  let obj = {}
  for key in keys(a:from)
    let changed = a:to[key] - a:from[key]
    if !s:is_vim && key ==# 'row'
      " Could be float
      let obj[key] = a:from[key] + changed * a:percent
    else
      let obj[key] = a:from[key] + float2nr(round(changed * a:percent))
    endif
  endfor
  return obj
endfunction
