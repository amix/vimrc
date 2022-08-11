scriptencoding utf-8
let s:is_vim = !has('nvim')
let s:pum_bufnr = 0
let s:pum_winid = 0
let s:pum_index = -1
let s:inserted = 0
let s:virtual_text = 0
let s:virtual_text_ns = 0
let s:ignore = s:is_vim || has('nvim-0.5.0') ? "\<Ignore>" : "\<space>\<bs>"
let s:hide_pum = has('nvim-0.6.1') || has('patch-8.2.3389')

function! coc#pum#visible() abort
  if !s:pum_winid
    return 0
  endif
  return getwinvar(s:pum_winid, 'float', 0) == 1
endfunction

function! coc#pum#winid() abort
  return s:pum_winid
endfunction

function! coc#pum#close_detail() abort
  let winid = coc#float#get_float_by_kind('pumdetail')
  if winid
    call coc#float#close(winid)
    if s:is_vim
      call timer_start(0, { -> execute('redraw')})
    endif
  endif
endfunction

function! coc#pum#close(...) abort
  if coc#float#valid(s:pum_winid)
    if get(a:, 1, '') ==# 'cancel'
      let input = getwinvar(s:pum_winid, 'input', '')
      let s:pum_index = -1
      call s:insert_word(input)
      call s:on_pum_change(0)
      doautocmd <nomodeline> TextChangedI
    elseif get(a:, 1, '') ==# 'confirm'
      let words = getwinvar(s:pum_winid, 'words', [])
      if s:pum_index >= 0
        let word = get(words, s:pum_index, '')
        call s:insert_word(word)
      endif
      doautocmd <nomodeline> TextChangedI
    endif
    call s:close_pum()
    if !get(a:, 2, 0)
      let pretext = strpart(getline('.'), 0, col('.') - 1)
      call coc#rpc#notify('CompleteStop', [get(a:, 1, ''), pretext])
    endif
  endif
endfunction

function! coc#pum#select_confirm() abort
  if s:pum_index < 0
    let s:pum_index = 0
    call s:on_pum_change(0)
  endif
  call coc#pum#close('confirm')
endfunction

function! coc#pum#insert() abort
  call timer_start(10, { -> s:insert_current()})
  return s:ignore
endfunction

function! coc#pum#_close() abort
  if coc#float#valid(s:pum_winid)
    call s:close_pum()
    if s:is_vim
      call timer_start(0, { -> execute('redraw')})
    endif
  endif
endfunction

function! s:insert_current() abort
  if coc#float#valid(s:pum_winid)
    if s:pum_index >= 0
      let words = getwinvar(s:pum_winid, 'words', [])
      let word = get(words, s:pum_index, '')
      call s:insert_word(word)
    endif
    doautocmd <nomodeline> TextChangedI
    call s:close_pum()
    let pretext = strpart(getline('.'), 0, col('.') - 1)
    call coc#rpc#notify('CompleteStop', ['', pretext])
  endif
endfunction

function! s:close_pum() abort
  call s:clear_virtual_text()
  call coc#float#close(s:pum_winid)
  let s:pum_winid = 0
  let winid = coc#float#get_float_by_kind('pumdetail')
  if winid
    call coc#float#close(winid)
  endif
endfunction

function! coc#pum#next(insert) abort
  call timer_start(10, { -> s:navigate(1, a:insert)})
  return s:ignore
endfunction

function! coc#pum#prev(insert) abort
  call timer_start(10, { -> s:navigate(0, a:insert)})
  return s:ignore
endfunction

function! coc#pum#stop() abort
  call timer_start(10, { -> coc#pum#close()})
  return s:ignore
endfunction

function! coc#pum#cancel() abort
  call timer_start(10, { -> coc#pum#close('cancel')})
  return s:ignore
endfunction

function! coc#pum#confirm() abort
  call timer_start(10, { -> coc#pum#close('confirm')})
  return s:ignore
endfunction

function! coc#pum#select(index, insert, confirm) abort
  if !coc#float#valid(s:pum_winid)
    return ''
  endif
  if a:index == -1
    call coc#pum#close('cancel')
    return ''
  endif
  let total = coc#compat#buf_line_count(s:pum_bufnr)
  if a:index < 0 || a:index >= total
    throw 'index out of range ' . a:index
  endif
  call s:select_by_index(a:index, a:insert)
  if a:confirm
    call coc#pum#close('confirm')
  endif
  return ''
endfunction

function! coc#pum#info() abort
  let bufnr = winbufnr(s:pum_winid)
  let size = coc#compat#buf_line_count(bufnr)
  let words = getwinvar(s:pum_winid, 'words', [])
  let word = s:pum_index < 0 ? '' : get(words, s:pum_index, '')
  if s:is_vim
    let pos = popup_getpos(s:pum_winid)
    let border = has_key(popup_getoptions(s:pum_winid), 'border')
    let add = pos['scrollbar'] && border ? 1 : 0
    return {
        \ 'word': word,
        \ 'index': s:pum_index,
        \ 'scrollbar': pos['scrollbar'],
        \ 'row': pos['line'] - 1,
        \ 'col': pos['col'] - 1,
        \ 'width': pos['width'] + add,
        \ 'height': pos['height'],
        \ 'size': size,
        \ 'border': border,
        \ 'inserted': s:inserted ? v:true : v:false,
        \ }
  else
    let scrollbar = coc#float#get_related(s:pum_winid, 'scrollbar')
    let winid = coc#float#get_related(s:pum_winid, 'border', s:pum_winid)
    let pos = nvim_win_get_position(winid)
    return {
        \ 'word': word,
        \ 'index': s:pum_index,
        \ 'scrollbar': scrollbar && nvim_win_is_valid(scrollbar) ? 1 : 0,
        \ 'row': pos[0],
        \ 'col': pos[1],
        \ 'width': nvim_win_get_width(winid),
        \ 'height': nvim_win_get_height(winid),
        \ 'size': size,
        \ 'border': winid != s:pum_winid,
        \ 'inserted': s:inserted ? v:true : v:false,
        \ }
  endif
endfunction

function! coc#pum#scroll(forward) abort
  if coc#pum#visible()
    let size = coc#compat#buf_line_count(s:pum_bufnr)
    let height = s:get_height(s:pum_winid)
    if size > height
      call timer_start(10, { -> s:scroll_pum(a:forward, height, size)})
    endif
  endif
  return s:ignore
endfunction

function! s:get_height(winid) abort
  if has('nvim')
    return nvim_win_get_height(a:winid)
  endif
  return get(popup_getpos(a:winid), 'core_height', 0)
endfunction

function! s:scroll_pum(forward, height, size) abort
  let topline = s:get_topline(s:pum_winid)
  if !a:forward && topline == 1
    if s:pum_index >= 0
      call s:select_line(s:pum_winid, 1)
      call s:on_pum_change(1)
    endif
    return
  endif
  if a:forward && topline + a:height - 1 >= a:size
    if s:pum_index >= 0
      call s:select_line(s:pum_winid, a:size)
      call s:on_pum_change(1)
    endif
    return
  endif
  call coc#float#scroll_win(s:pum_winid, a:forward, a:height)
  if s:pum_index >= 0
    let lnum = s:pum_index + 1
    let topline = s:get_topline(s:pum_winid)
    if lnum >= topline && lnum <= topline + a:height - 1
      return
    endif
    call s:select_line(s:pum_winid, topline)
    call s:on_pum_change(1)
  endif
endfunction

function! s:get_topline(winid) abort
  if has('nvim')
    let info = getwininfo(a:winid)[0]
    return info['topline']
  else
    let pos = popup_getpos(a:winid)
    return pos['firstline']
  endif
endfunction

function! s:navigate(next, insert) abort
  if !coc#float#valid(s:pum_winid)
    return
  endif
  let index = s:get_index(a:next)
  call s:select_by_index(index, a:insert)
endfunction

function! s:select_by_index(index, insert) abort
  call s:set_cursor(s:pum_winid, a:index + 1)
  if !s:is_vim
    call coc#float#nvim_scrollbar(s:pum_winid)
  endif
  if a:insert
    let s:inserted = 1
    if a:index < 0
      let input = getwinvar(s:pum_winid, 'input', '')
      call s:insert_word(input)
      call coc#pum#close_detail()
    else
      let words = getwinvar(s:pum_winid, 'words', [])
      let word = get(words, a:index, '')
      call s:insert_word(word)
    endif
    doautocmd <nomodeline> TextChangedP
  endif
  call s:on_pum_change(1)
endfunction

function! s:get_index(next) abort
  let size = coc#compat#buf_line_count(s:pum_bufnr)
  if a:next
    let index = s:pum_index + 1 == size ? -1 : s:pum_index + 1
  else
    let index = s:pum_index == -1 ? size - 1 : s:pum_index - 1
  endif
  return index
endfunction

function! s:insert_word(word) abort
  let parts = getwinvar(s:pum_winid, 'parts', [])
  if !empty(parts) && mode() ==# 'i'
    let curr = getline('.')
    if curr ==# parts[0].a:word.parts[1]
      return
    endif
    let saved_completeopt = &completeopt
    if saved_completeopt =~ 'menuone'
      noa set completeopt=menu
    endif
    noa call complete(strlen(parts[0]) + 1, [a:word])
    if s:hide_pum
      " exit complete state
      call feedkeys("\<C-x>\<C-z>", 'in')
    else
      let g:coc_disable_space_report = 1
      call feedkeys("\<space>\<bs>", 'in')
    endif
    execute 'noa set completeopt='.saved_completeopt
  endif
endfunction

" create or update pum with lines, CompleteOption and config.
" return winid & dimension
function! coc#pum#create(lines, opt, config) abort
  if mode() !=# 'i' || a:opt['line'] != line('.')
    return
  endif
  let len = col('.') - a:opt['col'] - 1
  if len < 0
    return
  endif
  let input = len == 0 ? '' : strpart(getline('.'), a:opt['col'], len)
  if input !=# a:opt['input']
    return
  endif
  let config = s:get_pum_dimension(a:lines, a:opt['col'], a:config)
  if empty(config)
    return
  endif
  let s:virtual_text = has('nvim-0.5.0') && a:opt['virtualText']
  if s:virtual_text && !s:virtual_text_ns
    let s:virtual_text_ns = coc#highlight#create_namespace('pum-virtual')
  endif
  let selected = a:opt['index'] + 1
  call extend(config, {
        \ 'lines': a:lines,
        \ 'relative': 'cursor',
        \ 'nopad': 1,
        \ 'cursorline': 1,
        \ 'index': a:opt['index'],
        \ 'focusable': v:false
        \ })
  call extend(config, coc#dict#pick(a:config, ['highlight', 'rounded', 'highlights', 'winblend', 'shadow', 'border', 'borderhighlight']))
  if empty(get(config, 'winblend', 0)) && exists('&pumblend')
    let config['winblend'] = &pumblend
  endif
  let result =  coc#float#create_float_win(s:pum_winid, s:pum_bufnr, config)
  if empty(result)
    return
  endif
  let s:inserted = 0
  let s:pum_winid = result[0]
  let s:pum_bufnr = result[1]
  call setwinvar(s:pum_winid, 'above', config['row'] < 0)
  let lnum = max([1, a:opt['index'] + 1])
  if s:is_vim
    call popup_setoptions(s:pum_winid, {
          \ 'firstline': s:get_firstline(lnum, len(a:lines), config['height'])
          \ })
  else
    let firstline = s:get_firstline(lnum, len(a:lines), config['height'])
    call coc#compat#execute(s:pum_winid, 'call winrestview({"lnum":'.lnum.',"topline":'.firstline.'})')
  endif
  let s:pum_index = get(config, 'index', -1)
  call coc#dialog#place_sign(s:pum_bufnr, s:pum_index + 1)
  call setwinvar(s:pum_winid, 'kind', 'pum')
  " content before col and content after cursor
  let linetext = getline('.')
  let parts = [strpart(linetext, 0, a:opt['col']), strpart(linetext, col('.') - 1)]
  call setwinvar(s:pum_winid, 'input', input)
  call setwinvar(s:pum_winid, 'parts', parts)
  call setwinvar(s:pum_winid, 'words', a:opt['words'])
  if !s:is_vim
    if len(a:lines) > config['height']
      redraw
      call coc#float#nvim_scrollbar(s:pum_winid)
    else
      call coc#float#close_related(s:pum_winid, 'scrollbar')
    endif
  endif
  call timer_start(10, { -> s:on_pum_change(0)})
endfunction

function! s:get_firstline(lnum, total, height) abort
  if a:lnum <= a:height
    return 1
  endif
  return min([a:total - a:height + 1, a:lnum  - (a:height*2/3)])
endfunction

function! s:on_pum_change(move) abort
  if coc#float#valid(s:pum_winid)
    if s:virtual_text_ns
      call s:insert_virtual_text()
    endif
    let ev = extend(coc#pum#info(), {'move': a:move ? v:true : v:false})
    call coc#rpc#notify('CocAutocmd', ['MenuPopupChanged', ev, win_screenpos(winnr())[0] + winline() - 2])
  endif
endfunction

function! s:get_pum_dimension(lines, col, config) abort
  let linecount = len(a:lines)
  let [lineIdx, colIdx] = coc#cursor#screen_pos()
  let bh = empty(get(a:config, 'border', [])) ? 0 : 2
  let width = min([&columns, max([exists('&pumwidth') ? &pumwidth : 15, a:config['width']])])
  let vh = &lines - &cmdheight - 1 - !empty(&tabline)
  if vh <= 0
    return v:null
  endif
  let pumheight = empty(&pumheight) ? vh : &pumheight
  let showTop = getwinvar(s:pum_winid, 'above', v:null)
  if type(showTop) != v:t_number
    if vh - lineIdx - bh - 1 < min([pumheight, linecount]) && lineIdx > vh - lineIdx
      let showTop = 1
    else
      let showTop = 0
    endif
  endif
  let height = showTop ? min([lineIdx - bh - !empty(&tabline), linecount, pumheight]) : min([vh - lineIdx - bh - 1, linecount, pumheight])
  if height <= 0
    return v:null
  endif
  let col = - (col('.') - a:col - 1) - 1
  let row = showTop ? - height : 1
  let delta = colIdx + col
  if delta < 0
    let col = col - delta
  elseif delta + width > &columns
    let col = max([-colIdx, col - (delta + width - &columns)])
  endif
  return {
        \ 'row': row,
        \ 'col': col,
        \ 'width': width,
        \ 'height': height
        \ }
endfunction

" can't use coc#dialog#set_cursor on vim8, don't know why
function! s:set_cursor(winid, line) abort
  if s:is_vim
    let pos = popup_getpos(a:winid)
    let lastline = pos['firstline'] + pos['core_height'] - 1
    if a:line > lastline
      call popup_setoptions(a:winid, {
            \ 'firstline': pos['firstline'] + a:line - lastline,
            \ })
    elseif a:line < pos['firstline']
      call popup_setoptions(a:winid, {
            \ 'firstline': max([1, a:line]),
            \ })
    endif
  endif
  call s:select_line(a:winid, a:line)
endfunction

function! s:select_line(winid, line) abort
  let s:pum_index = a:line - 1
  let lnum = max([1, a:line])
  if s:is_vim
    call coc#compat#execute(a:winid, 'exe '.lnum)
  else
    call nvim_win_set_cursor(a:winid, [lnum, 0])
  endif
  call coc#dialog#place_sign(winbufnr(a:winid), a:line)
endfunction

function! s:insert_virtual_text() abort
  if !s:virtual_text_ns
    return
  endif
  let bufnr = bufnr('%')
  if !s:virtual_text || !coc#pum#visible() || s:pum_index < 0
    call nvim_buf_clear_namespace(bufnr, s:virtual_text_ns, 0, -1)
  else
    " Check if could create
    let insert = ''
    let words = getwinvar(s:pum_winid, 'words', [])
    let word = get(words, s:pum_index, '')
    let parts = getwinvar(s:pum_winid, 'parts', [])
    let input = strpart(getline('.'), strlen(parts[0]), col('.') - 1)
    if strchars(word) > strchars(input) && strcharpart(word, 0, strchars(input)) ==# input
      let insert = strcharpart(word, strchars(input))
    endif
    call nvim_buf_clear_namespace(bufnr, s:virtual_text_ns, 0, -1)
    if !empty(insert)
      let opts = {
          \ 'hl_mode': 'combine',
          \ 'virt_text': [[insert, 'CocPumVirtualText']],
          \ 'virt_text_pos': 'overlay',
          \ 'virt_text_win_col': virtcol('.') - 1,
          \ }
      call nvim_buf_set_extmark(bufnr, s:virtual_text_ns, line('.') - 1, col('.') - 1, opts)
    endif
  endif
endfunction

function! s:clear_virtual_text() abort
  if s:virtual_text_ns
    call nvim_buf_clear_namespace(bufnr('%'), s:virtual_text_ns, 0, -1)
  endif
endfunction
