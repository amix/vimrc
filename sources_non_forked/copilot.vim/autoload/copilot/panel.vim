scriptencoding utf-8

if !exists('s:panel_id')
  let s:panel_id = 0
endif

let s:separator = repeat('─', 72)

function! s:Render(state) abort
  let bufnr = bufnr('^' . a:state.panel . '$')
  let state = a:state
  if !bufloaded(bufnr)
    return
  endif
  let sorted = a:state.items
  if !empty(get(a:state, 'error'))
    let lines = ['Error: ' . a:state.error.message]
    let sorted = []
  elseif get(a:state, 'percentage') == 100
    let lines = ['Synthesized ' . (len(sorted) == 1 ? '1 completion' : len(sorted) . ' completions')]
  else
    let lines = [substitute('Synthesizing ' . matchstr(get(a:state, 'message', ''), '\d\+\%(/\d\+\)\=') . ' completions', ' \+', ' ', 'g')]
  endif
  if len(sorted)
    call add(lines, 'Press <CR> on a completion to accept')
  endif
  let leads = {}
  for item in sorted
    let insert = split(item.insertText, "\r\n\\=\\|\n", 1)
    let insert[0] = strpart(a:state.line, 0, copilot#util#UTF16ToByteIdx(a:state.line, item.range.start.character)) . insert[0]
    let lines += [s:separator] + insert
    if !has_key(leads, string(item.range.start))
      let match = insert[0 : a:state.position.line - item.range.start.line]
      let match[-1] = strpart(match[-1], 0, copilot#util#UTF16ToByteIdx(match[-1], a:state.position.character))
      call map(match, { k, v -> escape(v, '][^$.*\~') })
      let leads[string(item.range.start)] = join(match, '\n')
    endif
  endfor
  try
    call setbufvar(bufnr, '&modifiable', 1)
    call setbufvar(bufnr, '&readonly', 0)
    call setbufline(bufnr, 1, lines)
  finally
    call setbufvar(bufnr, '&modifiable', 0)
  endtry
  call clearmatches()
  call matchadd('CopilotSuggestion', '\C^' . s:separator . '\n\zs\%(' . join(sort(values(leads), { a, b -> len(b) - len(a) }), '\|') . '\)', 10, 4)
endfunction

function! s:PartialResult(state, value) abort
  let items = type(a:value) == v:t_list ? a:value : a:value.items
  call extend(a:state.items, items)
  call s:Render(a:state)
endfunction

function! s:WorkDone(state, value) abort
  if has_key(a:value, 'message')
    let a:state.message = a:value.message
  endif
  if has_key(a:value, 'percentage')
    let a:state.percentage = a:value.percentage
    call s:Render(a:state)
  endif
endfunction

function! copilot#panel#Accept(...) abort
  let state = get(b:, 'copilot_panel', {})
  if empty(state.items)
    return ''
  endif
  if !has_key(state, 'bufnr') || !bufloaded(get(state, 'bufnr', -1))
    return "echoerr 'Buffer was closed'"
  endif
  let at = a:0 ? a:1 : line('.')
  let index = 0
  for lnum in range(1, at)
    if getline(lnum) ==# s:separator
      let index += 1
    endif
  endfor
  if index > 0 && index <= len(state.items)
    let item = state.items[index - 1]
    let lnum = item.range.start.line + 1
    if getbufline(state.bufnr, lnum) !=# [state.line]
      return 'echoerr "Buffer has changed since synthesizing completion"'
    endif
    let lines = split(item.insertText, "\n", 1)
    let old_first = getbufline(state.bufnr, item.range.start.line + 1)[0]
    let lines[0] = strpart(old_first, 0, copilot#util#UTF16ToByteIdx(old_first, item.range.start.character)) . lines[0]
    let old_last = getbufline(state.bufnr, item.range.end.line + 1)[0]
    let lines[-1] .= strpart(old_last, copilot#util#UTF16ToByteIdx(old_last, item.range.end.character))
    call deletebufline(state.bufnr, item.range.start.line + 1, item.range.end.line + 1)
    call appendbufline(state.bufnr, item.range.start.line, lines)
    call copilot#Request('workspace/executeCommand', item.command)
    bwipeout
    let win = bufwinnr(state.bufnr)
    if win > 0
      exe win . 'wincmd w'
      exe item.range.start.line + len(lines)
      if state.was_insert
        startinsert!
      else
        normal! $
      endif
    endif
  endif
  return ''
endfunction

function! s:Initialize(state) abort
  let &l:filetype = 'copilot' . (empty(a:state.filetype) ? '' : '.' . a:state.filetype)
  let &l:tabstop = a:state.tabstop
  nmap <buffer><script> <CR> <Cmd>exe copilot#panel#Accept()<CR>
  nmap <buffer><script> [[ <Cmd>call search('^─\{9,}\n.', 'bWe')<CR>
  nmap <buffer><script> ]] <Cmd>call search('^─\{9,}\n.', 'We')<CR>
endfunction

function! s:BufReadCmd() abort
  setlocal bufhidden=wipe buftype=nofile nobuflisted nomodifiable
  let state = get(b:, 'copilot_panel')
  if type(state) != v:t_dict
    return
  endif
  call s:Initialize(state)
  call s:Render(state)
  return ''
endfunction

function! s:Result(state, result) abort
  let a:state.percentage = 100
  call s:PartialResult(a:state, a:result)
endfunction

function! s:Error(state, error) abort
  let a:state.error = a:error
  call s:Render(a:state)
endfunction

function! copilot#panel#Open(opts) abort
  let s:panel_id += 1
  let state = {'items': [], 'filetype': &filetype, 'was_insert': mode() =~# '^[iR]', 'bufnr': bufnr(''), 'tabstop': &tabstop}
  let state.panel = 'copilot:///panel/' . s:panel_id
  if state.was_insert
    let state.position = copilot#util#AppendPosition()
    stopinsert
  else
    let state.position = {'line': a:opts.line1 >= 1 ? a:opts.line1 - 1 : 0, 'character': copilot#util#UTF16Width(getline('.'))}
  endif
  let state.line = getline(state.position.line + 1)
  let params = {
        \ 'textDocument': {'uri': state.bufnr},
        \ 'position': state.position,
        \ 'partialResultToken': function('s:PartialResult', [state]),
        \ 'workDoneToken': function('s:WorkDone', [state]),
        \ }
  let response = copilot#Request('textDocument/copilotPanelCompletion', params, function('s:Result', [state]), function('s:Error', [state]))
  exe substitute(a:opts.mods, '\C\<tab\>', '-tab', 'g') 'keepalt split' state.panel
  let b:copilot_panel = state
  call s:Initialize(state)
  call s:Render(state)
  return ''
endfunction

augroup github_copilot_panel
  autocmd!
  autocmd BufReadCmd copilot:///panel/* exe s:BufReadCmd()
augroup END
