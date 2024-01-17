if exists('g:autoloaded_copilot_panel')
  finish
endif
let g:autoloaded_copilot_panel = 1

scriptencoding utf-8

if !exists('s:panel_id')
  let s:panel_id = 0
endif

let s:separator = repeat('─', 72)

function! s:Solutions(state) abort
  return sort(values(get(a:state, 'solutions', {})), { a, b -> a.score < b.score })
endfunction

function! s:Render(panel_id) abort
  let bufnr = bufnr('^' . a:panel_id . '$')
  let state = getbufvar(bufnr, 'copilot_panel')
  if !bufloaded(bufnr) || type(state) != v:t_dict
    return
  endif
  let sorted = s:Solutions(state)
  if !empty(get(state, 'status', ''))
    let lines = ['Error: ' . state.status]
  else
    let target = get(state, 'count_target', '?')
    let received = has_key(state, 'status') ? target : len(sorted)
    let lines = ['Synthesiz' . (has_key(state, 'status') ? 'ed ' : 'ing ') . received . '/' . target . ' solutions (Duplicates hidden)']
  endif
  if len(sorted)
    call add(lines, 'Press <CR> on a solution to accept')
  endif
  for solution in sorted
    let lines += [s:separator] + split(solution.displayText, "\n", 1)
  endfor
  try
    call setbufvar(bufnr, '&modifiable', 1)
    call setbufvar(bufnr, '&readonly', 0)
    call setbufline(bufnr, 1, lines)
  finally
    call setbufvar(bufnr, '&modifiable', 0)
    call setbufvar(bufnr, '&readonly', 1)
  endtry
endfunction

function! copilot#panel#Solution(params, ...) abort
  let state = getbufvar('^' . a:params.panelId . '$', 'copilot_panel')
  if !bufloaded(a:params.panelId) || type(state) != v:t_dict
    return
  endif
  let state.solutions[a:params.solutionId] = a:params
  call s:Render(a:params.panelId)
endfunction

function! copilot#panel#SolutionsDone(params, ...) abort
  let state = getbufvar('^' . a:params.panelId . '$', 'copilot_panel')
  if !bufloaded(a:params.panelId) || type(state) != v:t_dict
    call copilot#logger#Debug('SolutionsDone: ' . a:params.panelId)
    return
  endif
  let state.status = get(a:params, 'message', '')
  call s:Render(a:params.panelId)
endfunction

function! copilot#panel#Accept(...) abort
  let state = get(b:, 'copilot_panel', {})
  let solutions = s:Solutions(state)
  if empty(solutions)
    return ''
  endif
  if !has_key(state, 'bufnr') || !bufloaded(get(state, 'bufnr', -1))
    return "echoerr 'Buffer was closed'"
  endif
  let at = a:0 ? a:1 : line('.')
  let solution_index = 0
  for lnum in range(1, at)
    if getline(lnum) ==# s:separator
      let solution_index += 1
    endif
  endfor
  if solution_index > 0 && solution_index <= len(solutions)
    let solution = solutions[solution_index - 1]
    let lnum = solution.range.start.line + 1
    if getbufline(state.bufnr, lnum) !=# [state.line]
      return 'echoerr "Buffer has changed since synthesizing solution"'
    endif
    let lines = split(solution.displayText, "\n", 1)
    let old_first = getline(solution.range.start.line + 1)
    let lines[0] = strpart(old_first, 0, copilot#doc#UTF16ToByteIdx(old_first, solution.range.start.character)) . lines[0]
    let old_last = getline(solution.range.end.line + 1)
    let lines[-1] .= strpart(old_last, copilot#doc#UTF16ToByteIdx(old_last, solution.range.start.character))
    call setbufline(state.bufnr, solution.range.start.line + 1, lines[0])
    call appendbufline(state.bufnr, solution.range.start.line + 1, lines[1:-1])
    call copilot#Request('notifyAccepted', {'uuid': solution.solutionId})
    bwipeout
    let win = bufwinnr(state.bufnr)
    if win > 0
      exe win . 'wincmd w'
      exe solution.range.start.line + len(lines)
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
  call clearmatches()
  call matchadd('CopilotSuggestion', '\C^' . s:separator . '\n\zs' . escape(a:state.line, '][^$.*\~'), 10, 4)
  nmap <buffer><script> <CR> <Cmd>exe copilot#panel#Accept()<CR>
  nmap <buffer><script> [[ <Cmd>call search('^─\{9,}\n.', 'bWe')<CR>
  nmap <buffer><script> ]] <Cmd>call search('^─\{9,}\n.', 'We')<CR>
endfunction

function! s:BufReadCmd() abort
  setlocal bufhidden=wipe buftype=nofile nobuflisted readonly nomodifiable
  let state = get(b:, 'copilot_panel')
  if type(state) != v:t_dict
    return
  endif
  call s:Initialize(state)
  call s:Render(expand('<amatch>'))
  return ''
endfunction

function! copilot#panel#Open(opts) abort
  let s:panel_id += 1
  let state = {'solutions': {}, 'filetype': &filetype, 'line': getline('.'), 'bufnr': bufnr(''), 'tabstop': &tabstop}
  let bufname = 'copilot:///' . s:panel_id
  let params = copilot#doc#Params({'panelId': bufname})
  let state.was_insert = mode() =~# '^[iR]'
  if state.was_insert
    stopinsert
  else
    let params.doc.position.character = copilot#doc#UTF16Width(state.line)
    let params.position.character = params.doc.position.character
  endif
  let response = copilot#Request('getPanelCompletions', params).Wait()
  if response.status ==# 'error'
    return 'echoerr ' . string(response.error.message)
  endif
  let state.count_target = response.result.solutionCountTarget
  exe substitute(a:opts.mods, '\C\<tab\>', '-tab', 'g') 'keepalt split' bufname
  let b:copilot_panel = state
  call s:Initialize(state)
  call s:Render(@%)
  return ''
endfunction

augroup github_copilot_panel
  autocmd!
  autocmd BufReadCmd copilot:///* exe s:BufReadCmd()
augroup END
