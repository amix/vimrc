" Primary functions {{{

function! gitgutter#all(force) abort
  let visible = tabpagebuflist()

  for bufnr in range(1, bufnr('$') + 1)
    if buflisted(bufnr)
      let file = expand('#'.bufnr.':p')
      if !empty(file)
        if index(visible, bufnr) != -1
          call gitgutter#process_buffer(bufnr, a:force)
        elseif a:force
          call s:reset_tick(bufnr)
        endif
      endif
    endif
  endfor
endfunction


function! gitgutter#process_buffer(bufnr, force) abort
  " NOTE a:bufnr is not necessarily the current buffer.

  if gitgutter#utility#is_active(a:bufnr)

    if has('patch-7.4.1559')
      let l:Callback = function('gitgutter#process_buffer', [a:bufnr, a:force])
    else
      let l:Callback = {'function': 'gitgutter#process_buffer', 'arguments': [a:bufnr, a:force]}
    endif
    let how = s:setup_path(a:bufnr, l:Callback)
    if [how] == ['async']  " avoid string-to-number conversion if how is a number
      return
    endif

    if a:force || s:has_fresh_changes(a:bufnr)

      let diff = 'NOT SET'
      try
        let diff = gitgutter#diff#run_diff(a:bufnr, g:gitgutter_diff_relative_to, 0)
      catch /gitgutter not tracked/
        call gitgutter#debug#log('Not tracked: '.gitgutter#utility#file(a:bufnr))
      catch /gitgutter diff failed/
        call gitgutter#debug#log('Diff failed: '.gitgutter#utility#file(a:bufnr))
        call gitgutter#hunk#reset(a:bufnr)
      endtry

      if diff != 'async' && diff != 'NOT SET'
        call gitgutter#diff#handler(a:bufnr, diff)
      endif

    endif
  endif
endfunction


function! gitgutter#disable() abort
  " get list of all buffers (across all tabs)
  for bufnr in range(1, bufnr('$') + 1)
    if buflisted(bufnr)
      let file = expand('#'.bufnr.':p')
      if !empty(file)
        call s:clear(bufnr)
      endif
    endif
  endfor

  let g:gitgutter_enabled = 0
endfunction

function! gitgutter#enable() abort
  let g:gitgutter_enabled = 1
  call gitgutter#all(1)
endfunction

function! gitgutter#toggle() abort
  if g:gitgutter_enabled
    call gitgutter#disable()
  else
    call gitgutter#enable()
  endif
endfunction


function! gitgutter#buffer_disable() abort
  let bufnr = bufnr('')
  call gitgutter#utility#setbufvar(bufnr, 'enabled', 0)
  call s:clear(bufnr)
endfunction

function! gitgutter#buffer_enable() abort
  let bufnr = bufnr('')
  call gitgutter#utility#setbufvar(bufnr, 'enabled', 1)
  call gitgutter#process_buffer(bufnr, 1)
endfunction

function! gitgutter#buffer_toggle() abort
  if gitgutter#utility#getbufvar(bufnr(''), 'enabled', 1)
    call gitgutter#buffer_disable()
  else
    call gitgutter#buffer_enable()
  endif
endfunction

" }}}

function! gitgutter#setup_maps()
  if !g:gitgutter_map_keys
    return
  endif

  " Note hasmapto() and maparg() operate on the current buffer.

  let bufnr = bufnr('')

  if gitgutter#utility#getbufvar(bufnr, 'mapped', 0)
    return
  endif

  if !hasmapto('<Plug>(GitGutterPrevHunk)') && maparg('[c', 'n') ==# ''
    nmap <buffer> [c <Plug>(GitGutterPrevHunk)
  endif
  if !hasmapto('<Plug>(GitGutterNextHunk)') && maparg(']c', 'n') ==# ''
    nmap <buffer> ]c <Plug>(GitGutterNextHunk)
  endif

  if !hasmapto('<Plug>(GitGutterStageHunk)', 'v') && maparg('<Leader>hs', 'x') ==# ''
    xmap <buffer> <Leader>hs <Plug>(GitGutterStageHunk)
  endif
  if !hasmapto('<Plug>(GitGutterStageHunk)', 'n') && maparg('<Leader>hs', 'n') ==# ''
    nmap <buffer> <Leader>hs <Plug>(GitGutterStageHunk)
  endif
  if !hasmapto('<Plug>(GitGutterUndoHunk)') && maparg('<Leader>hu', 'n') ==# ''
    nmap <buffer> <Leader>hu <Plug>(GitGutterUndoHunk)
  endif
  if !hasmapto('<Plug>(GitGutterPreviewHunk)') && maparg('<Leader>hp', 'n') ==# ''
    nmap <buffer> <Leader>hp <Plug>(GitGutterPreviewHunk)
  endif

  if !hasmapto('<Plug>(GitGutterTextObjectInnerPending)') && maparg('ic', 'o') ==# ''
    omap <buffer> ic <Plug>(GitGutterTextObjectInnerPending)
  endif
  if !hasmapto('<Plug>(GitGutterTextObjectOuterPending)') && maparg('ac', 'o') ==# ''
    omap <buffer> ac <Plug>(GitGutterTextObjectOuterPending)
  endif
  if !hasmapto('<Plug>(GitGutterTextObjectInnerVisual)') && maparg('ic', 'x') ==# ''
    xmap <buffer> ic <Plug>(GitGutterTextObjectInnerVisual)
  endif
  if !hasmapto('<Plug>(GitGutterTextObjectOuterVisual)') && maparg('ac', 'x') ==# ''
    xmap <buffer> ac <Plug>(GitGutterTextObjectOuterVisual)
  endif

  call gitgutter#utility#setbufvar(bufnr, 'mapped', 1)
endfunction

function! s:setup_path(bufnr, continuation)
  if gitgutter#utility#has_repo_path(a:bufnr) | return | endif

  return gitgutter#utility#set_repo_path(a:bufnr, a:continuation)
endfunction

function! s:has_fresh_changes(bufnr) abort
  return getbufvar(a:bufnr, 'changedtick') != gitgutter#utility#getbufvar(a:bufnr, 'tick')
endfunction

function! s:reset_tick(bufnr) abort
  call gitgutter#utility#setbufvar(a:bufnr, 'tick', 0)
endfunction

function! s:clear(bufnr)
  call gitgutter#sign#clear_signs(a:bufnr)
  call gitgutter#hunk#reset(a:bufnr)
  call s:reset_tick(a:bufnr)
  call gitgutter#utility#setbufvar(a:bufnr, 'path', '')
endfunction


" Note:
" - this runs synchronously
" - it ignores unsaved changes in buffers
" - it does not change to the repo root
function! gitgutter#quickfix(current_file)
  let cmd = g:gitgutter_git_executable.' '.g:gitgutter_git_args.' rev-parse --show-cdup'
  let path_to_repo = get(systemlist(cmd), 0, '')
  if !empty(path_to_repo) && path_to_repo[-1:] != '/'
    let path_to_repo .= '/'
  endif

  let locations = []
  let cmd = g:gitgutter_git_executable.' '.g:gitgutter_git_args.' --no-pager'.
        \ ' diff --no-ext-diff --no-color -U0'.
        \ ' --src-prefix=a/'.path_to_repo.' --dst-prefix=b/'.path_to_repo.' '.
        \ g:gitgutter_diff_args. ' '. g:gitgutter_diff_base
  if a:current_file
    let cmd = cmd.' -- '.expand('%:p')
  endif
  let diff = systemlist(cmd)
  let lnum = 0
  for line in diff
    if line =~ '^diff --git [^"]'
      let paths = line[11:]
      let mid = (len(paths) - 1) / 2
      let [fnamel, fnamer] = [paths[:mid-1], paths[mid+1:]]
      let fname = fnamel ==# fnamer ? fnamel : fnamel[2:]
    elseif line =~ '^diff --git "'
      let [_, fnamel, _, fnamer] = split(line, '"')
      let fname = fnamel ==# fnamer ? fnamel : fnamel[2:]
    elseif line =~ '^diff --cc [^"]'
      let fname = line[10:]
    elseif line =~ '^diff --cc "'
      let [_, fname] = split(line, '"')
    elseif line =~ '^@@'
      let lnum = matchlist(line, '+\(\d\+\)')[1]
    elseif lnum > 0
      call add(locations, {'filename': fname, 'lnum': lnum, 'text': line})
      let lnum = 0
    endif
  endfor
  if !g:gitgutter_use_location_list
    call setqflist(locations)
  else
    call setloclist(0, locations)
  endif
endfunction
