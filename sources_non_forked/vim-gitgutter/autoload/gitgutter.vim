let s:t_string = type('')

" Primary functions {{{

function! gitgutter#all(force) abort
  let visible = tabpagebuflist()

  for bufnr in range(1, bufnr('$') + 1)
    if buflisted(bufnr)
      let file = expand('#'.bufnr.':p')
      if !empty(file)
        if index(visible, bufnr) != -1
          call gitgutter#init_buffer(bufnr)
          call gitgutter#process_buffer(bufnr, a:force)
        elseif a:force
          call s:reset_tick(bufnr)
        endif
      endif
    endif
  endfor
endfunction


" Finds the file's path relative to the repo root.
function! gitgutter#init_buffer(bufnr)
  if gitgutter#utility#is_active(a:bufnr)
    let p = gitgutter#utility#repo_path(a:bufnr, 0)
    if type(p) != s:t_string || empty(p)
      call gitgutter#utility#set_repo_path(a:bufnr)
      call s:setup_maps()
    endif
  endif
endfunction


function! gitgutter#process_buffer(bufnr, force) abort
  " NOTE a:bufnr is not necessarily the current buffer.

  if gitgutter#utility#is_active(a:bufnr)
    if a:force || s:has_fresh_changes(a:bufnr)

      let diff = ''
      try
        let diff = gitgutter#diff#run_diff(a:bufnr, 'index', 0)
      catch /gitgutter not tracked/
        call gitgutter#debug#log('Not tracked: '.gitgutter#utility#file(a:bufnr))
      catch /gitgutter diff failed/
        call gitgutter#debug#log('Diff failed: '.gitgutter#utility#file(a:bufnr))
        call gitgutter#hunk#reset(a:bufnr)
      endtry

      if diff != 'async'
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

" }}}

function! s:setup_maps()
  if !g:gitgutter_map_keys
    return
  endif

  if !hasmapto('<Plug>GitGutterPrevHunk') && maparg('[c', 'n') ==# ''
    nmap <buffer> [c <Plug>GitGutterPrevHunk
  endif
  if !hasmapto('<Plug>GitGutterNextHunk') && maparg(']c', 'n') ==# ''
    nmap <buffer> ]c <Plug>GitGutterNextHunk
  endif

  if !hasmapto('<Plug>GitGutterStageHunk') && maparg('<Leader>hs', 'n') ==# ''
    nmap <buffer> <Leader>hs <Plug>GitGutterStageHunk
  endif
  if !hasmapto('<Plug>GitGutterUndoHunk') && maparg('<Leader>hu', 'n') ==# ''
    nmap <buffer> <Leader>hu <Plug>GitGutterUndoHunk
  endif
  if !hasmapto('<Plug>GitGutterPreviewHunk') && maparg('<Leader>hp', 'n') ==# ''
    nmap <buffer> <Leader>hp <Plug>GitGutterPreviewHunk
  endif

  if !hasmapto('<Plug>GitGutterTextObjectInnerPending') && maparg('ic', 'o') ==# ''
    omap <buffer> ic <Plug>GitGutterTextObjectInnerPending
  endif
  if !hasmapto('<Plug>GitGutterTextObjectOuterPending') && maparg('ac', 'o') ==# ''
    omap <buffer> ac <Plug>GitGutterTextObjectOuterPending
  endif
  if !hasmapto('<Plug>GitGutterTextObjectInnerVisual') && maparg('ic', 'x') ==# ''
    xmap <buffer> ic <Plug>GitGutterTextObjectInnerVisual
  endif
  if !hasmapto('<Plug>GitGutterTextObjectOuterVisual') && maparg('ac', 'x') ==# ''
    xmap <buffer> ac <Plug>GitGutterTextObjectOuterVisual
  endif
endfunction

function! s:has_fresh_changes(bufnr) abort
  return getbufvar(a:bufnr, 'changedtick') != gitgutter#utility#getbufvar(a:bufnr, 'tick')
endfunction

function! s:reset_tick(bufnr) abort
  call gitgutter#utility#setbufvar(a:bufnr, 'tick', 0)
endfunction

function! s:clear(bufnr)
  call gitgutter#sign#clear_signs(a:bufnr)
  call gitgutter#sign#remove_dummy_sign(a:bufnr, 1)
  call gitgutter#hunk#reset(a:bufnr)
  call s:reset_tick(a:bufnr)
endfunction
