let s:t_string = type('')

" Primary functions {{{

function! gitgutter#all(force) abort
  for bufnr in s:uniq(tabpagebuflist())
    let file = expand('#'.bufnr.':p')
    if !empty(file)
      call gitgutter#init_buffer(bufnr)
      call gitgutter#process_buffer(bufnr, a:force)
    endif
  endfor
endfunction


" Finds the file's path relative to the repo root.
function! gitgutter#init_buffer(bufnr)
  if gitgutter#utility#is_active(a:bufnr)
    let p = gitgutter#utility#repo_path(a:bufnr, 0)
    if type(p) != s:t_string || empty(p)
      call gitgutter#utility#set_repo_path(a:bufnr)
    endif
  endif
endfunction


function! gitgutter#process_buffer(bufnr, force) abort
  " NOTE a:bufnr is not necessarily the current buffer.

  if gitgutter#utility#is_active(a:bufnr)
    if a:force || s:has_fresh_changes(a:bufnr)

      let diff = ''
      try
        let diff = gitgutter#diff#run_diff(a:bufnr, 0)
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
  let buflist = []
  for i in range(tabpagenr('$'))
    call extend(buflist, tabpagebuflist(i + 1))
  endfor

  for bufnr in s:uniq(buflist)
    let file = expand('#'.bufnr.':p')
    if !empty(file)
      call s:clear(bufnr)
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

if exists('*uniq')  " Vim 7.4.218
  function! s:uniq(list)
    return uniq(sort(a:list))
  endfunction
else
  function! s:uniq(list)
    let processed = []
    for e in a:list
      if index(processed, e) == -1
        call add(processed, e)
      endif
    endfor
    return processed
  endfunction
endif
