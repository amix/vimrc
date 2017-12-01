function! gitgutter#hunk#set_hunks(hunks) abort
  call gitgutter#utility#setbufvar(gitgutter#utility#bufnr(), 'hunks', a:hunks)
  call s:reset_summary()
endfunction

function! gitgutter#hunk#hunks() abort
  return gitgutter#utility#getbufvar(gitgutter#utility#bufnr(), 'hunks', [])
endfunction

function! gitgutter#hunk#reset() abort
  call gitgutter#utility#setbufvar(gitgutter#utility#bufnr(), 'hunks', [])
  call s:reset_summary()
endfunction


function! gitgutter#hunk#summary(bufnr) abort
  return gitgutter#utility#getbufvar(a:bufnr, 'summary', [0,0,0])
endfunction

function! s:reset_summary() abort
  call gitgutter#utility#setbufvar(gitgutter#utility#bufnr(), 'summary', [0,0,0])
endfunction

function! gitgutter#hunk#increment_lines_added(count) abort
  let bufnr = gitgutter#utility#bufnr()
  let summary = gitgutter#hunk#summary(bufnr)
  let summary[0] += a:count
  call gitgutter#utility#setbufvar(bufnr, 'summary', summary)
endfunction

function! gitgutter#hunk#increment_lines_modified(count) abort
  let bufnr = gitgutter#utility#bufnr()
  let summary = gitgutter#hunk#summary(bufnr)
  let summary[1] += a:count
  call gitgutter#utility#setbufvar(bufnr, 'summary', summary)
endfunction

function! gitgutter#hunk#increment_lines_removed(count) abort
  let bufnr = gitgutter#utility#bufnr()
  let summary = gitgutter#hunk#summary(bufnr)
  let summary[2] += a:count
  call gitgutter#utility#setbufvar(bufnr, 'summary', summary)
endfunction


function! gitgutter#hunk#next_hunk(count) abort
  if gitgutter#utility#is_active()
    let current_line = line('.')
    let hunk_count = 0
    for hunk in gitgutter#hunk#hunks()
      if hunk[2] > current_line
        let hunk_count += 1
        if hunk_count == a:count
          execute 'normal!' hunk[2] . 'Gzv'
          return
        endif
      endif
    endfor
    call gitgutter#utility#warn('No more hunks')
  endif
endfunction

function! gitgutter#hunk#prev_hunk(count) abort
  if gitgutter#utility#is_active()
    let current_line = line('.')
    let hunk_count = 0
    for hunk in reverse(copy(gitgutter#hunk#hunks()))
      if hunk[2] < current_line
        let hunk_count += 1
        if hunk_count == a:count
          let target = hunk[2] == 0 ? 1 : hunk[2]
          execute 'normal!' target . 'Gzv'
          return
        endif
      endif
    endfor
    call gitgutter#utility#warn('No previous hunks')
  endif
endfunction

" Returns the hunk the cursor is currently in or an empty list if the cursor
" isn't in a hunk.
function! gitgutter#hunk#current_hunk() abort
  let current_hunk = []

  for hunk in gitgutter#hunk#hunks()
    if gitgutter#hunk#cursor_in_hunk(hunk)
      let current_hunk = hunk
      break
    endif
  endfor

  return current_hunk
endfunction

function! gitgutter#hunk#cursor_in_hunk(hunk) abort
  let current_line = line('.')

  if current_line == 1 && a:hunk[2] == 0
    return 1
  endif

  if current_line >= a:hunk[2] && current_line < a:hunk[2] + (a:hunk[3] == 0 ? 1 : a:hunk[3])
    return 1
  endif

  return 0
endfunction

" Returns the number of lines the current hunk is offset from where it would
" be if any changes above it in the file didn't exist.
function! gitgutter#hunk#line_adjustment_for_current_hunk() abort
  let adj = 0
  for hunk in gitgutter#hunk#hunks()
    if gitgutter#hunk#cursor_in_hunk(hunk)
      break
    else
      let adj += hunk[1] - hunk[3]
    endif
  endfor
  return adj
endfunction

function! gitgutter#hunk#text_object(inner) abort
  let hunk = gitgutter#hunk#current_hunk()

  if empty(hunk)
    return
  endif

  let [first_line, last_line] = [hunk[2], hunk[2] + hunk[3] - 1]

  if ! a:inner
    let lnum = last_line
    let eof = line('$')
    while lnum < eof && empty(getline(lnum + 1))
      let lnum +=1
    endwhile
    let last_line = lnum
  endif

  execute 'normal! 'first_line.'GV'.last_line.'G'
endfunction
