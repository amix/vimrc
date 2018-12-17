" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" Statusline
""""""""""""""""""""""""""""""""

" s:statuses is a global reference to all statuses. It stores the statuses per
" import paths (map[string]status), where each status is unique per its
" type. Current status dict is in form:
" {
"   'desc'        : 'Job description',
"   'state'       : 'Job state, such as success, failure, etc..',
"   'type'        : 'Job type, such as build, test, etc..'
"   'created_at'  : 'Time it was created as seconds since 1st Jan 1970'
" }
let s:statuses = {}

" timer_id for cleaner
let s:timer_id = 0

" last_status stores the last generated text per status
let s:last_status = ""

" Show returns the current status of the job for 20 seconds (configurable). It
" displays it in form of 'desc: [type|state]' if there is any state available,
" if not it returns an empty string. This function should be plugged directly
" into the statusline.
function! go#statusline#Show() abort
  " lazy initialiation of the cleaner
  if !s:timer_id
    " clean every 60 seconds all statuses
    let interval = go#config#StatuslineDuration()
    let s:timer_id = timer_start(interval, function('go#statusline#Clear'), {'repeat': -1})
  endif

  " nothing to show
  if empty(s:statuses)
    return ''
  endif

  let status_dir =  expand('%:p:h')

  if !has_key(s:statuses, status_dir)
    return ''
  endif

  let status = s:statuses[status_dir]
  if !has_key(status, 'desc') || !has_key(status, 'state') || !has_key(status, 'type')
    return ''
  endif

  let status_text = printf("[%s|%s]", status.type, status.state)
  if empty(status_text)
    return ''
  endif

  " only update highlight if status has changed.
  if status_text != s:last_status
    if status.state =~ "success" || status.state =~ "finished" || status.state =~ "pass"
      hi goStatusLineColor cterm=bold ctermbg=76 ctermfg=22 guibg=#5fd700 guifg=#005f00
    elseif status.state =~ "started" || status.state =~ "analysing" || status.state =~ "compiling"
      hi goStatusLineColor cterm=bold ctermbg=208 ctermfg=88 guibg=#ff8700 guifg=#870000
    elseif status.state =~ "failed"
      hi goStatusLineColor cterm=bold ctermbg=196 ctermfg=52 guibg=#ff0000 guifg=#5f0000
    endif
  endif

  let s:last_status = status_text
  return status_text
endfunction

" Update updates (adds) the statusline for the given status_dir with the
" given status dict. It overrides any previously set status.
function! go#statusline#Update(status_dir, status) abort
  let a:status.created_at = reltime()
  let s:statuses[a:status_dir] = a:status

  " force to update the statusline, otherwise the user needs to move the
  " cursor
  exe 'let &ro = &ro'

  " before we stop the timer, check if we have any previous jobs to be cleaned
  " up. Otherwise every job will reset the timer when this function is called
  " and thus old jobs will never be cleaned
  call go#statusline#Clear(0)

  " also reset the timer, so the user has time to see it in the statusline.
  " Setting the timer_id to 0 will trigger a new cleaner routine.
  call timer_stop(s:timer_id)
  let s:timer_id = 0
endfunction

" Clear clears all currently stored statusline data. The timer_id argument is
" just a placeholder so we can pass it to a timer_start() function if needed.
function! go#statusline#Clear(timer_id) abort
  for [status_dir, status] in items(s:statuses)
    let elapsed_time = reltimestr(reltime(status.created_at))
    " strip whitespace
    let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')

    if str2nr(elapsed_time) > 10
      call remove(s:statuses, status_dir)
    endif
  endfor

  if len(s:statuses) == 0
    let s:statuses = {}
  endif

  " force to update the statusline, otherwise the user needs to move the
  " cursor
  exe 'let &ro = &ro'
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
