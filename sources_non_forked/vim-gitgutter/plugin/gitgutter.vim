scriptencoding utf-8

if exists('g:loaded_gitgutter') || !has('signs') || &cp
  finish
endif
let g:loaded_gitgutter = 1

" Initialisation {{{

if v:version < 703 || (v:version == 703 && !has("patch105"))
  call gitgutter#utility#warn('Requires Vim 7.3.105')
  finish
endif

let s:nomodeline = (v:version > 703 || (v:version == 703 && has('patch442'))) ? '<nomodeline>' : ''

function! s:obsolete(var)
  if exists(a:var)
    call gitgutter#utility#warn(a:var.' is obsolete and has no effect.')
  endif
endfunction


let g:gitgutter_preview_win_location = get(g:, 'gitgutter_preview_win_location', 'bo')
if exists('*nvim_open_win')
  let g:gitgutter_preview_win_floating = get(g:, 'gitgutter_preview_win_floating', 1)
else
  let default = exists('&previewpopup') ? !empty(&previewpopup) : 0
  let g:gitgutter_preview_win_floating = get(g:, 'gitgutter_preview_win_floating', default)
endif
let g:gitgutter_enabled = get(g:, 'gitgutter_enabled', 1)
if exists('*sign_unplace')
  let g:gitgutter_max_signs = get(g:, 'gitgutter_max_signs', -1)
else
  let g:gitgutter_max_signs = get(g:, 'gitgutter_max_signs', 500)
endif
let g:gitgutter_signs             = get(g:, 'gitgutter_signs', 1)
let g:gitgutter_highlight_lines   = get(g:, 'gitgutter_highlight_lines', 0)
let g:gitgutter_highlight_linenrs = get(g:, 'gitgutter_highlight_linenrs', 0)
let g:gitgutter_sign_priority     = get(g:, 'gitgutter_sign_priority', 10)
" Nvim 0.4.0 has an expanding sign column
" The sign_place() function supports sign priority.
if (has('nvim-0.4.0') || exists('*sign_place')) && !exists('g:gitgutter_sign_allow_clobber')
  let g:gitgutter_sign_allow_clobber = 1
endif
let g:gitgutter_sign_allow_clobber   = get(g:, 'gitgutter_sign_allow_clobber', 0)
let g:gitgutter_set_sign_backgrounds = get(g:, 'gitgutter_set_sign_backgrounds', 0)
let g:gitgutter_sign_added           = get(g:, 'gitgutter_sign_added', '+')
let g:gitgutter_sign_modified        = get(g:, 'gitgutter_sign_modified', '~')
let g:gitgutter_sign_removed         = get(g:, 'gitgutter_sign_removed', '_')

if gitgutter#utility#supports_overscore_sign()
  let g:gitgutter_sign_removed_first_line = get(g:, 'gitgutter_sign_removed_first_line', '‾')
else
  let g:gitgutter_sign_removed_first_line = get(g:, 'gitgutter_sign_removed_first_line', '_^')
endif

let g:gitgutter_sign_removed_above_and_below = get(g:, 'gitgutter_sign_removed_above_and_below', '_¯')
let g:gitgutter_sign_modified_removed        = get(g:, 'gitgutter_sign_modified_removed', '~_')
let g:gitgutter_git_args                     = get(g:, 'gitgutter_git_args', '')
let g:gitgutter_diff_relative_to             = get(g:, 'gitgutter_diff_relative_to', 'index')
let g:gitgutter_diff_args                    = get(g:, 'gitgutter_diff_args', '')
let g:gitgutter_diff_base                    = get(g:, 'gitgutter_diff_base', '')
let g:gitgutter_map_keys                     = get(g:, 'gitgutter_map_keys', 1)
let g:gitgutter_terminal_reports_focus       = get(g:, 'gitgutter_terminal_reports_focus', 1)
let g:gitgutter_async                        = get(g:, 'gitgutter_async', 1)
let g:gitgutter_log                          = get(g:, 'gitgutter_log', 0)
let g:gitgutter_use_location_list            = get(g:, 'gitgutter_use_location_list', 0)
let g:gitgutter_close_preview_on_escape      = get(g:, 'gitgutter_close_preview_on_escape', 0)
let g:gitgutter_show_msg_on_hunk_jumping     = get(g:, 'gitgutter_show_msg_on_hunk_jumping', 1)

let g:gitgutter_git_executable = get(g:, 'gitgutter_git_executable', 'git')
if !executable(g:gitgutter_git_executable)
  if g:gitgutter_enabled
    call gitgutter#utility#warn('Cannot find git. Please set g:gitgutter_git_executable.')
  endif
  finish
endif

let default_grep = 'grep'
let g:gitgutter_grep = get(g:, 'gitgutter_grep', default_grep)
if !empty(g:gitgutter_grep)
  if executable(split(g:gitgutter_grep)[0])
    if $GREP_OPTIONS =~# '--color=always'
      let g:gitgutter_grep .= ' --color=never'
    endif
  else
    if g:gitgutter_grep !=# default_grep
      call gitgutter#utility#warn('Cannot find '.g:gitgutter_grep.'. Please check g:gitgutter_grep.')
    endif
    let g:gitgutter_grep = ''
  endif
endif

call gitgutter#highlight#define_highlights()
call gitgutter#highlight#define_signs()

" Prevent infinite loop where:
" - executing a job in the foreground launches a new window which takes the focus;
" - when the job finishes, focus returns to gvim;
" - the FocusGained event triggers a new job (see below).
if gitgutter#utility#windows() && !(g:gitgutter_async && gitgutter#async#available())
  set noshelltemp
endif

" }}}

" Primary functions {{{

command! -bar GitGutterAll call gitgutter#all(1)
command! -bar GitGutter    call gitgutter#process_buffer(bufnr(''), 1)

command! -bar GitGutterDisable call gitgutter#disable()
command! -bar GitGutterEnable  call gitgutter#enable()
command! -bar GitGutterToggle  call gitgutter#toggle()

command! -bar GitGutterBufferDisable call gitgutter#buffer_disable()
command! -bar GitGutterBufferEnable  call gitgutter#buffer_enable()
command! -bar GitGutterBufferToggle  call gitgutter#buffer_toggle()

command! -bar GitGutterQuickFix call gitgutter#quickfix(0)
command! -bar GitGutterQuickFixCurrentFile call gitgutter#quickfix(1)

" }}}

" Line highlights {{{

command! -bar GitGutterLineHighlightsDisable call gitgutter#highlight#line_disable()
command! -bar GitGutterLineHighlightsEnable  call gitgutter#highlight#line_enable()
command! -bar GitGutterLineHighlightsToggle  call gitgutter#highlight#line_toggle()

" }}}

" 'number' column highlights {{{
command! -bar GitGutterLineNrHighlightsDisable call gitgutter#highlight#linenr_disable()
command! -bar GitGutterLineNrHighlightsEnable  call gitgutter#highlight#linenr_enable()
command! -bar GitGutterLineNrHighlightsToggle  call gitgutter#highlight#linenr_toggle()
" }}}

" Signs {{{

command! -bar GitGutterSignsEnable  call gitgutter#sign#enable()
command! -bar GitGutterSignsDisable call gitgutter#sign#disable()
command! -bar GitGutterSignsToggle  call gitgutter#sign#toggle()

" }}}

" Hunks {{{

command! -bar -count=1 GitGutterNextHunk call gitgutter#hunk#next_hunk(<count>)
command! -bar -count=1 GitGutterPrevHunk call gitgutter#hunk#prev_hunk(<count>)

command! -bar -range=% GitGutterStageHunk call gitgutter#hunk#stage(<line1>,<line2>)
command! -bar GitGutterUndoHunk    call gitgutter#hunk#undo()
command! -bar GitGutterPreviewHunk call gitgutter#hunk#preview()

" Hunk text object
onoremap <silent> <Plug>(GitGutterTextObjectInnerPending) :<C-U>call gitgutter#hunk#text_object(1)<CR>
onoremap <silent> <Plug>(GitGutterTextObjectOuterPending) :<C-U>call gitgutter#hunk#text_object(0)<CR>
xnoremap <silent> <Plug>(GitGutterTextObjectInnerVisual)  :<C-U>call gitgutter#hunk#text_object(1)<CR>
xnoremap <silent> <Plug>(GitGutterTextObjectOuterVisual)  :<C-U>call gitgutter#hunk#text_object(0)<CR>


" Returns the git-diff hunks for the file or an empty list if there
" aren't any hunks.
"
" The return value is a list of lists.  There is one inner list per hunk.
"
"   [
"     [from_line, from_count, to_line, to_count],
"     [from_line, from_count, to_line, to_count],
"     ...
"   ]
"
" where:
"
" `from`  - refers to the staged file
" `to`    - refers to the working tree's file
" `line`  - refers to the line number where the change starts
" `count` - refers to the number of lines the change covers
function! GitGutterGetHunks()
  let bufnr = bufnr('')
  return gitgutter#utility#is_active(bufnr) ? gitgutter#hunk#hunks(bufnr) : []
endfunction

" Returns an array that contains a summary of the hunk status for the current
" window.  The format is [ added, modified, removed ], where each value
" represents the number of lines added/modified/removed respectively.
function! GitGutterGetHunkSummary()
  return gitgutter#hunk#summary(winbufnr(0))
endfunction

" }}}

" Folds {{{

command! -bar GitGutterFold call gitgutter#fold#toggle()

" }}}

command! -bar GitGutterDebug call gitgutter#debug#debug()

" Maps {{{

nnoremap <silent> <expr> <Plug>(GitGutterNextHunk) &diff ? ']c' : ":\<C-U>execute v:count1 . 'GitGutterNextHunk'\<CR>"
nnoremap <silent> <expr> <Plug>GitGutterNextHunk   &diff ? ']c' : ":\<C-U>call gitgutter#utility#warn('Please change your map \<lt>Plug>GitGutterNextHunk to \<lt>Plug>(GitGutterNextHunk)')\<CR>"
nnoremap <silent> <expr> <Plug>(GitGutterPrevHunk) &diff ? '[c' : ":\<C-U>execute v:count1 . 'GitGutterPrevHunk'\<CR>"
nnoremap <silent> <expr> <Plug>GitGutterPrevHunk   &diff ? '[c' : ":\<C-U>call gitgutter#utility#warn('Please change your map \<lt>Plug>GitGutterPrevHunk to \<lt>Plug>(GitGutterPrevHunk)')\<CR>"

xnoremap <silent> <Plug>(GitGutterStageHunk)   :GitGutterStageHunk<CR>
xnoremap <silent> <Plug>GitGutterStageHunk     :call gitgutter#utility#warn('Please change your map <lt>Plug>GitGutterStageHunk to <lt>Plug>(GitGutterStageHunk)')<CR>
nnoremap <silent> <Plug>(GitGutterStageHunk)   :GitGutterStageHunk<CR>
nnoremap <silent> <Plug>GitGutterStageHunk     :call gitgutter#utility#warn('Please change your map <lt>Plug>GitGutterStageHunk to <lt>Plug>(GitGutterStageHunk)')<CR>
nnoremap <silent> <Plug>(GitGutterUndoHunk)    :GitGutterUndoHunk<CR>
nnoremap <silent> <Plug>GitGutterUndoHunk      :call gitgutter#utility#warn('Please change your map <lt>Plug>GitGutterUndoHunk to <lt>Plug>(GitGutterUndoHunk)')<CR>
nnoremap <silent> <Plug>(GitGutterPreviewHunk) :GitGutterPreviewHunk<CR>
nnoremap <silent> <Plug>GitGutterPreviewHunk   :call gitgutter#utility#warn('Please change your map <lt>Plug>GitGutterPreviewHunk to <lt>Plug>(GitGutterPreviewHunk)')<CR>

" }}}

function! s:on_bufenter()
  call gitgutter#setup_maps()

  " To keep vim's start-up fast, do not process the buffer when vim is starting.
  " Instead process it a short time later.  Normally we would rely on our
  " CursorHold autocommand to handle this but it turns out CursorHold is not
  " guaranteed to fire if the user has not typed anything yet; so set up a
  " timer instead.  The disadvantage is that if CursorHold does fire, the
  " plugin will do a round of unnecessary work; but since there will not have
  " been any changes to the buffer since the first round, the second round
  " will be cheap.
  if has('vim_starting') && !$VIM_GITGUTTER_TEST
    if exists('*timer_start')
      call timer_start(&updatetime, 'GitGutterCursorHold')
    endif
    return
  endif

  if exists('t:gitgutter_didtabenter') && t:gitgutter_didtabenter
    let t:gitgutter_didtabenter = 0
    call gitgutter#all(!g:gitgutter_terminal_reports_focus)
  else
    call gitgutter#process_buffer(bufnr(''), !g:gitgutter_terminal_reports_focus)
  endif
endfunction

function! GitGutterCursorHold(timer)
  execute 'doautocmd' s:nomodeline 'gitgutter CursorHold'
endfunction

" Autocommands {{{

augroup gitgutter
  autocmd!

  autocmd TabEnter * let t:gitgutter_didtabenter = 1

  autocmd BufEnter * call s:on_bufenter()

  " Ensure Vim is always checking for CursorMoved to avoid CursorMoved
  " being fired at the wrong time in floating preview window on Neovim.
  " See vim/vim#2053.
  autocmd CursorMoved * execute ''

  autocmd CursorHold,CursorHoldI * call gitgutter#process_buffer(bufnr(''), 0)
  if exists('*timer_start') && has('lambda')
    autocmd FileChangedShellPost * call timer_start(1, {-> gitgutter#process_buffer(bufnr(''), 1)})
  else
    autocmd FileChangedShellPost * call gitgutter#process_buffer(bufnr(''), 1)
  endif

  " Ensure that all buffers are processed when opening vim with multiple files, e.g.:
  "
  "   vim -o file1 file2
  autocmd VimEnter * if winnr() != winnr('$') | call gitgutter#all(0) | endif

  autocmd ShellCmdPost * call gitgutter#all(1)
  autocmd BufLeave term://* call gitgutter#all(1)

  autocmd User FugitiveChanged call gitgutter#all(1)

  autocmd BufFilePre  * GitGutterBufferDisable
  autocmd BufFilePost * GitGutterBufferEnable

  " Handle all buffers when focus is gained, but only after it was lost.
  " FocusGained gets triggered on startup with Neovim at least already.
  " Therefore this tracks also if it was lost before.
  let s:focus_was_lost = 0
  autocmd FocusGained * if s:focus_was_lost | let s:focus_was_lost = 0 | call gitgutter#all(1) | endif
  autocmd FocusLost * let s:focus_was_lost = 1

  if exists('##VimResume')
    autocmd VimResume * call gitgutter#all(1)
  endif

  autocmd ColorScheme * call gitgutter#highlight#define_highlights()

  " Disable during :vimgrep
  autocmd QuickFixCmdPre  *vimgrep* let [g:gitgutter_was_enabled, g:gitgutter_enabled] = [g:gitgutter_enabled, 0]
  autocmd QuickFixCmdPost *vimgrep* let g:gitgutter_enabled = g:gitgutter_was_enabled | unlet g:gitgutter_was_enabled
augroup END

" }}}

" vim:set et sw=2 fdm=marker:
