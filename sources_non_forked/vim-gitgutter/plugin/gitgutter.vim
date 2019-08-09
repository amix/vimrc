scriptencoding utf-8

if exists('g:loaded_gitgutter') || !has('signs') || &cp
  finish
endif
let g:loaded_gitgutter = 1

" Initialisation {{{

if v:version < 703 || (v:version == 703 && !has("patch105"))
  call gitgutter#utility#warn('requires Vim 7.3.105')
  finish
endif

function! s:set(var, default) abort
  if !exists(a:var)
    if type(a:default)
      execute 'let' a:var '=' string(a:default)
    else
      execute 'let' a:var '=' a:default
    endif
  endif
endfunction

call s:set('g:gitgutter_enabled',                     1)
call s:set('g:gitgutter_max_signs',                 500)
call s:set('g:gitgutter_signs',                       1)
call s:set('g:gitgutter_highlight_lines',             0)
call s:set('g:gitgutter_sign_column_always',          0)
if g:gitgutter_sign_column_always && exists('&signcolumn')
  " Vim 7.4.2201.
  set signcolumn=yes
  let g:gitgutter_sign_column_always = 0
  call gitgutter#utility#warn('please replace "let g:gitgutter_sign_column_always=1" with "set signcolumn=yes"')
endif
call s:set('g:gitgutter_override_sign_column_highlight', 1)
call s:set('g:gitgutter_sign_added',                   '+')
call s:set('g:gitgutter_sign_modified',                '~')
call s:set('g:gitgutter_sign_removed',                 '_')

if gitgutter#utility#supports_overscore_sign()
  call s:set('g:gitgutter_sign_removed_first_line', '‾')
else
  call s:set('g:gitgutter_sign_removed_first_line', '_^')
endif

call s:set('g:gitgutter_sign_removed_above_and_below', '[')
call s:set('g:gitgutter_sign_modified_removed',       '~_')
call s:set('g:gitgutter_git_args',                      '')
call s:set('g:gitgutter_diff_args',                     '')
call s:set('g:gitgutter_diff_base',                     '')
call s:set('g:gitgutter_map_keys',                       1)
call s:set('g:gitgutter_terminal_reports_focus',         1)
call s:set('g:gitgutter_async',                          1)
call s:set('g:gitgutter_log',                            0)

call s:set('g:gitgutter_git_executable', 'git')
if !executable(g:gitgutter_git_executable)
  call gitgutter#utility#warn('cannot find git. Please set g:gitgutter_git_executable.')
endif

let default_grep = 'grep'
call s:set('g:gitgutter_grep', default_grep)
if !empty(g:gitgutter_grep)
  if executable(split(g:gitgutter_grep)[0])
    if $GREP_OPTIONS =~# '--color=always'
      let g:gitgutter_grep .= ' --color=never'
    endif
  else
    if g:gitgutter_grep !=# default_grep
      call gitgutter#utility#warn('cannot find '.g:gitgutter_grep.'. Please check g:gitgutter_grep.')
    endif
    let g:gitgutter_grep = ''
  endif
endif

call gitgutter#highlight#define_sign_column_highlight()
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

" }}}

" Line highlights {{{

command! -bar GitGutterLineHighlightsDisable call gitgutter#highlight#line_disable()
command! -bar GitGutterLineHighlightsEnable  call gitgutter#highlight#line_enable()
command! -bar GitGutterLineHighlightsToggle  call gitgutter#highlight#line_toggle()

" }}}

" Signs {{{

command! -bar GitGutterSignsEnable  call gitgutter#sign#enable()
command! -bar GitGutterSignsDisable call gitgutter#sign#disable()
command! -bar GitGutterSignsToggle  call gitgutter#sign#toggle()

" }}}

" Hunks {{{

command! -bar -count=1 GitGutterNextHunk call gitgutter#hunk#next_hunk(<count>)
command! -bar -count=1 GitGutterPrevHunk call gitgutter#hunk#prev_hunk(<count>)

command! -bar GitGutterStageHunk   call gitgutter#hunk#stage()
command! -bar GitGutterUndoHunk    call gitgutter#hunk#undo()
command! -bar GitGutterPreviewHunk call gitgutter#hunk#preview()

" Hunk text object
onoremap <silent> <Plug>GitGutterTextObjectInnerPending :<C-U>call gitgutter#hunk#text_object(1)<CR>
onoremap <silent> <Plug>GitGutterTextObjectOuterPending :<C-U>call gitgutter#hunk#text_object(0)<CR>
xnoremap <silent> <Plug>GitGutterTextObjectInnerVisual  :<C-U>call gitgutter#hunk#text_object(1)<CR>
xnoremap <silent> <Plug>GitGutterTextObjectOuterVisual  :<C-U>call gitgutter#hunk#text_object(0)<CR>


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

nnoremap <silent> <expr> <Plug>GitGutterNextHunk &diff ? ']c' : ":\<C-U>execute v:count1 . 'GitGutterNextHunk'\<CR>"
nnoremap <silent> <expr> <Plug>GitGutterPrevHunk &diff ? '[c' : ":\<C-U>execute v:count1 . 'GitGutterPrevHunk'\<CR>"

nnoremap <silent> <Plug>GitGutterStageHunk   :GitGutterStageHunk<CR>
nnoremap <silent> <Plug>GitGutterUndoHunk    :GitGutterUndoHunk<CR>
nnoremap <silent> <Plug>GitGutterPreviewHunk :GitGutterPreviewHunk<CR>

" }}}

function! s:on_bufenter()
  if exists('t:gitgutter_didtabenter') && t:gitgutter_didtabenter
    let t:gitgutter_didtabenter = 0
    call gitgutter#all(!g:gitgutter_terminal_reports_focus)
  else
    call gitgutter#init_buffer(bufnr(''))
    call gitgutter#process_buffer(bufnr(''), !g:gitgutter_terminal_reports_focus)
  endif
endfunction

" Autocommands {{{

augroup gitgutter
  autocmd!

  autocmd TabEnter * let t:gitgutter_didtabenter = 1

  autocmd BufEnter * call s:on_bufenter()

  autocmd CursorHold,CursorHoldI * call gitgutter#process_buffer(bufnr(''), 0)
  autocmd FileChangedShellPost   * call gitgutter#process_buffer(bufnr(''), 1)

  " Ensure that all buffers are processed when opening vim with multiple files, e.g.:
  "
  "   vim -o file1 file2
  autocmd VimEnter * if winnr() != winnr('$') | call gitgutter#all(0) | endif

  autocmd ShellCmdPost * call gitgutter#all(1)
  autocmd BufLeave term://* call gitgutter#all(1)

  " Handle all buffers when focus is gained, but only after it was lost.
  " FocusGained gets triggered on startup with Neovim at least already.
  " Therefore this tracks also if it was lost before.
  let s:focus_was_lost = 0
  autocmd FocusGained * if s:focus_was_lost | let focus_was_lost = 0 | call gitgutter#all(1) | endif
  autocmd FocusLost * let s:focus_was_lost = 1

  if exists('##VimResume')
    autocmd VimResume * call gitgutter#all(1)
  endif

  autocmd ColorScheme * call gitgutter#highlight#define_sign_column_highlight() | call gitgutter#highlight#define_highlights()

  " Disable during :vimgrep
  autocmd QuickFixCmdPre  *vimgrep* let g:gitgutter_enabled = 0
  autocmd QuickFixCmdPost *vimgrep* let g:gitgutter_enabled = 1
augroup END

" }}}

" vim:set et sw=2 fdm=marker:
