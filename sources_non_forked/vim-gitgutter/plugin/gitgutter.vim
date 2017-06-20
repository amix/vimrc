scriptencoding utf-8

if exists('g:loaded_gitgutter') || !has('signs') || &cp
  finish
endif
let g:loaded_gitgutter = 1

" Initialisation {{{

" Realtime sign updates require Vim 7.3.105+.
if v:version < 703 || (v:version == 703 && !has("patch105"))
  let g:gitgutter_realtime = 0
endif

" Eager updates require gettabvar()/settabvar().
if !exists("*gettabvar")
  let g:gitgutter_eager = 0
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
call s:set('g:gitgutter_override_sign_column_highlight', 1)
call s:set('g:gitgutter_realtime',                    1)
call s:set('g:gitgutter_eager',                       1)
call s:set('g:gitgutter_sign_added',                '+')
call s:set('g:gitgutter_sign_modified',             '~')
call s:set('g:gitgutter_sign_removed',              '_')
try
  call s:set('g:gitgutter_sign_removed_first_line', '‾')
catch /E239/
  let g:gitgutter_sign_removed_first_line = '_^'
endtry

call s:set('g:gitgutter_sign_modified_removed',    '~_')
call s:set('g:gitgutter_diff_args',                  '')
call s:set('g:gitgutter_diff_base',                  '')
call s:set('g:gitgutter_map_keys',                    1)
call s:set('g:gitgutter_avoid_cmd_prompt_on_windows', 1)
call s:set('g:gitgutter_async',                       1)
call s:set('g:gitgutter_log',                         0)
call s:set('g:gitgutter_git_executable',          'git')

if !executable(g:gitgutter_git_executable)
  call gitgutter#utility#warn('cannot find git. Please set g:gitgutter_git_executable.')
endif

call gitgutter#highlight#define_sign_column_highlight()
call gitgutter#highlight#define_highlights()
call gitgutter#highlight#define_signs()

" }}}

" Primary functions {{{

command -bar GitGutterAll call gitgutter#all()
command -bar GitGutter    call gitgutter#process_buffer(bufnr(''), 0)

command -bar GitGutterDisable call gitgutter#disable()
command -bar GitGutterEnable  call gitgutter#enable()
command -bar GitGutterToggle  call gitgutter#toggle()

" }}}

" Line highlights {{{

command -bar GitGutterLineHighlightsDisable call gitgutter#line_highlights_disable()
command -bar GitGutterLineHighlightsEnable  call gitgutter#line_highlights_enable()
command -bar GitGutterLineHighlightsToggle  call gitgutter#line_highlights_toggle()

" }}}

" Signs {{{

command -bar GitGutterSignsEnable  call gitgutter#signs_enable()
command -bar GitGutterSignsDisable call gitgutter#signs_disable()
command -bar GitGutterSignsToggle  call gitgutter#signs_toggle()

" }}}

" Hunks {{{

command -bar -count=1 GitGutterNextHunk call gitgutter#hunk#next_hunk(<count>)
command -bar -count=1 GitGutterPrevHunk call gitgutter#hunk#prev_hunk(<count>)

command -bar GitGutterStageHunk   call gitgutter#stage_hunk()
command -bar GitGutterUndoHunk    call gitgutter#undo_hunk()
command -bar GitGutterRevertHunk  echomsg 'GitGutterRevertHunk is deprecated. Use GitGutterUndoHunk'<Bar>call gitgutter#undo_hunk()
command -bar GitGutterPreviewHunk call gitgutter#preview_hunk()

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
  return gitgutter#utility#is_active() ? gitgutter#hunk#hunks() : []
endfunction

" Returns an array that contains a summary of the hunk status for the current
" window.  The format is [ added, modified, removed ], where each value
" represents the number of lines added/modified/removed respectively.
function! GitGutterGetHunkSummary()
  return gitgutter#hunk#summary(winbufnr(0))
endfunction

" }}}

command -bar GitGutterDebug call gitgutter#debug#debug()

" Maps {{{

nnoremap <silent> <expr> <Plug>GitGutterNextHunk &diff ? ']c' : ":\<C-U>execute v:count1 . 'GitGutterNextHunk'\<CR>"
nnoremap <silent> <expr> <Plug>GitGutterPrevHunk &diff ? '[c' : ":\<C-U>execute v:count1 . 'GitGutterPrevHunk'\<CR>"

if g:gitgutter_map_keys
  if !hasmapto('<Plug>GitGutterPrevHunk') && maparg('[c', 'n') ==# ''
    nmap [c <Plug>GitGutterPrevHunk
  endif
  if !hasmapto('<Plug>GitGutterNextHunk') && maparg(']c', 'n') ==# ''
    nmap ]c <Plug>GitGutterNextHunk
  endif
endif


nnoremap <silent> <Plug>GitGutterStageHunk   :GitGutterStageHunk<CR>
nnoremap <silent> <Plug>GitGutterUndoHunk    :GitGutterUndoHunk<CR>
nnoremap <silent> <Plug>GitGutterPreviewHunk :GitGutterPreviewHunk<CR>

if g:gitgutter_map_keys
  if !hasmapto('<Plug>GitGutterStageHunk') && maparg('<Leader>hs', 'n') ==# ''
    nmap <Leader>hs <Plug>GitGutterStageHunk
  endif
  if !hasmapto('<Plug>GitGutterUndoHunk') && maparg('<Leader>hu', 'n') ==# ''
    nmap <Leader>hu <Plug>GitGutterUndoHunk
    nmap <Leader>hr <Plug>GitGutterUndoHunk:echomsg '<Leader>hr is deprecated. Use <Leader>hu'<CR>
  endif
  if !hasmapto('<Plug>GitGutterPreviewHunk') && maparg('<Leader>hp', 'n') ==# ''
    nmap <Leader>hp <Plug>GitGutterPreviewHunk
  endif

  if !hasmapto('<Plug>GitGutterTextObjectInnerPending') && maparg('ic', 'o') ==# ''
    omap ic <Plug>GitGutterTextObjectInnerPending
  endif
  if !hasmapto('<Plug>GitGutterTextObjectOuterPending') && maparg('ac', 'o') ==# ''
    omap ac <Plug>GitGutterTextObjectOuterPending
  endif
  if !hasmapto('<Plug>GitGutterTextObjectInnerVisual') && maparg('ic', 'x') ==# ''
    xmap ic <Plug>GitGutterTextObjectInnerVisual
  endif
  if !hasmapto('<Plug>GitGutterTextObjectOuterVisual') && maparg('ac', 'x') ==# ''
    xmap ac <Plug>GitGutterTextObjectOuterVisual
  endif
endif

" }}}

" Autocommands {{{

augroup gitgutter
  autocmd!

  if g:gitgutter_realtime
    autocmd CursorHold,CursorHoldI * call gitgutter#process_buffer(bufnr(''), 1)
  endif

  if g:gitgutter_eager
    autocmd BufWritePost,FileChangedShellPost,ShellCmdPost * call gitgutter#process_buffer(bufnr(''), 0)

    autocmd BufEnter *
          \  if gettabvar(tabpagenr(), 'gitgutter_didtabenter') |
          \   call settabvar(tabpagenr(), 'gitgutter_didtabenter', 0) |
          \   call gitgutter#all() |
          \ else |
          \   call gitgutter#process_buffer(bufnr(''), 0) |
          \ endif

    autocmd TabEnter * call settabvar(tabpagenr(), 'gitgutter_didtabenter', 1)

    if !has('gui_win32')
      autocmd FocusGained * call gitgutter#all()
    endif

  else
    autocmd BufRead,BufWritePost,FileChangedShellPost * call gitgutter#process_buffer(bufnr(''), 0)
  endif

  autocmd ColorScheme * call gitgutter#highlight#define_sign_column_highlight() | call gitgutter#highlight#define_highlights()

  " Disable during :vimgrep
  autocmd QuickFixCmdPre  *vimgrep* let g:gitgutter_enabled = 0
  autocmd QuickFixCmdPost *vimgrep* let g:gitgutter_enabled = 1
augroup END

" }}}

" vim:set et sw=2 fdm=marker:
