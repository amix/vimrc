" Author: w0rp <devw0rp@gmail.com>
" Description: Main entry point for the plugin: sets up prefs and autocommands
"   Preferences can be set in vimrc files and so on to configure ale

" Sanity Checks

if exists('g:loaded_ale_dont_use_this_in_other_plugins_please')
    finish
endif

" Set a special flag used only by this plugin for preventing doubly
" loading the script.
let g:loaded_ale_dont_use_this_in_other_plugins_please = 1

" A flag for detecting if the required features are set.
if has('nvim')
    let s:has_features = has('timers') && has('nvim-0.2.0')
else
    " Check if Job and Channel functions are available, instead of the
    " features. This works better on old MacVim versions.
    let s:has_features = has('timers') && exists('*job_start') && exists('*ch_close_in')
endif

if !s:has_features
    " Only output a warning if editing some special files.
    if index(['', 'gitcommit'], &filetype) == -1
        execute 'echoerr ''ALE requires NeoVim >= 0.2.0 or Vim 8 with +timers +job +channel'''
        execute 'echoerr ''Please update your editor appropriately.'''
    endif

    " Stop here, as it won't work.
    finish
endif

" Set this flag so that other plugins can use it, like airline.
let g:loaded_ale = 1

" This global variable is used internally by ALE for tracking information for
" each buffer which linters are being run against.
let g:ale_buffer_info = {}
" This global Dictionary tracks data for fixing code. Don't mess with it.
let g:ale_fix_buffer_data = {}

" User Configuration

" This option prevents ALE autocmd commands from being run for particular
" filetypes which can cause issues.
let g:ale_filetype_blacklist = [
\   'dirvish',
\   'nerdtree',
\   'qf',
\   'tags',
\   'unite',
\]

" This Dictionary configures which linters are enabled for which languages.
let g:ale_linters = get(g:, 'ale_linters', {})
" This option can be changed to only enable explicitly selected linters.
let g:ale_linters_explicit = get(g:, 'ale_linters_explicit', 0)

" This Dictionary configures which functions will be used for fixing problems.
let g:ale_fixers = get(g:, 'ale_fixers', {})

" This Dictionary allows users to set up filetype aliases for new filetypes.
let g:ale_linter_aliases = get(g:, 'ale_linter_aliases', {})

" This flag can be set with a number of milliseconds for delaying the
" execution of a linter when text is changed. The timeout will be set and
" cleared each time text is changed, so repeated edits won't trigger the
" jobs for linting until enough time has passed after editing is done.
let g:ale_lint_delay = get(g:, 'ale_lint_delay', 200)

" This flag can be set to 'never' to disable linting when text is changed.
" This flag can also be set to 'insert' or 'normal' to lint when text is
" changed only in insert or normal mode respectively.
let g:ale_lint_on_text_changed = get(g:, 'ale_lint_on_text_changed', 'always')

" This flag can be set to 1 to enable linting when leaving insert mode.
let g:ale_lint_on_insert_leave = get(g:, 'ale_lint_on_insert_leave', 0)

" This flag can be set to 0 to disable linting when the buffer is entered.
let g:ale_lint_on_enter = get(g:, 'ale_lint_on_enter', 1)

" This flag can be set to 1 to enable linting when a buffer is written.
let g:ale_lint_on_save = get(g:, 'ale_lint_on_save', 1)

" This flag can be set to 1 to enable linting when the filetype is changed.
let g:ale_lint_on_filetype_changed = get(g:, 'ale_lint_on_filetype_changed', 1)

" This flag can be set to 1 to enable automatically fixing files on save.
let g:ale_fix_on_save = get(g:, 'ale_fix_on_save', 0)

" This flag may be set to 0 to disable ale. After ale is loaded, :ALEToggle
" should be used instead.
let g:ale_enabled = get(g:, 'ale_enabled', 1)

" These flags dictates if ale uses the quickfix or the loclist (loclist is the
" default, quickfix overrides loclist).
let g:ale_set_loclist = get(g:, 'ale_set_loclist', 1)
let g:ale_set_quickfix = get(g:, 'ale_set_quickfix', 0)

" This flag can be set to 0 to disable setting signs.
" This is enabled by default only if the 'signs' feature exists.
let g:ale_set_signs = get(g:, 'ale_set_signs', has('signs'))

" This flag can be set to 0 to disable setting error highlights.
let g:ale_set_highlights = get(g:, 'ale_set_highlights', has('syntax'))

" This flag can be set to 0 to disable echoing when the cursor moves.
let g:ale_echo_cursor = get(g:, 'ale_echo_cursor', 1)

" This flag can be set to 1 to automatically show errors in the preview window.
let g:ale_cursor_detail = get(g:, 'ale_cursor_detail', 0)

" This flag can be set to 1 to enable virtual text when the cursor moves.
let g:ale_virtualtext_cursor = get(g:, 'ale_virtualtext_cursor', 0)

" This flag can be set to 1 to automatically close the preview window upon
" entering Insert Mode.
let g:ale_close_preview_on_insert = get(g:, 'ale_close_preview_on_insert', 0)

" This flag can be set to 0 to disable balloon support.
let g:ale_set_balloons = get(g:, 'ale_set_balloons', has('balloon_eval') && has('gui_running'))

" This flag can be set to 0 to disable warnings for trailing whitespace
let g:ale_warn_about_trailing_whitespace = get(g:, 'ale_warn_about_trailing_whitespace', 1)
" This flag can be set to 0 to disable warnings for trailing blank lines
let g:ale_warn_about_trailing_blank_lines = get(g:, 'ale_warn_about_trailing_blank_lines', 1)

" A flag for enabling or disabling the command history.
let g:ale_history_enabled = get(g:, 'ale_history_enabled', 1)

" A flag for storing the full output of commands in the history.
let g:ale_history_log_output = get(g:, 'ale_history_log_output', 1)

" Enable automatic completion with LSP servers and tsserver
let g:ale_completion_enabled = get(g:, 'ale_completion_enabled', 0)

" Enable automatic detection of pipenv for Python linters.
let g:ale_python_auto_pipenv = get(g:, 'ale_python_auto_pipenv', 0)

if g:ale_set_balloons
    call ale#balloon#Enable()
endif

if g:ale_completion_enabled
    call ale#completion#Enable()
endif

" Define commands for moving through warnings and errors.
command! -bar ALEPrevious :call ale#loclist_jumping#Jump('before', 0)
command! -bar ALEPreviousWrap :call ale#loclist_jumping#Jump('before', 1)
command! -bar ALENext :call ale#loclist_jumping#Jump('after', 0)
command! -bar ALENextWrap :call ale#loclist_jumping#Jump('after', 1)
command! -bar ALEFirst :call ale#loclist_jumping#JumpToIndex(0)
command! -bar ALELast :call ale#loclist_jumping#JumpToIndex(-1)

" A command for showing error details.
command! -bar ALEDetail :call ale#cursor#ShowCursorDetail()

" Define commands for turning ALE on or off.
command! -bar ALEToggle :call ale#toggle#Toggle()
command! -bar ALEEnable :call ale#toggle#Enable()
command! -bar ALEDisable :call ale#toggle#Disable()
command! -bar ALEReset :call ale#toggle#Reset()
" Commands for turning ALE on or off for a buffer.
command! -bar ALEToggleBuffer :call ale#toggle#ToggleBuffer(bufnr(''))
command! -bar ALEEnableBuffer :call ale#toggle#EnableBuffer(bufnr(''))
command! -bar ALEDisableBuffer :call ale#toggle#DisableBuffer(bufnr(''))
command! -bar ALEResetBuffer :call ale#toggle#ResetBuffer(bufnr(''))
" A command to stop all LSP-like clients, including tsserver.
command! -bar ALEStopAllLSPs :call ale#lsp#reset#StopAllLSPs()

" A command for linting manually.
command! -bar ALELint :call ale#Queue(0, 'lint_file')

" Define a command to get information about current filetype.
command! -bar ALEInfo :call ale#debugging#Info()
" The same, but copy output to your clipboard.
command! -bar ALEInfoToClipboard :call ale#debugging#InfoToClipboard()
" Copy ALE information to a file.
command! -bar -nargs=1 ALEInfoToFile :call ale#debugging#InfoToFile(<f-args>)

" Fix problems in files.
command! -bar -nargs=* -complete=customlist,ale#fix#registry#CompleteFixers ALEFix :call ale#fix#Fix(bufnr(''), '', <f-args>)
" Suggest registered functions to use for fixing problems.
command! -bar ALEFixSuggest :call ale#fix#registry#Suggest(&filetype)

" Go to definition for tsserver and LSP
command! -bar ALEGoToDefinition :call ale#definition#GoTo({})
command! -bar ALEGoToDefinitionInTab :call ale#definition#GoTo({'open_in_tab': 1})

" Find references for tsserver and LSP
command! -bar ALEFindReferences :call ale#references#Find()

" Show summary information for the cursor.
command! -bar ALEHover :call ale#hover#ShowAtCursor()

" Show documentation for the cursor.
command! -bar ALEDocumentation :call ale#hover#ShowDocumentationAtCursor()

" Search for appearances of a symbol, such as a type name or function name.
command! -nargs=1 ALESymbolSearch :call ale#symbol#Search(<q-args>)

" <Plug> mappings for commands
nnoremap <silent> <Plug>(ale_previous) :ALEPrevious<Return>
nnoremap <silent> <Plug>(ale_previous_wrap) :ALEPreviousWrap<Return>
nnoremap <silent> <Plug>(ale_next) :ALENext<Return>
nnoremap <silent> <Plug>(ale_next_wrap) :ALENextWrap<Return>
nnoremap <silent> <Plug>(ale_first) :ALEFirst<Return>
nnoremap <silent> <Plug>(ale_last) :ALELast<Return>
nnoremap <silent> <Plug>(ale_toggle) :ALEToggle<Return>
nnoremap <silent> <Plug>(ale_enable) :ALEEnable<Return>
nnoremap <silent> <Plug>(ale_disable) :ALEDisable<Return>
nnoremap <silent> <Plug>(ale_reset) :ALEReset<Return>
nnoremap <silent> <Plug>(ale_toggle_buffer) :ALEToggleBuffer<Return>
nnoremap <silent> <Plug>(ale_enable_buffer) :ALEEnableBuffer<Return>
nnoremap <silent> <Plug>(ale_disable_buffer) :ALEDisableBuffer<Return>
nnoremap <silent> <Plug>(ale_reset_buffer) :ALEResetBuffer<Return>
nnoremap <silent> <Plug>(ale_lint) :ALELint<Return>
nnoremap <silent> <Plug>(ale_detail) :ALEDetail<Return>
nnoremap <silent> <Plug>(ale_fix) :ALEFix<Return>
nnoremap <silent> <Plug>(ale_go_to_definition) :ALEGoToDefinition<Return>
nnoremap <silent> <Plug>(ale_go_to_definition_in_tab) :ALEGoToDefinitionInTab<Return>
nnoremap <silent> <Plug>(ale_find_references) :ALEFindReferences<Return>
nnoremap <silent> <Plug>(ale_hover) :ALEHover<Return>
nnoremap <silent> <Plug>(ale_documentation) :ALEDocumentation<Return>

" Set up autocmd groups now.
call ale#events#Init()

" Housekeeping

augroup ALECleanupGroup
    autocmd!
    " Clean up buffers automatically when they are unloaded.
    autocmd BufDelete * if exists('*ale#engine#Cleanup') | call ale#engine#Cleanup(str2nr(expand('<abuf>'))) | endif
    autocmd QuitPre * call ale#events#QuitEvent(str2nr(expand('<abuf>')))

    if exists('##VimSuspend')
      autocmd VimSuspend * if exists('*ale#engine#CleanupEveryBuffer') | call ale#engine#CleanupEveryBuffer() | endif
    endif
augroup END
