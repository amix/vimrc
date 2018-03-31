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
    let s:has_features = has('timers')
else
    " Check if Job and Channel functions are available, instead of the
    " features. This works better on old MacVim versions.
    let s:has_features = has('timers') && exists('*job_start') && exists('*ch_close_in')
endif

if !s:has_features
    " Only output a warning if editing some special files.
    if index(['', 'gitcommit'], &filetype) == -1
        execute 'echoerr ''ALE requires NeoVim >= 0.1.5 or Vim 8 with +timers +job +channel'''
        execute 'echoerr ''Please update your editor appropriately.'''
    endif

    " Stop here, as it won't work.
    finish
endif

if has('nvim') && !has('nvim-0.2.0') && !get(g:, 'ale_use_deprecated_neovim')
    execute 'echom ''ALE support for NeoVim versions below 0.2.0 is deprecated.'''
    execute 'echom ''Use `let g:ale_use_deprecated_neovim = 1` to silence this warning for now.'''
endif

" This flag can be set to 0 to disable emitting conflict warnings.
let g:ale_emit_conflict_warnings = get(g:, 'ale_emit_conflict_warnings', 1)

if g:ale_emit_conflict_warnings
\&& match(&runtimepath, '[/\\]ale[/\\]after') < 0
    " Add the after directory to the runtimepath
    " This is only done if the after directory isn't already in runtimepath
    let &runtimepath .= ',' . expand('<sfile>:p:h:h') . '/after'
endif

" Set this flag so that other plugins can use it, like airline.
let g:loaded_ale = 1

" Set the TMPDIR environment variable if it is not set automatically.
" This can automatically fix some environments.
if has('unix') && empty($TMPDIR)
    let $TMPDIR = '/tmp'
endif

" This global variable is used internally by ALE for tracking information for
" each buffer which linters are being run against.
let g:ale_buffer_info = {}

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
call ale#Set('linters', {})
" This option can be changed to only enable explicitly selected linters.
call ale#Set('linters_explicit', 0)

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

call ale#Set('fix_on_save', 0)

" This flag may be set to 0 to disable ale. After ale is loaded, :ALEToggle
" should be used instead.
let g:ale_enabled = get(g:, 'ale_enabled', 1)

" These flags dictates if ale uses the quickfix or the loclist (loclist is the
" default, quickfix overrides loclist).
let g:ale_set_loclist = get(g:, 'ale_set_loclist', 1)
let g:ale_set_quickfix = get(g:, 'ale_set_quickfix', 0)

" This flag dictates if ale open the configured loclist
let g:ale_open_list = get(g:, 'ale_open_list', 0)

" This flag dictates if ale keeps open loclist even if there is no error in loclist
let g:ale_keep_list_window_open = get(g:, 'ale_keep_list_window_open', 0)

" This flag dictates that quickfix windows should be opened vertically
let g:ale_list_vertical = get(g:, 'ale_list_vertical', 0)

" The window size to set for the quickfix and loclist windows
call ale#Set('list_window_size', 10)

" This flag can be set to 0 to disable setting signs.
" This is enabled by default only if the 'signs' feature exists.
let g:ale_set_signs = get(g:, 'ale_set_signs', has('signs'))
" This flag can be set to some integer to control the maximum number of signs
" that ALE will set.
let g:ale_max_signs = get(g:, 'ale_max_signs', -1)

" This flag can be set to 1 to enable changing the sign column colors when
" there are errors.
call ale#Set('change_sign_column_color', 0)

" This flag can be set to 0 to disable setting error highlights.
let g:ale_set_highlights = get(g:, 'ale_set_highlights', has('syntax'))

" These variables dictate what sign is used to indicate errors and warnings.
call ale#Set('sign_error', '>>')
call ale#Set('sign_style_error', g:ale_sign_error)
call ale#Set('sign_warning', '--')
call ale#Set('sign_style_warning', g:ale_sign_warning)
call ale#Set('sign_info', g:ale_sign_warning)

" This variable sets an offset which can be set for sign IDs.
" This ID can be changed depending on what IDs are set for other plugins.
" The dummy sign will use the ID exactly equal to the offset.
let g:ale_sign_offset = get(g:, 'ale_sign_offset', 1000000)

" This flag can be set to 1 to keep sign gutter always open
let g:ale_sign_column_always = get(g:, 'ale_sign_column_always', 0)

" A string format for the echoed message
call ale#Set('echo_msg_format', '%code: %%s')
" The same for the loclist.
call ale#Set('loclist_msg_format', g:ale_echo_msg_format)

" Strings used for severity in the echoed message
let g:ale_echo_msg_error_str = get(g:, 'ale_echo_msg_error_str', 'Error')
let g:ale_echo_msg_info_str = get(g:, 'ale_echo_msg_info_str', 'Info')
let g:ale_echo_msg_warning_str = get(g:, 'ale_echo_msg_warning_str', 'Warning')

" This flag can be set to 0 to disable echoing when the cursor moves.
let g:ale_echo_cursor = get(g:, 'ale_echo_cursor', 1)
" Controls the milliseconds delay before echoing a message.
let g:ale_echo_delay = get(g:, 'ale_echo_delay', 10)

" This flag can be set to 0 to disable balloon support.
call ale#Set('set_balloons', has('balloon_eval'))

" A deprecated setting for ale#statusline#Status()
" See :help ale#statusline#Count() for getting status reports.
let g:ale_statusline_format = get(g:, 'ale_statusline_format',
\   ['%d error(s)', '%d warning(s)', 'OK']
\)

" This flag can be set to 0 to disable warnings for trailing whitespace
call ale#Set('warn_about_trailing_whitespace', 1)
" This flag can be set to 0 to disable warnings for trailing blank lines
call ale#Set('warn_about_trailing_blank_lines', 1)

" A flag for controlling the maximum size of the command history to store.
let g:ale_max_buffer_history_size = get(g:, 'ale_max_buffer_history_size', 20)

" A flag for enabling or disabling the command history.
let g:ale_history_enabled = get(g:, 'ale_history_enabled', 1)

" A flag for storing the full output of commands in the history.
let g:ale_history_log_output = get(g:, 'ale_history_log_output', 1)

" A flag for caching failed executable checks.
" This is off by default, because it will cause problems.
call ale#Set('cache_executable_check_failures', 0)

" A dictionary mapping regular expression patterns to arbitrary buffer
" variables to be set. Useful for configuration ALE based on filename
" patterns.
call ale#Set('pattern_options', {})
call ale#Set('pattern_options_enabled', !empty(g:ale_pattern_options))

" A maximum file size for checking for errors.
call ale#Set('maximum_file_size', 0)

" Remapping of linter problems.
call ale#Set('type_map', {})

" Enable automatic completion with LSP servers and tsserver
call ale#Set('completion_enabled', 0)
call ale#Set('completion_delay', 100)
call ale#Set('completion_max_suggestions', 50)

" A setting for wrapping commands.
call ale#Set('command_wrapper', '')

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

" Fix problems in files.
command! -bar ALEFix :call ale#fix#Fix()
" Suggest registered functions to use for fixing problems.
command! -bar ALEFixSuggest :call ale#fix#registry#Suggest(&filetype)

" Go to definition for tsserver and LSP
command! -bar ALEGoToDefinition :call ale#definition#GoTo({})
command! -bar ALEGoToDefinitionInTab :call ale#definition#GoTo({'open_in_tab': 1})

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

" Set up autocmd groups now.
call ale#toggle#InitAuGroups()

" Housekeeping

augroup ALECleanupGroup
    autocmd!
    " Clean up buffers automatically when they are unloaded.
    autocmd BufDelete * call ale#engine#Cleanup(str2nr(expand('<abuf>')))
    autocmd QuitPre * call ale#events#QuitEvent(str2nr(expand('<abuf>')))
augroup END

" Backwards Compatibility

function! ALELint(delay) abort
    if !get(g:, 'ale_deprecation_ale_lint', 0)
        execute 'echom ''ALELint() is deprecated, use ale#Queue() instead.'''
        let g:ale_deprecation_ale_lint = 1
    endif

    call ale#Queue(a:delay)
endfunction

function! ALEGetStatusLine() abort
    if !get(g:, 'ale_deprecation_ale_get_status_line', 0)
        execute 'echom ''ALEGetStatusLine() is deprecated.'''
        let g:ale_deprecation_ale_get_status_line = 1
    endif

    return ale#statusline#Status()
endfunction
