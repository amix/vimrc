" Author: w0rp <devw0rp@gmail.com>, David Alexander <opensource@thelonelyghost.com>
" Description: Primary code path for the plugin
"   Manages execution of linters when requested by autocommands

" Strings used for severity in the echoed message
let g:ale_echo_msg_error_str = get(g:, 'ale_echo_msg_error_str', 'Error')
let g:ale_echo_msg_info_str = get(g:, 'ale_echo_msg_info_str', 'Info')
let g:ale_echo_msg_log_str = get(g:, 'ale_echo_msg_log_str', 'Log')
let g:ale_echo_msg_warning_str = get(g:, 'ale_echo_msg_warning_str', 'Warning')
" Ignoring linters, for disabling some, or ignoring LSP diagnostics.
let g:ale_linters_ignore = get(g:, 'ale_linters_ignore', {})
let g:ale_disable_lsp = get(g:, 'ale_disable_lsp', 0)

" LSP window/showMessage format
let g:ale_lsp_show_message_format = get(g:, 'ale_lsp_show_message_format', '%severity%:%linter%: %s')
" Valid values mimic LSP definitions (error, warning and information; log is
" never shown)
let g:ale_lsp_show_message_severity = get(g:, 'ale_lsp_show_message_severity', 'error')

let s:lint_timer = -1
let s:getcmdwintype_exists = exists('*getcmdwintype')

" Return 1 if a file is too large for ALE to handle.
function! ale#FileTooLarge(buffer) abort
    let l:max = getbufvar(a:buffer, 'ale_maximum_file_size', get(g:, 'ale_maximum_file_size', 0))

    return l:max > 0 ? (line2byte(line('$') + 1) > l:max) : 0
endfunction

" A function for checking various conditions whereby ALE just shouldn't
" attempt to do anything, say if particular buffer types are open in Vim.
function! ale#ShouldDoNothing(buffer) abort
    " The checks are split into separate if statements to make it possible to
    " profile each check individually with Vim's profiling tools.
    "
    " Do nothing if ALE is disabled.
    if !getbufvar(a:buffer, 'ale_enabled', get(g:, 'ale_enabled', 0))
        return 1
    endif

    " Don't perform any checks when newer NeoVim versions are exiting.
    if get(v:, 'exiting', v:null) isnot v:null
        return 1
    endif

    let l:filetype = getbufvar(a:buffer, '&filetype')

    " Do nothing when there's no filetype.
    if l:filetype is# ''
        return 1
    endif

    " Do nothing for diff buffers.
    if getbufvar(a:buffer, '&diff')
        return 1
    endif

    " Do nothing for blacklisted files.
    if index(get(g:, 'ale_filetype_blacklist', []), l:filetype) >= 0
        return 1
    endif

    " Do nothing if running from command mode.
    if s:getcmdwintype_exists && !empty(getcmdwintype())
        return 1
    endif

    let l:filename = fnamemodify(bufname(a:buffer), ':t')

    " Do nothing for directories.
    if l:filename is# '.'
        return 1
    endif

    " Don't start linting and so on when an operator is pending.
    if ale#util#Mode(1) is# 'no'
        return 1
    endif

    " Do nothing if running in the sandbox.
    if ale#util#InSandbox()
        return 1
    endif

    " Do nothing if the file is too large.
    if ale#FileTooLarge(a:buffer)
        return 1
    endif

    " Do nothing from CtrlP buffers with CtrlP-funky.
    if exists(':CtrlPFunky') is 2
    \&& getbufvar(a:buffer, '&l:statusline') =~# 'CtrlPMode.*funky'
        return 1
    endif

    return 0
endfunction

function! s:Lint(buffer, should_lint_file, timer_id) abort
    " Use the filetype from the buffer
    let l:filetype = getbufvar(a:buffer, '&filetype')
    let l:linters = ale#linter#Get(l:filetype)

    " Apply ignore lists for linters only if needed.
    let l:ignore_config = ale#Var(a:buffer, 'linters_ignore')
    let l:disable_lsp = ale#Var(a:buffer, 'disable_lsp')
    let l:linters = !empty(l:ignore_config) || l:disable_lsp
    \   ? ale#engine#ignore#Exclude(l:filetype, l:linters, l:ignore_config, l:disable_lsp)
    \   : l:linters

    " Tell other sources that they can start checking the buffer now.
    let g:ale_want_results_buffer = a:buffer
    silent doautocmd <nomodeline> User ALEWantResults
    unlet! g:ale_want_results_buffer

    " Don't set up buffer data and so on if there are no linters to run.
    if !has_key(g:ale_buffer_info, a:buffer) && empty(l:linters)
        return
    endif

    " Clear lint_file linters, or only run them if the file exists.
    let l:lint_file = empty(l:linters)
    \   || (a:should_lint_file && filereadable(expand('#' . a:buffer . ':p')))

    call ale#engine#RunLinters(a:buffer, l:linters, l:lint_file)
endfunction

" (delay, [linting_flag, buffer_number])
function! ale#Queue(delay, ...) abort
    if a:0 > 2
        throw 'too many arguments!'
    endif

    let l:buffer = get(a:000, 1, v:null)

    if l:buffer is v:null
        let l:buffer = bufnr('')
    endif

    if type(l:buffer) isnot v:t_number
        throw 'buffer_number must be a Number'
    endif

    if ale#ShouldDoNothing(l:buffer)
        return
    endif

    " Default linting_flag to ''
    let l:should_lint_file = get(a:000, 0) is# 'lint_file'

    if s:lint_timer != -1
        call timer_stop(s:lint_timer)
        let s:lint_timer = -1
    endif

    if a:delay > 0
        let s:lint_timer = timer_start(
        \   a:delay,
        \   function('s:Lint', [l:buffer, l:should_lint_file])
        \)
    else
        call s:Lint(l:buffer, l:should_lint_file, 0)
    endif
endfunction

let s:current_ale_version = [2, 6, 0]

" A function used to check for ALE features in files outside of the project.
function! ale#Has(feature) abort
    let l:match = matchlist(a:feature, '\c\v^ale-(\d+)\.(\d+)(\.(\d+))?$')

    if !empty(l:match)
        let l:version = [l:match[1] + 0, l:match[2] + 0, l:match[4] + 0]

        return ale#semver#GTE(s:current_ale_version, l:version)
    endif

    return 0
endfunction

" Given a buffer number and a variable name, look for that variable in the
" buffer scope, then in global scope. If the name does not exist in the global
" scope, an exception will be thrown.
"
" Every variable name will be prefixed with 'ale_'.
function! ale#Var(buffer, variable_name) abort
    let l:full_name = 'ale_' . a:variable_name
    let l:vars = getbufvar(str2nr(a:buffer), '', {})

    return get(l:vars, l:full_name, g:[l:full_name])
endfunction

" Initialize a variable with a default value, if it isn't already set.
"
" Every variable name will be prefixed with 'ale_'.
function! ale#Set(variable_name, default) abort
    let l:full_name = 'ale_' . a:variable_name

    if !has_key(g:, l:full_name)
        let g:[l:full_name] = a:default
    endif
endfunction

" Given a string for adding to a command, return the string padded with a
" space on the left if it is not empty. Otherwise return an empty string.
"
" This can be used for making command strings cleaner and easier to test.
function! ale#Pad(string) abort
    return !empty(a:string) ? ' ' . a:string : ''
endfunction

" Given a environment variable name and a value, produce part of a command for
" setting an environment variable before running a command. The syntax will be
" valid for cmd on Windows, or most shells on Unix.
function! ale#Env(variable_name, value) abort
    if has('win32')
        return 'set ' . a:variable_name . '=' . ale#Escape(a:value) . ' && '
    endif

    return a:variable_name . '=' . ale#Escape(a:value) . ' '
endfunction

" Escape a string suitably for each platform.
" shellescape does not work on Windows.
function! ale#Escape(str) abort
    if fnamemodify(&shell, ':t') is? 'cmd.exe'
        " If the string contains spaces, it will be surrounded by quotes.
        " Otherwise, special characters will be escaped with carets (^).
        return substitute(
        \   a:str =~# ' '
        \       ?  '"' .  substitute(a:str, '"', '""', 'g') . '"'
        \       : substitute(a:str, '\v([&|<>^])', '^\1', 'g'),
        \   '%',
        \   '%%',
        \   'g',
        \)
    endif

    return shellescape (a:str)
endfunction

" Get the loclist item message according to a given format string.
"
" See `:help g:ale_loclist_msg_format` and `:help g:ale_echo_msg_format`
function! ale#GetLocItemMessage(item, format_string) abort
    let l:msg = a:format_string
    let l:severity = g:ale_echo_msg_warning_str
    let l:code = get(a:item, 'code', '')
    let l:type = get(a:item, 'type', 'E')
    let l:linter_name = get(a:item, 'linter_name', '')
    let l:code_repl = !empty(l:code) ? '\=submatch(1) . l:code . submatch(2)' : ''

    if l:type is# 'E'
        let l:severity = g:ale_echo_msg_error_str
    elseif l:type is# 'I'
        let l:severity = g:ale_echo_msg_info_str
    endif

    " Replace special markers with certain information.
    " \=l:variable is used to avoid escaping issues.
    let l:msg = substitute(l:msg, '\V%severity%', '\=l:severity', 'g')
    let l:msg = substitute(l:msg, '\V%linter%', '\=l:linter_name', 'g')
    let l:msg = substitute(l:msg, '\v\%([^\%]*)code([^\%]*)\%', l:code_repl, 'g')
    " Replace %s with the text.
    let l:msg = substitute(l:msg, '\V%s', '\=a:item.text', 'g')

    return l:msg
endfunction
