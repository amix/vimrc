" Author: w0rp <devw0rp@gmail.com>, David Alexander <opensource@thelonelyghost.com>
" Description: Primary code path for the plugin
"   Manages execution of linters when requested by autocommands

let s:lint_timer = -1
let s:queued_buffer_number = -1
let s:should_lint_file_for_buffer = {}
let s:error_delay_ms = 1000 * 60 * 2

let s:timestamp_map = {}

" Given a key for a script variable for tracking the time to wait until
" a given function should be called, a funcref for a function to call, and
" a List of arguments, call the function and return whatever value it returns.
"
" If the function throws an exception, then the function will not be called
" for a while, and 0 will be returned instead.
function! ale#CallWithCooldown(timestamp_key, func, arglist) abort
    let l:now = ale#util#ClockMilliseconds()

    if l:now < get(s:timestamp_map, a:timestamp_key, -1)
        return 0
    endif

    let s:timestamp_map[a:timestamp_key] = l:now + s:error_delay_ms

    let l:return_value = call(a:func, a:arglist)

    let s:timestamp_map[a:timestamp_key] = -1

    return l:return_value
endfunction

" Return 1 if a file is too large for ALE to handle.
function! ale#FileTooLarge() abort
    let l:max = ale#Var(bufnr(''), 'maximum_file_size')

    return l:max > 0 ? (line2byte(line('$') + 1) > l:max) : 0
endfunction

let s:getcmdwintype_exists = exists('*getcmdwintype')

" A function for checking various conditions whereby ALE just shouldn't
" attempt to do anything, say if particular buffer types are open in Vim.
function! ale#ShouldDoNothing(buffer) abort
    " The checks are split into separate if statements to make it possible to
    " profile each check individually with Vim's profiling tools.

    " Don't perform any checks when newer NeoVim versions are exiting.
    if get(v:, 'exiting', v:null) isnot v:null
        return 1
    endif

    " Do nothing for blacklisted files
    if index(g:ale_filetype_blacklist, getbufvar(a:buffer, '&filetype')) >= 0
        return 1
    endif

    " Do nothing if running from command mode
    if s:getcmdwintype_exists && !empty(getcmdwintype())
        return 1
    endif

    let l:filename = fnamemodify(bufname(a:buffer), ':t')

    if l:filename is# '.'
        return 1
    endif

    " Do nothing if running in the sandbox
    if ale#util#InSandbox()
        return 1
    endif

    " Do nothing if ALE is disabled.
    if !ale#Var(a:buffer, 'enabled')
        return 1
    endif

    " Do nothing if the file is too large.
    if ale#FileTooLarge()
        return 1
    endif

    " Do nothing from CtrlP buffers with CtrlP-funky.
    if exists(':CtrlPFunky') is 2
    \&& getbufvar(a:buffer, '&l:statusline') =~# 'CtrlPMode.*funky'
        return 1
    endif

    return 0
endfunction

" (delay, [linting_flag, buffer_number])
function! ale#Queue(delay, ...) abort
    if a:0 > 2
        throw 'too many arguments!'
    endif

    " Default linting_flag to ''
    let l:linting_flag = get(a:000, 0, '')
    let l:buffer = get(a:000, 1, bufnr(''))

    return ale#CallWithCooldown(
    \   'dont_queue_until',
    \   function('s:ALEQueueImpl'),
    \   [a:delay, l:linting_flag, l:buffer],
    \)
endfunction

function! s:ALEQueueImpl(delay, linting_flag, buffer) abort
    if a:linting_flag isnot# '' && a:linting_flag isnot# 'lint_file'
        throw "linting_flag must be either '' or 'lint_file'"
    endif

    if type(a:buffer) != type(0)
        throw 'buffer_number must be a Number'
    endif

    if ale#ShouldDoNothing(a:buffer)
        return
    endif

    " Remember that we want to check files for this buffer.
    " We will remember this until we finally run the linters, via any event.
    if a:linting_flag is# 'lint_file'
        let s:should_lint_file_for_buffer[a:buffer] = 1
    endif

    if s:lint_timer != -1
        call timer_stop(s:lint_timer)
        let s:lint_timer = -1
    endif

    let l:linters = ale#linter#Get(getbufvar(a:buffer, '&filetype'))

    " Don't set up buffer data and so on if there are no linters to run.
    if empty(l:linters)
        " If we have some previous buffer data, then stop any jobs currently
        " running and clear everything.
        if has_key(g:ale_buffer_info, a:buffer)
            call ale#engine#RunLinters(a:buffer, [], 1)
        endif

        return
    endif

    if a:delay > 0
        let s:queued_buffer_number = a:buffer
        let s:lint_timer = timer_start(a:delay, function('ale#Lint'))
    else
        call ale#Lint(-1, a:buffer)
    endif
endfunction

function! ale#Lint(...) abort
    if a:0 > 1
        " Use the buffer number given as the optional second argument.
        let l:buffer = a:2
    elseif a:0 > 0 && a:1 == s:lint_timer
        " Use the buffer number for the buffer linting was queued for.
        let l:buffer = s:queued_buffer_number
    else
        " Use the current buffer number.
        let l:buffer = bufnr('')
    endif

    return ale#CallWithCooldown(
    \   'dont_lint_until',
    \   function('s:ALELintImpl'),
    \   [l:buffer],
    \)
endfunction

function! s:ALELintImpl(buffer) abort
    if ale#ShouldDoNothing(a:buffer)
        return
    endif

    " Use the filetype from the buffer
    let l:linters = ale#linter#Get(getbufvar(a:buffer, '&filetype'))
    let l:should_lint_file = 0

    " Check if we previously requested checking the file.
    if has_key(s:should_lint_file_for_buffer, a:buffer)
        unlet s:should_lint_file_for_buffer[a:buffer]
        " Lint files if they exist.
        let l:should_lint_file = filereadable(expand('#' . a:buffer . ':p'))
    endif

    call ale#engine#RunLinters(a:buffer, l:linters, l:should_lint_file)
endfunction

" Reset flags indicating that files should be checked for all buffers.
function! ale#ResetLintFileMarkers() abort
    let s:should_lint_file_for_buffer = {}
endfunction

function! ale#ResetErrorDelays() abort
    let s:timestamp_map = {}
endfunction

let g:ale_has_override = get(g:, 'ale_has_override', {})

" Call has(), but check a global Dictionary so we can force flags on or off
" for testing purposes.
function! ale#Has(feature) abort
    return get(g:ale_has_override, a:feature, has(a:feature))
endfunction

" Given a buffer number and a variable name, look for that variable in the
" buffer scope, then in global scope. If the name does not exist in the global
" scope, an exception will be thrown.
"
" Every variable name will be prefixed with 'ale_'.
function! ale#Var(buffer, variable_name) abort
    let l:nr = str2nr(a:buffer)
    let l:full_name = 'ale_' . a:variable_name

    if bufexists(l:nr)
        let l:vars = getbufvar(l:nr, '')
    elseif has_key(g:, 'ale_fix_buffer_data')
        let l:vars = get(g:ale_fix_buffer_data, l:nr, {'vars': {}}).vars
    else
        let l:vars = {}
    endif

    return get(l:vars, l:full_name, g:[l:full_name])
endfunction

" Initialize a variable with a default value, if it isn't already set.
"
" Every variable name will be prefixed with 'ale_'.
function! ale#Set(variable_name, default) abort
    let l:full_name = 'ale_' . a:variable_name
    let l:value = get(g:, l:full_name, a:default)
    let g:[l:full_name] = l:value

    return l:value
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
