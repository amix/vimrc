" Author: w0rp <devw0rp@gmail.com>
" Description: This file implements debugging information for ALE

let s:global_variable_list = [
\    'ale_cache_executable_check_failures',
\    'ale_change_sign_column_color',
\    'ale_command_wrapper',
\    'ale_completion_delay',
\    'ale_completion_enabled',
\    'ale_completion_max_suggestions',
\    'ale_echo_cursor',
\    'ale_echo_msg_error_str',
\    'ale_echo_msg_format',
\    'ale_echo_msg_info_str',
\    'ale_echo_msg_warning_str',
\    'ale_enabled',
\    'ale_fix_on_save',
\    'ale_fixers',
\    'ale_history_enabled',
\    'ale_history_log_output',
\    'ale_keep_list_window_open',
\    'ale_lint_delay',
\    'ale_lint_on_enter',
\    'ale_lint_on_filetype_changed',
\    'ale_lint_on_insert_leave',
\    'ale_lint_on_save',
\    'ale_lint_on_text_changed',
\    'ale_linter_aliases',
\    'ale_linters',
\    'ale_linters_explicit',
\    'ale_list_vertical',
\    'ale_list_window_size',
\    'ale_loclist_msg_format',
\    'ale_lsp_root',
\    'ale_max_buffer_history_size',
\    'ale_max_signs',
\    'ale_maximum_file_size',
\    'ale_open_list',
\    'ale_pattern_options',
\    'ale_pattern_options_enabled',
\    'ale_set_balloons',
\    'ale_set_highlights',
\    'ale_set_loclist',
\    'ale_set_quickfix',
\    'ale_set_signs',
\    'ale_sign_column_always',
\    'ale_sign_error',
\    'ale_sign_info',
\    'ale_sign_offset',
\    'ale_sign_style_error',
\    'ale_sign_style_warning',
\    'ale_sign_warning',
\    'ale_statusline_format',
\    'ale_type_map',
\    'ale_use_global_executables',
\    'ale_virtualtext_cursor',
\    'ale_warn_about_trailing_blank_lines',
\    'ale_warn_about_trailing_whitespace',
\]

function! s:Echo(message) abort
    execute 'echo a:message'
endfunction

function! s:GetLinterVariables(filetype, linter_names) abort
    let l:variable_list = []
    let l:filetype_parts = split(a:filetype, '\.')

    for l:key in keys(g:)
        " Extract variable names like: 'ale_python_flake8_executable'
        let l:match = matchlist(l:key, '\v^ale_([^_]+)_([^_]+)_.+$')

        " Include matching variables.
        if !empty(l:match)
        \&& index(l:filetype_parts, l:match[1]) >= 0
        \&& index(a:linter_names, l:match[2]) >= 0
            call add(l:variable_list, l:key)
        endif
    endfor

    call sort(l:variable_list)

    return l:variable_list
endfunction

function! s:EchoLinterVariables(variable_list) abort
    for l:key in a:variable_list
        call s:Echo('let g:' . l:key . ' = ' . string(g:[l:key]))

        if has_key(b:, l:key)
            call s:Echo('let b:' . l:key . ' = ' . string(b:[l:key]))
        endif
    endfor
endfunction

function! s:EchoGlobalVariables() abort
    for l:key in s:global_variable_list
        call s:Echo('let g:' . l:key . ' = ' . string(get(g:, l:key, v:null)))

        if has_key(b:, l:key)
            call s:Echo('let b:' . l:key . ' = ' . string(b:[l:key]))
        endif
    endfor
endfunction

" Echo a command that was run.
function! s:EchoCommand(item) abort
    let l:status_message = a:item.status

    " Include the exit code in output if we have it.
    if a:item.status is# 'finished'
        let l:status_message .= ' - exit code ' . a:item.exit_code
    endif

    call s:Echo('(' . l:status_message . ') ' . string(a:item.command))

    if g:ale_history_log_output && has_key(a:item, 'output')
        if empty(a:item.output)
            call s:Echo('')
            call s:Echo('<<<NO OUTPUT RETURNED>>>')
            call s:Echo('')
        else
            call s:Echo('')
            call s:Echo('<<<OUTPUT STARTS>>>')

            for l:line in a:item.output
                call s:Echo(l:line)
            endfor

            call s:Echo('<<<OUTPUT ENDS>>>')
            call s:Echo('')
        endif
    endif
endfunction

" Echo the results of an executable check.
function! s:EchoExecutable(item) abort
    call s:Echo(printf(
    \   '(executable check - %s) %s',
    \   a:item.status ? 'success' : 'failure',
    \   a:item.command,
    \))
endfunction

function! s:EchoCommandHistory() abort
    let l:buffer = bufnr('%')

    for l:item in ale#history#Get(l:buffer)
        if l:item.job_id is# 'executable'
            call s:EchoExecutable(l:item)
        else
            call s:EchoCommand(l:item)
        endif
    endfor
endfunction

function! s:EchoLinterAliases(all_linters) abort
    let l:first = 1

    for l:linter in a:all_linters
        if !empty(l:linter.aliases)
            if l:first
                call s:Echo('   Linter Aliases:')
            endif

            let l:first = 0

            call s:Echo(string(l:linter.name) . ' -> ' . string(l:linter.aliases))
        endif
    endfor
endfunction

function! s:EchoLSPErrorMessages(all_linter_names) abort
    let l:lsp_error_messages = get(g:, 'ale_lsp_error_messages', {})
    let l:header_echoed = 0

    for l:linter_name in a:all_linter_names
        let l:error_list = get(l:lsp_error_messages, l:linter_name, [])

        if !empty(l:error_list)
            if !l:header_echoed
                call s:Echo(' LSP Error Messages:')
                call s:Echo('')
            endif

            call s:Echo('(Errors for ' . l:linter_name . ')')

            for l:message in l:error_list
                for l:line in split(l:message, "\n")
                    call s:Echo(l:line)
                endfor
            endfor
        endif
    endfor
endfunction

function! ale#debugging#Info() abort
    let l:filetype = &filetype

    " We get the list of enabled linters for free by the above function.
    let l:enabled_linters = deepcopy(ale#linter#Get(l:filetype))

    " But have to build the list of available linters ourselves.
    let l:all_linters = []
    let l:linter_variable_list = []

    for l:part in split(l:filetype, '\.')
        let l:aliased_filetype = ale#linter#ResolveFiletype(l:part)
        call extend(l:all_linters, ale#linter#GetAll(l:aliased_filetype))
    endfor

    let l:all_names = map(copy(l:all_linters), 'v:val[''name'']')
    let l:enabled_names = map(copy(l:enabled_linters), 'v:val[''name'']')

    " Load linter variables to display
    " This must be done after linters are loaded.
    let l:variable_list = s:GetLinterVariables(l:filetype, l:enabled_names)

    let l:fixers = ale#fix#registry#SuggestedFixers(l:filetype)
    let l:fixers = uniq(sort(l:fixers[0] + l:fixers[1]))
    let l:fixers_string = join(map(copy(l:fixers), '"\n  " . v:val'), '')

    call s:Echo(' Current Filetype: ' . l:filetype)
    call s:Echo('Available Linters: ' . string(l:all_names))
    call s:EchoLinterAliases(l:all_linters)
    call s:Echo('  Enabled Linters: ' . string(l:enabled_names))
    call s:Echo(' Suggested Fixers: ' . l:fixers_string)
    call s:Echo(' Linter Variables:')
    call s:Echo('')
    call s:EchoLinterVariables(l:variable_list)
    call s:Echo(' Global Variables:')
    call s:Echo('')
    call s:EchoGlobalVariables()
    call s:EchoLSPErrorMessages(l:all_names)
    call s:Echo('  Command History:')
    call s:Echo('')
    call s:EchoCommandHistory()
endfunction

function! ale#debugging#InfoToClipboard() abort
    redir => l:output
        silent call ale#debugging#Info()
    redir END

    let @+ = l:output
    call s:Echo('ALEInfo copied to your clipboard')
endfunction

function! ale#debugging#InfoToFile(filename) abort
    let l:expanded_filename = expand(a:filename)

    redir => l:output
        silent call ale#debugging#Info()
    redir END

    call writefile(split(l:output, "\n"), l:expanded_filename)
    call s:Echo('ALEInfo written to ' . l:expanded_filename)
endfunction
