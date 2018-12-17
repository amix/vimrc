" Author: Patrick Lewis - https://github.com/patricklewis, thenoseman - https://github.com/thenoseman
" Description: haml-lint for Haml files

call ale#Set('haml_hamllint_executable', 'haml-lint')

function! ale_linters#haml#hamllint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'haml_hamllint_executable')
endfunction

function! ale_linters#haml#hamllint#GetCommand(buffer) abort
    let l:prefix = ''

    let l:rubocop_config_file_path = ale#path#FindNearestFile(a:buffer, '.rubocop.yml')
    let l:hamllint_config_file_path = ale#path#FindNearestFile(a:buffer, '.haml-lint.yml')

    " Set HAML_LINT_RUBOCOP_CONF variable as it is needed for haml-lint to
    " pick up the rubocop config.
    "
    " See https://github.com/brigade/haml-lint/blob/master/lib/haml_lint/linter/rubocop.rb#L89
    "     HamlLint::Linter::RuboCop#rubocop_flags
    if !empty(l:rubocop_config_file_path)
        if ale#Has('win32')
            let l:prefix = 'set HAML_LINT_RUBOCOP_CONF=' . ale#Escape(l:rubocop_config_file_path) . ' &&'
        else
            let l:prefix = 'HAML_LINT_RUBOCOP_CONF=' . ale#Escape(l:rubocop_config_file_path)
        endif
    endif

    return (!empty(l:prefix) ? l:prefix . ' ' : '')
    \   . ale_linters#haml#hamllint#GetExecutable(a:buffer)
    \   . (!empty(l:hamllint_config_file_path) ? ' --config ' . ale#Escape(l:hamllint_config_file_path) : '')
    \   . ' %t'
endfunction

function! ale_linters#haml#hamllint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " <path>:51 [W] RuboCop: Use the new Ruby 1.9 hash syntax.
    let l:pattern = '\v^.*:(\d+) \[([EW])\] (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'type': l:match[2],
        \   'text': l:match[3]
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('haml', {
\   'name': 'hamllint',
\   'executable_callback': 'ale_linters#haml#hamllint#GetExecutable',
\   'command_callback': 'ale_linters#haml#hamllint#GetCommand',
\   'callback': 'ale_linters#haml#hamllint#Handle'
\})
