" Author: Eddie Lebow https://github.com/elebow
" Description: rails_best_practices, a code metric tool for rails projects

let g:ale_ruby_rails_best_practices_options =
\   get(g:, 'ale_ruby_rails_best_practices_options', '')

function! ale_linters#ruby#rails_best_practices#Handle(buffer, lines) abort
    let l:output = []

    for l:warning in ale#util#FuzzyJSONDecode(a:lines, [])
        if !ale#path#IsBufferPath(a:buffer, l:warning.filename)
            continue
        endif

        call add(l:output, {
        \    'lnum': l:warning.line_number + 0,
        \    'type': 'W',
        \    'text': l:warning.message,
        \})
    endfor

    return l:output
endfunction

function! ale_linters#ruby#rails_best_practices#GetCommand(buffer) abort
    let l:executable = ale#handlers#rails_best_practices#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'bundle$'
    \   ? ' exec rails_best_practices'
    \   : ''

    let l:rails_root = ale#ruby#FindRailsRoot(a:buffer)

    if l:rails_root is? ''
        return ''
    endif

    let l:output_file = ale#Has('win32') ? '%t ' : '/dev/stdout '
    let l:cat_file = ale#Has('win32') ? '; type %t' : ''

    return ale#Escape(l:executable) . l:exec_args
    \    . ' --silent -f json --output-file ' . l:output_file
    \    . ale#Var(a:buffer, 'ruby_rails_best_practices_options')
    \    . ale#Escape(l:rails_root)
    \    . l:cat_file
endfunction

call ale#linter#Define('ruby', {
\    'name': 'rails_best_practices',
\    'executable_callback': 'ale#handlers#rails_best_practices#GetExecutable',
\    'command_callback': 'ale_linters#ruby#rails_best_practices#GetCommand',
\    'callback': 'ale_linters#ruby#rails_best_practices#Handle',
\    'lint_file': 1,
\})
