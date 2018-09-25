" Author: Eddie Lebow https://github.com/elebow
" Description: rails_best_practices, a code metric tool for rails projects

call ale#Set('ruby_rails_best_practices_options', '')
call ale#Set('ruby_rails_best_practices_executable', 'rails_best_practices')

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
    let l:rails_root = ale#ruby#FindRailsRoot(a:buffer)

    if l:rails_root is? ''
        return ''
    endif

    let l:executable = ale#Var(a:buffer, 'ruby_rails_best_practices_executable')
    let l:output_file = ale#Has('win32') ? '%t ' : '/dev/stdout '
    let l:cat_file = ale#Has('win32') ? '; type %t' : ''

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'rails_best_practices')
    \    . ' --silent -f json --output-file ' . l:output_file
    \    . ale#Var(a:buffer, 'ruby_rails_best_practices_options')
    \    . ale#Escape(l:rails_root)
    \    . l:cat_file
endfunction

call ale#linter#Define('ruby', {
\    'name': 'rails_best_practices',
\    'executable_callback': ale#VarFunc('ruby_rails_best_practices_executable'),
\    'command_callback': 'ale_linters#ruby#rails_best_practices#GetCommand',
\    'callback': 'ale_linters#ruby#rails_best_practices#Handle',
\    'lint_file': 1,
\})
