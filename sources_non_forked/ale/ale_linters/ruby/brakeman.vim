" Author: Eddie Lebow https://github.com/elebow
" Description: Brakeman, a static analyzer for Rails security

call ale#Set('ruby_brakeman_options', '')
call ale#Set('ruby_brakeman_executable', 'brakeman')
call ale#Set('ruby_brakeman_options', '')

function! ale_linters#ruby#brakeman#Handle(buffer, lines) abort
    let l:output = []
    let l:json = ale#util#FuzzyJSONDecode(a:lines, {})
    let l:sep = has('win32') ? '\' : '/'
    " Brakeman always outputs paths relative to the Rails app root
    let l:rails_root = ale#ruby#FindRailsRoot(a:buffer)

    for l:warning in get(l:json, 'warnings', [])
        let l:text = l:warning.warning_type . ' ' . l:warning.message . ' (' . l:warning.confidence . ')'
        let l:line = l:warning.line != v:null ? l:warning.line : 1

        call add(l:output, {
        \   'filename': l:rails_root . l:sep .  l:warning.file,
        \   'lnum': l:line,
        \   'type': 'W',
        \   'text': l:text,
        \})
    endfor

    return l:output
endfunction

function! ale_linters#ruby#brakeman#GetCommand(buffer) abort
    let l:rails_root = ale#ruby#FindRailsRoot(a:buffer)

    if l:rails_root is? ''
        return ''
    endif

    let l:executable = ale#Var(a:buffer, 'ruby_brakeman_executable')

    return ale#ruby#EscapeExecutable(l:executable, 'brakeman')
    \    . ' -f json -q '
    \    . ale#Var(a:buffer, 'ruby_brakeman_options')
    \    . ' -p ' . ale#Escape(l:rails_root)
endfunction

call ale#linter#Define('ruby', {
\    'name': 'brakeman',
\    'executable': {b -> ale#Var(b, 'ruby_brakeman_executable')},
\    'command': function('ale_linters#ruby#brakeman#GetCommand'),
\    'callback': 'ale_linters#ruby#brakeman#Handle',
\    'lint_file': 1,
\})
