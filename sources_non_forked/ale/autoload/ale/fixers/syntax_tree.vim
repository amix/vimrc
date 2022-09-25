call ale#Set('ruby_syntax_tree_options', '')
call ale#Set('ruby_syntax_tree_executable', 'stree')

function! ale#fixers#syntax_tree#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_syntax_tree_executable')
    let l:options = ale#Var(a:buffer, 'ruby_syntax_tree_options')

    return ale#ruby#EscapeExecutable(l:executable, 'stree')
    \   . ' write'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %t'
endfunction

function! ale#fixers#syntax_tree#Fix(buffer) abort
    return {
    \   'command': ale#fixers#syntax_tree#GetCommand(a:buffer),
    \   'read_temporary_file': 1,
    \}
endfunction
