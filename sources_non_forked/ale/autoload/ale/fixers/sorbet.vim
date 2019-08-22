call ale#Set('ruby_sorbet_executable', 'srb')
call ale#Set('ruby_sorbet_options', '')

function! ale#fixers#sorbet#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_sorbet_executable')
    let l:options = ale#Var(a:buffer, 'ruby_sorbet_options')

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'srb')
    \   . ' tc'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --autocorrect --file %t'
endfunction

function! ale#fixers#sorbet#Fix(buffer) abort
    return {
    \   'command': ale#fixers#sorbet#GetCommand(a:buffer),
    \   'read_temporary_file': 1,
    \}
endfunction
