call ale#Set('ruby_rubocop_options', '')
call ale#Set('ruby_rubocop_executable', 'rubocop')

function! ale#handlers#rubocop#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'ruby_rubocop_executable')
endfunction
