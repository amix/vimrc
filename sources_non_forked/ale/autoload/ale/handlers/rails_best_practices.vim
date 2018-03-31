call ale#Set('ruby_rails_best_practices_options', '')
call ale#Set('ruby_rails_best_practices_executable', 'rails_best_practices')

function! ale#handlers#rails_best_practices#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'ruby_rails_best_practices_executable')
endfunction
