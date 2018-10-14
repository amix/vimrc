call ale#Set('json_jq_executable', 'jq')
call ale#Set('json_jq_options', '')

function! ale#fixers#jq#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'json_jq_executable')
endfunction

function! ale#fixers#jq#Fix(buffer) abort
     let l:options = ale#Var(a:buffer, 'json_jq_options')

     return {
     \  'command': ale#Escape(ale#fixers#jq#GetExecutable(a:buffer))
     \      . ' . ' . l:options,
     \}
endfunction
