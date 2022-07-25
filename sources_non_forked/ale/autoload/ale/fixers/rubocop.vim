call ale#Set('ruby_rubocop_options', '')
call ale#Set('ruby_rubocop_auto_correct_all', 0)
call ale#Set('ruby_rubocop_executable', 'rubocop')

" Rubocop fixer outputs diagnostics first and then the fixed
" output. These are delimited by a "=======" string that we
" look for to remove everything before it.
function! ale#fixers#rubocop#PostProcess(buffer, output) abort
    let l:line = 0

    for l:output in a:output
        let l:line = l:line + 1

        if l:output =~# "^=\\+$"
            break
        endif
    endfor

    return a:output[l:line :]
endfunction

function! ale#fixers#rubocop#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_rubocop_executable')
    let l:options = ale#Var(a:buffer, 'ruby_rubocop_options')
    let l:auto_correct_all = ale#Var(a:buffer, 'ruby_rubocop_auto_correct_all')

    return ale#ruby#EscapeExecutable(l:executable, 'rubocop')
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . (l:auto_correct_all ? ' --auto-correct-all' : ' --auto-correct')
    \   . ' --force-exclusion --stdin %s'
endfunction

function! ale#fixers#rubocop#Fix(buffer) abort
    return {
    \   'command': ale#fixers#rubocop#GetCommand(a:buffer),
    \   'process_with': 'ale#fixers#rubocop#PostProcess'
    \}
endfunction
