" Author: Roeland Moors - https://github.com/roelandmoors
" Description: ERB Lint, support for https://github.com/Shopify/erb-lint

call ale#Set('eruby_erblint_executable', 'erblint')
call ale#Set('eruby_erblint_options', '')


" Erblint fixer outputs diagnostics first and then the fixed
" output. These are delimited by something like this:
" ================ /path/to/demo.html.erb ==================
" We only need the output after this
function! ale#fixers#erblint#PostProcess(buffer, output) abort
    let l:line = 0

    for l:output in a:output
        let l:line = l:line + 1

        if l:output =~# "^=\\+.*=\\+$"
            break
        endif
    endfor

    return a:output[l:line :]
endfunction

function! ale#fixers#erblint#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'eruby_erblint_executable')
    let l:options = ale#Var(a:buffer, 'eruby_erblint_options')

    return ale#ruby#EscapeExecutable(l:executable, 'erblint')
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --autocorrect --stdin %s'
endfunction

function! ale#fixers#erblint#Fix(buffer) abort
    return {
    \   'command': ale#fixers#erblint#GetCommand(a:buffer),
    \   'process_with': 'ale#fixers#erblint#PostProcess'
    \}
endfunction
