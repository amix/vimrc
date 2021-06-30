" Author: sheerun (Adam Stankiewicz) <sheerun@sher.pl>
" Description: Integration of Prettier Standard with ALE.

call ale#Set('javascript_prettier_standard_executable', 'prettier-standard')
call ale#Set('javascript_prettier_standard_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_prettier_standard_options', '')

function! ale#fixers#prettier_standard#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_prettier_standard', [
    \   'node_modules/prettier-standard/lib/index.js',
    \   'node_modules/.bin/prettier-standard',
    \])
endfunction

function! ale#fixers#prettier_standard#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'javascript_prettier_standard_options')

    return {
    \   'command': ale#Escape(ale#fixers#prettier_standard#GetExecutable(a:buffer))
    \       . ' --stdin'
    \       . ' --stdin-filepath=%s'
    \       . ' ' . l:options,
    \}
endfunction
