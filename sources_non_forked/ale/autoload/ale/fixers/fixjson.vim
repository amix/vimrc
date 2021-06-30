" Author: rhysd <https://rhysd.github.io>
" Description: Integration of fixjson with ALE.

call ale#Set('json_fixjson_executable', 'fixjson')
call ale#Set('json_fixjson_options', '')
call ale#Set('json_fixjson_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#fixers#fixjson#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'json_fixjson', [
    \   'node_modules/.bin/fixjson',
    \])
endfunction

function! ale#fixers#fixjson#Fix(buffer) abort
    let l:executable = ale#Escape(ale#fixers#fixjson#GetExecutable(a:buffer))
    let l:filename = ale#Escape(bufname(a:buffer))
    let l:command = l:executable . ' --stdin-filename ' . l:filename

    let l:options = ale#Var(a:buffer, 'json_fixjson_options')

    if l:options isnot# ''
        let l:command .= ' ' . l:options
    endif

    return {
    \   'command': l:command
    \}
endfunction
