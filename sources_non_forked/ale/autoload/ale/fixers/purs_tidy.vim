" Author: toastal <toastal@posteo.net>
" Description: Integration of purs-tidy with ALE.

call ale#Set('purescript_tidy_executable', 'purs-tidy')
call ale#Set('purescript_tidy_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('purescript_tidy_options', '')

function! ale#fixers#purs_tidy#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'purescript_tidy', [
    \   'node_modules/purescript-tidy/bin/index.js',
    \   'node_modules/.bin/purs-tidy',
    \])
endfunction

function! ale#fixers#purs_tidy#Fix(buffer) abort
    let l:executable = ale#fixers#purs_tidy#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'purescript_tidy_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' format'
    \       . ale#Pad(l:options)
    \}
endfunction
