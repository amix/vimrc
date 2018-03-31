" Author: KabbAmine <amine.kabb@gmail.com>, deathmaz <00maz1987@gmail.com>, diartyz <diartyz@gmail.com>
" Description: HTMLHint for checking html files

call ale#Set('html_htmlhint_options', '')
call ale#Set('html_htmlhint_executable', 'htmlhint')
call ale#Set('html_htmlhint_use_global', 0)

function! ale_linters#html#htmlhint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'html_htmlhint', [
    \   'node_modules/.bin/htmlhint',
    \])
endfunction

function! ale_linters#html#htmlhint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'html_htmlhint_options')
    let l:config = l:options !~# '--config'
    \   ? ale#path#FindNearestFile(a:buffer, '.htmlhintrc')
    \   : ''

    if !empty(l:config)
        let l:options .= ' --config ' . ale#Escape(l:config)
    endif

    if !empty(l:options)
        let l:options = substitute(l:options, '--format=unix', '', '')
    endif

    return ale#Escape(ale_linters#html#htmlhint#GetExecutable(a:buffer))
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --format=unix %t'
endfunction

call ale#linter#Define('html', {
\   'name': 'htmlhint',
\   'executable_callback': 'ale_linters#html#htmlhint#GetExecutable',
\   'command_callback': 'ale_linters#html#htmlhint#GetCommand',
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
