" Author: Mahmoud Mostafa <mah@moud.info>
" Description: Fixing files with stylelint.

call ale#Set('stylelint_executable', 'stylelint')
call ale#Set('stylelint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('stylelint_options', '')

function! ale#fixers#stylelint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'stylelint', [
    \   'node_modules/stylelint/bin/stylelint.js',
    \   'node_modules/.bin/stylelint',
    \])
endfunction

function! ale#fixers#stylelint#Fix(buffer) abort
    let l:executable = ale#fixers#stylelint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'stylelint_options')

    return {
    \   'command': ale#path#BufferCdString(a:buffer)
    \       . ale#node#Executable(a:buffer, l:executable)
    \       . ' %t'
    \       . ale#Pad(l:options)
    \       . ' --fix',
    \   'read_temporary_file': 1,
    \}
endfunction
