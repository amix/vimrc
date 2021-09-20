" Author: Mahmoud Mostafa <mah@moud.info>
" Description: Fixing files with stylelint.

call ale#Set('stylelint_executable', 'stylelint')
call ale#Set('stylelint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('stylelint_options', '')

function! ale#fixers#stylelint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'stylelint', [
    \   'node_modules/stylelint/bin/stylelint.js',
    \   'node_modules/.bin/stylelint',
    \])
endfunction

function! ale#fixers#stylelint#Fix(buffer) abort
    let l:executable = ale#fixers#stylelint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'stylelint_options')

    return {
    \   'cwd': '%s:h',
    \   'command': ale#node#Executable(a:buffer, l:executable)
    \       . ale#Pad(l:options)
    \       . ' --fix --stdin --stdin-filename %s',
    \   'read_temporary_file': 0,
    \}
endfunction
