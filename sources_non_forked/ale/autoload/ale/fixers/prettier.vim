" Author: tunnckoCore (Charlike Mike Reagent) <mameto2011@gmail.com>,
"         w0rp <devw0rp@gmail.com>, morhetz (Pavel Pertsev) <morhetz@gmail.com>
" Description: Integration of Prettier with ALE.

call ale#Set('javascript_prettier_executable', 'prettier')
call ale#Set('javascript_prettier_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_prettier_options', '')

function! ale#fixers#prettier#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_prettier', [
    \   'node_modules/.bin/prettier_d',
    \   'node_modules/prettier-cli/index.js',
    \   'node_modules/.bin/prettier',
    \])
endfunction

function! ale#fixers#prettier#Fix(buffer) abort
    let l:executable = ale#fixers#prettier#GetExecutable(a:buffer)

    let l:command = ale#semver#HasVersion(l:executable)
    \   ? ''
    \   : ale#Escape(l:executable) . ' --version'

    return {
    \   'command': l:command,
    \   'chain_with': 'ale#fixers#prettier#ApplyFixForVersion',
    \}
endfunction

function! ale#fixers#prettier#ApplyFixForVersion(buffer, version_output) abort
    let l:executable = ale#fixers#prettier#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'javascript_prettier_options')

    let l:version = ale#semver#GetVersion(l:executable, a:version_output)

    " 1.4.0 is the first version with --stdin-filepath
    if ale#semver#GTE(l:version, [1, 4, 0])
        return {
        \   'command': ale#path#BufferCdString(a:buffer)
        \       . ale#Escape(l:executable)
        \       . (!empty(l:options) ? ' ' . l:options : '')
        \       . ' --stdin-filepath %s --stdin',
        \}
    endif

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' %t'
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --write',
    \   'read_temporary_file': 1,
    \}
endfunction
