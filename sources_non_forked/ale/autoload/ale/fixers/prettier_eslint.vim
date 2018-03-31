" Author: tunnckoCore (Charlike Mike Reagent) <mameto2011@gmail.com>,
"         w0rp <devw0rp@gmail.com>, morhetz (Pavel Pertsev) <morhetz@gmail.com>
" Description: Integration between Prettier and ESLint.

function! ale#fixers#prettier_eslint#SetOptionDefaults() abort
    call ale#Set('javascript_prettier_eslint_executable', 'prettier-eslint')
    call ale#Set('javascript_prettier_eslint_use_global', 0)
    call ale#Set('javascript_prettier_eslint_options', '')
endfunction

call ale#fixers#prettier_eslint#SetOptionDefaults()

function! ale#fixers#prettier_eslint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_prettier_eslint', [
    \   'node_modules/prettier-eslint-cli/dist/index.js',
    \   'node_modules/.bin/prettier-eslint',
    \])
endfunction

function! ale#fixers#prettier_eslint#Fix(buffer) abort
    let l:executable = ale#fixers#prettier_eslint#GetExecutable(a:buffer)

    let l:command = ale#semver#HasVersion(l:executable)
    \   ? ''
    \   : ale#Escape(l:executable) . ' --version'

    return {
    \   'command': l:command,
    \   'chain_with': 'ale#fixers#prettier_eslint#ApplyFixForVersion',
    \}
endfunction

function! ale#fixers#prettier_eslint#ApplyFixForVersion(buffer, version_output) abort
    let l:options = ale#Var(a:buffer, 'javascript_prettier_eslint_options')
    let l:executable = ale#fixers#prettier_eslint#GetExecutable(a:buffer)

    let l:version = ale#semver#GetVersion(l:executable, a:version_output)

    " 4.2.0 is the first version with --eslint-config-path
    let l:config = ale#semver#GTE(l:version, [4, 2, 0])
    \   ? ale#handlers#eslint#FindConfig(a:buffer)
    \   : ''
    let l:eslint_config_option = !empty(l:config)
    \   ? ' --eslint-config-path ' . ale#Escape(l:config)
    \   : ''

    " 4.4.0 is the first version with --stdin-filepath
    if ale#semver#GTE(l:version, [4, 4, 0])
        return {
        \   'command': ale#path#BufferCdString(a:buffer)
        \       . ale#Escape(l:executable)
        \       . l:eslint_config_option
        \       . (!empty(l:options) ? ' ' . l:options : '')
        \       . ' --stdin-filepath %s --stdin',
        \}
    endif

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' %t'
    \       . l:eslint_config_option
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --write',
    \   'read_temporary_file': 1,
    \}
endfunction
