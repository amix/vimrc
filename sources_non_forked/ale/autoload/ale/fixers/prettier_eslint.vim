" Author: tunnckoCore (Charlike Mike Reagent) <mameto2011@gmail.com>,
"         w0rp <devw0rp@gmail.com>, morhetz (Pavel Pertsev) <morhetz@gmail.com>
" Description: Integration between Prettier and ESLint.

call ale#Set('javascript_prettier_eslint_executable', 'prettier-eslint')
call ale#Set('javascript_prettier_eslint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_prettier_eslint_options', '')

function! ale#fixers#prettier_eslint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_prettier_eslint', [
    \   'node_modules/prettier-eslint-cli/dist/index.js',
    \   'node_modules/.bin/prettier-eslint',
    \])
endfunction

function! ale#fixers#prettier_eslint#Fix(buffer) abort
    return ale#semver#RunWithVersionCheck(
    \   a:buffer,
    \   ale#fixers#prettier_eslint#GetExecutable(a:buffer),
    \   '%e --version',
    \   function('ale#fixers#prettier_eslint#ApplyFixForVersion'),
    \)
endfunction

function! ale#fixers#prettier_eslint#ApplyFixForVersion(buffer, version) abort
    let l:options = ale#Var(a:buffer, 'javascript_prettier_eslint_options')
    let l:executable = ale#fixers#prettier_eslint#GetExecutable(a:buffer)

    " 4.2.0 is the first version with --eslint-config-path
    let l:config = ale#semver#GTE(a:version, [4, 2, 0])
    \   ? ale#handlers#eslint#FindConfig(a:buffer)
    \   : ''
    let l:eslint_config_option = !empty(l:config)
    \   ? ' --eslint-config-path ' . ale#Escape(l:config)
    \   : ''

    " 4.4.0 is the first version with --stdin-filepath
    if ale#semver#GTE(a:version, [4, 4, 0])
        return {
        \   'cwd': '%s:h',
        \   'command': ale#Escape(l:executable)
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
