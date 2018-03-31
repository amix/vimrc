" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing files with eslint.

function! ale#fixers#eslint#Fix(buffer) abort
    let l:executable = ale#handlers#eslint#GetExecutable(a:buffer)

    let l:command = ale#semver#HasVersion(l:executable)
    \   ? ''
    \   : ale#node#Executable(a:buffer, l:executable) . ' --version'

    return {
    \   'command': l:command,
    \   'chain_with': 'ale#fixers#eslint#ApplyFixForVersion',
    \}
endfunction

function! ale#fixers#eslint#ProcessFixDryRunOutput(buffer, output) abort
    for l:item in ale#util#FuzzyJSONDecode(a:output, [])
        return split(get(l:item, 'output', ''), "\n")
    endfor

    return []
endfunction

function! ale#fixers#eslint#ProcessEslintDOutput(buffer, output) abort
    " If the output is an error message, don't use it.
    for l:line in a:output[:10]
        if l:line =~# '^Error:'
            return []
        endif
    endfor

    return a:output
endfunction

function! ale#fixers#eslint#ApplyFixForVersion(buffer, version_output) abort
    let l:executable = ale#handlers#eslint#GetExecutable(a:buffer)
    let l:version = ale#semver#GetVersion(l:executable, a:version_output)

    let l:config = ale#handlers#eslint#FindConfig(a:buffer)

    if empty(l:config)
        return 0
    endif

    " Use --fix-to-stdout with eslint_d
    if l:executable =~# 'eslint_d$' && ale#semver#GTE(l:version, [3, 19, 0])
        return {
        \   'command': ale#node#Executable(a:buffer, l:executable)
        \       . ' --stdin-filename %s --stdin --fix-to-stdout',
        \   'process_with': 'ale#fixers#eslint#ProcessEslintDOutput',
        \}
    endif

    " 4.9.0 is the first version with --fix-dry-run
    if ale#semver#GTE(l:version, [4, 9, 0])
        return {
        \   'command': ale#node#Executable(a:buffer, l:executable)
        \       . ' --stdin-filename %s --stdin --fix-dry-run --format=json',
        \   'process_with': 'ale#fixers#eslint#ProcessFixDryRunOutput',
        \}
    endif

    return {
    \   'command': ale#node#Executable(a:buffer, l:executable)
    \       . ' -c ' . ale#Escape(l:config)
    \       . ' --fix %t',
    \   'read_temporary_file': 1,
    \}
endfunction
