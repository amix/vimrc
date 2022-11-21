" Author: Albert Marquez - https://github.com/a-marquez
" Description: Fixing files with XO.

function! ale#fixers#xo#Fix(buffer) abort
    let l:executable = ale#handlers#xo#GetExecutable(a:buffer)
    let l:options = ale#handlers#xo#GetOptions(a:buffer)

    return ale#semver#RunWithVersionCheck(
    \   a:buffer,
    \   l:executable,
    \   '%e --version',
    \   {b, v -> ale#fixers#xo#ApplyFixForVersion(b, v, l:executable, l:options)}
    \)
endfunction

function! ale#fixers#xo#ApplyFixForVersion(buffer, version, executable, options) abort
    let l:executable = ale#node#Executable(a:buffer, a:executable)
    let l:options = ale#Pad(a:options)

    " 0.30.0 is the first version with a working --stdin --fix
    if ale#semver#GTE(a:version, [0, 30, 0])
        return {
        \   'command': l:executable
        \       . ' --stdin --stdin-filename %s'
        \       . ' --fix'
        \       . l:options,
        \}
    endif

    return {
    \   'command': l:executable
    \       . ' --fix %t'
    \       . l:options,
    \   'read_temporary_file': 1,
    \}
endfunction
