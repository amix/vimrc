" Author: carakan <carakan@gmail.com>
" Description: Fixing files with tslint.

function! ale#fixers#tslint#Fix(buffer) abort
    let l:executable = ale_linters#typescript#tslint#GetExecutable(a:buffer)

    let l:tslint_config_path = ale#path#ResolveLocalPath(
    \   a:buffer,
    \   'tslint.json',
    \   ale#Var(a:buffer, 'typescript_tslint_config_path')
    \)
    let l:tslint_config_option = !empty(l:tslint_config_path)
    \   ? ' -c ' . ale#Escape(l:tslint_config_path)
    \   : ''

    return {
    \   'command': ale#node#Executable(a:buffer, l:executable)
    \       . l:tslint_config_option
    \       . ' --fix %t',
    \   'read_temporary_file': 1,
    \}
endfunction
