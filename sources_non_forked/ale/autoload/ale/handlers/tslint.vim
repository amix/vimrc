function! ale#handlers#tslint#InitVariables() abort
    call ale#Set('typescript_tslint_executable', 'tslint')
    call ale#Set('typescript_tslint_config_path', '')
    call ale#Set('typescript_tslint_rules_dir', '')
    call ale#Set('typescript_tslint_use_global', get(g:, 'ale_use_global_executables', 0))
    call ale#Set('typescript_tslint_ignore_empty_files', 0)
endfunction

function! ale#handlers#tslint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'typescript_tslint', [
    \   'node_modules/.bin/tslint',
    \])
endfunction
