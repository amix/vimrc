" Author: Nat Williams <nat.williams@gmail.com>
" Description: tflint for Terraform files
"
" See: https://www.terraform.io/
"      https://github.com/wata727/tflint

call ale#Set('terraform_tflint_options', '')
call ale#Set('terraform_tflint_executable', 'tflint')

function! ale_linters#terraform#tflint#Handle(buffer, lines) abort
    let l:output = []

    for l:error in ale#util#FuzzyJSONDecode(a:lines, [])
        if l:error.type is# 'ERROR'
            let l:type = 'E'
        elseif l:error.type is# 'NOTICE'
            let l:type = 'I'
        else
            let l:type = 'W'
        endif

        call add(l:output, {
        \   'lnum': l:error.line,
        \   'text': l:error.message,
        \   'type': l:type,
        \   'code': l:error.detector,
        \})
    endfor

    return l:output
endfunction

function! ale_linters#terraform#tflint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'terraform_tflint_executable')
endfunction

function! ale_linters#terraform#tflint#GetCommand(buffer) abort
    let l:cmd = ale#Escape(ale#Var(a:buffer, 'terraform_tflint_executable'))

    let l:config_file = ale#path#FindNearestFile(a:buffer, '.tflint.hcl')
    if !empty(l:config_file)
        let l:cmd .= ' --config ' . ale#Escape(l:config_file)
    endif

    let l:opts = ale#Var(a:buffer, 'terraform_tflint_options')
    if !empty(l:opts)
        let l:cmd .= ' ' . l:opts
    endif

    let l:cmd .= ' -f json %t'

    return l:cmd
endfunction

call ale#linter#Define('terraform', {
\   'name': 'tflint',
\   'executable_callback': 'ale_linters#terraform#tflint#GetExecutable',
\   'command_callback': 'ale_linters#terraform#tflint#GetCommand',
\   'callback': 'ale_linters#terraform#tflint#Handle',
\})

" vim:sw=4
