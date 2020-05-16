" Author: Nat Williams <nat.williams@gmail.com>
" Description: tflint for Terraform files
"
" See: https://www.terraform.io/
"      https://github.com/wata727/tflint

call ale#Set('terraform_tflint_options', '')
call ale#Set('terraform_tflint_executable', 'tflint')

function! ale_linters#terraform#tflint#Handle(buffer, lines) abort
    let l:output = []
    let l:pattern = '\v^(.*):(\d+),(\d+)-(\d+)?,?(\d+): (.{-1,}); (.+)$'
    let l:json = ale#util#FuzzyJSONDecode(a:lines, {})

    " This is a rough test for tflint's output format
    " On versions prior to 0.11 it outputs all errors as a single level list
    if type(l:json) is v:t_list
        for l:error in l:json
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
    else
        for l:error in get(l:json, 'errors', [])
            for l:match in ale#util#GetMatches(l:error.message, [l:pattern])
                if l:match[4] is# ''
                    let l:match[4] = l:match[2]
                endif

                call add(l:output, {
                \   'filename': l:match[1],
                \   'lnum': str2nr(l:match[2]),
                \   'col': str2nr(l:match[3]),
                \   'end_lnum': str2nr(l:match[4]),
                \   'end_col': str2nr(l:match[5]),
                \   'text': l:match[7],
                \   'code': l:match[6],
                \   'type': 'E',
                \})
            endfor
        endfor

        for l:error in get(l:json, 'issues', [])
            if l:error.rule.severity is# 'ERROR'
                let l:type = 'E'
            elseif l:error.rule.severity is# 'NOTICE'
                let l:type = 'I'
            else
                let l:type = 'W'
            endif

            call add(l:output, {
            \   'filename': l:error.range.filename,
            \   'lnum': l:error.range.start.line,
            \   'col': l:error.range.start.column,
            \   'end_lnum': l:error.range.end.line,
            \   'end_col': l:error.range.end.column,
            \   'text': l:error.message,
            \   'code': l:error.rule.name,
            \   'type': l:type,
            \})
        endfor
    endif

    return l:output
endfunction

function! ale_linters#terraform#tflint#GetCommand(buffer) abort
    let l:cmd = '%e'

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
\   'executable': {b -> ale#Var(b, 'terraform_tflint_executable')},
\   'command': function('ale_linters#terraform#tflint#GetCommand'),
\   'callback': 'ale_linters#terraform#tflint#Handle',
\})
