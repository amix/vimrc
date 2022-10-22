" Description: tfsec for Terraform files
"
" See: https://www.terraform.io/
"      https://github.com/aquasecurity/tfsec

call ale#Set('terraform_tfsec_options', '')
call ale#Set('terraform_tfsec_executable', 'tfsec')

let s:separator = has('win32') ? '\' : '/'

function! ale_linters#terraform#tfsec#Handle(buffer, lines) abort
    let l:output = []
    let l:json = ale#util#FuzzyJSONDecode(a:lines, {})

    " if there's no warning, 'result' is `null`.
    if empty(get(l:json, 'results'))
        return l:output
    endif

    for l:result in get(l:json, 'results', [])
        if l:result.severity is# 'LOW'
            let l:type = 'I'
        elseif l:result.severity is# 'CRITICAL'
            let l:type = 'E'
        else
            let l:type = 'W'
        endif

        call add(l:output, {
        \   'filename': l:result.location.filename,
        \   'lnum': l:result.location.start_line,
        \   'end_lnum': l:result.location.end_line,
        \   'text': l:result.description,
        \   'code': l:result.long_id,
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

" Construct command arguments to tfsec with `terraform_tfsec_options`.
function! ale_linters#terraform#tfsec#GetCommand(buffer) abort
    let l:cmd = '%e'

    let l:config = ale_linters#terraform#tfsec#FindConfig(a:buffer)

    if !empty(l:config)
        let l:cmd .= ' --config-file ' . l:config
    endif

    let l:opts = ale#Var(a:buffer, 'terraform_tfsec_options')

    if !empty(l:opts)
        let l:cmd .= ' ' . l:opts
    endif

    let l:cmd .= ' --format json'

    return l:cmd
endfunction

" Find the nearest configuration file of tfsec.
function! ale_linters#terraform#tfsec#FindConfig(buffer) abort
    let l:config_dir = ale#path#FindNearestDirectory(a:buffer, '.tfsec')

    if !empty(l:config_dir)
        " https://aquasecurity.github.io/tfsec/v1.28.0/guides/configuration/config/
        for l:basename in ['config.yml', 'config.json']
            let l:config = ale#path#Simplify(join([l:config_dir, l:basename], s:separator))

            if filereadable(l:config)
                return ale#Escape(l:config)
            endif
        endfor
    endif

    return ''
endfunction

call ale#linter#Define('terraform', {
\   'name': 'tfsec',
\   'executable': {b -> ale#Var(b, 'terraform_tfsec_executable')},
\   'cwd': '%s:h',
\   'command': function('ale_linters#terraform#tfsec#GetCommand'),
\   'callback': 'ale_linters#terraform#tfsec#Handle',
\})
