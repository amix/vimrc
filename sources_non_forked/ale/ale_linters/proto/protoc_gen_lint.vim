" Author: Jeff Willette <jrwillette88@gmail.com>
" Description: run the protoc-gen-lint plugin for the protoc binary

call ale#Set('proto_protoc_gen_lint_options', '')

function! ale_linters#proto#protoc_gen_lint#GetCommand(buffer) abort
    let l:dirname = expand('#' . a:buffer . ':p:h')

    let l:options = ['-I ' . ale#Escape(l:dirname)]

    if !empty(ale#Var(a:buffer, 'proto_protoc_gen_lint_options'))
        let l:options += [ale#Var(a:buffer, 'proto_protoc_gen_lint_options')]
    endif

    let l:options += ['--lint_out=. ' . '%s']

    return 'protoc' . ' ' . join(l:options)
endfunction

call ale#linter#Define('proto', {
\   'name': 'protoc-gen-lint',
\   'lint_file': 1,
\   'output_stream': 'stderr',
\   'executable': 'protoc',
\   'command_callback': 'ale_linters#proto#protoc_gen_lint#GetCommand',
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
