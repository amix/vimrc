" Author: Mohammed Chelouti - https://github.com/motato1
" Description: Deno lsp linter for TypeScript files.

call ale#linter#Define('typescript', {
\   'name': 'deno',
\   'lsp': 'stdio',
\   'executable': function('ale#handlers#deno#GetExecutable'),
\   'command': '%e lsp',
\   'project_root': function('ale#handlers#deno#GetProjectRoot'),
\   'initialization_options': function('ale_linters#typescript#deno#GetInitializationOptions'),
\})

function! ale_linters#typescript#deno#GetInitializationOptions(buffer) abort
    let l:options = {
    \   'enable': v:true,
    \   'lint': v:true,
    \   'unstable': v:false,
    \   }

    if ale#Var(a:buffer, 'deno_unstable')
        let l:options.unstable = v:true
    endif

    return l:options
endfunction
