" Author: Arnold Chand <creativenull@outlook.com>
" Description: Volar Language Server integration for ALE adopted from
"              nvim-lspconfig and volar/packages/shared/src/types.ts

call ale#Set('vue_volar_executable', 'vue-language-server')
call ale#Set('vue_volar_use_global', 1)
call ale#Set('vue_volar_init_options', {
\   'typescript': { 'tsdk': '' },
\})

function! ale_linters#vue#volar#GetProjectRoot(buffer) abort
    let l:project_roots = [
    \   'package.json',
    \   'vite.config.js',
    \   'vite.config.mjs',
    \   'vite.config.cjs',
    \   'vite.config.ts',
    \   '.git',
    \   bufname(a:buffer)
    \]

    for l:project_root in l:project_roots
        let l:nearest_filepath = ale#path#FindNearestFile(a:buffer, l:project_root)

        if !empty(l:nearest_filepath)
            return fnamemodify(l:nearest_filepath, ':h')
        endif
    endfor

    return ''
endfunction

function! ale_linters#vue#volar#GetInitializationOptions(buffer) abort
    let l:tsserver_path = ale#path#FindNearestDirectory(a:buffer, 'node_modules/typescript/lib')

    if l:tsserver_path is# ''
        " no-custom-checks
        echohl WarningMsg
        " no-custom-checks
        echom '[volar] Must have typescript installed in project, please install via `npm install -D typescript`.'
        " no-custom-checks
        echohl None
    endif

    let l:init_options = ale#Var(a:buffer, 'vue_volar_init_options')
    let l:init_options.typescript.tsdk = l:tsserver_path

    return l:init_options
endfunction

call ale#linter#Define('vue', {
\   'name': 'volar',
\   'language': 'vue',
\   'lsp': 'stdio',
\   'executable': {b -> ale#path#FindExecutable(b, 'vue_volar', ['node_modules/.bin/vue-language-server'])},
\   'command': '%e --stdio',
\   'project_root': function('ale_linters#vue#volar#GetProjectRoot'),
\   'initialization_options': function('ale_linters#vue#volar#GetInitializationOptions'),
\})
