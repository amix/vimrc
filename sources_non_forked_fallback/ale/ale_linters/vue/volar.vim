" Author: Arnold Chand <creativenull@outlook.com>
" Description: Volar Language Server integration for ALE adopted from
"              nvim-lspconfig and volar/packages/shared/src/types.ts

call ale#Set('vue_volar_executable', 'vue-language-server')
call ale#Set('vue_volar_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('vue_volar_init_options', {
\   'documentFeatures': {
\       'documentColor': v:false,
\       'documentFormatting': {
\           'defaultPrintWidth': 100,
\       },
\       'documentSymbol': v:true,
\       'foldingRange': v:true,
\       'linkedEditingRange': v:true,
\       'selectionRange': v:true,
\   },
\   'languageFeatures': {
\       'callHierarchy': v:true,
\       'codeAction': v:true,
\       'codeLens': v:true,
\       'completion': {
\           'defaultAttrNameCase': 'kebabCase',
\           'defaultTagNameCase': 'both',
\           'getDocumentNameCaseRequest': v:false,
\           'getDocumentSelectionRequest': v:false,
\       },
\       'definition': v:true,
\       'diagnostics': v:true,
\       'documentHighlight': v:true,
\       'documentLink': v:true,
\       'hover': v:true,
\       'references': v:true,
\       'rename': v:true,
\       'renameFileRefactoring': v:true,
\       'schemaRequestService': v:true,
\       'semanticTokens': v:false,
\       'signatureHelp': v:true,
\       'typeDefinition': v:true,
\       'workspaceSymbol': v:false,
\   },
\   'typescript': {
\       'serverPath': '',
\       'localizedPath': v:null,
\   },
\})

function! ale_linters#vue#volar#GetProjectRoot(buffer) abort
    let l:project_roots = ['package.json', 'vite.config.js', '.git', bufname(a:buffer)]

    for l:project_root in l:project_roots
        let l:nearest_filepath = ale#path#FindNearestFile(a:buffer, l:project_root)

        if !empty(l:nearest_filepath)
            return fnamemodify(l:nearest_filepath, ':h')
        endif
    endfor

    return ''
endfunction

function! ale_linters#vue#volar#GetInitializationOptions(buffer) abort
    let l:tsserver_path = ale#path#FindNearestExecutable(a:buffer, [
    \   'node_modules/typescript/lib/tsserverlibrary.js'
    \ ])
    let l:init_options = ale#Var(a:buffer, 'vue_volar_init_options')
    let l:init_options.typescript.serverPath = l:tsserver_path

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
