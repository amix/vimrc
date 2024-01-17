" Author: w0rp <devw0rp@gmail.com>
" Description: Code for ignoring linters. Only loaded and if configured.

" A map for remapping lspconfig server names to linter names or aliases in
" ALE. We should change the names where they will conflict with names in ALE.
"
" Notes on names from nvim-lspconfig not included here.
"
" * 'rubocop' is run in a language server mode
" * 'eslint' is run via 'vscode-eslint-language-server'
let s:lspconfig_map = {
\   'als': 'adals',
\   'ansiblels': 'ansible-language-server',
\   'bicep': 'bicep_language_server',
\   'cmake': 'cmake_language_server',
\   'denols': 'deno',
\   'erlangls': 'erlang_ls',
\   'html': 'vscodehtml',
\   'ocamlls': 'ocaml-language-server',
\   'ols': 'odin-lsp',
\   'puppet': 'puppet_languageserver',
\}

" Given a filetype and a configuration for ignoring linters, return a List of
" Strings for linter names to ignore.
function! ale#engine#ignore#GetList(filetype, config) abort
    if type(a:config) is v:t_list
        return a:config
    endif

    if type(a:config) is v:t_dict
        let l:names_to_remove = []

        for l:part in split(a:filetype , '\.')
            call extend(l:names_to_remove, get(a:config, l:part, []))
        endfor

        return l:names_to_remove
    endif

    return []
endfunction

" This function can be mocked in tests.
function! ale#engine#ignore#GetLSPConfigNames() abort
    return luaeval('require ''ale.util''.configured_lspconfig_servers()')
endfunction

function! s:GetMappedLSPConfigNames() abort
    " Check the lspconfig flag before calling luaeval.
    if !get(g:, 'lspconfig', 0)
        return []
    endif

    let l:lspconfig_servers = ale#engine#ignore#GetLSPConfigNames()

    return map(
    \   !empty(l:lspconfig_servers) ? l:lspconfig_servers : [],
    \   {_, val -> get(s:lspconfig_map, val, val) }
    \)
endfunction

" Given a List of linter descriptions, exclude the linters to be ignored.
function! ale#engine#ignore#Exclude(filetype, all_linters, config, disable_lsp) abort
    let l:names_to_remove = ale#engine#ignore#GetList(a:filetype, a:config)

    " If configured to automatically ignore otherwise configured LSP linter
    " names, add them to the names to remove. This could ignore linters
    " with matching names that are not marked as LSP linters.
    if a:disable_lsp is# 'auto'
        call extend(l:names_to_remove, s:GetMappedLSPConfigNames())
    endif

    let l:ignore_all_lsps = a:disable_lsp is 1 || a:disable_lsp is v:true
    let l:filtered_linters = []

    for l:linter in a:all_linters
        let l:should_include = index(l:names_to_remove, l:linter.name) == -1
        let l:i = 0

        while l:should_include && l:i < len(l:linter.aliases)
            let l:name = l:linter.aliases[l:i]
            let l:should_include = index(l:names_to_remove, l:name) == -1
            let l:i += 1
        endwhile

        if l:should_include && l:ignore_all_lsps
            let l:should_include = empty(get(l:linter, 'lsp'))
        endif

        if l:should_include
            call add(l:filtered_linters, l:linter)
        endif
    endfor

    return l:filtered_linters
endfunction
