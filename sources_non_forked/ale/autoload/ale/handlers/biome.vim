" Author: Filip Gospodinov <f@gospodinov.ch>
" Description: Functions for working with biome, for checking or fixing files.

call ale#Set('biome_executable', 'biome')
call ale#Set('biome_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('biome_options', '')
call ale#Set('biome_fixer_apply_unsafe', 0)
call ale#Set('biome_lsp_project_root', '')

function! ale#handlers#biome#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'biome', [
    \   'node_modules/@biomejs/cli-linux-x64/biome',
    \   'node_modules/@biomejs/cli-linux-arm64/biome',
    \   'node_modules/@biomejs/cli-win32-x64/biome.exe',
    \   'node_modules/@biomejs/cli-win32-arm64/biome.exe',
    \   'node_modules/@biomejs/cli-darwin-x64/biome',
    \   'node_modules/@biomejs/cli-darwin-arm64/biome',
    \   'node_modules/.bin/biome',
    \])
endfunction

function! ale#handlers#biome#GetLanguage(buffer) abort
    return getbufvar(a:buffer, '&filetype')
endfunction

function! ale#handlers#biome#GetProjectRoot(buffer) abort
    let l:project_root = ale#Var(a:buffer, 'biome_lsp_project_root')

    if !empty(l:project_root)
        return l:project_root
    endif

    let l:possible_project_roots = [
    \   'biome.json',
    \   'biome.jsonc',
    \   'package.json',
    \   '.git',
    \   bufname(a:buffer),
    \]

    for l:possible_root in l:possible_project_roots
        let l:project_root = ale#path#FindNearestFile(a:buffer, l:possible_root)

        if empty(l:project_root)
            let l:project_root = ale#path#FindNearestDirectory(a:buffer, l:possible_root)
        endif

        if !empty(l:project_root)
            " dir:p expands to /full/path/to/dir/ whereas
            " file:p expands to /full/path/to/file (no trailing slash)
            " Appending '/' ensures that :h:h removes the path's last segment
            " regardless of whether it is a directory or not.
            return fnamemodify(l:project_root . '/', ':p:h:h')
        endif
    endfor

    return ''
endfunction
