" Author: antew - https://github.com/antew
" Description: elm-language-server integration for elm (diagnostics, formatting, and more)

call ale#Set('elm_ls_executable', 'elm-language-server')
call ale#Set('elm_ls_use_global', get(g:, 'ale_use_global_executables', 1))

" elm-language-server will search for local and global binaries, if empty
call ale#Set('elm_ls_elm_path', '')
call ale#Set('elm_ls_elm_format_path', '')
call ale#Set('elm_ls_elm_test_path', '')
call ale#Set('elm_ls_elm_analyse_trigger', 'change')

function! elm_ls#GetRootDir(buffer) abort
    let l:elm_json = ale#path#FindNearestFile(a:buffer, 'elm.json')

    return !empty(l:elm_json) ? fnamemodify(l:elm_json, ':p:h') : ''
endfunction

function! elm_ls#GetOptions(buffer) abort
    return {
    \   'elmPath': ale#Var(a:buffer, 'elm_ls_elm_path'),
    \   'elmFormatPath': ale#Var(a:buffer, 'elm_ls_elm_format_path'),
    \   'elmTestPath': ale#Var(a:buffer, 'elm_ls_elm_test_path'),
    \   'elmAnalyseTrigger': ale#Var(a:buffer, 'elm_ls_elm_analyse_trigger'),
    \}
endfunction

call ale#linter#Define('elm', {
\   'name': 'elm_ls',
\   'lsp': 'stdio',
\   'executable': {b -> ale#path#FindExecutable(b, 'elm_ls', [
\       'node_modules/.bin/elm-language-server',
\       'node_modules/.bin/elm-lsp',
\       'elm-lsp'
\   ])},
\   'command': '%e --stdio',
\   'project_root': function('elm_ls#GetRootDir'),
\   'language': 'elm',
\   'initialization_options': function('elm_ls#GetOptions')
\})
