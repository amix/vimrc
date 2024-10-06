" Author: antew - https://github.com/antew
" Description: elm-language-server integration for elm (diagnostics, formatting, and more)

call ale#Set('elm_ls_executable', 'elm-language-server')
call ale#Set('elm_ls_use_global', get(g:, 'ale_use_global_executables', 1))

" elm-language-server will search for local and global binaries, if empty
call ale#Set('elm_ls_elm_path', '')
call ale#Set('elm_ls_elm_format_path', '')
call ale#Set('elm_ls_elm_test_path', '')
call ale#Set('elm_ls_elm_analyse_trigger', 'change')

function! ale_linters#elm#ls#GetProjectRoot(buffer) abort
    let l:elm_json = ale#path#FindNearestFile(a:buffer, 'elm.json')

    return !empty(l:elm_json) ? fnamemodify(l:elm_json, ':p:h') : ''
endfunction

function! ale_linters#elm#ls#GetInitializationOptions(buffer) abort
    return {
    \   'elmPath': ale#Var(a:buffer, 'elm_ls_elm_path'),
    \   'elmFormatPath': ale#Var(a:buffer, 'elm_ls_elm_format_path'),
    \   'elmTestPath': ale#Var(a:buffer, 'elm_ls_elm_test_path'),
    \   'elmAnalyseTrigger': ale#Var(a:buffer, 'elm_ls_elm_analyse_trigger'),
    \}
endfunction

call ale#linter#Define('elm', {
\   'name': 'ls',
\   'aliases': ['elm_ls'],
\   'lsp': 'stdio',
\   'executable': {b -> ale#path#FindExecutable(b, 'elm_ls', [
\       'node_modules/.bin/elm-language-server',
\       'node_modules/.bin/elm-lsp',
\       'elm-lsp'
\   ])},
\   'command': '%e --stdio',
\   'project_root': function('ale_linters#elm#ls#GetProjectRoot'),
\   'language': 'elm',
\   'initialization_options': function('ale_linters#elm#ls#GetInitializationOptions')
\})
