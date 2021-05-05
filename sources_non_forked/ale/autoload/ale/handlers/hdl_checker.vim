" Author:      suoto <andre820@gmail.com>
" Description: Adds support for HDL Code Checker, which wraps vcom/vlog, ghdl
"              or xvhdl. More info on https://github.com/suoto/hdl_checker

call ale#Set('hdl_checker_executable', 'hdl_checker')
call ale#Set('hdl_checker_config_file', has('unix') ? '.hdl_checker.config' : '_hdl_checker.config')
call ale#Set('hdl_checker_options', '')

" Use this as a function so we can mock it on testing. Need to do this because
" test files are inside /testplugin (which refers to the ale repo), which will
" always have a .git folder
function! ale#handlers#hdl_checker#IsDotGit(path) abort
    return ! empty(a:path) && isdirectory(a:path)
endfunction

" Sould return (in order of preference)
" 1. Nearest config file
" 2. Nearest .git directory
" 3. The current path
function! ale#handlers#hdl_checker#GetProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(
    \   a:buffer,
    \   ale#Var(a:buffer, 'hdl_checker_config_file'))

    if !empty(l:project_root)
        return fnamemodify(l:project_root, ':h')
    endif

    " Search for .git to use as root
    let l:project_root = ale#path#FindNearestDirectory(a:buffer, '.git')

    if ale#handlers#hdl_checker#IsDotGit(l:project_root)
        return fnamemodify(l:project_root, ':h:h')
    endif

    return ''
endfunction

function! ale#handlers#hdl_checker#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'hdl_checker_executable')
endfunction

function! ale#handlers#hdl_checker#GetCommand(buffer) abort
    let l:command = ale#Escape(ale#handlers#hdl_checker#GetExecutable(a:buffer)) . ' --lsp'

    " Add extra parameters only if config has been set
    let l:options = ale#Var(a:buffer, 'hdl_checker_options')

    if ! empty(l:options)
        let l:command = l:command . ' ' . l:options
    endif

    return l:command
endfunction

" To allow testing
function! ale#handlers#hdl_checker#GetInitOptions(buffer) abort
    return {'project_file': ale#Var(a:buffer, 'hdl_checker_config_file')}
endfunction

" Define the hdl_checker linter for a given filetype.
function! ale#handlers#hdl_checker#DefineLinter(filetype) abort
    call ale#linter#Define(a:filetype, {
    \   'name': 'hdl-checker',
    \   'lsp': 'stdio',
    \   'language': a:filetype,
    \   'executable': function('ale#handlers#hdl_checker#GetExecutable'),
    \   'command': function('ale#handlers#hdl_checker#GetCommand'),
    \   'project_root': function('ale#handlers#hdl_checker#GetProjectRoot'),
    \   'initialization_options': function('ale#handlers#hdl_checker#GetInitOptions'),
    \ })
endfunction

