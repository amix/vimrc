" Author: ophirr33 <coghlan.ty@gmail.com>
" Description: TCP lsp client for sbt Server

call ale#Set('scala_sbtserver_address', '127.0.0.1:4273')
call ale#Set('scala_sbtserver_project_root', '')

function! ale_linters#scala#sbtserver#GetProjectRoot(buffer) abort
    let l:project_root = ale#Var(a:buffer, 'scala_sbtserver_project_root')

    if l:project_root is? ''
        let l:project_root = ale#path#FindNearestFile(a:buffer, 'build.sbt')

        return !empty(l:project_root) ? fnamemodify(l:project_root, ':h') : ''
    endif

    return l:project_root
endfunction

function! ale_linters#scala#sbtserver#GetAddress(buffer) abort
    let l:address = ale#Var(a:buffer, 'scala_sbtserver_address')

    return l:address
endfunction

call ale#linter#Define('scala', {
\   'name': 'sbtserver',
\   'lsp': 'socket',
\   'address': function('ale_linters#scala#sbtserver#GetAddress'),
\   'language': 'scala',
\   'project_root': function('ale_linters#scala#sbtserver#GetProjectRoot'),
\})
