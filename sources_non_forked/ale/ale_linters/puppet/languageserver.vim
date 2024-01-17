" Author: Alexander Olofsson <alexander.olofsson@liu.se>
" Description: Puppet Language Server integration for ALE

call ale#Set('puppet_languageserver_executable', 'puppet-languageserver')

function! ale_linters#puppet#languageserver#GetProjectRoot(buffer) abort
    " Note: The metadata.json file is recommended for Puppet 4+ modules, but
    " there's no requirement to have it, so fall back to the other possible
    " Puppet module directories
    let l:root_path = ale#path#FindNearestFile(a:buffer, 'metadata.json')

    if !empty(l:root_path)
        return fnamemodify(l:root_path, ':h')
    endif

    for l:test_path in [
    \   'manifests',
    \   'templates',
    \]
        let l:root_path = ale#path#FindNearestDirectory(a:buffer, l:test_path)

        if !empty(l:root_path)
            return fnamemodify(l:root_path, ':h:h')
        endif
    endfor

    return ''
endfunction

call ale#linter#Define('puppet', {
\   'name': 'languageserver',
\   'aliases': ['puppet_languageserver'],
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'puppet_languageserver_executable')},
\   'command': '%e --stdio',
\   'language': 'puppet',
\   'project_root': function('ale_linters#puppet#languageserver#GetProjectRoot'),
\})
