" Author: Fred Emmott <fe@fb.com>
" Description: Hack support via `hack lsp`

call ale#Set('hack_hack_executable', 'hh_client')

function! ale_linters#hack#hack#GetProjectRoot(buffer) abort
    let l:hhconfig = ale#path#FindNearestFile(a:buffer, '.hhconfig')

    return !empty(l:hhconfig) ? fnamemodify(l:hhconfig, ':h') : ''
endfunction

function! ale_linters#hack#hack#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'hack_hack_executable')
endfunction

call ale#linter#Define('hack', {
\   'name': 'hack',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#hack#hack#GetExecutable'),
\   'command': '%e lsp --from vim-ale',
\   'project_root': function('ale_linters#hack#hack#GetProjectRoot'),
\})
