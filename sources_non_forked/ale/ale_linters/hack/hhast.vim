" Author: Fred Emmott <fe@fb.com>
" Description: Hack support via `hhast lsp`

call ale#Set('hack_hhast_executable', 'vendor/bin/hhast-lint')

function! ale_linters#hack#hhast#GetProjectRoot(buffer) abort
    " Find the hack root, then figure out if it's also an HHAST root.
    " Don't try to use lint configurations from vendor/foo/bar/hhast-lint.json
    let l:hhconfig = ale#path#FindNearestFile(a:buffer, '.hhconfig')

    if empty(l:hhconfig)
      return ''
    endif

    let l:root = fnamemodify(l:hhconfig, ':h')
    let l:hhast_config = findfile('hhast-lint.json', l:root)

    return !empty(l:hhast_config) ? l:root : ''
endfunction

function! ale_linters#hack#hhast#GetExecutable(buffer) abort
    let l:root = ale_linters#hack#hhast#GetProjectRoot(a:buffer)
    let l:relative = ale#Var(a:buffer, 'hack_hhast_executable')
    let l:absolute = findfile(l:relative, l:root)

    return !empty(l:absolute) ? l:absolute : ''
endfunction

function! ale_linters#hack#hhast#GetInitializationOptions(buffer) abort
    return {'lintMode': 'open-files'}
endfunction

call ale#linter#Define('hack', {
\   'name': 'hhast',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#hack#hhast#GetExecutable',
\   'command': '%e --mode lsp --from vim-ale',
\   'project_root_callback': 'ale_linters#hack#hhast#GetProjectRoot',
\   'initialization_options_callback': 'ale_linters#hack#hhast#GetInitializationOptions',
\})
