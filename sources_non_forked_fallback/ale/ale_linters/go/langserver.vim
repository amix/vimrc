" Author: Horacio Sanson <https://github.com/hsanson>
" Description: Support for go-langserver https://github.com/sourcegraph/go-langserver

call ale#Set('go_langserver_executable', 'go-langserver')
call ale#Set('go_langserver_options', '')

function! ale_linters#go#langserver#GetCommand(buffer) abort
    let l:executable = [ale#Escape(ale#Var(a:buffer, 'go_langserver_executable'))]
    let l:options = ale#Var(a:buffer, 'go_langserver_options')
    let l:options = substitute(l:options, '-gocodecompletion', '', 'g')
    let l:options = filter(split(l:options, ' '), 'empty(v:val) != 1')

    if ale#Var(a:buffer, 'completion_enabled')
        call add(l:options, '-gocodecompletion')
    endif

    let l:options = uniq(sort(l:options))
    let l:env = ale#go#EnvString(a:buffer)

    return l:env . join(extend(l:executable, l:options), ' ')
endfunction

call ale#linter#Define('go', {
\   'name': 'golangserver',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'go_langserver_executable')},
\   'command': function('ale_linters#go#langserver#GetCommand'),
\   'project_root': function('ale#go#FindProjectRoot'),
\})
