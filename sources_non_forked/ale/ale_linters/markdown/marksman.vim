" Author: Peter Benjamin <petermbenjamin@gmail.com>
" Description: Write Markdown with code assist and intelligence in the comfort of your favourite editor.

call ale#Set('markdown_marksman_executable', 'marksman')

function! ale_linters#markdown#marksman#GetCommand(buffer) abort
    return '%e server'
endfunction

function! ale_linters#markdown#marksman#GetProjectRoot(buffer) abort
    " Find nearest .marksman.toml
    let l:marksman_toml = ale#path#FindNearestFile(a:buffer, '.marksman.toml')

    if !empty(l:marksman_toml)
        return fnamemodify(l:marksman_toml, ':h')
    endif

    " Find nearest .git/ directory
    let l:project_root = finddir('.git/..', expand('#' . a:buffer . '...').';')

    if !empty(l:project_root)
        return l:project_root
    endif

    return ''
endfunction

call ale#linter#Define('markdown', {
\   'name': 'marksman',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'markdown_marksman_executable')},
\   'command': function('ale_linters#markdown#marksman#GetCommand'),
\   'project_root': function('ale_linters#markdown#marksman#GetProjectRoot'),
\   'initialization_options': {},
\})
