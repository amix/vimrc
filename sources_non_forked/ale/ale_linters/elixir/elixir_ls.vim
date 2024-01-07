" Author: Jon Parise <jon@indelible.org>
" Description: ElixirLS integration (https://github.com/elixir-lsp/elixir-ls)

call ale#Set('elixir_elixir_ls_release', 'elixir-ls')
call ale#Set('elixir_elixir_ls_config', {})

function! ale_linters#elixir#elixir_ls#GetExecutable(buffer) abort
    let l:dir = ale#path#Simplify(ale#Var(a:buffer, 'elixir_elixir_ls_release'))
    let l:cmd = has('win32') ? '\language_server.bat' : '/language_server.sh'

    return l:dir . l:cmd
endfunction

call ale#linter#Define('elixir', {
\   'name': 'elixir_ls',
\   'aliases': ['elixir-ls', 'elixirls'],
\   'lsp': 'stdio',
\   'executable': function('ale_linters#elixir#elixir_ls#GetExecutable'),
\   'command': function('ale_linters#elixir#elixir_ls#GetExecutable'),
\   'project_root': function('ale#handlers#elixir#FindMixUmbrellaRoot'),
\   'lsp_config': {b -> ale#Var(b, 'elixir_elixir_ls_config')},
\})
