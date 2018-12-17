" Author: Jon Parise <jon@indelible.org>
" Description: ElixirLS integration (https://github.com/JakeBecker/elixir-ls)

call ale#Set('elixir_elixir_ls_release', 'elixir-ls')
call ale#Set('elixir_elixir_ls_config', {})

function! ale_linters#elixir#elixir_ls#GetExecutable(buffer) abort
    let l:dir = ale#path#Simplify(ale#Var(a:buffer, 'elixir_elixir_ls_release'))
    let l:cmd = ale#Has('win32') ? '\language_server.bat' : '/language_server.sh'

    return l:dir . l:cmd
endfunction

call ale#linter#Define('elixir', {
\   'name': 'elixir-ls',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#elixir#elixir_ls#GetExecutable',
\   'command_callback': 'ale_linters#elixir#elixir_ls#GetExecutable',
\   'project_root_callback': 'ale#handlers#elixir#FindMixUmbrellaRoot',
\   'lsp_config_callback': ale#VarFunc('elixir_elixir_ls_config'),
\})
