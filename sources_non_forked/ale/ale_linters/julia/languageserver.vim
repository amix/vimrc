" Author: Bartolomeo Stellato <bartolomeo.stellato@gmail.com>
" Description: A language server for Julia

" Set julia executable variable
call ale#Set('julia_executable', 'julia')

function! ale_linters#julia#languageserver#GetCommand(buffer) abort
    let l:julia_executable = ale#Var(a:buffer, 'julia_executable')
    let l:cmd_string = 'using LanguageServer; server = LanguageServer.LanguageServerInstance(isdefined(Base, :stdin) ? stdin : STDIN, isdefined(Base, :stdout) ? stdout : STDOUT, false); server.runlinter = true; run(server);'

    return ale#Escape(l:julia_executable) . ' --startup-file=no --history-file=no -e ' . ale#Escape(l:cmd_string)
endfunction

call ale#linter#Define('julia', {
\   'name': 'languageserver',
\   'lsp': 'stdio',
\   'executable_callback': ale#VarFunc('julia_executable'),
\   'command_callback': 'ale_linters#julia#languageserver#GetCommand',
\   'language': 'julia',
\   'project_root_callback': 'ale#julia#FindProjectRoot',
\})
