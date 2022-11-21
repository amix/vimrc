" Author: Bartolomeo Stellato <bartolomeo.stellato@gmail.com>
" Description: A language server for Julia

" Set julia executable variable
call ale#Set('julia_executable', 'julia')

function! ale_linters#julia#languageserver#GetCommand(buffer) abort
    let l:julia_executable = ale#Var(a:buffer, 'julia_executable')
    let l:cmd_string = 'using LanguageServer; using Pkg; import StaticLint; import SymbolServer; server = LanguageServer.LanguageServerInstance(isdefined(Base, :stdin) ? stdin : STDIN, isdefined(Base, :stdout) ? stdout : STDOUT, dirname(Pkg.Types.Context().env.project_file)); server.runlinter = true; run(server);'

    return ale#Escape(l:julia_executable) . ' --project=@. --startup-file=no --history-file=no -e ' . ale#Escape(l:cmd_string)
endfunction

call ale#linter#Define('julia', {
\   'name': 'languageserver',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'julia_executable')},
\   'command': function('ale_linters#julia#languageserver#GetCommand'),
\   'language': 'julia',
\   'project_root': function('ale#julia#FindProjectRoot'),
\})
