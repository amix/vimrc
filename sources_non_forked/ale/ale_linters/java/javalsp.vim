" Author: Horacio Sanson <https://github.com/hsanson>
" Description: Support for the Java language server https://github.com/georgewfraser/vscode-javac

call ale#Set('java_javalsp_jar', 'javacs.jar')

function! ale_linters#java#javalsp#Executable(buffer) abort
    return 'java'
endfunction

function! ale_linters#java#javalsp#Command(buffer) abort
    let l:jar = ale#Var(a:buffer, 'java_javalsp_jar')

    return ale#Escape('java -cp ' . l:jar . ' -Xverify:none org.javacs.Main')
endfunction

call ale#linter#Define('java', {
\   'name': 'javalsp',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#java#javalsp#Executable',
\   'command_callback': 'ale_linters#java#javalsp#Command',
\   'language': 'java',
\   'project_root_callback': 'ale#java#FindProjectRoot',
\})
