" Author: MTDL9 <https://github.com/MTDL9>
" Description: Support for the Kotlin language server https://github.com/fwcd/KotlinLanguageServer

call ale#Set('kotlin_languageserver_executable', 'kotlin-language-server')

function! ale_linters#kotlin#languageserver#GetProjectRoot(buffer) abort
    let l:gradle_root = ale#gradle#FindProjectRoot(a:buffer)

    if !empty(l:gradle_root)
        return l:gradle_root
    endif

    let l:maven_pom_file = ale#path#FindNearestFile(a:buffer, 'pom.xml')

    if !empty(l:maven_pom_file)
        return fnamemodify(l:maven_pom_file, ':h')
    endif

    return ''
endfunction

call ale#linter#Define('kotlin', {
\   'name': 'languageserver',
\   'lsp': 'stdio',
\   'executable_callback': ale#VarFunc('kotlin_languageserver_executable'),
\   'command_callback': '%e',
\   'language': 'kotlin',
\   'project_root_callback': 'ale_linters#kotlin#languageserver#GetProjectRoot',
\})
