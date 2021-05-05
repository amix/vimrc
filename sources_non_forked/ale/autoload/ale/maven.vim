" Description: Functions for working with Maven projects.
"
" Given a buffer number, find a Maven project root.
function! ale#maven#FindProjectRoot(buffer) abort
    let l:wrapper_path = ale#path#FindNearestFile(a:buffer, 'mvnw')

    if !empty(l:wrapper_path)
        return fnamemodify(l:wrapper_path, ':h')
    endif

    let l:pom_path = ale#path#FindNearestFile(a:buffer, 'pom.xml')

    if !empty(l:pom_path)
        return fnamemodify(l:pom_path, ':h')
    endif

    return ''
endfunction

" Given a buffer number, find the path to the executable.
" First search on the path for 'mvnw' (mvnw.cmd on Windows), if nothing is found,
" try the global command. Returns an empty string if cannot find the executable.
function! ale#maven#FindExecutable(buffer) abort
    let l:wrapper_cmd = has('unix') ? 'mvnw' : 'mvnw.cmd'
    let l:wrapper_path = ale#path#FindNearestFile(a:buffer, l:wrapper_cmd)

    if !empty(l:wrapper_path) && executable(l:wrapper_path)
        return l:wrapper_path
    endif

    if executable('mvn')
        return 'mvn'
    endif

    return ''
endfunction

" Given a buffer number, get a working directory and command to print the
" classpath of the root project.
"
" Returns an empty string for the command if Maven is not detected.
function! ale#maven#BuildClasspathCommand(buffer) abort
    let l:executable = ale#maven#FindExecutable(a:buffer)

    if !empty(l:executable)
        let l:project_root = ale#maven#FindProjectRoot(a:buffer)

        if !empty(l:project_root)
            return [
            \   l:project_root,
            \   ale#Escape(l:executable) . ' dependency:build-classpath'
            \]
        endif
    endif

    return ['', '']
endfunction
