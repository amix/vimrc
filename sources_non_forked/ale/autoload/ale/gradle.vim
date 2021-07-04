" Author: Michael Pardo <michael@michaelpardo.com>
" Description: Functions for working with Gradle projects.

let s:script_path = fnamemodify(resolve(expand('<sfile>:p')), ':h')
let s:init_path = has('win32')
\   ? s:script_path . '\gradle\init.gradle'
\   : s:script_path . '/gradle/init.gradle'

function! ale#gradle#GetInitPath() abort
    return s:init_path
endfunction

" Given a buffer number, find a Gradle project root.
function! ale#gradle#FindProjectRoot(buffer) abort
    let l:gradlew_path = ale#path#FindNearestFile(a:buffer, 'gradlew')

    if !empty(l:gradlew_path)
        return fnamemodify(l:gradlew_path, ':h')
    endif

    let l:settings_path = ale#path#FindNearestFile(a:buffer, 'settings.gradle')

    if !empty(l:settings_path)
        return fnamemodify(l:settings_path, ':h')
    endif

    let l:build_path = ale#path#FindNearestFile(a:buffer, 'build.gradle')

    if !empty(l:build_path)
        return fnamemodify(l:build_path, ':h')
    endif

    return ''
endfunction

" Given a buffer number, find the path to the executable.
" First search on the path for 'gradlew', if nothing is found, try the global
" command. Returns an empty string if cannot find the executable.
function! ale#gradle#FindExecutable(buffer) abort
    let l:gradlew_path = ale#path#FindNearestFile(a:buffer, 'gradlew')

    if !empty(l:gradlew_path)
        return l:gradlew_path
    endif

    if executable('gradle')
        return 'gradle'
    endif

    return ''
endfunction

" Given a buffer number, get a working directory and command to print the
" classpath of the root project.
"
" Returns an empty string for the command if Gradle is not detected.
function! ale#gradle#BuildClasspathCommand(buffer) abort
    let l:executable = ale#gradle#FindExecutable(a:buffer)

    if !empty(l:executable)
        let l:project_root = ale#gradle#FindProjectRoot(a:buffer)

        if !empty(l:project_root)
            return [
            \   l:project_root,
            \   ale#Escape(l:executable)
            \   . ' -I ' . ale#Escape(s:init_path)
            \   . ' -q printClasspath'
            \]
        endif
    endif

    return ['', '']
endfunction
