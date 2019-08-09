" Author: Francis Agyapong <francisgyapong2@gmail.com>
" Description: A linter for the Kotlin programming language that uses kotlinc

let g:ale_kotlin_kotlinc_options = get(g:, 'ale_kotlin_kotlinc_options', '')
let g:ale_kotlin_kotlinc_enable_config = get(g:, 'ale_kotlin_kotlinc_enable_config', 0)
let g:ale_kotlin_kotlinc_config_file = get(g:, 'ale_kotlin_kotlinc_config_file', '.ale_kotlinc_config')
let g:ale_kotlin_kotlinc_classpath = get(g:, 'ale_kotlin_kotlinc_classpath', '')
let g:ale_kotlin_kotlinc_sourcepath = get(g:, 'ale_kotlin_kotlinc_sourcepath', '')
let g:ale_kotlin_kotlinc_use_module_file = get(g:, 'ale_kotlin_kotlinc_use_module_file', 0)
let g:ale_kotlin_kotlinc_module_filename = get(g:, 'ale_kotlin_kotlinc_module_filename', 'module.xml')

let s:classpath_sep = has('unix') ? ':' : ';'

function! ale_linters#kotlin#kotlinc#RunWithImportPaths(buffer) abort
    let l:command = ''

    " exec maven/gradle only if classpath is not set
    if ale#Var(a:buffer, 'kotlin_kotlinc_classpath') isnot# ''
        return ale_linters#kotlin#kotlinc#GetCommand(a:buffer, [], {})
    endif

    let l:pom_path = ale#path#FindNearestFile(a:buffer, 'pom.xml')

    if !empty(l:pom_path) && executable('mvn')
        let l:command = ale#path#CdString(fnamemodify(l:pom_path, ':h'))
        \   . 'mvn dependency:build-classpath'
    endif

    " Try to use Gradle if Maven isn't available.
    if empty(l:command)
        let l:command = ale#gradle#BuildClasspathCommand(a:buffer)
    endif

    if empty(l:command)
        return ale_linters#kotlin#kotlinc#GetCommand(a:buffer, [], {})
    endif

    return ale#command#Run(
    \   a:buffer,
    \   l:command,
    \   function('ale_linters#kotlin#kotlinc#GetCommand')
    \)
endfunction

function! s:BuildClassPathOption(buffer, import_paths) abort
    " Filter out lines like [INFO], etc.
    let l:class_paths = filter(a:import_paths[:], 'v:val !~# ''[''')
    call extend(
    \   l:class_paths,
    \   split(ale#Var(a:buffer, 'kotlin_kotlinc_classpath'), s:classpath_sep),
    \)

    return !empty(l:class_paths)
    \   ? ' -cp ' . ale#Escape(join(l:class_paths, s:classpath_sep))
    \   : ''
endfunction

function! ale_linters#kotlin#kotlinc#GetCommand(buffer, import_paths, meta) abort
    let l:kotlinc_opts = ale#Var(a:buffer, 'kotlin_kotlinc_options')
    let l:command = 'kotlinc '

    " If the config file is enabled and readable, source it
    if ale#Var(a:buffer, 'kotlin_kotlinc_enable_config')
        let l:conf = expand(ale#Var(a:buffer, 'kotlin_kotlinc_config_file'), 1)

        if filereadable(l:conf)
            execute 'source ' . fnameescape(l:conf)
        endif
    endif

    " If use module and module file is readable use that and return
    if ale#Var(a:buffer, 'kotlin_kotlinc_use_module_file')
        let l:module_filename = ale#Escape(expand(ale#Var(a:buffer, 'kotlin_kotlinc_module_filename'), 1))

        if filereadable(l:module_filename)
            let l:kotlinc_opts .= ' -module ' . l:module_filename
            let l:command .= 'kotlinc ' . l:kotlinc_opts

            return l:command
        endif
    endif

    " We only get here if not using module or the module file not readable
    if ale#Var(a:buffer, 'kotlin_kotlinc_classpath') isnot# ''
        let l:kotlinc_opts .= ' -cp ' . ale#Var(a:buffer, 'kotlin_kotlinc_classpath')
    else
        " get classpath from maven/gradle
        let l:kotlinc_opts .= s:BuildClassPathOption(a:buffer, a:import_paths)
    endif

    let l:fname = ''

    if ale#Var(a:buffer, 'kotlin_kotlinc_sourcepath') isnot# ''
        let l:fname .= expand(ale#Var(a:buffer, 'kotlin_kotlinc_sourcepath'), 1) . ' '
    else
        " Find the src directory for files in this project.
        let l:project_root = ale#gradle#FindProjectRoot(a:buffer)

        if !empty(l:project_root)
            let l:src_dir = l:project_root
        else
            let l:src_dir = ale#path#FindNearestDirectory(a:buffer, 'src/main/java')
            \   . ' ' . ale#path#FindNearestDirectory(a:buffer, 'src/main/kotlin')
        endif

        let l:fname .= expand(l:src_dir, 1) . ' '
    endif

    let l:fname .= ale#Escape(expand('#' . a:buffer . ':p'))
    let l:command .= l:kotlinc_opts . ' ' . l:fname

    return l:command
endfunction

function! ale_linters#kotlin#kotlinc#Handle(buffer, lines) abort
    let l:code_pattern = '^\(.*\):\([0-9]\+\):\([0-9]\+\):\s\+\(error\|warning\):\s\+\(.*\)'
    let l:general_pattern = '^\(warning\|error\|info\):\s*\(.*\)'
    let l:output = []

    for l:line in a:lines
        let l:match = matchlist(l:line, l:code_pattern)

        if len(l:match) == 0
            continue
        endif

        let l:file = l:match[1]
        let l:line = l:match[2] + 0
        let l:column = l:match[3] + 0
        let l:type = l:match[4]
        let l:text = l:match[5]

        let l:buf_abspath = fnamemodify(l:file, ':p')
        let l:curbuf_abspath = expand('#' . a:buffer . ':p')

        " Skip if file is not loaded
        if l:buf_abspath isnot# l:curbuf_abspath
            continue
        endif

        let l:type_marker_str = l:type is# 'warning' ? 'W' : 'E'

        call add(l:output, {
        \   'lnum': l:line,
        \   'col': l:column,
        \   'text': l:text,
        \   'type': l:type_marker_str,
        \})
    endfor

    " Non-code related messages
    for l:line in a:lines
        let l:match = matchlist(l:line, l:general_pattern)

        if len(l:match) == 0
            continue
        endif

        let l:type = l:match[1]
        let l:text = l:match[2]

        let l:type_marker_str = l:type is# 'warning' || l:type is# 'info' ? 'W' : 'E'

        call add(l:output, {
        \   'lnum': 1,
        \   'text': l:text,
        \   'type': l:type_marker_str,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('kotlin', {
\   'name': 'kotlinc',
\   'executable': 'kotlinc',
\   'command': function('ale_linters#kotlin#kotlinc#RunWithImportPaths'),
\   'callback': 'ale_linters#kotlin#kotlinc#Handle',
\   'lint_file': 1,
\})
