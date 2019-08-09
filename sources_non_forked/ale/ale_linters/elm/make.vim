" Author: buffalocoder - https://github.com/buffalocoder, soywod - https://github.com/soywod, hecrj - https://github.com/hecrj
" Description: Elm linting in Ale. Closely follows the Syntastic checker in https://github.com/ElmCast/elm-vim.

call ale#Set('elm_make_executable', 'elm')
call ale#Set('elm_make_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#elm#make#Handle(buffer, lines) abort
    let l:output = []
    let l:unparsed_lines = []

    for l:line in a:lines
        if l:line[0] is# '{'
            " Elm 0.19
            call ale_linters#elm#make#HandleElm019Line(l:line, l:output)
        elseif l:line[0] is# '['
            " Elm 0.18
            call ale_linters#elm#make#HandleElm018Line(l:line, l:output)
        elseif l:line isnot# 'Successfully generated /dev/null'
            call add(l:unparsed_lines, l:line)
        endif
    endfor

    if len(l:unparsed_lines) > 0
        call add(l:output, {
        \    'lnum': 1,
        \    'type': 'E',
        \    'text': l:unparsed_lines[0],
        \    'detail': join(l:unparsed_lines, "\n")
        \})
    endif

    return l:output
endfunction

function! ale_linters#elm#make#HandleElm019Line(line, output) abort
    let l:report = json_decode(a:line)

    if l:report.type is? 'error'
        " General problem
        let l:details = ale_linters#elm#make#ParseMessage(l:report.message)

        if empty(l:report.path)
            let l:report.path = 'Elm'
        endif

        if ale_linters#elm#make#FileIsBuffer(l:report.path)
            call add(a:output, {
            \    'lnum': 1,
            \    'type': 'E',
            \    'text': l:details,
            \})
        else
            call add(a:output, {
            \    'lnum': 1,
            \    'type': 'E',
            \    'text': l:report.path .' - '. l:details,
            \    'detail': l:report.path ." ----------\n\n". l:details,
            \})
        endif
    else
        " Compilation errors
        for l:error in l:report.errors
            let l:file_is_buffer = ale_linters#elm#make#FileIsBuffer(l:error.path)

            for l:problem in l:error.problems
                let l:details = ale_linters#elm#make#ParseMessage(l:problem.message)

                if l:file_is_buffer
                    " Buffer module has problems
                    call add(a:output, {
                    \    'lnum': l:problem.region.start.line,
                    \    'col': l:problem.region.start.column,
                    \    'end_lnum': l:problem.region.end.line,
                    \    'end_col': l:problem.region.end.column,
                    \    'type': 'E',
                    \    'text': l:details,
                    \})
                else
                    " Imported module has problems
                    let l:location = l:error.path .':'. l:problem.region.start.line
                    call add(a:output, {
                    \    'lnum': 1,
                    \    'type': 'E',
                    \    'text': l:location .' - '. l:details,
                    \    'detail': l:location ." ----------\n\n". l:details,
                    \})
                endif
            endfor
        endfor
    endif
endfunction

function! ale_linters#elm#make#HandleElm018Line(line, output) abort
    let l:errors = json_decode(a:line)

    for l:error in l:errors
        let l:file_is_buffer = ale_linters#elm#make#FileIsBuffer(l:error.file)

        if l:file_is_buffer
            " Current buffer has problems
            call add(a:output, {
            \    'lnum': l:error.region.start.line,
            \    'col': l:error.region.start.column,
            \    'end_lnum': l:error.region.end.line,
            \    'end_col': l:error.region.end.column,
            \    'type': (l:error.type is? 'error') ? 'E' : 'W',
            \    'text': l:error.overview,
            \    'detail': l:error.overview . "\n\n" . l:error.details
            \})
        elseif l:error.type is? 'error'
            " Imported module has errors
            let l:location = l:error.file .':'. l:error.region.start.line

            call add(a:output, {
            \    'lnum': 1,
            \    'type': 'E',
            \    'text': l:location .' - '. l:error.overview,
            \    'detail': l:location ." ----------\n\n". l:error.overview . "\n\n" . l:error.details
            \})
        endif
    endfor
endfunction

function! ale_linters#elm#make#FileIsBuffer(path) abort
    return ale#path#IsTempName(a:path)
endfunction

function! ale_linters#elm#make#ParseMessage(message) abort
    return join(map(copy(a:message), 'ale_linters#elm#make#ParseMessageItem(v:val)'), '')
endfunction

function! ale_linters#elm#make#ParseMessageItem(item) abort
    if type(a:item) is v:t_string
        return a:item
    else
        return a:item.string
    endif
endfunction

function! ale_linters#elm#make#GetPackageFile(buffer) abort
    let l:elm_json = ale#path#FindNearestFile(a:buffer, 'elm.json')

    if empty(l:elm_json)
        " Fallback to Elm 0.18
        let l:elm_json = ale#path#FindNearestFile(a:buffer, 'elm-package.json')
    endif

    return l:elm_json
endfunction

function! ale_linters#elm#make#IsVersionGte19(buffer) abort
    let l:elm_json = ale_linters#elm#make#GetPackageFile(a:buffer)

    if l:elm_json =~# '-package'
        return 0
    else
        return 1
    endif
endfunction

function! ale_linters#elm#make#GetRootDir(buffer) abort
    let l:elm_json = ale_linters#elm#make#GetPackageFile(a:buffer)

    if empty(l:elm_json)
        return ''
    else
        return fnamemodify(l:elm_json, ':p:h')
    endif
endfunction

function! ale_linters#elm#make#IsTest(buffer) abort
    let l:root_dir = ale_linters#elm#make#GetRootDir(a:buffer)

    if empty(l:root_dir)
        return 0
    endif

    let l:tests_dir = join([l:root_dir, 'tests', ''], has('win32') ? '\' : '/')

    let l:buffer_path = fnamemodify(bufname(a:buffer), ':p')

    if stridx(l:buffer_path, l:tests_dir) == 0
        return 1
    else
        return 0
    endif
endfunction

" Return the command to execute the linter in the projects directory.
" If it doesn't, then this will fail when imports are needed.
function! ale_linters#elm#make#GetCommand(buffer) abort
    let l:executable = ale_linters#elm#make#GetExecutable(a:buffer)
    let l:root_dir = ale_linters#elm#make#GetRootDir(a:buffer)
    let l:is_v19 = ale_linters#elm#make#IsVersionGte19(a:buffer)
    let l:is_using_elm_test = l:executable =~# 'elm-test$'

    if empty(l:root_dir)
        let l:dir_set_cmd = ''
    else
        let l:dir_set_cmd = 'cd ' . ale#Escape(l:root_dir) . ' && '
    endif

    " elm-test needs to know the path of elm-make if elm isn't installed globally.
    " https://github.com/rtfeldman/node-test-runner/blob/57728f10668f2d2ab3179e7e3208bcfa9a1f19aa/README.md#--compiler
    if l:is_v19 && l:is_using_elm_test
        let l:elm_make_executable = ale#node#FindExecutable(a:buffer, 'elm_make', ['node_modules/.bin/elm'])
        let l:elm_test_compiler_flag = ' --compiler ' . l:elm_make_executable . ' '
    else
        let l:elm_test_compiler_flag = ' '
    endif

    " The elm compiler, at the time of this writing, uses '/dev/null' as
    " a sort of flag to tell the compiler not to generate an output file,
    " which is why this is hard coded here.
    " Source: https://github.com/elm-lang/elm-compiler/blob/19d5a769b30ec0b2fc4475985abb4cd94cd1d6c3/builder/src/Generate/Output.hs#L253
    return l:dir_set_cmd . '%e make --report=json --output=/dev/null' . l:elm_test_compiler_flag . '%t'
endfunction

function! ale_linters#elm#make#GetExecutable(buffer) abort
    let l:is_test = ale_linters#elm#make#IsTest(a:buffer)
    let l:is_v19 = ale_linters#elm#make#IsVersionGte19(a:buffer)

    if l:is_test && l:is_v19
        return ale#node#FindExecutable(
        \   a:buffer,
        \   'elm_make',
        \   ['node_modules/.bin/elm-test', 'node_modules/.bin/elm']
        \)
    else
        return ale#node#FindExecutable(a:buffer, 'elm_make', ['node_modules/.bin/elm'])
    endif
endfunction

call ale#linter#Define('elm', {
\   'name': 'make',
\   'executable': function('ale_linters#elm#make#GetExecutable'),
\   'output_stream': 'both',
\   'command': function('ale_linters#elm#make#GetCommand'),
\   'callback': 'ale_linters#elm#make#Handle'
\})
