" Author: buffalocoder - https://github.com/buffalocoder, soywod - https://github.com/soywod
" Description: Elm linting in Ale. Closely follows the Syntastic checker in https://github.com/ElmCast/elm-vim.

call ale#Set('elm_make_executable', 'elm-make')
call ale#Set('elm_make_use_global', 0)

function! ale_linters#elm#make#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'elm_make', [
    \   'node_modules/.bin/elm-make',
    \])
endfunction

function! ale_linters#elm#make#Handle(buffer, lines) abort
    let l:output = []
    let l:is_windows = has('win32')
    let l:temp_dir = l:is_windows ? $TMP : $TMPDIR
    let l:unparsed_lines = []
    for l:line in a:lines
        if l:line[0] is# '['
            let l:errors = json_decode(l:line)

            for l:error in l:errors
                " Check if file is from the temp directory.
                " Filters out any errors not related to the buffer.
                if l:is_windows
                    let l:file_is_buffer = l:error.file[0:len(l:temp_dir) - 1] is? l:temp_dir
                else
                    let l:file_is_buffer = l:error.file[0:len(l:temp_dir) - 1] is# l:temp_dir
                endif

                if l:file_is_buffer
                    call add(l:output, {
                    \    'lnum': l:error.region.start.line,
                    \    'col': l:error.region.start.column,
                    \    'end_lnum': l:error.region.end.line,
                    \    'end_col': l:error.region.end.column,
                    \    'type': (l:error.type is? 'error') ? 'E' : 'W',
                    \    'text': l:error.overview,
                    \    'detail': l:error.overview . "\n\n" . l:error.details
                    \})
                endif
            endfor
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

" Return the command to execute the linter in the projects directory.
" If it doesn't, then this will fail when imports are needed.
function! ale_linters#elm#make#GetCommand(buffer) abort
    let l:elm_package = ale#path#FindNearestFile(a:buffer, 'elm-package.json')
    let l:elm_exe = ale_linters#elm#make#GetExecutable(a:buffer)
    if empty(l:elm_package)
        let l:dir_set_cmd = ''
    else
        let l:root_dir = fnamemodify(l:elm_package, ':p:h')
        let l:dir_set_cmd = 'cd ' . ale#Escape(l:root_dir) . ' && '
    endif

    " The elm-make compiler, at the time of this writing, uses '/dev/null' as
    " a sort of flag to tell the compiler not to generate an output file,
    " which is why this is hard coded here. It does not use NUL on Windows.
    " Source: https://github.com/elm-lang/elm-make/blob/master/src/Flags.hs
    let l:elm_cmd = ale#Escape(l:elm_exe)
    \   . ' --report=json'
    \   . ' --output=/dev/null'

    return l:dir_set_cmd . ' ' . l:elm_cmd . ' %t'
endfunction

call ale#linter#Define('elm', {
\    'name': 'make',
\    'executable_callback': 'ale_linters#elm#make#GetExecutable',
\    'output_stream': 'both',
\    'command_callback': 'ale_linters#elm#make#GetCommand',
\    'callback': 'ale_linters#elm#make#Handle'
\})
