" Author: Martino Pilia <martino.pilia@gmail.com>
" Description: Lint Ada files with GCC

call ale#Set('ada_gcc_executable', 'gcc')

" -gnatwa: activate most optional warnings
" -gnatq: try semantic analysis even if syntax errors have been found
call ale#Set('ada_gcc_options', '-gnatwa -gnatq')

function! ale_linters#ada#gcc#GetCommand(buffer) abort
    " Build a suitable output file name. The output file is specified because
    " the .ali file may be created even if no code generation is attempted.
    " The output file name must match the source file name (except for the
    " extension), so here we cannot use the null file as output.
    let l:tmp_dir = fnamemodify(ale#engine#CreateDirectory(a:buffer), ':p')
    let l:out_file = l:tmp_dir . fnamemodify(bufname(a:buffer), ':t:r') . '.o'

    " -gnatc: Check syntax and semantics only (no code generation attempted)
    return '%e -x ada -c -gnatc'
    \   . ' -o ' . ale#Escape(l:out_file)
    \   . ' -I ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))
    \   . ale#Pad(ale#Var(a:buffer, 'ada_gcc_options'))
    \   . ' %t'
endfunction

" For the message format please refer to:
"   https://gcc.gnu.org/onlinedocs/gnat_ugn/Output-and-Error-Message-Control.html
"   https://gcc.gnu.org/onlinedocs/gnat_ugn/Warning-Message-Control.html
function! ale_linters#ada#gcc#Handle(buffer, lines) abort
    " Error format: <filename>:<lnum>:<col>: <text>
    " Warning format: <filename>:<lnum>:<col>: warning: <text>
    let l:re = '\v(.+):([0-9]+):([0-9]+):\s+(warning:)?\s*(.+)\s*'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:re)
        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': str2nr(l:match[2]),
        \   'col': str2nr(l:match[3]),
        \   'type': l:match[4] is# 'warning:' ? 'W' : 'E',
        \   'text': l:match[5],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('ada', {
\   'name': 'gcc',
\   'output_stream': 'stderr',
\   'executable_callback': ale#VarFunc('ada_gcc_executable'),
\   'command_callback': 'ale_linters#ada#gcc#GetCommand',
\   'callback': 'ale_linters#ada#gcc#Handle',
\})
