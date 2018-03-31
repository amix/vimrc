" Author: Brandon Roehl - https://github.com/BrandonRoehl
" Description: Ruby MRI for Ruby files

call ale#Set('ruby_ruby_executable', 'ruby')

function! ale_linters#ruby#ruby#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'ruby_ruby_executable')
endfunction

function! ale_linters#ruby#ruby#GetCommand(buffer) abort
    let l:executable = ale_linters#ruby#ruby#GetExecutable(a:buffer)

    return ale#Escape(l:executable) . ' -w -c -T1 %t'
endfunction

call ale#linter#Define('ruby', {
\   'name': 'ruby',
\   'executable_callback': 'ale_linters#ruby#ruby#GetExecutable',
\   'command_callback': 'ale_linters#ruby#ruby#GetCommand',
\   'output_stream': 'stderr',
\   'callback': 'ale#handlers#ruby#HandleSyntaxErrors',
\})
