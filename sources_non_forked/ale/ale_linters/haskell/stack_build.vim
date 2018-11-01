" Author: Jake Zimmerman <jake@zimmerman.io>
" Description: Like stack-ghc, but for entire projects
"
" Note: Ideally, this would *only* typecheck. Right now, it also does codegen.
" See <https://github.com/commercialhaskell/stack/issues/977>.

call ale#Set('haskell_stack_build_options', '--fast')

function! ale_linters#haskell#stack_build#GetCommand(buffer) abort
    let l:flags = ale#Var(a:buffer, 'haskell_stack_build_options')

    return 'stack build ' . l:flags
endfunction

call ale#linter#Define('haskell', {
\   'name': 'stack_build',
\   'aliases': ['stack-build'],
\   'output_stream': 'stderr',
\   'executable_callback': 'ale#handlers#haskell#GetStackExecutable',
\   'command_callback': 'ale_linters#haskell#stack_build#GetCommand',
\   'lint_file': 1,
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
