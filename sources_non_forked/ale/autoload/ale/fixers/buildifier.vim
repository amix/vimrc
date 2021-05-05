" Author: Jon Parise <jon@indelible.org>
" Description: Format Bazel BUILD and .bzl files with buildifier.
"
call ale#Set('bazel_buildifier_executable', 'buildifier')
call ale#Set('bazel_buildifier_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('bazel_buildifier_options', '')

function! ale#fixers#buildifier#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'bazel_buildifier', [
    \   'buildifier',
    \])
endfunction

function! ale#fixers#buildifier#Fix(buffer) abort
    let l:executable = ale#Escape(ale#fixers#buildifier#GetExecutable(a:buffer))
    let l:options = ale#Var(a:buffer, 'bazel_buildifier_options')
    let l:filename = ale#Escape(bufname(a:buffer))

    let l:command = l:executable . ' -mode fix -lint fix -path ' . l:filename

    if l:options isnot# ''
        let l:command .= ' ' . l:options
    endif

    return {'command': l:command . ' -'}
endfunction
