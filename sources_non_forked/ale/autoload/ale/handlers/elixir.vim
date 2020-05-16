" Author: Matteo Centenaro (bugant) - https://github.com/bugant
" Author: Jon Parise <jon@indelible.org>
" Description: Functions for working with Elixir projects

" Find the root directory for an elixir project that uses mix.
function! ale#handlers#elixir#FindMixProjectRoot(buffer) abort
    let l:mix_file = ale#path#FindNearestFile(a:buffer, 'mix.exs')

    if !empty(l:mix_file)
        return fnamemodify(l:mix_file, ':p:h')
    endif

    return '.'
endfunction

" Similar to ale#handlers#elixir#FindMixProjectRoot but also continue the
" search upward for a potential umbrella project root. If an umbrella root
" does not exist, the initial project root will be returned.
function! ale#handlers#elixir#FindMixUmbrellaRoot(buffer) abort
    let l:app_root = ale#handlers#elixir#FindMixProjectRoot(a:buffer)
    let l:umbrella_root = fnamemodify(l:app_root, ':h:h')

    if filereadable(l:umbrella_root . '/mix.exs')
        return l:umbrella_root
    endif

    return l:app_root
endfunction
