" Author: Matteo Centenaro (bugant) - https://github.com/bugant
"
" Description: find the root directory for an elixir project that uses mix

function! ale#handlers#elixir#FindMixProjectRoot(buffer) abort
    let l:mix_file = ale#path#FindNearestFile(a:buffer, 'mix.exs')

    if !empty(l:mix_file)
      return fnamemodify(l:mix_file, ':p:h')
    endif

    return '.'
endfunction
