" Author: evnu - https://github.com/evnu
" Author: colbydehart - https://github.com/colbydehart
" Description: Mix compile checking for Elixir files

function! ale_linters#elixir#mix#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " Error format
    " ** (CompileError) apps/sim/lib/sim/server.ex:87: undefined function update_in/4
    "
    " TODO: Warning format
    " warning: variable "foobar" does not exist and is being expanded to "foobar()", please use parentheses to remove the ambiguity or change the variable name
    let l:pattern = '\v\(([^\)]+Error)\) ([^:]+):([^:]+): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:type = 'E'
        let l:text = l:match[4]

        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': l:match[3] + 0,
        \   'col': 0,
        \   'type': l:type,
        \   'text': l:text,
        \})
    endfor

    return l:output
endfunction

function! ale_linters#elixir#mix#FindProjectRoot(buffer) abort
    let l:mix_file = ale#path#FindNearestFile(a:buffer, 'mix.exs')

    if !empty(l:mix_file)
      return fnamemodify(l:mix_file, ':p:h')
    endif

    return '.'
endfunction

function! ale_linters#elixir#mix#GetCommand(buffer) abort
    let l:project_root = ale_linters#elixir#mix#FindProjectRoot(a:buffer)

    let l:temp_dir = ale#engine#CreateDirectory(a:buffer)

    let l:mix_build_path = has('win32')
    \   ? 'set MIX_BUILD_PATH=' . ale#Escape(l:temp_dir) . ' &&'
    \   : 'MIX_BUILD_PATH=' . ale#Escape(l:temp_dir)

    return ale#path#CdString(l:project_root)
          \ . l:mix_build_path
          \ . ' mix compile %s'
endfunction

call ale#linter#Define('elixir', {
\   'name': 'mix',
\   'executable': 'mix',
\   'command_callback': 'ale_linters#elixir#mix#GetCommand',
\   'callback': 'ale_linters#elixir#mix#Handle',
\   'lint_file': 1,
\})
