if !exists('g:ale_swift_swiftpm_executable')
  let g:ale_swift_swiftpm_executable = 'swift'
endif

if !exists('g:ale_swift_swiftpm_arguments')
  let g:ale_swift_swiftpm_arguments = 'build'
endif

function! ale_linters#swift#swiftpm#GetExecutable(buffer)
  if !filereadable('Package.swift')
    return ''
  endif

  return g:ale_swift_swiftpm_executable
endfunction

function! ale_linters#swift#swiftpm#GetCommand(buffer)
  return g:ale_swift_swiftpm_executable
        \ . ' '
        \ . g:ale_swift_swiftpm_arguments
endfunction

function! ale_linters#swift#swiftpm#Handle(buffer, lines)
  " Match and ignore file path (anything but :)
  " Match and capture line number
  " Match and capture column number
  " Match and capture anything in the message
  let l:pattern = '^[^:]\+:\(\d\+\):\(\d\+\):\s*error:\s*\(.*\)$'
  let l:output = []

  for l:line in a:lines
    let l:match = matchlist(l:line, l:pattern)

    if len(l:match) == 0
      continue
    endif

    let l:line_number = l:match[1]
    let l:column = l:match[2]
    let l:text = l:match[3]

    call add(l:output, {
          \ 'bufnr': a:buffer,
          \ 'lnum': l:line_number,
          \ 'vcol': 0,
          \ 'col': l:column,
          \ 'text': l:text,
          \ 'type': 'E',
        \ })
  endfor

  return l:output
endfunction

call ale#linter#Define('swift', {
      \ 'name': 'swiftpm',
      \ 'executable_callback': 'ale_linters#swift#swiftpm#GetExecutable',
      \ 'command_callback': 'ale_linters#swift#swiftpm#GetCommand',
      \ 'callback': 'ale_linters#swift#swiftpm#Handle',
    \ })
