function! ale_linters#swift#swiftlint#GetExecutable(buffer)
  if get(g:, 'ale_swift_swiftlint_use_defaults', 0) || filereadable('.swiftlint.yml')
    return 'swiftlint'
  endif

  return ''
endfunction

function! ale_linters#swift#swiftlint#Handle(buffer, lines)
  " Match and ignore file path (anything but ':')
  " Match and capture line number
  " Match and capture optional column number
  " Match and capture warning level ('error' or 'warning')
  " Match and capture anything in the message
  let l:pattern = '^[^:]\+:\(\d\+\):\(\d\+\)\?:\?\s*\(\w\+\):\s*\(.*\)$'
  let l:output = []

  for l:line in a:lines
    let l:match = matchlist(l:line, l:pattern)

    if len(l:match) == 0
      continue
    endif

    let l:line_number = l:match[1]
    let l:column = l:match[2]
    let l:type = toupper(l:match[3][0])
    let l:text = l:match[4]

    call add(l:output, {
          \ 'bufnr': a:buffer,
          \ 'lnum': l:line_number,
          \ 'vcol': 0,
          \ 'col': l:column,
          \ 'text': l:text,
          \ 'type': l:type,
        \ })
  endfor

  return l:output
endfunction

call ale#linter#Define('swift', {
      \ 'name': 'swiftlint',
      \ 'executable_callback': 'ale_linters#swift#swiftlint#GetExecutable',
      \ 'command': 'swiftlint lint --use-stdin',
      \ 'callback': 'ale_linters#swift#swiftlint#Handle',
    \ })
