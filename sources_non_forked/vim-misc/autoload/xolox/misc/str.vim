" String handling.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 23, 2013
" URL: http://peterodding.com/code/vim/misc/

function! xolox#misc#str#slug(s) " {{{1
  " Convert a string to a "slug" - something that can be safely used in
  " filenames and URLs without worrying about quoting/escaping of special
  " characters.
  return join(split(tolower(a:s), '\W\+'), '-')
endfunction

function! xolox#misc#str#ucfirst(s) " {{{1
  " Uppercase the first character in a string (the first argument).
  return substitute(a:s, '^.', '\U\0', '')
endfunction

function! xolox#misc#str#compact(s) " {{{1
  " Compact whitespace in a string (the first argument).
  return join(split(a:s), " ")
endfunction

function! xolox#misc#str#trim(s) " {{{1
  " Trim all whitespace from the start and end of a string (the first
  " argument).
  return substitute(a:s, '^\_s*\(.\{-}\)\_s*$', '\1', '')
endfunction

function! xolox#misc#str#indent(text, num_spaces) " {{{1
  " Indent all lines in a multi-line string (the first argument) with a
  " specific number of *space characters* (the second argument, an integer).
  let lines = split(a:text, "\n")
  let indent = repeat(' ', a:num_spaces)
  let [idx, limit] = [0, len(lines)]
  while idx < limit
    if lines[idx] =~ '\S'
      let lines[idx] = indent . lines[idx]
    endif
    let idx += 1
  endwhile
  return join(lines, "\n")
endfunction

function! xolox#misc#str#dedent(text) " {{{1
  " Remove common whitespace from a multi line string.
  let lines = split(a:text, "\n")
  " First we need to determine the common indentation of all non-empty lines.
  for line in lines
    if line =~ '\S'
      let indent = matchstr(line, '^\s*')
      if !exists('common_indent')
        let common_indent = indent
      elseif len(indent) < len(common_indent)
        let common_indent = indent
      endif
    endif
  endfor
  " Now we will strip the common indentation.
  let [idx, limit] = [0, len(lines)]
  let pattern = '^' . common_indent
  while idx < limit
    let lines[idx] = substitute(lines[idx], pattern, '', '')
    let idx += 1
  endwhile
  return join(lines, "\n")
endfunction

" vim: ts=2 sw=2 et
