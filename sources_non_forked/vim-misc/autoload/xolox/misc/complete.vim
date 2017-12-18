" Tab completion for user defined commands.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: July 9, 2014
" URL: http://peterodding.com/code/vim/misc/

function! xolox#misc#complete#keywords(arglead, cmdline, cursorpos)
  " This function can be used to perform keyword completion for user defined
  " Vim commands based on the contents of the current buffer. Here's an
  " example of how you would use it:
  "
  "     :command -nargs=* -complete=customlist,xolox#misc#complete#keywords MyCmd call s:MyCmd(<f-args>)
  let words = {}
  for line in getline(1, '$')
    for word in split(line, '\W\+')
      let words[word] = 1
    endfor
  endfor
  let arguments = [keys(filter(words, 'v:key =~# a:arglead'))]
  if &ignorecase
    call add(arguments, 1)
  endif
  return call('sort', arguments)
endfunction

" vim: ts=2 sw=2 et
