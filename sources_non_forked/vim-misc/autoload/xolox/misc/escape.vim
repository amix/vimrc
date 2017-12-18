" String escaping functions.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: May 19, 2013
" URL: http://peterodding.com/code/vim/misc/

function! xolox#misc#escape#pattern(string) " {{{1
  " Takes a single string argument and converts it into a [:substitute]
  " [subcmd] / [substitute()] [subfun] pattern string that matches the given
  " string literally.
  "
  " [subfun]: http://vimdoc.sourceforge.net/htmldoc/eval.html#substitute()
  " [subcmd]: http://vimdoc.sourceforge.net/htmldoc/change.html#:substitute
  if type(a:string) == type('')
    let string = escape(a:string, '^$.*\~[]')
    return substitute(string, '\n', '\\n', 'g')
  endif
  return ''
endfunction

function! xolox#misc#escape#substitute(string) " {{{1
  " Takes a single string argument and converts it into a [:substitute]
  " [subcmd] / [substitute()] [subfun] replacement string that inserts the
  " given string literally.
  if type(a:string) == type('')
    let string = escape(a:string, '\&~%')
    return substitute(string, '\n', '\\r', 'g')
  endif
  return ''
endfunction

function! xolox#misc#escape#shell(string) " {{{1
  " Takes a single string argument and converts it into a quoted command line
  " argument.
  "
  " I was going to add a long rant here about Vim's ['shellslash' option]
  " [shellslash], but really, it won't make any difference. Let's just suffice
  " to say that I have yet to encounter a single person out there who uses
  " this option for its intended purpose (running a UNIX style shell on
  " Microsoft Windows).
  "
  " [shellslash]: http://vimdoc.sourceforge.net/htmldoc/options.html#'shellslash'
  if xolox#misc#os#is_win()
    try
      let ssl_save = &shellslash
      set noshellslash
      return shellescape(a:string)
    finally
      let &shellslash = ssl_save
    endtry
  else
    return shellescape(a:string)
  endif
endfunction

" vim: ts=2 sw=2 et
