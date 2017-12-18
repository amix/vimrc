" Persist/recall Vim values from/to files.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 30, 2014
" URL: http://peterodding.com/code/vim/misc/
"
" Vim's [string()][] function can be used to serialize Vim script values like
" numbers, strings, lists, dictionaries and composites of them to a string
" which can later be evaluated using the [eval()][] function to turn it back
" into the original value. This Vim script provides functions to use these
" functions to persist and recall Vim values from/to files. This is very
" useful for communication between (possibly concurrent) Vim processes.

function! xolox#misc#persist#load(filename, ...) " {{{1
  " Read a Vim value like a number, string, list or dictionary from a file
  " using [readfile()][] and [eval()][]. The first argument is the filename of
  " the file to read (a string). The optional second argument specifies the
  " default value which is returned when the file can't be loaded. This
  " function returns the loaded value or the default value (which itself
  " defaults to the integer 0).
  "
  " [eval()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#eval()
  " [readfile()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#readfile()
  let default_value = exists('a:1') ? a:1 : 0
  try
    let lines = readfile(a:filename)
    return eval(join(lines, "\n"))
  catch
    return default_value
  endtry
endfunction

function! xolox#misc#persist#save(filename, value) " {{{1
  " Write a Vim value like a number, string, list or dictionary to a file
  " using [string()][] and [writefile()][]. The first argument is the filename
  " of the file to write (a string) and the second argument is the value to
  " write (any value).
  "
  " This function writes the serialized value to an intermediate file which is
  " then renamed into place atomically. This avoids issues with concurrent
  " processes where for example a producer has written a partial file which is
  " read by a consumer before the file is complete. In this case the consumer
  " would read a corrupt value. The rename trick avoids this problem.
  "
  " [string()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#string()
  " [writefile()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#writefile()
  return xolox#misc#perm#update(a:filename, split(string(a:value), "\n"))
endfunction

" vim: ts=2 sw=2 et
