" Human friendly string formatting for Vim.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 2, 2013
" URL: http://peterodding.com/code/vim/misc/

function! xolox#misc#format#pluralize(count, singular, plural) " {{{1
  " Concatenate a counter (the first argument, expected to be an integer) with
  " a singular or plural label (the second and third arguments, both expected
  " to be strings).
  if a:count == 0
    return printf('no %s', a:plural)
  else
    return printf('%i %s', a:count, a:count == 1 ? a:singular : a:plural)
  endif
endfunction

function! xolox#misc#format#timestamp(ts) " {{{1
  " Format a time stamp (a string containing a formatted floating point
  " number) into a human friendly format, for example 70 seconds is phrased as
  " "1 minute and 10 seconds".
  let seconds = a:ts + 0
  " Fast common case with extra precision from reltime().
  if seconds < 5
    let extract = matchstr(a:ts, '^\d\+\(\.0*[1-9][1-9]\?\)\?')
    if extract =~ '[123456789]'
      return extract . ' second' . (extract != '1' ? 's' : '')
    endif
  endif
  " Generic but slow code.
  let result = []
  for [name, size] in [['day', 60 * 60 * 24], ['hour', 60 * 60], ['minute', 60], ['second', 1]]
    if seconds >= size
      let counter = seconds / size
      let seconds = seconds % size
      let suffix = counter != 1 ? 's' : ''
      call add(result, printf('%i %s%s', counter, name, suffix))
    endif
  endfor
  " Format the resulting text?
  if len(result) == 1
    return result[0]
  else
    return join(result[0:-2], ', ') . ' and ' . result[-1]
  endif
endfunction
