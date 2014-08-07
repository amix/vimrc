" Timing of long during operations.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 2, 2013
" URL: http://peterodding.com/code/vim/misc/

if !exists('g:timer_enabled')
  let g:timer_enabled = 0
endif

if !exists('g:timer_verbosity')
  let g:timer_verbosity = 1
endif

let s:has_reltime = has('reltime')
let s:unique_marker = 'xolox#misc#timer#value'

function! xolox#misc#timer#start() " {{{1
  " Start a timer. This returns a list which can later be passed to
  " `xolox#misc#timer#stop()`.
  return [s:unique_marker, s:has_reltime ? reltime() : localtime()]
endfunction

function! xolox#misc#timer#stop(...) " {{{1
  " Show a formatted debugging message to the user, if the user has enabled
  " increased verbosity by setting Vim's ['verbose'] [verbose] option to one
  " (1) or higher.
  "
  " This function has the same argument handling as Vim's [printf()] [printf]
  " function with one difference: At the point where you want the elapsed time
  " to be embedded, you write `%s` and you pass the list returned by
  " `xolox#misc#timer#start()` as an argument.
  "
  " [verbose]: http://vimdoc.sourceforge.net/htmldoc/options.html#'verbose'
  " [printf]: http://vimdoc.sourceforge.net/htmldoc/eval.html#printf()
  if (g:timer_enabled || &verbose >= g:timer_verbosity)
    call call('xolox#misc#msg#info', map(copy(a:000), 's:convert_value(v:val)'))
  endif
endfunction

function! xolox#misc#timer#force(...) " {{{1
  " Show a formatted message to the user. This function has the same argument
  " handling as Vim's [printf()] [printf] function with one difference: At the
  " point where you want the elapsed time to be embedded, you write `%s` and
  " you pass the list returned by `xolox#misc#timer#start()` as an argument.
  call call('xolox#misc#msg#info', map(copy(a:000), 's:convert_value(v:val)'))
endfunction

function! s:convert_value(value) " {{{1
  if type(a:value) == type([]) && len(a:value) == 2 && a:value[0] == s:unique_marker
    if s:has_reltime
      let ts = xolox#misc#str#trim(reltimestr(reltime(a:value[1])))
    else
      let ts = localtime() - a:value[1]
    endif
    return xolox#misc#format#timestamp(ts)
  else
    return a:value
  endif
endfunction

" vim: ts=2 sw=2 et
