" Timing of long during operations.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: July 19, 2014
" URL: http://peterodding.com/code/vim/misc/

if !exists('g:timer_enabled')
  let g:timer_enabled = 0
endif

if !exists('g:timer_verbosity')
  let g:timer_verbosity = 1
endif

let s:has_reltime = has('reltime')
let s:unique_marker = 'xolox#misc#timer#value'

function! xolox#misc#timer#resumable() " {{{1
  " Create a resumable timer object. This returns an object (a dictionary with
  " functions) with the following "methods":
  "
  "  - `start()` instructs the timer object to start counting elapsed time
  "    (when a timer object is created it is not automatically started).
  "
  "  - `stop()` instructs the timer object to stop counting elapsed time.
  "    This adds the time elapsed since `start()` was last called to the
  "    total elapsed time. This method will raise an error if called out of
  "    sequence.
  "
  "  - `format()` takes the total elapsed time and reports it as a string
  "    containing a formatted floating point number.
  "
  " Timer objects are meant to accurately time short running operations so
  " they're dependent on Vim's [reltime()][] and [reltimestr()][] functions.
  " In order to make it possible to use timer objects in my Vim plug-ins
  " unconditionally there's a fall back to [localtime()][] when [reltime()][]
  " is not available. In this mode the timer objects are not very useful but
  " at least they shouldn't raise errors.
  "
  " [localtime()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#localtime()
  " [reltime()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#reltime()
  " [reltimestr()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#reltimestr()
  let object = {'total': [0, 0]}
  function object.start() dict
    if s:has_reltime
      let self.current = reltime()
    else
      let self.current = localtime()
    endif
  endfunction
  function object.stop() dict
    if empty(get(self, 'current'))
      throw "timer.stop() called on a timer that was never started!"
    endif
    if s:has_reltime
      let elapsed_time_string = xolox#misc#str#trim(reltimestr(reltime(self.current)))
      " This is a bit silly (converting to a string and then parsing that) but
      " the value of reltime() is documented as being platform specific...
      let [seconds, microseconds] = split(elapsed_time_string, '\.')
      let self.total[0] += substitute(seconds, '^0*', '', '')
      let self.total[1] += substitute(microseconds, '^0*', '', '')
      let self.current = []
    else
      let self.total[0] += localtime() - self.current
      let self.current = 0
    endif
  endfunction
  function object.format() dict
    let seconds = self.total[0]
    let microseconds = self.total[1]
    if microseconds >= 1000000
      let additional_seconds = microseconds / 1000000
      let seconds += additional_seconds
      let microseconds -= additional_seconds * 1000000
    endif
    return printf('%i.%06i', seconds, microseconds)
  endfunction
  return object
endfunction

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
    call call('xolox#misc#msg#info', map(copy(a:000), 'xolox#misc#timer#convert(v:val)'))
  endif
endfunction

function! xolox#misc#timer#force(...) " {{{1
  " Show a formatted message to the user. This function has the same argument
  " handling as Vim's [printf()] [printf] function with one difference: At the
  " point where you want the elapsed time to be embedded, you write `%s` and
  " you pass the list returned by `xolox#misc#timer#start()` as an argument.
  call call('xolox#misc#msg#info', map(copy(a:000), 'xolox#misc#timer#convert(v:val)'))
endfunction

function! xolox#misc#timer#convert(value) " {{{1
  " Convert the value returned by `xolox#misc#timer#start()` to a string
  " representation of the elapsed time since `xolox#misc#timer#start()` was
  " called. Other values are returned unmodified (this allows using it with
  " Vim's [map()][] function).
  "
  " [map()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#map()
  if type(a:value) == type([]) && len(a:value) == 2 && a:value[0] == s:unique_marker
    if s:has_reltime
      let ts = xolox#misc#str#trim(reltimestr(reltime(a:value[1])))
    else
      let ts = localtime() - a:value[1]
    endif
    return xolox#misc#format#timestamp(ts)
  endif
  return a:value
endfunction

" vim: ts=2 sw=2 et
