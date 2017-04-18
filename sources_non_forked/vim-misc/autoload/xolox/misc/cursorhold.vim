" Rate limiting for Vim's CursorHold event.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 22, 2014
" URL: http://peterodding.com/code/vim/misc/
"
" Several of my Vim plug-ins (e.g. [vim-easytags][], [vim-notes][] and
" [vim-session][]) use Vim's [CursorHold][] and [CursorHoldI][] events to
" perform periodic tasks when the user doesn't press any keys for a couple of
" seconds. These events by default fire after four seconds, this is
" configurable using Vim's ['updatetime'][] option. The problem that this
" script solves is that there are Vim plug-ins which set the ['updatetime'][]
" option to unreasonably low values, thereby breaking my Vim plug-ins and
" probably a lot of other Vim plug-ins out there. When users complain about
" this I can tell them that another Vim plug-in is to blame, but users don't
" care for the difference, their Vim is broken! So I implemented a workaround.
" This script enables registration of [CursorHold][] event handlers with a
" configurable interval (expressed in seconds). The event handlers will be
" called no more than once every interval.
"
" ['updatetime']: http://vimdoc.sourceforge.net/htmldoc/options.html#'updatetime'
" [CursorHold]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#CursorHold
" [CursorHoldI]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#CursorHoldI
" [vim-easytags]: http://peterodding.com/code/vim/easytags/
" [vim-notes]: http://peterodding.com/code/vim/notes/
" [vim-session]: http://peterodding.com/code/vim/session/

if !exists('g:xolox#misc#cursorhold#handlers')
  let g:xolox#misc#cursorhold#handlers = []
endif

function! xolox#misc#cursorhold#register(options)
  " Register a [CursorHold][] event handler with a custom interval. This
  " function takes a single argument which is a dictionary with the following
  " fields:
  "
  "  - **function** (required): The name of the event handler function (a
  "    string).
  "
  "  - **arguments** (optional): A list of arguments to pass to the event
  "    handler function (defaults to an empty list).
  "
  "  - **interval** (optional): The number of seconds between calls to the
  "    event handler (defaults to 4).
  call add(g:xolox#misc#cursorhold#handlers, copy(a:options))
endfunction

function! xolox#misc#cursorhold#autocmd()
  " The 'top level event handler' that's called by Vim whenever the
  " [CursorHold][] or [CursorHoldI][] event fires. It iterates through the
  " event handlers registered using `xolox#misc#cursorhold#register()` and
  " calls each event handler at the appropriate interval, keeping track of
  " the time when each event handler was last run.
  for handler in g:xolox#misc#cursorhold#handlers
    let function = handler['function']
    let last_run = get(handler, 'last_run', 0)
    let interval = get(handler, 'interval', 4)
    call xolox#misc#msg#debug("vim-misc %s: Checking handler %s with interval %i and last run %i ..", g:xolox#misc#version, function, interval, last_run)
    " Rate limit in case &updatetime is set (very) low.
    let time_until_next_run = (last_run + interval) - localtime()
    if time_until_next_run > 0
      call xolox#misc#msg#debug("vim-misc %s: Rate limiting handler %s (time until next run: %i seconds).", g:xolox#misc#version, function, time_until_next_run)
    else
      call xolox#misc#msg#debug("vim-misc %s: Running handler %s ..", g:xolox#misc#version, function)
      call call(function, get(handler, 'arguments', []))
      let handler['last_run'] = localtime()
    endif
  endfor
endfunction

" vim: ts=2 sw=2 et
