" Asynchronous Vim script evaluation.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: September 17, 2014
" URL: http://peterodding.com/code/vim/misc/
"
" The `xolox#misc#async#call()` function builds on top of `xolox#misc#os#exec()`
" to support asynchronous evaluation of Vim scripts. The first (and for now
" only) use case is my [vim-easytags][] plug-in which has a bunch of
" conflicting requirements:
"
" 1. I want the [vim-easytags][] plug-in to be as portable as possible.
"    Ideally everything is implemented in Vim script because that's the only
"    thing I can rely on to be available for all potential users of the
"    plug-in!
"
" 2. Because of point one I've been forced to implement tags file reading,
"    parsing, (fold case) sorting and writing in Vim script. This is fine for
"    small tags files but once they grow to a couple of megabytes it becomes
"    annoying because Vim is unresponsive during tags file updates (key
"    presses are fortunately buffered due to Vim's input model but that
"    doesn't make it a nice user experience :-).
"
" 3. I could (and did in the past) come up with all sorts of hacks to speed
"    things up without switching away from Vim script, but none of them are
"    going to solve the fundamental problem that Vim's unresponsive hiccups
"    become longer as tags files grow larger.
"
" By now it should be clear where this is heading: _Why not handle tags file
" updates in a Vim process that runs in the background without blocking the
" Vim process that the user is interacting with?_ It turns out that there are
" quite a few details to take care of, but with those out of the way, it might
" just work! I'm actually hoping to make asynchronous updates the default mode
" in [vim-easytags][]. This means I need this functionality to be as
" portable and robust as possible.
"
" **Status:** This code has seen little testing so I wouldn't trust it too
" much just yet. On the other hand, as I said, my intention is to make this
" functionality as portable and robust as possible. You be the judge :-).
"
" [vim-easytags]: http://peterodding.com/code/vim/easytags/

if !exists('g:xolox#misc#async#counter')
  " Increasing integer number used to match asynchronous responses to the
  " requests that generated them.
  let g:xolox#misc#async#counter = 1
endif

if !exists('g:xolox#misc#async#requests')
  " Queue of asynchronous requests that haven't received a response yet.
  let g:xolox#misc#async#requests = {}
endif

function! xolox#misc#async#call(options) " {{{1
  " Call a Vim script function asynchronously by starting a hidden Vim process
  " in the background. Once the function returns the hidden Vim process
  " terminates itself. This function takes a single argument which is a
  " dictionary with the following key/value pairs:
  "
  "  - **function** (required): The name of the Vim function to call inside
  "    the child process (a string). I suggest using an [autoload][] function
  "    for this, see below.
  "
  "  - **arguments** (optional): A list of arguments to pass to the function.
  "    This list is serialized to a string using [string()][] and deserialized
  "    using [eval()][].
  "
  "  - **callback** (optional): The name of a Vim function to call in the
  "    parent process when the child process has completed (a string).
  "
  "  - **clientserver** (optional): If this is true (1) the child process will
  "    notify the parent process when it has finished (the default is true).
  "    This works using Vim's client/server support which is not always
  "    available. As a fall back Vim's [CursorHold][] automatic command is
  "    also supported (although the effect is not quite as instantaneous :-).
  "
  " This functionality is experimental and non trivial to use, so consider
  " yourself warned :-).
  "
  " **Limitations**
  "
  " I'm making this functionality available in [vim-misc][] because I think it
  " can be useful to other plug-ins, however if you are going to use it you
  " should be aware of the following limitations:
  "
  "  - Because of the use of multiple processes this functionality is only
  "    suitable for 'heavy' tasks.
  "
  "  - The function arguments are serialized to a string which is passed to
  "    the hidden Vim process as a command line argument, so the amount of
  "    data you can pass will be limited by your operating environment.
  "
  "  - The hidden Vim process is explicitly isolated from the user in several
  "    ways (see below for more details). This is to make sure that the hidden
  "    Vim processes are fast and don't clobber the user's editing sessions in
  "    any way.
  "
  " **Changes to how Vim normally works**
  "
  " You have to be aware that the hidden Vim process is initialized in a
  " specific way that is very different from your regular Vim editing
  " sessions:
  "
  "  - Your [vimrc][] file is ignored using the `-u NONE` command line option.
  "
  "  - Your [gvimrc][] file (if you even knew it existed ;-) is ignored using
  "    the `-U NONE` command line option.
  "
  "  - Plug-in loading is skipped using the `--noplugin` command line option.
  "
  "  - Swap files (see [swap-file][]) are disabled using the `-n` command line
  "    option. This makes sure asynchronous Vim processes don't disturb the
  "    user's editing session.
  "
  "  - Your [viminfo][] file is ignored using the `-i NONE` command line
  "    option. Just like with swap files this makes sure asynchronous Vim
  "    processes don't disturb the user's editing session.
  "
  "  - No-compatible mode is enabled using the `-N` command line option
  "    (usually the existence of your vimrc script would have achieved the
  "    same effect but since we disable loading of your vimrc we need to spell
  "    things out for Vim).
  "
  " **Use an auto-load function**
  "
  " The function you want to call is identified by its name which has to be
  " defined, but I just explained above that all regular initialization is
  " disabled for asynchronous Vim processes, so what gives? The answer is to
  " use an [autoload][] function. This should work fine because the
  " asynchronous Vim process 'inherits' the value of the ['runtimepath'][]
  " option from your editing session.
  "
  " ['runtimepath']: http://vimdoc.sourceforge.net/htmldoc/options.html#'runtimepath'
  " [autoload]: http://vimdoc.sourceforge.net/htmldoc/eval.html#autoload
  " [CursorHold]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#CursorHold
  " [eval()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#eval()
  " [gvimrc]: http://vimdoc.sourceforge.net/htmldoc/gui.html#gvimrc
  " [string()]: http://vimdoc.sourceforge.net/htmldoc/eval.html#string()
  " [swap-file]: http://vimdoc.sourceforge.net/htmldoc/recover.html#swap-file
  " [vim-misc]: http://peterodding.com/code/vim/misc/
  " [viminfo]: http://vimdoc.sourceforge.net/htmldoc/starting.html#viminfo
  " [vimrc]: http://vimdoc.sourceforge.net/htmldoc/starting.html#vimrc
  let unique_number = g:xolox#misc#async#counter
  let g:xolox#misc#async#counter += 1
  let request = {'function': a:options['function']}
  let request['arguments'] = get(a:options, 'arguments', [])
  let request['starttime'] = xolox#misc#timer#start()
  let request['number'] = unique_number
  let callback = get(a:options, 'callback')
  if !empty(callback)
    let request['callback'] = callback
  endif
  if get(a:options, 'clientserver', 1) && !empty(v:servername)
    let request['servername'] = v:servername
  else
    let temporary_file = tempname()
    let request['temporary_file'] = temporary_file
  endif
  let vim_command = printf('let &rtp = %s | call xolox#misc#async#inside_child(%s)', string(&rtp), string(request))
  call xolox#misc#msg#debug("vim-misc %s: Generated asynchronous Vim command #%i: %s", g:xolox#misc#version, unique_number, vim_command)
  let quoted_program = xolox#misc#escape#shell(xolox#misc#os#find_vim('vim'))
  let quoted_command = xolox#misc#escape#shell(vim_command)
  let shell_command = printf('%s -u NONE -U NONE --noplugin -n -N -i NONE --cmd %s', quoted_program, quoted_command)
  call xolox#misc#msg#debug("vim-misc %s: Generated asynchronous shell command #%i: %s", g:xolox#misc#version, unique_number, shell_command)
  call xolox#misc#os#exec({'command': shell_command, 'async': 1})
  let g:xolox#misc#async#requests[unique_number] = request
endfunction

function! xolox#misc#async#inside_child(request) " {{{1
  " Entry point inside the hidden Vim process that runs in the background.
  " Invoked indirectly by `xolox#misc#async#call()` because it runs a command
  " similar to the following:
  "
  "     vim --cmd 'call xolox#misc#async#inside_child(...)'
  "
  " This function is responsible for calling the user defined function,
  " capturing exceptions and reporting the results back to the parent Vim
  " process using Vim's client/server support or a temporary file.
  try
    let response = {'number': a:request['number']}
    let starttime = xolox#misc#timer#start()
    try
      " Call the user defined function and store its result.
      let response['result'] = call(a:request['function'], a:request['arguments'])
    catch
      " Intercept errors raised by the user defined function.
      let response['exception'] = v:exception
      let response['throwpoint'] = v:throwpoint
    endtry
    " Record the elapsed time.
    let response['elapsed_time'] = xolox#misc#timer#convert(starttime)
    " Communicate the results back to the master Vim process.
    let servername = get(a:request, 'servername', '')
    if !empty(servername)
      " Actively notify the parent process using Vim's client/server support?
      call remote_expr(servername, printf('xolox#misc#async#callback_to_parent(%s)', string(response)))
    else
      " 'Passively' notify the parent process by creating the expected
      " temporary file.
      call xolox#misc#persist#save(a:request['temporary_file'], response)
    endif
  finally
    " Make sure we terminate this hidden Vim process.
    quitall!
  endtry
endfunction

function! xolox#misc#async#callback_to_parent(response) " {{{1
  " When Vim was compiled with client/server support this function (in the
  " parent process) will be called by `xolox#misc#async#inside_child()` (in
  " the child process) after the user defined function has returned. This
  " enables more or less instant callbacks after running an asynchronous
  " function.
  let unique_number = a:response['number']
  let request = g:xolox#misc#async#requests[unique_number]
  call xolox#misc#timer#stop("vim-misc %s: Processing asynchronous callback #%i after %s ..", g:xolox#misc#version, unique_number, request['starttime'])
  call remove(g:xolox#misc#async#requests, unique_number)
  let callback = get(request, 'callback')
  if !empty(callback)
    call call(callback, [a:response])
  endif
endfunction

function! xolox#misc#async#periodic_callback() " {{{1
  " When client/server support is not being used the vim-misc plug-in
  " improvises: It uses Vim's [CursorHold][] event to periodically check if an
  " asynchronous process has written its results to one of the expected
  " temporary files. If a response is found the temporary file is read and
  " deleted and then `xolox#misc#async#callback_to_parent()` is called to
  " process the response.
  "
  " [CursorHold]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#CursorHold
  if !empty(g:xolox#misc#async#requests)
    let num_processed = 0
    call xolox#misc#msg#debug("vim-misc %s: Checking for asynchronous responses (%i responses not yet received) ..", g:xolox#misc#version, len(g:xolox#misc#async#requests))
    for unique_number in sort(keys(g:xolox#misc#async#requests))
      let request = g:xolox#misc#async#requests[unique_number]
      let temporary_file = get(request, 'temporary_file', '')
      if !empty(temporary_file) && getfsize(temporary_file) > 0
        try
          call xolox#misc#msg#debug("vim-misc %s: Found asynchronous response by %s in %s ..", g:xolox#misc#version, request['function'], temporary_file)
          call xolox#misc#async#callback_to_parent(xolox#misc#persist#load(temporary_file))
          let num_processed += 1
        finally
          call delete(temporary_file)
        endtry
      endif
    endfor
    call xolox#misc#msg#debug("vim-misc %s: Processed %i asynchronous responses (%i responses not yet received).", g:xolox#misc#version, num_processed, len(g:xolox#misc#async#requests))
  endif
endfunction

" }}}1

" The interval in the options below is set to one (1) although the default
" value for &updatetime is four seconds. Because vim-misc never modifies
" &updatetime the interval will effectively default to four seconds unless the
" user has set &updatetime to a lower value themselves.
call xolox#misc#cursorhold#register({'function': 'xolox#misc#async#periodic_callback', 'interval': 1})

" vim: ts=2 sw=2 et
