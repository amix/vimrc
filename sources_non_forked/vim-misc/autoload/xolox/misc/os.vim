" Operating system interfaces.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: May 21, 2015
" URL: http://peterodding.com/code/vim/misc/

function! xolox#misc#os#is_mac() " {{{1
  " Returns 1 (true) when on Mac OS X, 0 (false) otherwise. You would expect
  " this to simply check the Vim feature list, but for some obscure reason the
  " `/usr/bin/vim` included in Mac OS X (verified on version 10.7.5) returns 0
  " (false) in response to `has('mac')`, so we check the output of `uname`
  " to avoid false negatives.
  if !exists('s:is_mac')
    " By default we assume we are *not* on Mac OS X.
    let s:is_mac = 0
    if has('mac') || has('macunix') || has('gui_mac')
      " If Vim's feature list indicates we are on Mac OS X, we have our answer :-).
      let s:is_mac = 1
    elseif !xolox#misc#os#is_win()
      " Otherwise we check the output of `uname' to avoid false negatives.
      let result = xolox#misc#os#exec({'command': 'uname', 'check': 0})
      if result['exit_code'] == 0 && get(result['stdout'], 0, '') == 'Darwin'
        let s:is_mac = 1
      endif
    endif
  endif
  return s:is_mac
endfunction

function! xolox#misc#os#is_win() " {{{1
  " Returns 1 (true) when on Microsoft Windows, 0 (false) otherwise.
  return has('win16') || has('win32') || has('win64')
endfunction

function! xolox#misc#os#find_vim(...) " {{{1
  " Returns the program name of Vim as a string. On Windows and UNIX this just
  " [v:progname] [] as an absolute pathname while on Mac OS X there is
  " some special magic to find MacVim's executable even though it's usually
  " not on the executable search path. If you want, you can override the
  " value returned from this function by setting the global variable
  " `g:xolox#misc#os#vim_progname`.
  "
  " By default the choice of console Vim vs graphical Vim is made based on
  " the value of [v:progname] [], but if you have a preference you can pass
  " the string `vim` or `gvim` as the first and only argument.
  "
  " [v:progname]: http://vimdoc.sourceforge.net/htmldoc/eval.html#v:progname
  if exists('a:1')
    let program_name = a:1
  else
    let program_name = v:progname
  endif
  if exists('g:xolox#misc#os#vim_progname')
    let pathname = g:xolox#misc#os#vim_progname
  else
    let pathname = ''
  endif
  if empty(pathname) && xolox#misc#os#is_mac()
    " Special handling for Mac OS X where MacVim is usually not on the $PATH.
    " This always returns the "Vim" executable and not "MacVim" (regardless of
    " the caller's preference) because "MacVim" has funky dock magic going on.
    call xolox#misc#msg#debug("vim-misc %s: Trying MacVim workaround to find Vim executable ..", g:xolox#misc#version)
    let segments = xolox#misc#path#split($VIMRUNTIME)
    if segments[-3:] == ['Resources', 'vim', 'runtime']
      let pathname = xolox#misc#path#join(segments[0:-4] + ['MacOS', 'Vim'])
      call xolox#misc#msg#debug("vim-misc %s: The MacVim workaround resulted in the Vim executable %s.", g:xolox#misc#version, string(pathname))
    endif
  endif
  if empty(pathname)
    " Default logic.
    call xolox#misc#msg#debug("vim-misc %s: Looking for Vim executable named %s on search path ..", g:xolox#misc#version, string(program_name))
    let candidates = xolox#misc#path#which(program_name)
    if !empty(candidates)
      call xolox#misc#msg#debug("vim-misc %s: Found %i candidate(s) on search path: %s.", g:xolox#misc#version, len(candidates), string(candidates))
      let pathname = candidates[0]
    endif
  endif
  call xolox#misc#msg#debug("vim-misc %s: Reporting Vim executable %s.", g:xolox#misc#version, string(pathname))
  return pathname
endfunction

function! xolox#misc#os#exec(options) " {{{1
  " Execute an external command (hiding the console on Microsoft Windows when
  " my [vim-shell plug-in] [vim-shell] is installed).
  "
  " Expects a dictionary with the following key/value pairs as the first
  " argument:
  "
  " - **command** (required): The command line to execute
  " - **async** (optional): set this to 1 (true) to execute the command in the
  "   background (asynchronously)
  " - **stdin** (optional): a string or list of strings with the input for the
  "   external command
  " - **check** (optional): set this to 0 (false) to disable checking of the
  "   exit code of the external command (by default an exception will be
  "   raised when the command fails)
  "
  " Returns a dictionary with one or more of the following key/value pairs:
  "
  " - **command** (always available): the generated command line that was used
  "   to run the external command
  " - **exit_code** (only in synchronous mode): the exit status of the
  "   external command (an integer, zero on success)
  " - **stdout** (only in synchronous mode): the output of the command on the
  "   standard output stream (a list of strings, one for each line)
  " - **stderr** (only in synchronous mode): the output of the command on the
  "   standard error stream (as a list of strings, one for each line)
  "
  " [vim-shell]: http://peterodding.com/code/vim/shell/
  try

    " Unpack the options.
    let cmd = a:options['command']
    let async = get(a:options, 'async', 0)

    " We need to know in a couple of places whether we are on Windows.
    let is_win = xolox#misc#os#is_win()

    " Use vim-shell so we don't pop up a console window on Windows? If the
    " caller specifically asks us *not* to use vim-shell, we'll respect that
    " choice; this is very useful for automated tests :-).
    if get(a:options, 'use_dll', 1) == 0
      let use_dll = 0
    else
      let use_dll = xolox#misc#os#can_use_dll()
    endif

    " Decide whether to redirect the standard output and standard error
    " streams to temporary files.
    let redirect_output = !async && (use_dll || !is_win)

    " Write the input for the external command to a temporary file?
    if has_key(a:options, 'stdin') && use_dll
      let tempin = tempname()
      if type(a:options['stdin']) == type([])
        let lines = a:options['stdin']
      else
        let lines = split(a:options['stdin'], "\n")
      endif
      call writefile(lines, tempin)
      let cmd .= ' < ' . xolox#misc#escape#shell(tempin)
    endif

    " Redirect the standard output and/or standard error streams of the
    " external process to temporary files? (only in synchronous mode)
    if redirect_output
      let tempout = tempname()
      let temperr = tempname()
      let cmd = printf('(%s) 1>%s 2>%s', cmd, xolox#misc#escape#shell(tempout), xolox#misc#escape#shell(temperr))
    endif

    " Use vim-shell or system() to execute the external command?
    if use_dll
      call xolox#misc#msg#debug("vim-misc %s: Executing external command using compiled DLL: %s", g:xolox#misc#version, cmd)
      let exit_code = xolox#shell#execute_with_dll(cmd, async)
    else

      " Enable asynchronous mode (very platform specific).
      if async
        if is_win
          " As pointed out in issue 17 [1] the use of `:!start' on Windows
          " requires characters like `!', `%' and `#' to be escaped with a
          " backslash [2]. Vim's shellescape() function knows how to escape
          " these special characters however the use of `:!start' is an
          " implementation detail of xolox#misc#os#exec() so I don't want to
          " bother callers (who perform the shell escaping) with such a
          " specific implementation detail. This is why I resort to manually
          " escaping characters documented to have a special meaning [2].
          "
          " [1] https://github.com/xolox/vim-misc/issues/17
          " [2] All characters interpreted specially in shell command lines
          "     executed from Vim's command mode, refer to `:help :!' for
          "     details.
          let cmd = printf('start /b %s', escape(cmd, "\\\n!%#"))
        elseif has('unix')
          let cmd = printf('(%s) &', cmd)
        else
          call xolox#misc#msg#warn("vim-misc %s: I don't know how to execute the command %s asynchronously on your platform! Falling back to synchronous mode...", g:xolox#misc#version, cmd)
        endif
      endif

      " On UNIX we explicitly execute the command line using 'sh' instead of
      " the default shell, because we assume that standard output and standard
      " error can be redirected separately, but (t)csh does not support this
      " (and it might be the default shell).
      if has('unix')
        call xolox#misc#msg#debug("vim-misc %s: Generated shell expression: %s", g:xolox#misc#version, cmd)
        let cmd = printf('sh -c %s', xolox#misc#escape#shell(cmd))
      endif

      " Let the user know what's happening (in case they're interested).
      if async && is_win
        call xolox#misc#msg#debug("vim-misc %s: Executing external command using !start command: %s", g:xolox#misc#version, cmd)
        silent execute '!' . cmd
      else
        call xolox#misc#msg#debug("vim-misc %s: Executing external command using system() function: %s", g:xolox#misc#version, cmd)
        let arguments = [cmd]
        if has_key(a:options, 'stdin')
          if type(a:options['stdin']) == type([])
            call add(arguments, join(a:options['stdin'], "\n"))
          else
            call add(arguments, a:options['stdin'])
          endif
        endif
        let stdout = call('system', arguments)
        let exit_code = v:shell_error
      endif

    endif

    " Return the results as a dictionary with one or more key/value pairs.
    let result = {'command': cmd}
    if !async
      let result['exit_code'] = exit_code
      " Get the standard output of the command.
      if redirect_output
        let result['stdout'] = s:readfile(tempout, 'standard output', a:options['command'])
      elseif exists('stdout')
        let result['stdout'] = split(stdout, "\n")
      else
        let result['stdout'] = []
      endif
      " Get the standard error of the command.
      if exists('temperr')
        let result['stderr'] = s:readfile(temperr, 'standard error', a:options['command'])
      else
        let result['stderr'] = []
      endif
      " If we just executed a synchronous command and the caller didn't
      " specifically ask us *not* to check the exit code of the external
      " command, we'll do so now. The idea here is that it should be easy
      " to 'do the right thing'.
      if get(a:options, 'check', 1) && exit_code != 0
        " Prepare an error message with enough details so the user can investigate.
        let msg = printf("vim-misc %s: External command failed with exit code %d!", g:xolox#misc#version, result['exit_code'])
        let msg .= printf("\nCommand line: %s", result['command'])
        " If the external command reported an error, we'll include it in our message.
        if !empty(result['stderr'])
          " This is where we would normally expect to find an error message.
          let msg .= printf("\nOutput on standard output stream:\n%s", join(result['stderr'], "\n"))
        elseif !empty(result['stdout'])
          " Exuberant Ctags on Windows XP reports errors on standard output :-x.
          let msg .= printf("\nOutput on standard error stream:\n%s", join(result['stdout'], "\n"))
        endif
        throw msg
      endif
    endif
    return result

  finally
    " Cleanup any temporary files we created.
    for name in ['tempin', 'tempout', 'temperr']
      if exists(name)
        call delete({name})
      endif
    endfor
  endtry

endfunction

function! xolox#misc#os#can_use_dll() " {{{1
  " If a) we're on Microsoft Windows, b) the vim-shell plug-in is installed
  " and c) the compiled DLL included in vim-shell works, we can use the
  " vim-shell plug-in to execute external commands! Returns 1 (true)
  " if we can use the DLL, 0 (false) otherwise.
  let can_use_dll = 0
  try
    let can_use_dll = xolox#shell#can_use_dll()
  catch /^Vim\%((\a\+)\)\=:E117/
    " Silence E117.
  endtry
  return can_use_dll
endfunction

function! s:readfile(fname, label, cmd) " {{{1
  try
    return readfile(a:fname)
  catch
    call xolox#misc#msg#warn("vim-misc %s: Failed to read temporary file (%s) with %s of external command: %s! (external command: %s)", g:xolox#misc#version, a:fname, a:label, v:exception, a:cmd)
    return []
  endtry
endfunction

" vim: ts=2 sw=2 et
