" Author: w0rp <devw0rp@gmail.com>
" Description: APIs for working with Asynchronous jobs, with an API normalised
" between Vim 8 and NeoVim.
"
" Important functions are described below. They are:
"
"   ale#job#Start(command, options) -> job_id
"   ale#job#IsRunning(job_id) -> 1 if running, 0 otherwise.
"   ale#job#Stop(job_id)

if !has_key(s:, 'job_map')
    let s:job_map = {}
endif

" A map from timer IDs to jobs, for tracking jobs that need to be killed
" with SIGKILL if they don't terminate right away.
if !has_key(s:, 'job_kill_timers')
    let s:job_kill_timers = {}
endif

function! s:KillHandler(timer) abort
    let l:job = remove(s:job_kill_timers, a:timer)
    call job_stop(l:job, 'kill')
endfunction

" Note that jobs and IDs are the same thing on NeoVim.
function! ale#job#JoinNeovimOutput(job, last_line, data, mode, callback) abort
    if a:mode is# 'raw'
        call a:callback(a:job, join(a:data, "\n"))
        return ''
    endif

    let l:lines = a:data[:-2]

    if len(a:data) > 1
        let l:lines[0] = a:last_line . l:lines[0]
        let l:new_last_line = a:data[-1]
    else
        let l:new_last_line = a:last_line . get(a:data, 0, '')
    endif

    for l:line in l:lines
        call a:callback(a:job, l:line)
    endfor

    return l:new_last_line
endfunction

function! s:NeoVimCallback(job, data, event) abort
    let l:info = s:job_map[a:job]

    if a:event is# 'stdout'
        let l:info.out_cb_line = ale#job#JoinNeovimOutput(
        \   a:job,
        \   l:info.out_cb_line,
        \   a:data,
        \   l:info.mode,
        \   ale#util#GetFunction(l:info.out_cb),
        \)
    elseif a:event is# 'stderr'
        let l:info.err_cb_line = ale#job#JoinNeovimOutput(
        \   a:job,
        \   l:info.err_cb_line,
        \   a:data,
        \   l:info.mode,
        \   ale#util#GetFunction(l:info.err_cb),
        \)
    else
        if has_key(l:info, 'out_cb') && !empty(l:info.out_cb_line)
            call ale#util#GetFunction(l:info.out_cb)(a:job, l:info.out_cb_line)
        endif

        if has_key(l:info, 'err_cb') && !empty(l:info.err_cb_line)
            call ale#util#GetFunction(l:info.err_cb)(a:job, l:info.err_cb_line)
        endif

        try
            call ale#util#GetFunction(l:info.exit_cb)(a:job, a:data)
        finally
            " Automatically forget about the job after it's done.
            if has_key(s:job_map, a:job)
                call remove(s:job_map, a:job)
            endif
        endtry
    endif
endfunction

function! s:VimOutputCallback(channel, data) abort
    let l:job = ch_getjob(a:channel)
    let l:job_id = ale#job#ParseVim8ProcessID(string(l:job))

    " Only call the callbacks for jobs which are valid.
    if l:job_id > 0 && has_key(s:job_map, l:job_id)
        call ale#util#GetFunction(s:job_map[l:job_id].out_cb)(l:job_id, a:data)
    endif
endfunction

function! s:VimErrorCallback(channel, data) abort
    let l:job = ch_getjob(a:channel)
    let l:job_id = ale#job#ParseVim8ProcessID(string(l:job))

    " Only call the callbacks for jobs which are valid.
    if l:job_id > 0 && has_key(s:job_map, l:job_id)
        call ale#util#GetFunction(s:job_map[l:job_id].err_cb)(l:job_id, a:data)
    endif
endfunction

function! s:VimCloseCallback(channel) abort
    let l:job = ch_getjob(a:channel)
    let l:job_id = ale#job#ParseVim8ProcessID(string(l:job))
    let l:info = get(s:job_map, l:job_id, {})

    if empty(l:info)
        return
    endif

    " job_status() can trigger the exit handler.
    " The channel can close before the job has exited.
    if job_status(l:job) is# 'dead'
        try
            if !empty(l:info) && has_key(l:info, 'exit_cb')
                call ale#util#GetFunction(l:info.exit_cb)(l:job_id, l:info.exit_code)
            endif
        finally
            " Automatically forget about the job after it's done.
            if has_key(s:job_map, l:job_id)
                call remove(s:job_map, l:job_id)
            endif
        endtry
    endif
endfunction

function! s:VimExitCallback(job, exit_code) abort
    let l:job_id = ale#job#ParseVim8ProcessID(string(a:job))
    let l:info = get(s:job_map, l:job_id, {})

    if empty(l:info)
        return
    endif

    let l:info.exit_code = a:exit_code

    " The program can exit before the data has finished being read.
    if ch_status(job_getchannel(a:job)) is# 'closed'
        try
            if !empty(l:info) && has_key(l:info, 'exit_cb')
                call ale#util#GetFunction(l:info.exit_cb)(l:job_id, a:exit_code)
            endif
        finally
            " Automatically forget about the job after it's done.
            if has_key(s:job_map, l:job_id)
                call remove(s:job_map, l:job_id)
            endif
        endtry
    endif
endfunction

function! ale#job#ParseVim8ProcessID(job_string) abort
    return matchstr(a:job_string, '\d\+') + 0
endfunction

function! ale#job#ValidateArguments(command, options) abort
    if a:options.mode isnot# 'nl' && a:options.mode isnot# 'raw'
        throw 'Invalid mode: ' . a:options.mode
    endif
endfunction

function! s:PrepareWrappedCommand(original_wrapper, command) abort
    let l:match = matchlist(a:command, '\v^(.*(\&\&|;)) *(.*)$')
    let l:prefix = ''
    let l:command = a:command

    if !empty(l:match)
        let l:prefix = l:match[1] . ' '
        let l:command = l:match[3]
    endif

    let l:format = a:original_wrapper

    if l:format =~# '%@'
        let l:wrapped = substitute(l:format, '%@', ale#Escape(l:command), '')
    else
        if l:format !~# '%\*'
            let l:format .= ' %*'
        endif

        let l:wrapped = substitute(l:format, '%\*', l:command, '')
    endif

    return l:prefix . l:wrapped
endfunction

function! ale#job#PrepareCommand(buffer, command) abort
    let l:wrapper = ale#Var(a:buffer, 'command_wrapper')

    let l:command = !empty(l:wrapper)
    \ ? s:PrepareWrappedCommand(l:wrapper, a:command)
    \ : a:command

    " The command will be executed in a subshell. This fixes a number of
    " issues, including reading the PATH variables correctly, %PATHEXT%
    " expansion on Windows, etc.
    "
    " NeoVim handles this issue automatically if the command is a String,
    " but we'll do this explicitly, so we use the same exact command for both
    " versions.
    if has('win32')
        return 'cmd /s/c "' . l:command . '"'
    endif

    if &shell =~? 'fish$'
        return ['/bin/sh', '-c', l:command]
    endif

    return split(&shell) + split(&shellcmdflag) + [l:command]
endfunction

" Start a job with options which are agnostic to Vim and NeoVim.
"
" The following options are accepted:
"
" out_cb  - A callback for receiving stdin.  Arguments: (job_id, data)
" err_cb  - A callback for receiving stderr. Arguments: (job_id, data)
" exit_cb - A callback for program exit.     Arguments: (job_id, status_code)
" mode    - A mode for I/O. Can be 'nl' for split lines or 'raw'.
function! ale#job#Start(command, options) abort
    call ale#job#ValidateArguments(a:command, a:options)

    let l:job_info = copy(a:options)
    let l:job_options = {}

    if has('nvim')
        if has_key(a:options, 'out_cb')
            let l:job_options.on_stdout = function('s:NeoVimCallback')
            let l:job_info.out_cb_line = ''
        endif

        if has_key(a:options, 'err_cb')
            let l:job_options.on_stderr = function('s:NeoVimCallback')
            let l:job_info.err_cb_line = ''
        endif

        if has_key(a:options, 'exit_cb')
            let l:job_options.on_exit = function('s:NeoVimCallback')
        endif

        let l:job_info.job = jobstart(a:command, l:job_options)
        let l:job_id = l:job_info.job
    else
        let l:job_options = {
        \   'in_mode': l:job_info.mode,
        \   'out_mode': l:job_info.mode,
        \   'err_mode': l:job_info.mode,
        \}

        if has_key(a:options, 'out_cb')
            let l:job_options.out_cb = function('s:VimOutputCallback')
        endif

        if has_key(a:options, 'err_cb')
            let l:job_options.err_cb = function('s:VimErrorCallback')
        endif

        if has_key(a:options, 'exit_cb')
            " Set a close callback to which simply calls job_status()
            " when the channel is closed, which can trigger the exit callback
            " earlier on.
            let l:job_options.close_cb = function('s:VimCloseCallback')
            let l:job_options.exit_cb = function('s:VimExitCallback')
        endif

        " Vim 8 will read the stdin from the file's buffer.
        let l:job_info.job = job_start(a:command, l:job_options)
        let l:job_id = ale#job#ParseVim8ProcessID(string(l:job_info.job))
    endif

    if l:job_id > 0
        " Store the job in the map for later only if we can get the ID.
        let s:job_map[l:job_id] = l:job_info
    endif

    return l:job_id
endfunction

" Send raw data to the job.
function! ale#job#SendRaw(job_id, string) abort
    if has('nvim')
        call jobsend(a:job_id, a:string)
    else
        call ch_sendraw(job_getchannel(s:job_map[a:job_id].job), a:string)
    endif
endfunction

" Given a job ID, return 1 if the job is currently running.
" Invalid job IDs will be ignored.
function! ale#job#IsRunning(job_id) abort
    if has('nvim')
        try
            " In NeoVim, if the job isn't running, jobpid() will throw.
            call jobpid(a:job_id)
            return 1
        catch
        endtry
    elseif has_key(s:job_map, a:job_id)
        let l:job = s:job_map[a:job_id].job
        return job_status(l:job) is# 'run'
    endif

    return 0
endfunction

" Given a Job ID, stop that job.
" Invalid job IDs will be ignored.
function! ale#job#Stop(job_id) abort
    if !has_key(s:job_map, a:job_id)
        return
    endif

    if has('nvim')
        " FIXME: NeoVim kills jobs on a timer, but will not kill any processes
        " which are child processes on Unix. Some work needs to be done to
        " kill child processes to stop long-running processes like pylint.
        silent! call jobstop(a:job_id)
    else
        let l:job = s:job_map[a:job_id].job

        " We must close the channel for reading the buffer if it is open
        " when stopping a job. Otherwise, we will get errors in the status line.
        if ch_status(job_getchannel(l:job)) is# 'open'
            call ch_close_in(job_getchannel(l:job))
        endif

        " Ask nicely for the job to stop.
        call job_stop(l:job)

        if ale#job#IsRunning(l:job)
            " Set a 100ms delay for killing the job with SIGKILL.
            let s:job_kill_timers[timer_start(100, function('s:KillHandler'))] = l:job
        endif
    endif
endfunction
