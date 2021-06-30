" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for making testing ALE easier.
"
" This file should not typically be loaded during the normal execution of ALE.

" Change the directory for checking things in particular test directories
"
" This function will set the g:dir variable, which represents the working
" directory after changing the path. This variable allows a test to change
" directories, and then switch back to a directory at the start of the test
" run.
"
" This function should be run in a Vader Before: block.
function! ale#test#SetDirectory(docker_path) abort
    if a:docker_path[:len('/testplugin/') - 1] isnot# '/testplugin/'
        throw 'docker_path must start with /testplugin/!'
    endif

    " Try to switch directory, which will fail when running tests directly,
    " and not through the Docker image.
    silent! execute 'cd ' . fnameescape(a:docker_path)
    let g:dir = getcwd() " no-custom-checks
endfunction

" When g:dir is defined, switch back to the directory we saved, and then
" delete that variable.
"
" The filename will be reset to dummy.txt
"
" This function should be run in a Vader After: block.
function! ale#test#RestoreDirectory() abort
    call ale#test#SetFilename('dummy.txt')
    silent execute 'cd ' . fnameescape(g:dir)
    unlet! g:dir
endfunction

" Get a filename for the current buffer using a relative path to the script.
"
" If a g:dir variable is set, it will be used as the path to the directory
" containing the test file.
function! ale#test#GetFilename(path) abort
    let l:dir = get(g:, 'dir', '')

    if empty(l:dir)
        let l:dir = getcwd() " no-custom-checks
    endif

    let l:full_path = ale#path#IsAbsolute(a:path)
    \   ? a:path
    \   : l:dir . '/' . a:path

    return ale#path#Simplify(l:full_path)
endfunction

" Change the filename for the current buffer using a relative path to
" the script without running autocmd commands.
"
" If a g:dir variable is set, it will be used as the path to the directory
" containing the test file.
function! ale#test#SetFilename(path) abort
    let l:full_path = ale#test#GetFilename(a:path)
    silent! noautocmd execute 'file ' . fnameescape(l:full_path)
endfunction

function! s:RemoveModule(results) abort
    for l:item in a:results
        if has_key(l:item, 'module')
            call remove(l:item, 'module')
        endif
    endfor
endfunction

" Return loclist data without the module string, only in newer Vim versions.
function! ale#test#GetLoclistWithoutModule() abort
    let l:results = getloclist(0)
    call s:RemoveModule(l:results)

    return l:results
endfunction

function! ale#test#GetQflistWithoutModule() abort
    let l:results = getqflist()
    call s:RemoveModule(l:results)

    return l:results
endfunction

function! ale#test#GetPreviewWindowText() abort
    for l:window in range(1, winnr('$'))
        if getwinvar(l:window, '&previewwindow', 0)
            let l:buffer = winbufnr(l:window)

            return getbufline(l:buffer, 1, '$')
        endif
    endfor
endfunction

" This function can be called with a timeout to wait for all jobs to finish.
" If the jobs to not finish in the given number of milliseconds,
" an exception will be thrown.
"
" The time taken will be a very rough approximation, and more time may be
" permitted than is specified.
function! ale#test#WaitForJobs(deadline) abort
    let l:start_time = ale#events#ClockMilliseconds()

    if l:start_time == 0
        throw 'Failed to read milliseconds from the clock!'
    endif

    let l:job_list = []

    " Gather all of the jobs from every buffer.
    for [l:buffer, l:data] in items(ale#command#GetData())
        call extend(l:job_list, map(keys(l:data.jobs), 'str2nr(v:val)'))
    endfor

    " NeoVim has a built-in API for this, so use that.
    if has('nvim')
        let l:nvim_code_list = jobwait(l:job_list, a:deadline)

        if index(l:nvim_code_list, -1) >= 0
            throw 'Jobs did not complete on time!'
        endif

        return
    endif

    let l:should_wait_more = 1

    while l:should_wait_more
        let l:should_wait_more = 0

        for l:job_id in l:job_list
            if ale#job#IsRunning(l:job_id)
                let l:now = ale#events#ClockMilliseconds()

                if l:now - l:start_time > a:deadline
                    " Stop waiting after a timeout, so we don't wait forever.
                    throw 'Jobs did not complete on time!'
                endif

                " Wait another 10 milliseconds
                let l:should_wait_more = 1
                sleep 10ms
                break
            endif
        endfor
    endwhile

    " Sleep for a small amount of time after all jobs finish.
    " This seems to be enough to let handlers after jobs end run, and
    " prevents the occasional failure where this function exits after jobs
    " end, but before handlers are run.
    sleep 10ms

    " We must check the buffer data again to see if new jobs started for
    " linters with chained commands.
    let l:has_new_jobs = 0

    " Check again to see if any jobs are running.
    for l:info in values(g:ale_buffer_info)
        for [l:job_id, l:linter] in get(l:info, 'job_list', [])
            if ale#job#IsRunning(l:job_id)
                let l:has_new_jobs = 1
                break
            endif
        endfor
    endfor

    if l:has_new_jobs
        " We have to wait more. Offset the timeout by the time taken so far.
        let l:now = ale#events#ClockMilliseconds()
        let l:new_deadline = a:deadline - (l:now - l:start_time)

        if l:new_deadline <= 0
            " Enough time passed already, so stop immediately.
            throw 'Jobs did not complete on time!'
        endif

        call ale#test#WaitForJobs(l:new_deadline)
    endif
endfunction

function! ale#test#FlushJobs() abort
    " The variable is checked for in a loop, as calling one series of
    " callbacks can trigger a further series of callbacks.
    while exists('g:ale_run_synchronously_callbacks')
        let l:callbacks = g:ale_run_synchronously_callbacks
        unlet g:ale_run_synchronously_callbacks

        for l:Callback in l:callbacks
            call l:Callback()
        endfor
    endwhile
endfunction
