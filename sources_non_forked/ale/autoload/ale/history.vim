" Author: w0rp <devw0rp@gmail.com>
" Description: Tools for managing command history

" A flag for controlling the maximum size of the command history to store.
let g:ale_max_buffer_history_size = get(g:, 'ale_max_buffer_history_size', 20)

" Return a shallow copy of the command history for a given buffer number.
function! ale#history#Get(buffer) abort
    return copy(getbufvar(a:buffer, 'ale_history', []))
endfunction

function! ale#history#Add(buffer, status, job_id, command) abort
    if g:ale_max_buffer_history_size <= 0
        " Don't save anything if the history isn't a positive number.
        call setbufvar(a:buffer, 'ale_history', [])

        return
    endif

    let l:history = getbufvar(a:buffer, 'ale_history', [])

    " Remove the first item if we hit the max history size.
    if len(l:history) >= g:ale_max_buffer_history_size
        let l:history = l:history[1:]
    endif

    call add(l:history, {
    \   'status': a:status,
    \   'job_id': a:job_id,
    \   'command': a:command,
    \})

    call setbufvar(a:buffer, 'ale_history', l:history)
endfunction

function! s:FindHistoryItem(buffer, job_id) abort
    " Search backwards to find a matching job ID. IDs might be recycled,
    " so finding the last one should be good enough.
    for l:obj in reverse(ale#history#Get(a:buffer))
        if l:obj.job_id == a:job_id
            return l:obj
        endif
    endfor

    return {}
endfunction

" Set an exit code for a command which finished.
function! ale#history#SetExitCode(buffer, job_id, exit_code) abort
    let l:obj = s:FindHistoryItem(a:buffer, a:job_id)

    " If we find a match, then set the code and status.
    let l:obj.exit_code = a:exit_code
    let l:obj.status = 'finished'
endfunction

" Set the output for a command which finished.
function! ale#history#RememberOutput(buffer, job_id, output) abort
    let l:obj = s:FindHistoryItem(a:buffer, a:job_id)

    let l:obj.output = a:output
endfunction
