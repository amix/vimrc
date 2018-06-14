" Author: w0rp <devw0rp@gmail.com>
" Description: Echoes lint message for the current line, if any

" Controls the milliseconds delay before echoing a message.
let g:ale_echo_delay = get(g:, 'ale_echo_delay', 10)
" A string format for the echoed message.
let g:ale_echo_msg_format = get(g:, 'ale_echo_msg_format', '%code: %%s')

let s:cursor_timer = -1
let s:last_pos = [0, 0, 0]

function! ale#cursor#TruncatedEcho(original_message) abort
    let l:message = a:original_message
    " Change tabs to spaces.
    let l:message = substitute(l:message, "\t", ' ', 'g')
    " Remove any newlines in the message.
    let l:message = substitute(l:message, "\n", '', 'g')

    " We need to remember the setting for shortmess and reset it again.
    let l:shortmess_options = &l:shortmess

    try
        let l:cursor_position = getcurpos()

        " The message is truncated and saved to the history.
        setlocal shortmess+=T
        exec "norm! :echomsg l:message\n"

        " Reset the cursor position if we moved off the end of the line.
        " Using :norm and :echomsg can move the cursor off the end of the
        " line.
        if l:cursor_position != getcurpos()
            call setpos('.', l:cursor_position)
        endif
    finally
        let &l:shortmess = l:shortmess_options
    endtry
endfunction

function! s:FindItemAtCursor() abort
    let l:buf = bufnr('')
    let l:info = get(g:ale_buffer_info, l:buf, {})
    let l:loclist = get(l:info, 'loclist', [])
    let l:pos = getcurpos()
    let l:index = ale#util#BinarySearch(l:loclist, l:buf, l:pos[1], l:pos[2])
    let l:loc = l:index >= 0 ? l:loclist[l:index] : {}

    return [l:info, l:loc]
endfunction

function! s:StopCursorTimer() abort
    if s:cursor_timer != -1
        call timer_stop(s:cursor_timer)
        let s:cursor_timer = -1
    endif
endfunction

function! ale#cursor#EchoCursorWarning(...) abort
    return ale#CallWithCooldown('dont_echo_until', function('s:EchoImpl'), [])
endfunction

function! s:EchoImpl() abort
    if !g:ale_echo_cursor
        return
    endif

    " Only echo the warnings in normal mode, otherwise we will get problems.
    if mode() isnot# 'n'
        return
    endif

    if ale#ShouldDoNothing(bufnr(''))
        return
    endif

    let l:buffer = bufnr('')
    let [l:info, l:loc] = s:FindItemAtCursor()

    if !empty(l:loc)
        let l:format = ale#Var(l:buffer, 'echo_msg_format')
        let l:msg = ale#GetLocItemMessage(l:loc, l:format)
        call ale#cursor#TruncatedEcho(l:msg)
        let l:info.echoed = 1
    elseif get(l:info, 'echoed')
        " We'll only clear the echoed message when moving off errors once,
        " so we don't continually clear the echo line.
        execute 'echo'
        let l:info.echoed = 0
    endif
endfunction

function! ale#cursor#EchoCursorWarningWithDelay() abort
    if !g:ale_echo_cursor
        return
    endif

    " Only echo the warnings in normal mode, otherwise we will get problems.
    if mode() isnot# 'n'
        return
    endif

    call s:StopCursorTimer()

    let l:pos = getcurpos()[0:2]

    " Check the current buffer, line, and column number against the last
    " recorded position. If the position has actually changed, *then*
    " we should echo something. Otherwise we can end up doing processing
    " the echo message far too frequently.
    if l:pos != s:last_pos
        let l:delay = ale#Var(bufnr(''), 'echo_delay')

        let s:last_pos = l:pos
        let s:cursor_timer = timer_start(
        \   l:delay,
        \   function('ale#cursor#EchoCursorWarning')
        \)
    endif
endfunction

function! ale#cursor#ShowCursorDetail() abort
    " Only echo the warnings in normal mode, otherwise we will get problems.
    if mode() isnot# 'n'
        return
    endif

    if ale#ShouldDoNothing(bufnr(''))
        return
    endif

    call s:StopCursorTimer()

    let [l:info, l:loc] = s:FindItemAtCursor()

    if !empty(l:loc)
        let l:message = get(l:loc, 'detail', l:loc.text)

        call ale#preview#Show(split(l:message, "\n"))
        execute 'echo'
    endif
endfunction
