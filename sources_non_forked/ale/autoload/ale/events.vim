" Author: w0rp <devw0rp@gmail.com>
" Description: ALE functions for autocmd events.

" Get the number of milliseconds since some vague, but consistent, point in
" the past.
"
" This function can be used for timing execution, etc.
"
" The time will be returned as a Number.
function! ale#events#ClockMilliseconds() abort
    return float2nr(reltimefloat(reltime()) * 1000)
endfunction

function! ale#events#QuitEvent(buffer) abort
    " Remember when ALE is quitting for BufWrite, etc.
    call setbufvar(a:buffer, 'ale_quitting', ale#events#ClockMilliseconds())
endfunction

function! ale#events#QuitRecently(buffer) abort
    let l:time = getbufvar(a:buffer, 'ale_quitting', 0)

    return l:time && ale#events#ClockMilliseconds() - l:time < 1000
endfunction

function! ale#events#SaveEvent(buffer) abort
    let l:should_lint = ale#Var(a:buffer, 'enabled') && g:ale_lint_on_save

    if l:should_lint
        call setbufvar(a:buffer, 'ale_save_event_fired', 1)
    endif

    if ale#Var(a:buffer, 'fix_on_save')
        let l:will_fix = ale#fix#Fix(a:buffer, 'save_file')
        let l:should_lint = l:should_lint && !l:will_fix
    endif

    if l:should_lint && !ale#events#QuitRecently(a:buffer)
        call ale#Queue(0, 'lint_file', a:buffer)
    endif
endfunction

function! s:LintOnEnter(buffer) abort
    if ale#Var(a:buffer, 'enabled')
    \&& g:ale_lint_on_enter
    \&& has_key(b:, 'ale_file_changed')
        call remove(b:, 'ale_file_changed')
        call ale#Queue(0, 'lint_file', a:buffer)
    endif
endfunction

function! ale#events#EnterEvent(buffer) abort
    " When entering a buffer, we are no longer quitting it.
    call setbufvar(a:buffer, 'ale_quitting', 0)
    let l:filetype = getbufvar(a:buffer, '&filetype')
    call setbufvar(a:buffer, 'ale_original_filetype', l:filetype)

    call s:LintOnEnter(a:buffer)
endfunction

function! ale#events#FileTypeEvent(buffer, new_filetype) abort
    let l:filetype = getbufvar(a:buffer, 'ale_original_filetype', '')

    " If we're setting the filetype for the first time after it was blank,
    " and the option for linting on enter is off, then we should set this
    " filetype as the original filetype. Otherwise ALE will still appear to
    " lint files because of the BufEnter event, etc.
    if empty(l:filetype) && !ale#Var(a:buffer, 'lint_on_enter')
        call setbufvar(a:buffer, 'ale_original_filetype', a:new_filetype)
    elseif a:new_filetype isnot# l:filetype
        call ale#Queue(300, 'lint_file', a:buffer)
    endif
endfunction

function! ale#events#FileChangedEvent(buffer) abort
    call setbufvar(a:buffer, 'ale_file_changed', 1)

    if bufnr('') == a:buffer
        call s:LintOnEnter(a:buffer)
    endif
endfunction

function! ale#events#Init() abort
    " This value used to be a Boolean as a Number, and is now a String.
    let l:text_changed = '' . g:ale_lint_on_text_changed

    augroup ALEEvents
        autocmd!

        " These events always need to be set up.
        autocmd BufEnter,BufRead * call ale#pattern_options#SetOptions(str2nr(expand('<abuf>')))
        autocmd BufWritePost * call ale#events#SaveEvent(str2nr(expand('<abuf>')))

        if g:ale_enabled
            if l:text_changed is? 'always' || l:text_changed is# '1'
                autocmd TextChanged,TextChangedI * call ale#Queue(g:ale_lint_delay)
            elseif l:text_changed is? 'normal'
                autocmd TextChanged * call ale#Queue(g:ale_lint_delay)
            elseif l:text_changed is? 'insert'
                autocmd TextChangedI * call ale#Queue(g:ale_lint_delay)
            endif

            " Handle everything that needs to happen when buffers are entered.
            autocmd BufEnter * call ale#events#EnterEvent(str2nr(expand('<abuf>')))

            if g:ale_lint_on_enter
                autocmd BufWinEnter,BufRead * call ale#Queue(0, 'lint_file', str2nr(expand('<abuf>')))
                " Track when the file is changed outside of Vim.
                autocmd FileChangedShellPost * call ale#events#FileChangedEvent(str2nr(expand('<abuf>')))
            endif

            if g:ale_lint_on_filetype_changed
                " Only start linting if the FileType actually changes after
                " opening a buffer. The FileType will fire when buffers are opened.
                autocmd FileType * call ale#events#FileTypeEvent(
                \   str2nr(expand('<abuf>')),
                \   expand('<amatch>')
                \)
            endif

            if g:ale_lint_on_insert_leave
                autocmd InsertLeave * call ale#Queue(0)
            endif

            if g:ale_echo_cursor
                autocmd CursorMoved,CursorHold * if exists('*ale#engine#Cleanup') | call ale#cursor#EchoCursorWarningWithDelay() | endif
                " Look for a warning to echo as soon as we leave Insert mode.
                " The script's position variable used when moving the cursor will
                " not be changed here.
                autocmd InsertLeave * if exists('*ale#engine#Cleanup') | call ale#cursor#EchoCursorWarning() | endif
            endif
        endif
    augroup END
endfunction
