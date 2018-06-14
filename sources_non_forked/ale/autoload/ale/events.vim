" Author: w0rp <devw0rp@gmail.com>

function! ale#events#QuitEvent(buffer) abort
    " Remember when ALE is quitting for BufWrite, etc.
    call setbufvar(a:buffer, 'ale_quitting', ale#util#ClockMilliseconds())
endfunction

function! ale#events#QuitRecently(buffer) abort
    let l:time = getbufvar(a:buffer, 'ale_quitting', 0)

    return l:time && ale#util#ClockMilliseconds() - l:time < 1000
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
