function! ale#toggle#InitAuGroups() abort
    " This value used to be a Boolean as a Number, and is now a String.
    let l:text_changed = '' . g:ale_lint_on_text_changed

    augroup ALEPatternOptionsGroup
        autocmd!
        autocmd BufEnter,BufRead * call ale#pattern_options#SetOptions(str2nr(expand('<abuf>')))
    augroup END

    augroup ALERunOnTextChangedGroup
        autocmd!
        if g:ale_enabled
            if l:text_changed is? 'always' || l:text_changed is# '1'
                autocmd TextChanged,TextChangedI * call ale#Queue(g:ale_lint_delay)
            elseif l:text_changed is? 'normal'
                autocmd TextChanged * call ale#Queue(g:ale_lint_delay)
            elseif l:text_changed is? 'insert'
                autocmd TextChangedI * call ale#Queue(g:ale_lint_delay)
            endif
        endif
    augroup END

    augroup ALERunOnEnterGroup
        autocmd!
        if g:ale_enabled
            " Handle everything that needs to happen when buffers are entered.
            autocmd BufEnter * call ale#events#EnterEvent(str2nr(expand('<abuf>')))
        endif
        if g:ale_enabled && g:ale_lint_on_enter
            autocmd BufWinEnter,BufRead * call ale#Queue(0, 'lint_file', str2nr(expand('<abuf>')))
            " Track when the file is changed outside of Vim.
            autocmd FileChangedShellPost * call ale#events#FileChangedEvent(str2nr(expand('<abuf>')))
        endif
    augroup END

    augroup ALERunOnFiletypeChangeGroup
        autocmd!
        if g:ale_enabled && g:ale_lint_on_filetype_changed
            " Only start linting if the FileType actually changes after
            " opening a buffer. The FileType will fire when buffers are opened.
            autocmd FileType * call ale#events#FileTypeEvent(
            \   str2nr(expand('<abuf>')),
            \   expand('<amatch>')
            \)
        endif
    augroup END

    augroup ALERunOnSaveGroup
        autocmd!
        autocmd BufWritePost * call ale#events#SaveEvent(str2nr(expand('<abuf>')))
    augroup END

    augroup ALERunOnInsertLeave
        autocmd!
        if g:ale_enabled && g:ale_lint_on_insert_leave
            autocmd InsertLeave * call ale#Queue(0)
        endif
    augroup END

    augroup ALECursorGroup
        autocmd!
        if g:ale_enabled && g:ale_echo_cursor
            autocmd CursorMoved,CursorHold * call ale#cursor#EchoCursorWarningWithDelay()
            " Look for a warning to echo as soon as we leave Insert mode.
            " The script's position variable used when moving the cursor will
            " not be changed here.
            autocmd InsertLeave * call ale#cursor#EchoCursorWarning()
        endif
    augroup END

    if !g:ale_enabled
        augroup! ALERunOnTextChangedGroup
        augroup! ALERunOnEnterGroup
        augroup! ALERunOnInsertLeave
        augroup! ALECursorGroup
    endif
endfunction

function! s:EnablePreamble() abort
    " Set pattern options again, if enabled.
    if g:ale_pattern_options_enabled
        call ale#pattern_options#SetOptions(bufnr(''))
    endif

    " Lint immediately, including running linters against the file.
    call ale#Queue(0, 'lint_file')
endfunction

function! s:DisablePostamble() abort
    " Remove highlights for the current buffer now.
    if g:ale_set_highlights
        call ale#highlight#UpdateHighlights()
    endif
endfunction

function! s:CleanupEveryBuffer() abort
    for l:key in keys(g:ale_buffer_info)
        " The key could be a filename or a buffer number, so try and
        " convert it to a number. We need a number for the other
        " functions.
        let l:buffer = str2nr(l:key)

        if l:buffer > 0
            " Stop all jobs and clear the results for everything, and delete
            " all of the data we stored for the buffer.
            call ale#engine#Cleanup(l:buffer)
        endif
    endfor
endfunction

function! ale#toggle#Toggle() abort
    let g:ale_enabled = !get(g:, 'ale_enabled')

    if g:ale_enabled
        call s:EnablePreamble()

        if g:ale_set_balloons
            call ale#balloon#Enable()
        endif
    else
        call s:CleanupEveryBuffer()
        call s:DisablePostamble()

        if has('balloon_eval')
            call ale#balloon#Disable()
        endif
    endif

    call ale#toggle#InitAuGroups()
endfunction

function! ale#toggle#Enable() abort
    if !g:ale_enabled
        " Set pattern options again, if enabled.
        if g:ale_pattern_options_enabled
            call ale#pattern_options#SetOptions(bufnr(''))
        endif

        call ale#toggle#Toggle()
    endif
endfunction

function! ale#toggle#Disable() abort
    if g:ale_enabled
        call ale#toggle#Toggle()
    endif
endfunction

function! ale#toggle#Reset() abort
    call s:CleanupEveryBuffer()
    call ale#highlight#UpdateHighlights()
endfunction

function! ale#toggle#ToggleBuffer(buffer) abort
    " Get the new value for the toggle.
    let l:enabled = !getbufvar(a:buffer, 'ale_enabled', 1)

    " Disabling ALE globally removes autocmd events, so we cannot enable
    " linting locally when linting is disabled globally
    if l:enabled && !g:ale_enabled
        execute 'echom ''ALE cannot be enabled locally when disabled globally'''
        return
    endif

    call setbufvar(a:buffer, 'ale_enabled', l:enabled)

    if l:enabled
        call s:EnablePreamble()
    else
        " Stop all jobs and clear the results for everything, and delete
        " all of the data we stored for the buffer.
        call ale#engine#Cleanup(a:buffer)
        call s:DisablePostamble()
    endif
endfunction

function! ale#toggle#EnableBuffer(buffer) abort
    " ALE is enabled by default for all buffers.
    if !getbufvar(a:buffer, 'ale_enabled', 1)
        call ale#toggle#ToggleBuffer(a:buffer)
    endif
endfunction

function! ale#toggle#DisableBuffer(buffer) abort
    if getbufvar(a:buffer, 'ale_enabled', 1)
        call ale#toggle#ToggleBuffer(a:buffer)
    endif
endfunction

function! ale#toggle#ResetBuffer(buffer) abort
    call ale#engine#Cleanup(a:buffer)
    call ale#highlight#UpdateHighlights()
endfunction
