function! s:EnablePreamble() abort
    " Set pattern options again, if enabled.
    if get(g:, 'ale_pattern_options_enabled', 0)
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

    if g:ale_virtualtext_cursor isnot# 'disabled' && g:ale_virtualtext_cursor != 0
        call ale#virtualtext#Clear(bufnr(''))
    endif
endfunction

function! ale#toggle#Toggle() abort
    let g:ale_enabled = !get(g:, 'ale_enabled')

    if g:ale_enabled
        call s:EnablePreamble()

        if g:ale_set_balloons
            call ale#balloon#Enable()
        endif
    else
        call ale#engine#CleanupEveryBuffer()
        call s:DisablePostamble()

        if exists('*ale#balloon#Disable')
            call ale#balloon#Disable()
        endif
    endif

    call ale#events#Init()
endfunction

function! ale#toggle#Enable() abort
    if !g:ale_enabled
        call ale#toggle#Toggle()
    endif
endfunction

function! ale#toggle#Disable() abort
    if g:ale_enabled
        call ale#toggle#Toggle()
    endif
endfunction

function! ale#toggle#Reset() abort
    call ale#engine#CleanupEveryBuffer()
    call ale#highlight#UpdateHighlights()
endfunction

function! ale#toggle#ToggleBuffer(buffer) abort
    " Get the new value for the toggle.
    let l:enabled = !getbufvar(a:buffer, 'ale_enabled', 1)

    " Disabling ALE globally removes autocmd events, so we cannot enable
    " linting locally when linting is disabled globally
    if l:enabled && !g:ale_enabled
        " no-custom-checks
        echom 'ALE cannot be enabled locally when disabled globally'

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
