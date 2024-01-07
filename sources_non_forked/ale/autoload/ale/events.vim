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

    if ale#Var(a:buffer, 'fix_on_save') && !ale#events#QuitRecently(a:buffer)
        let l:will_fix = ale#fix#Fix(a:buffer, 'save_file')
        let l:should_lint = l:should_lint && !l:will_fix
    endif

    if l:should_lint && !ale#events#QuitRecently(a:buffer)
        call ale#Queue(0, 'lint_file', a:buffer)
    endif
endfunction

function! ale#events#LintOnEnter(buffer) abort
    " Unmark a file as being changed outside of Vim after we try to check it.
    call setbufvar(a:buffer, 'ale_file_changed', 0)

    if ale#Var(a:buffer, 'enabled') && g:ale_lint_on_enter
        call ale#Queue(0, 'lint_file', a:buffer)
    endif
endfunction

function! ale#events#ReadOrEnterEvent(buffer) abort
    " Apply pattern options if the variable is set.
    if get(g:, 'ale_pattern_options_enabled', 1)
    \&& !empty(get(g:, 'ale_pattern_options'))
        call ale#pattern_options#SetOptions(a:buffer)
    endif

    " When entering a buffer, we are no longer quitting it.
    call setbufvar(a:buffer, 'ale_quitting', 0)
    let l:filetype = getbufvar(a:buffer, '&filetype')
    call setbufvar(a:buffer, 'ale_original_filetype', l:filetype)

    " If the file changed outside of Vim, check it on BufEnter,BufRead
    if getbufvar(a:buffer, 'ale_file_changed')
        call ale#events#LintOnEnter(a:buffer)
    endif
endfunction

function! ale#events#FileTypeEvent(buffer, new_filetype) abort
    " The old filetype will be set to an empty string by the BuFEnter event,
    " and not linting when the old filetype hasn't been set yet prevents
    " buffers being checked when you enter them when linting on enter is off.
    let l:old_filetype = getbufvar(a:buffer, 'ale_original_filetype', v:null)

    if l:old_filetype isnot v:null
    \&& !empty(a:new_filetype)
    \&& a:new_filetype isnot# l:old_filetype
        " Remember what the new filetype is.
        call setbufvar(a:buffer, 'ale_original_filetype', a:new_filetype)

        if g:ale_lint_on_filetype_changed
            call ale#Queue(300, 'lint_file', a:buffer)
        endif
    endif
endfunction

function! ale#events#FileChangedEvent(buffer) abort
    call setbufvar(a:buffer, 'ale_file_changed', 1)

    if bufnr('') == a:buffer
        call ale#events#LintOnEnter(a:buffer)
    endif
endfunction

" A timer for emulating InsertLeave.
"
" We only need a single timer, and we'll lint the last buffer we entered
" insert mode on.
if !exists('s:insert_leave_timer')
    let s:insert_leave_timer = -1
endif

function! ale#events#EmulateInsertLeave(buffer) abort
    if mode() is# 'n'
        call timer_stop(s:insert_leave_timer)
        call ale#Queue(0, '', a:buffer)
    endif
endfunction

function! ale#events#InsertEnterEvent(buffer) abort
    if g:ale_close_preview_on_insert && exists('*ale#preview#CloseIfTypeMatches')
        call ale#preview#CloseIfTypeMatches('ale-preview')
    endif

    " Start a repeating timer if the use might not trigger InsertLeave, so we
    " can emulate its behavior.
    if ale#Var(a:buffer, 'lint_on_insert_leave')
    \&& maparg("\<C-c>", 'i') isnot# '<Esc>'
        call timer_stop(s:insert_leave_timer)
        let s:insert_leave_timer = timer_start(
        \   100,
        \   {-> ale#events#EmulateInsertLeave(a:buffer) },
        \   {'repeat': -1}
        \)
    endif
endfunction

function! ale#events#InsertLeaveEvent(buffer) abort
    if ale#Var(a:buffer, 'lint_on_insert_leave')
        " Kill the InsertLeave emulation if the event fired.
        call timer_stop(s:insert_leave_timer)
        call ale#Queue(0)
    endif

    " Look for a warning to echo as soon as we leave Insert mode.
    " The script's position variable used when moving the cursor will
    " not be changed here.
    "
    " We don't echo this message in emulated insert leave mode, as the user
    " may want less work to happen on pressing <C-c> versus <Esc>
    if exists('*ale#engine#Cleanup')
        call ale#cursor#EchoCursorWarning()

        if g:ale_virtualtext_cursor is# 'current' || g:ale_virtualtext_cursor is# 1 || g:ale_virtualtext_cursor is# '1'
            " Show a virtualtext message if enabled.
            call ale#virtualtext#ShowCursorWarning()
        endif
    endif
endfunction

function! ale#events#Init() abort
    " This value used to be a Boolean as a Number, and is now a String.
    let l:text_changed = '' . g:ale_lint_on_text_changed

    augroup ALEEvents
        autocmd!

        " These events always need to be set up.
        autocmd BufEnter,BufRead * call ale#events#ReadOrEnterEvent(str2nr(expand('<abuf>')))
        autocmd BufWritePost * call ale#events#SaveEvent(str2nr(expand('<abuf>')))

        if g:ale_enabled
            if l:text_changed is? 'always' || l:text_changed is# '1'
                autocmd TextChanged,TextChangedI * call ale#Queue(ale#Var(str2nr(expand('<abuf>')), 'lint_delay'))
            elseif l:text_changed is? 'normal'
                autocmd TextChanged * call ale#Queue(ale#Var(str2nr(expand('<abuf>')), 'lint_delay'))
            elseif l:text_changed is? 'insert'
                autocmd TextChangedI * call ale#Queue(ale#Var(str2nr(expand('<abuf>')), 'lint_delay'))
            endif

            if g:ale_lint_on_enter
                autocmd BufWinEnter * call ale#events#LintOnEnter(str2nr(expand('<abuf>')))
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

            " Add an InsertEnter event if we need to close the preview window
            " on entering insert mode, or if we want to run ALE on leaving
            " insert mode and <C-c> is not the same as <Esc>.
            "
            " We will emulate leaving insert mode for users that might not
            " trigger InsertLeave.
            if g:ale_close_preview_on_insert
            \|| (g:ale_lint_on_insert_leave && maparg("\<C-c>", 'i') isnot# '<Esc>')
                autocmd InsertEnter * call ale#events#InsertEnterEvent(str2nr(expand('<abuf>')))
            endif

            let l:add_insert_leave_event = g:ale_lint_on_insert_leave

            if g:ale_echo_cursor || g:ale_cursor_detail
                " We need to make the message display on InsertLeave
                let l:add_insert_leave_event = 1

                autocmd CursorMoved,CursorHold * if exists('*ale#engine#Cleanup') | call ale#cursor#EchoCursorWarningWithDelay() | endif
            endif

            if g:ale_virtualtext_cursor is# 'current' || g:ale_virtualtext_cursor is# 1 || g:ale_virtualtext_cursor is# '1'
                " We need to make the message display on InsertLeave
                let l:add_insert_leave_event = 1

                autocmd CursorMoved,CursorHold * if exists('*ale#engine#Cleanup') | call ale#virtualtext#ShowCursorWarningWithDelay() | endif
            endif

            if l:add_insert_leave_event
                autocmd InsertLeave * call ale#events#InsertLeaveEvent(str2nr(expand('<abuf>')))
            endif

            if g:ale_hover_cursor
                autocmd CursorHold * if exists('*ale#lsp#Send') | call ale#hover#ShowTruncatedMessageAtCursor() | endif
            endif
        endif
    augroup END

    augroup AleURISchemes
        autocmd!

        autocmd BufNewFile,BufReadPre jdt://** call ale#uri#jdt#ReadJDTLink(expand('<amatch>'))
    augroup END
endfunction
