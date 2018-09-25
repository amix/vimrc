" Author: w0rp <devw0rp@gmail.com>
" Description: A linter for checking ALE project code itself.

function! ale_linters#vim#ale_custom_linting_rules#GetExecutable(buffer) abort
    let l:filename = expand('#' . a:buffer . ':p')
    let l:dir_list = []

    for l:dir in split(&runtimepath, ',')
        if l:filename[:len(l:dir) - 1] is# l:dir
            call add(l:dir_list, l:dir)
        endif
    endfor

    return !empty(l:dir_list)
    \   ? findfile('test/script/custom-linting-rules', join(l:dir_list, ','))
    \   : ''
endfunction

function! s:GetALEProjectDir(buffer) abort
    let l:executable = ale_linters#vim#ale_custom_linting_rules#GetExecutable(a:buffer)

    return ale#path#Dirname(ale#path#Dirname(ale#path#Dirname(l:executable)))
endfunction

function! ale_linters#vim#ale_custom_linting_rules#GetCommand(buffer) abort
    let l:dir = s:GetALEProjectDir(a:buffer)

    let l:temp_dir = ale#engine#CreateDirectory(a:buffer)
    let l:temp_file = l:temp_dir . '/example.vim'

    let l:lines = getbufline(a:buffer, 1, '$')
    call ale#util#Writefile(a:buffer, l:lines, l:temp_file)

    return ale#path#CdString(l:dir) . '%e ' . ale#Escape(l:temp_dir)
endfunction

function! ale_linters#vim#ale_custom_linting_rules#Handle(buffer, lines) abort
    let l:dir = s:GetALEProjectDir(a:buffer)
    let l:output = []
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+) (.+)$'

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        " Ignore trailing whitespace errors if we've turned them off.
        if !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
        \&& l:match[3] is# 'Trailing whitespace'
            continue
        endif

        call add(l:output, {
        \   'lnum': l:match[2],
        \   'text': l:match[3],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('vim', {
\   'name': 'ale_custom_linting_rules',
\   'executable_callback': 'ale_linters#vim#ale_custom_linting_rules#GetExecutable',
\   'command_callback': 'ale_linters#vim#ale_custom_linting_rules#GetCommand',
\   'callback': 'ale_linters#vim#ale_custom_linting_rules#Handle',
\   'read_buffer': 0,
\})
