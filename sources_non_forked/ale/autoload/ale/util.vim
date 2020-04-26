" Author: w0rp <devw0rp@gmail.com>
" Description: Contains miscellaneous functions

" A wrapper function for mode() so we can test calls for it.
function! ale#util#Mode(...) abort
    return call('mode', a:000)
endfunction

" A wrapper function for feedkeys so we can test calls for it.
function! ale#util#FeedKeys(...) abort
    return call('feedkeys', a:000)
endfunction

" Show a message in as small a window as possible.
"
" Vim 8 does not support echoing long messages from asynchronous callbacks,
" but NeoVim does. Small messages can be echoed in Vim 8, and larger messages
" have to be shown in preview windows.
function! ale#util#ShowMessage(string) abort
    if !has('nvim')
        call ale#preview#CloseIfTypeMatches('ale-preview.message')
    endif

    " We have to assume the user is using a monospace font.
    if has('nvim') || (a:string !~? "\n" && len(a:string) < &columns)
        execute 'echo a:string'
    else
        call ale#preview#Show(split(a:string, "\n"), {
        \   'filetype': 'ale-preview.message',
        \   'stay_here': 1,
        \})
    endif
endfunction

" A wrapper function for execute, so we can test executing some commands.
function! ale#util#Execute(expr) abort
    execute a:expr
endfunction

if !exists('g:ale#util#nul_file')
    " A null file for sending output to nothing.
    let g:ale#util#nul_file = '/dev/null'

    if has('win32')
        let g:ale#util#nul_file = 'nul'
    endif
endif

" Given a job, a buffered line of data, a list of parts of lines, a mode data
" is being read in, and a callback, join the lines of output for a NeoVim job
" or socket together, and call the callback with the joined output.
"
" Note that jobs and IDs are the same thing on NeoVim.
function! ale#util#JoinNeovimOutput(job, last_line, data, mode, callback) abort
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

" Return the number of lines for a given buffer.
function! ale#util#GetLineCount(buffer) abort
    return len(getbufline(a:buffer, 1, '$'))
endfunction

function! ale#util#GetFunction(string_or_ref) abort
    if type(a:string_or_ref) is v:t_string
        return function(a:string_or_ref)
    endif

    return a:string_or_ref
endfunction

" Open the file (at the given line).
" options['open_in'] can be:
"   current-buffer (default)
"   tab
"   split
"   vsplit
function! ale#util#Open(filename, line, column, options) abort
    let l:open_in = get(a:options, 'open_in', 'current-buffer')
    let l:args_to_open = '+' . a:line . ' ' . fnameescape(a:filename)

    if l:open_in is# 'tab'
        call ale#util#Execute('tabedit ' . l:args_to_open)
    elseif l:open_in is# 'split'
        call ale#util#Execute('split ' . l:args_to_open)
    elseif l:open_in is# 'vsplit'
        call ale#util#Execute('vsplit ' . l:args_to_open)
    elseif bufnr(a:filename) isnot bufnr('')
        " Open another file only if we need to.
        call ale#util#Execute('edit ' . l:args_to_open)
    else
        normal! m`
    endif

    call cursor(a:line, a:column)
    normal! zz
endfunction

let g:ale#util#error_priority = 5
let g:ale#util#warning_priority = 4
let g:ale#util#info_priority = 3
let g:ale#util#style_error_priority = 2
let g:ale#util#style_warning_priority = 1

function! ale#util#GetItemPriority(item) abort
    if a:item.type is# 'I'
        return g:ale#util#info_priority
    endif

    if a:item.type is# 'W'
        if get(a:item, 'sub_type', '') is# 'style'
            return g:ale#util#style_warning_priority
        endif

        return g:ale#util#warning_priority
    endif

    if get(a:item, 'sub_type', '') is# 'style'
        return g:ale#util#style_error_priority
    endif

    return g:ale#util#error_priority
endfunction

" Compare two loclist items for ALE, sorted by their buffers, filenames, and
" line numbers and column numbers.
function! ale#util#LocItemCompare(left, right) abort
    if a:left.bufnr < a:right.bufnr
        return -1
    endif

    if a:left.bufnr > a:right.bufnr
        return 1
    endif

    if a:left.bufnr == -1
        if a:left.filename < a:right.filename
            return -1
        endif

        if a:left.filename > a:right.filename
            return 1
        endif
    endif

    if a:left.lnum < a:right.lnum
        return -1
    endif

    if a:left.lnum > a:right.lnum
        return 1
    endif

    if a:left.col < a:right.col
        return -1
    endif

    if a:left.col > a:right.col
        return 1
    endif

    " When either of the items lacks a problem type, then the two items should
    " be considered equal. This is important for loclist jumping.
    if !has_key(a:left, 'type') || !has_key(a:right, 'type')
        return 0
    endif

    let l:left_priority = ale#util#GetItemPriority(a:left)
    let l:right_priority = ale#util#GetItemPriority(a:right)

    if l:left_priority < l:right_priority
        return -1
    endif

    if l:left_priority > l:right_priority
        return 1
    endif

    return 0
endfunction

" Compare two loclist items, including the text for the items.
"
" This function can be used for de-duplicating lists.
function! ale#util#LocItemCompareWithText(left, right) abort
    let l:cmp_value = ale#util#LocItemCompare(a:left, a:right)

    if l:cmp_value
        return l:cmp_value
    endif

    if a:left.text < a:right.text
        return -1
    endif

    if a:left.text > a:right.text
        return 1
    endif

    return 0
endfunction

" This function will perform a binary search and a small sequential search
" on the list to find the last problem in the buffer and line which is
" on or before the column. The index of the problem will be returned.
"
" -1 will be returned if nothing can be found.
function! ale#util#BinarySearch(loclist, buffer, line, column) abort
    let l:min = 0
    let l:max = len(a:loclist) - 1

    while 1
        if l:max < l:min
            return -1
        endif

        let l:mid = (l:min + l:max) / 2
        let l:item = a:loclist[l:mid]

        " Binary search for equal buffers, equal lines, then near columns.
        if l:item.bufnr < a:buffer
            let l:min = l:mid + 1
        elseif l:item.bufnr > a:buffer
            let l:max = l:mid - 1
        elseif l:item.lnum < a:line
            let l:min = l:mid + 1
        elseif l:item.lnum > a:line
            let l:max = l:mid - 1
        else
            " This part is a small sequential search.
            let l:index = l:mid

            " Search backwards to find the first problem on the line.
            while l:index > 0
            \&& a:loclist[l:index - 1].bufnr == a:buffer
            \&& a:loclist[l:index - 1].lnum == a:line
                let l:index -= 1
            endwhile

            " Find the last problem on or before this column.
            while l:index < l:max
            \&& a:loclist[l:index + 1].bufnr == a:buffer
            \&& a:loclist[l:index + 1].lnum == a:line
            \&& a:loclist[l:index + 1].col <= a:column
                let l:index += 1
            endwhile

            " Scan forwards to find the last item on the column for the item
            " we found, which will have the most serious problem.
            let l:item_column = a:loclist[l:index].col

            while l:index < l:max
            \&& a:loclist[l:index + 1].bufnr == a:buffer
            \&& a:loclist[l:index + 1].lnum == a:line
            \&& a:loclist[l:index + 1].col == l:item_column
                let l:index += 1
            endwhile

            return l:index
        endif
    endwhile
endfunction

" A function for testing if a function is running inside a sandbox.
" See :help sandbox
function! ale#util#InSandbox() abort
    try
        let &l:equalprg=&l:equalprg
    catch /E48/
        " E48 is the sandbox error.
        return 1
    endtry

    return 0
endfunction

function! ale#util#Tempname() abort
    let l:clear_tempdir = 0

    if exists('$TMPDIR') && empty($TMPDIR)
        let l:clear_tempdir = 1
        let $TMPDIR = '/tmp'
    endif

    try
        let l:name = tempname() " no-custom-checks
    finally
        if l:clear_tempdir
            let $TMPDIR = ''
        endif
    endtry

    return l:name
endfunction

" Given a single line, or a List of lines, and a single pattern, or a List
" of patterns, return all of the matches for the lines(s) from the given
" patterns, using matchlist().
"
" Only the first pattern which matches a line will be returned.
function! ale#util#GetMatches(lines, patterns) abort
    let l:matches = []
    let l:lines = type(a:lines) is v:t_list ? a:lines : [a:lines]
    let l:patterns = type(a:patterns) is v:t_list ? a:patterns : [a:patterns]

    for l:line in l:lines
        for l:pattern in l:patterns
            let l:match = matchlist(l:line, l:pattern)

            if !empty(l:match)
                call add(l:matches, l:match)
                break
            endif
        endfor
    endfor

    return l:matches
endfunction

function! s:LoadArgCount(function) abort
    try
        let l:output = execute('function a:function')
    catch /E123/
        return 0
    endtry

    let l:match = matchstr(split(l:output, "\n")[0], '\v\([^)]+\)')[1:-2]
    let l:arg_list = filter(split(l:match, ', '), 'v:val isnot# ''...''')

    return len(l:arg_list)
endfunction

" Given the name of a function, a Funcref, or a lambda, return the number
" of named arguments for a function.
function! ale#util#FunctionArgCount(function) abort
    let l:Function = ale#util#GetFunction(a:function)
    let l:count = s:LoadArgCount(l:Function)

    " If we failed to get the count, forcibly load the autoload file, if the
    " function is an autoload function. autoload functions aren't normally
    " defined until they are called.
    if l:count == 0
        let l:function_name = matchlist(string(l:Function), 'function([''"]\(.\+\)[''"])')[1]

        if l:function_name =~# '#'
            execute 'runtime autoload/' . join(split(l:function_name, '#')[:-2], '/') . '.vim'
            let l:count = s:LoadArgCount(l:Function)
        endif
    endif

    return l:count
endfunction

" Escape a string so the characters in it will be safe for use inside of PCRE
" or RE2 regular expressions without characters having special meanings.
function! ale#util#EscapePCRE(unsafe_string) abort
    return substitute(a:unsafe_string, '\([\-\[\]{}()*+?.^$|]\)', '\\\1', 'g')
endfunction

" Escape a string so that it can be used as a literal string inside an evaled
" vim command.
function! ale#util#EscapeVim(unsafe_string) abort
    return "'" . substitute(a:unsafe_string, "'", "''", 'g') . "'"
endfunction


" Given a String or a List of String values, try and decode the string(s)
" as a JSON value which can be decoded with json_decode. If the JSON string
" is invalid, the default argument value will be returned instead.
"
" This function is useful in code where the data can't be trusted to be valid
" JSON, and where throwing exceptions is mostly just irritating.
function! ale#util#FuzzyJSONDecode(data, default) abort
    if empty(a:data)
        return a:default
    endif

    let l:str = type(a:data) is v:t_string ? a:data : join(a:data, '')

    try
        let l:result = json_decode(l:str)

        " Vim 8 only uses the value v:none for decoding blank strings.
        if !has('nvim') && l:result is v:none
            return a:default
        endif

        return l:result
    catch /E474/
        return a:default
    endtry
endfunction

" Write a file, including carriage return characters for DOS files.
"
" The buffer number is required for determining the fileformat setting for
" the buffer.
function! ale#util#Writefile(buffer, lines, filename) abort
    let l:corrected_lines = getbufvar(a:buffer, '&fileformat') is# 'dos'
    \   ? map(copy(a:lines), 'substitute(v:val, ''\r*$'', ''\r'', '''')')
    \   : a:lines

    call writefile(l:corrected_lines, a:filename, 'S') " no-custom-checks
endfunction

if !exists('s:patial_timers')
    let s:partial_timers = {}
endif

function! s:ApplyPartialTimer(timer_id) abort
    if has_key(s:partial_timers, a:timer_id)
        let [l:Callback, l:args] = remove(s:partial_timers, a:timer_id)
        call call(l:Callback, [a:timer_id] + l:args)
    endif
endfunction

" Given a delay, a callback, a List of arguments, start a timer with
" timer_start() and call the callback provided with [timer_id] + args.
"
" The timer must not be stopped with timer_stop().
" Use ale#util#StopPartialTimer() instead, which can stop any timer, and will
" clear any arguments saved for executing callbacks later.
function! ale#util#StartPartialTimer(delay, callback, args) abort
    let l:timer_id = timer_start(a:delay, function('s:ApplyPartialTimer'))
    let s:partial_timers[l:timer_id] = [a:callback, a:args]

    return l:timer_id
endfunction

function! ale#util#StopPartialTimer(timer_id) abort
    call timer_stop(a:timer_id)

    if has_key(s:partial_timers, a:timer_id)
        call remove(s:partial_timers, a:timer_id)
    endif
endfunction

" Given a possibly multi-byte string and a 1-based character position on a
" line, return the 1-based byte position on that line.
function! ale#util#Col(str, chr) abort
    if a:chr < 2
        return a:chr
    endif

    return strlen(join(split(a:str, '\zs')[0:a:chr - 2], '')) + 1
endfunction

function! ale#util#FindItemAtCursor(buffer) abort
    let l:info = get(g:ale_buffer_info, a:buffer, {})
    let l:loclist = get(l:info, 'loclist', [])
    let l:pos = getpos('.')
    let l:index = ale#util#BinarySearch(l:loclist, a:buffer, l:pos[1], l:pos[2])
    let l:loc = l:index >= 0 ? l:loclist[l:index] : {}

    return [l:info, l:loc]
endfunction

function! ale#util#Input(message, value) abort
    return input(a:message, a:value)
endfunction

function! ale#util#HasBuflineApi() abort
    return exists('*deletebufline') && exists('*setbufline')
endfunction

" Sets buffer contents to lines
function! ale#util#SetBufferContents(buffer, lines) abort
    let l:has_bufline_api = ale#util#HasBuflineApi()

    if !l:has_bufline_api && a:buffer isnot bufnr('')
        return
    endif

    " If the file is in DOS mode, we have to remove carriage returns from
    " the ends of lines before calling setline(), or we will see them
    " twice.
    let l:new_lines = getbufvar(a:buffer, '&fileformat') is# 'dos'
    \   ? map(copy(a:lines), 'substitute(v:val, ''\r\+$'', '''', '''')')
    \   : a:lines
    let l:first_line_to_remove = len(l:new_lines) + 1

    " Use a Vim API for setting lines in other buffers, if available.
    if l:has_bufline_api
        call setbufline(a:buffer, 1, l:new_lines)
        call deletebufline(a:buffer, l:first_line_to_remove, '$')
    " Fall back on setting lines the old way, for the current buffer.
    else
        let l:old_line_length = line('$')

        if l:old_line_length >= l:first_line_to_remove
            let l:save = winsaveview()
            silent execute
            \   l:first_line_to_remove . ',' . l:old_line_length . 'd_'
            call winrestview(l:save)
        endif

        call setline(1, l:new_lines)
    endif

    return l:new_lines
endfunction
