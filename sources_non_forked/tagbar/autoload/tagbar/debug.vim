function! tagbar#debug#start_debug(...) abort
    let filename = a:0 > 0 ? a:1 : ''

    if empty(filename)
        let s:debug_file = 'tagbardebug.log'
    else
        let s:debug_file = filename
    endif

    " Clear log file and start it with version info
    exe 'redir! > ' . s:debug_file
    silent version
    redir END

    " Check whether the log file could be created
    if !filewritable(s:debug_file)
        echomsg 'Tagbar: Unable to create log file ' . s:debug_file
        let s:debug_file = ''
        return
    endif

    let s:debug_enabled = 1
endfunction

function! tagbar#debug#stop_debug() abort
    let s:debug_enabled = 0
    let s:debug_file = ''
endfunction

function! tagbar#debug#log(msg) abort
    if s:debug_enabled
        execute 'redir >> ' . s:debug_file
        silent echon s:gettime() . ': ' . a:msg . "\n"
        redir END
    endif
endfunction

function! tagbar#debug#log_ctags_output(output) abort
    if s:debug_enabled
        exe 'redir! > ' . s:debug_file . '.ctags_out'
        silent echon a:output
        redir END
    endif
endfunction

function! tagbar#debug#enabled() abort
    return s:debug_enabled
endfunction

if has('reltime')
    function! s:gettime() abort
        let time = split(reltimestr(reltime()), '\.')
        return strftime('%Y-%m-%d %H:%M:%S.', time[0]) . time[1]
    endfunction
else
    function! s:gettime() abort
        return strftime('%Y-%m-%d %H:%M:%S')
    endfunction
endif

let s:debug_enabled = 0
let s:debug_file = ''
