" Author: w0rp <devw0rp@gmail.com>

" Get the shell type for a buffer, based on the hashbang line.
function! ale#handlers#sh#GetShellType(buffer) abort
    let l:bang_line = get(getbufline(a:buffer, 1), 0, '')

    " Take the shell executable from the hashbang, if we can.
    if l:bang_line[:1] is# '#!'
        " Remove options like -e, etc.
        let l:command = substitute(l:bang_line, ' --\?[a-zA-Z0-9]\+', '', 'g')

        for l:possible_shell in ['bash', 'dash', 'ash', 'tcsh', 'csh', 'zsh', 'ksh', 'sh']
            if l:command =~# l:possible_shell . '\s*$'
                return l:possible_shell
            endif
        endfor
    endif

    return ''
endfunction
