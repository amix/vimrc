" Author: w0rp <devw0rp@gmail.com>

function! ale#handlers#sh#GetShellType(buffer) abort
    let l:shebang = get(getbufline(a:buffer, 1), 0, '')

    let l:command = ''

    " Take the shell executable from the shebang, if we can.
    if l:shebang[:1] is# '#!'
        " Remove options like -e, etc.
        let l:command = substitute(l:shebang, ' --\?[a-zA-Z0-9]\+', '', 'g')
    endif

    " With no shebang line, attempt to use Vim's buffer-local variables.
    if l:command is# ''
        if getbufvar(a:buffer, 'is_bash', 0)
            let l:command = 'bash'
        elseif getbufvar(a:buffer, 'is_sh', 0)
            let l:command = 'sh'
        elseif getbufvar(a:buffer, 'is_kornshell', 0)
            let l:command = 'ksh'
        endif
    endif

    " If we couldn't find a shebang, try the filetype
    if l:command is# ''
        let l:command = &filetype
    endif

    for l:possible_shell in ['bash', 'dash', 'ash', 'tcsh', 'csh', 'zsh', 'ksh', 'sh']
        if l:command =~# l:possible_shell . '\s*$'
            return l:possible_shell
        endif
    endfor

    return ''
endfunction
