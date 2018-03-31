" Author: w0rp <devw0rp@gmail.com>
" Description: Special command formatting for creating temporary files and
" passing buffer filenames easily.

function! s:TemporaryFilename(buffer) abort
    let l:filename = fnamemodify(bufname(a:buffer), ':t')

    if empty(l:filename)
        " If the buffer's filename is empty, create a dummy filename.
        let l:ft = getbufvar(a:buffer, '&filetype')
        let l:filename = 'file' . ale#filetypes#GuessExtension(l:ft)
    endif

    " Create a temporary filename, <temp_dir>/<original_basename>
    " The file itself will not be created by this function.
    return tempname() . (has('win32') ? '\' : '/') . l:filename
endfunction

" Given a command string, replace every...
" %s -> with the current filename
" %t -> with the name of an unused file in a temporary directory
" %% -> with a literal %
function! ale#command#FormatCommand(buffer, command, pipe_file_if_needed) abort
    let l:temporary_file = ''
    let l:command = a:command

    " First replace all uses of %%, used for literal percent characters,
    " with an ugly string.
    let l:command = substitute(l:command, '%%', '<<PERCENTS>>', 'g')

    " Replace all %s occurrences in the string with the name of the current
    " file.
    if l:command =~# '%s'
        let l:filename = fnamemodify(bufname(a:buffer), ':p')
        let l:command = substitute(l:command, '%s', '\=ale#Escape(l:filename)', 'g')
    endif

    if l:command =~# '%t'
        " Create a temporary filename, <temp_dir>/<original_basename>
        " The file itself will not be created by this function.
        let l:temporary_file = s:TemporaryFilename(a:buffer)
        let l:command = substitute(l:command, '%t', '\=ale#Escape(l:temporary_file)', 'g')
    endif

    " Finish formatting so %% becomes %.
    let l:command = substitute(l:command, '<<PERCENTS>>', '%', 'g')

    if a:pipe_file_if_needed && empty(l:temporary_file)
        " If we are to send the Vim buffer to a command, we'll do it
        " in the shell. We'll write out the file to a temporary file,
        " and then read it back in, in the shell.
        let l:temporary_file = s:TemporaryFilename(a:buffer)
        let l:command = l:command . ' < ' . ale#Escape(l:temporary_file)
    endif

    return [l:temporary_file, l:command]
endfunction
