" Author: zigford <zigford@gmail.com>
" Description: Functions for integrating with Powershell linters.

" Write a powershell script to a temp file for execution
" return the command used to execute it
function! s:TemporaryPSScript(buffer, input) abort
    let l:filename = 'script.ps1'
    " Create a temp dir to house our temp .ps1 script
    " a temp dir is needed as powershell needs the .ps1
    " extension
    let l:tempdir = ale#util#Tempname() . (has('win32') ? '\' : '/')
    let l:tempscript = l:tempdir . l:filename
    " Create the temporary directory for the file, unreadable by 'other'
    " users.
    call mkdir(l:tempdir, '', 0750)
    " Automatically delete the directory later.
    call ale#command#ManageDirectory(a:buffer, l:tempdir)
    " Write the script input out to a file.
    call ale#util#Writefile(a:buffer, a:input, l:tempscript)

    return l:tempscript
endfunction

function! ale#powershell#RunPowerShell(buffer, base_var_name, command) abort
    let l:executable = ale#Var(a:buffer, a:base_var_name . '_executable')
    let l:tempscript = s:TemporaryPSScript(a:buffer, a:command)

    return ale#Escape(l:executable)
    \ . ' -Exe Bypass -NoProfile -File '
    \ . ale#Escape(l:tempscript)
    \ . ' %t'
endfunction
