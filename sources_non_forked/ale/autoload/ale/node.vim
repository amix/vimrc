" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for working with Node executables.

call ale#Set('windows_node_executable_path', 'node.exe')

" Create a executable string which executes a Node.js script command with a
" Node.js executable if needed.
"
" The executable string should not be escaped before passing it to this
" function, the executable string will be escaped when returned by this
" function.
"
" The executable is only prefixed for Windows machines
function! ale#node#Executable(buffer, executable) abort
    if has('win32') && a:executable =~? '\.js$'
        let l:node = ale#Var(a:buffer, 'windows_node_executable_path')

        return ale#Escape(l:node) . ' ' . ale#Escape(a:executable)
    endif

    return ale#Escape(a:executable)
endfunction
