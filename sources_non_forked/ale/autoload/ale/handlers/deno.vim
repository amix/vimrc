" Author: Mohammed Chelouti - https://github.com/motato1
"         Arnold Chand <creativenull@outlook.com>
" Description: Handler functions for Deno.

call ale#Set('deno_executable', 'deno')
call ale#Set('deno_unstable', 0)
call ale#Set('deno_importMap', 'import_map.json')
call ale#Set('deno_lsp_project_root', '')

function! ale#handlers#deno#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'deno_executable')
endfunction

" Find project root for Deno's language server.
"
" Deno projects do not require a project or configuration file at the project root.
" This means the root directory has to be guessed,
" unless it is explicitly specified by the user.
"
" The project root is determined by ...
" 1. using a user-specified value from deno_lsp_project_root
" 2. looking for common top-level files/dirs
" 3. using the buffer's directory
function! ale#handlers#deno#GetProjectRoot(buffer) abort
    let l:project_root = ale#Var(a:buffer, 'deno_lsp_project_root')

    if !empty(l:project_root)
        return l:project_root
    endif

    let l:possible_project_roots = [
    \   'deno.json',
    \   'deno.jsonc',
    \   'tsconfig.json',
    \   '.git',
    \   bufname(a:buffer),
    \]

    for l:possible_root in l:possible_project_roots
        let l:project_root = ale#path#FindNearestFile(a:buffer, l:possible_root)

        if empty(l:project_root)
            let l:project_root = ale#path#FindNearestDirectory(a:buffer, l:possible_root)
        endif

        if !empty(l:project_root)
            " dir:p expands to /full/path/to/dir/ whereas
            " file:p expands to /full/path/to/file (no trailing slash)
            " Appending '/' ensures that :h:h removes the path's last segment
            " regardless of whether it is a directory or not.
            return fnamemodify(l:project_root . '/', ':p:h:h')
        endif
    endfor

    return ''
endfunction

" Initialization Options for deno, for javascript and typescript
function! ale#handlers#deno#GetInitializationOptions(buffer) abort
    let l:options = {
    \   'enable': v:true,
    \   'lint': v:true,
    \   'unstable': v:false,
    \   'importMap': ale#path#FindNearestFile(a:buffer, 'import_map.json'),
    \   }

    if ale#Var(a:buffer, 'deno_unstable')
        let l:options.unstable = v:true
    endif

    if ale#Var(a:buffer, 'deno_importMap') isnot# ''
        let l:options.importMap = ale#path#FindNearestFile(a:buffer, ale#Var(a:buffer, 'deno_importMap'))
    endif

    return l:options
endfunction
