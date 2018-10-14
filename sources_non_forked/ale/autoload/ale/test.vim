" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for making testing ALE easier.
"
" This file should not typically be loaded during the normal execution of ALE.

" Change the directory for checking things in particular test directories
"
" This function will set the g:dir variable, which represents the working
" directory after changing the path. This variable allows a test to change
" directories, and then switch back to a directory at the start of the test
" run.
"
" This function should be run in a Vader Before: block.
function! ale#test#SetDirectory(docker_path) abort
    if a:docker_path[:len('/testplugin/') - 1] isnot# '/testplugin/'
        throw 'docker_path must start with /testplugin/!'
    endif

    " Try to switch directory, which will fail when running tests directly,
    " and not through the Docker image.
    silent! execute 'cd ' . fnameescape(a:docker_path)
    let g:dir = getcwd() " no-custom-checks
endfunction

" When g:dir is defined, switch back to the directory we saved, and then
" delete that variable.
"
" The filename will be reset to dummy.txt
"
" This function should be run in a Vader After: block.
function! ale#test#RestoreDirectory() abort
    call ale#test#SetFilename('dummy.txt')
    silent execute 'cd ' . fnameescape(g:dir)
    unlet! g:dir
endfunction

" Change the filename for the current buffer using a relative path to
" the script without running autocmd commands.
"
" If a g:dir variable is set, it will be used as the path to the directory
" containing the test file.
function! ale#test#SetFilename(path) abort
    let l:dir = get(g:, 'dir', '')

    if empty(l:dir)
        let l:dir = getcwd() " no-custom-checks
    endif

    let l:full_path = ale#path#IsAbsolute(a:path)
    \   ? a:path
    \   : l:dir . '/' . a:path

    silent! noautocmd execute 'file ' . fnameescape(ale#path#Simplify(l:full_path))
endfunction

function! s:RemoveModule(results) abort
    for l:item in a:results
      if has_key(l:item, 'module')
        call remove(l:item, 'module')
      endif
    endfor
endfunction

" Return loclist data without the module string, only in newer Vim versions.
function! ale#test#GetLoclistWithoutModule() abort
    let l:results = getloclist(0)
    call s:RemoveModule(l:results)

    return l:results
endfunction

function! ale#test#GetQflistWithoutModule() abort
    let l:results = getqflist()
    call s:RemoveModule(l:results)

    return l:results
endfunction
