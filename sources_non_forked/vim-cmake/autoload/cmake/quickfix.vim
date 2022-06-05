" ==============================================================================
" Location:    autoload/cmake/quickfix.vim
" Description: Functions for populating the quickfix window
" ==============================================================================

let s:quickfix = {}
let s:quickfix.list = {}
let s:quickfix.list.items = []
let s:quickfix.list.title = 'CMakeBuild'
let s:quickfix.id = -1

let s:filters = [
        \ 'v:val.valid == 1',
        \ 'filereadable(bufname(v:val.bufnr))',
        \ ]

let s:logger = cmake#logger#Get()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Public functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Generate Quickfix list from lines
"
" Params:
"     lines_to_parse : List
"         list of lines to parse to generate Quickfix list
"
function! s:quickfix.Generate(lines_to_parse) abort
    call s:logger.LogDebug('Invoked: s:quickfix.Generate()')
    " Create a list of quickfix items from the output of the last command.
    let l:list = getqflist({'lines': a:lines_to_parse})
    let l:self.list.items = filter(l:list.items, join(s:filters, ' && '))
    " If a quickfix list for Vim-CMake exists, make that list active and replace
    " its items with the new ones.
    if getqflist({'id': l:self.id}).id == l:self.id
        let l:current = getqflist({'nr': 0}).nr
        let l:target = getqflist({'id': l:self.id, 'nr': 0}).nr
        if l:current > l:target
            execute 'silent colder ' . (l:current - l:target)
        elseif l:current < l:target
            execute 'silent cnewer ' . (l:target - l:current)
        endif
        call setqflist([], 'r', {'items': l:self.list.items})
        call s:logger.LogDebug('Replaced existing Quickfix list')
    " Otherwise, create a new quickfix list.
    else
        call setqflist([], ' ', l:self.list)
        call s:logger.LogDebug('Created new Quickfix list')
    endif
    let l:self.id = getqflist({'nr': 0, 'id': 0}).id
endfunction

function! cmake#quickfix#Get() abort
    return s:quickfix
endfunction
