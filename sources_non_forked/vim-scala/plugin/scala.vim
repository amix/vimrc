" Vim plugin file
" Language:             Scala
" Maintainer:           Derek Wyatt
" URL:                  https://github.com/derekwyatt/vim-scala
" License:              Apache 2
" ----------------------------------------------------------------------------

if exists('g:loaded_scala') || &cp
  finish
endif
let g:loaded_scala = 1

"
" Sort imports
"
" author: Leonard Ehrenfried <leonard.ehrenfried@gmail.com>
"
function! SortScalaImports()
  let save_cursor = getpos(".")

  if exists('g:scala_sort_across_groups') && g:scala_sort_across_groups
    call s:sortAcrossGroups()
  else
    call s:sortInsideGroups()
  end

  "move cursor to where it was before the function call
  call setpos('.', save_cursor)

endfunction

" Iterates over _all_ imports and puts them into 3 groups
"
" 1. Java/Scala imports like java.util.UUID
" 2. Third party libraries
" 3. First party libraries (ie. your own stuff)
"
function! s:sortAcrossGroups()
  let curr = 1
  let first_line = -1
  let last_line = -1
  let trailing_newlines = 0
  let java_scala_imports = []
  let first_party_imports = []
  let third_party_imports = []

  " loop over lines in buffer
  while curr <= line('$')

    let line = getline(curr)

    if line =~ "^import"
      if first_line == -1
        let first_line = curr
      endif

      if line =~ '^import \(java\(x\)\?\|scala\)\.'
        call add(java_scala_imports, line)
      elseif exists('g:scala_first_party_namespaces')
        let regex = '^import '.g:scala_first_party_namespaces
        if line =~ regex
          call add(first_party_imports, line)
        else
          call add(third_party_imports, line)
        endif
      else
        call add(third_party_imports, line)
      endif

      let trailing_newlines = 0
    elseif empty(line)
      let trailing_newlines = trailing_newlines + 1
    elseif first_line != -1
      let last_line = curr - trailing_newlines - 1
      " break out when you have found the first non-import, non-empty line
      break
    endif

    let curr = curr + 1
  endwhile

  call cursor(first_line, 0)
  let to_delete = last_line - first_line

  if to_delete > 0
    execute 'd'to_delete
  endif

  call s:sortAndPrint(first_party_imports)
  call s:sortAndPrint(third_party_imports)
  call s:sortAndPrint(java_scala_imports)

  if first_line != -1
    " remove extra blank line at top
    execute 'delete'
  endif

  call cursor(last_line + 2, 0)
  if empty(getline(line(".")))
    execute 'delete'
  endif

endfunction

function! s:sortInsideGroups()
  call cursor(1, 1)

  let start = 1
  let end = 1

  " repeat until we find no more matches
  while(start > 0 && end > 0)
    let pos = line(".")
    " find first line with import
    let start = search('^import', 'cW')
    " find next line which starts with an import, ends with a newline
    " and the next line is not an import
    " the 'c' flag accepts matches at the current position allowing single line groups
    let end = search('^\import.*\n\(import\)\@!', 'cW')

    execute start','end'sort i'

    call cursor(end + 1, 0)

    " stop if end is the last line in the file
    if line("$") == end
      break
    endif

  endwhile
endfunction

function! s:sortAndPrint(imports)
  if len(a:imports) > 0
    call sort(a:imports, "s:sortIgnoreCase")
    call append(line("."), "")
    call append(line("."), a:imports)
  endif
endfunction

" this useless function exists purely so the sort() ignores case
" this is needed so scalaz/Scalaz appears next to each other
function! s:sortIgnoreCase(i1, i2)
  return a:i1 == a:i2 ? 0 : a:i1 > a:i2 ? 1 : -1
endfunction

command! SortScalaImports call SortScalaImports()

" vim:set sw=2 sts=2 ts=8 et:
