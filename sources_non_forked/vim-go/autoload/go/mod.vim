" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:go_major_version = ""

function! go#mod#Format() abort
  " go mod only exists in `v1.11`
  if empty(s:go_major_version)
    let tokens = matchlist(go#util#System("go version"), '\d\+.\(\d\+\)\(\.\d\+\)\? ')
    let s:go_major_version = str2nr(tokens[1])
  endif

  if s:go_major_version < "11" 
    call go#util#EchoError("Go v1.11 is required to format go.mod file")
    return
  endif

  let fname = fnamemodify(expand("%"), ':p:gs?\\?/?')

  " Save cursor position and many other things.
  let l:curw = winsaveview()

  " Write current unsaved buffer to a temp file
  let l:tmpname = tempname() . '.mod'
  call writefile(go#util#GetLines(), l:tmpname)
  if go#util#IsWin()
    let l:tmpname = tr(l:tmpname, '\', '/')
  endif

  let current_col = col('.')
  let l:args = ['go', 'mod', 'edit', '--fmt', l:tmpname]
  let [l:out, l:err] = go#util#Exec(l:args)
  let diff_offset = len(readfile(l:tmpname)) - line('$')

  if l:err == 0
    call go#mod#update_file(l:tmpname, fname)
  else
    let errors = s:parse_errors(fname, l:out)
    call s:show_errors(errors)
  endif

  " We didn't use the temp file, so clean up
  call delete(l:tmpname)

  " Restore our cursor/windows positions.
  call winrestview(l:curw)

  " be smart and jump to the line the new statement was added/removed
  call cursor(line('.') + diff_offset, current_col)

  " Syntax highlighting breaks less often.
  syntax sync fromstart
endfunction

" update_file updates the target file with the given formatted source
function! go#mod#update_file(source, target)
  " remove undo point caused via BufWritePre
  try | silent undojoin | catch | endtry

  let old_fileformat = &fileformat
  if exists("*getfperm")
    " save file permissions
    let original_fperm = getfperm(a:target)
  endif

  call rename(a:source, a:target)

  " restore file permissions
  if exists("*setfperm") && original_fperm != ''
    call setfperm(a:target , original_fperm)
  endif

  " reload buffer to reflect latest changes
  silent edit!

  let &fileformat = old_fileformat
  let &syntax = &syntax

  let l:listtype = go#list#Type("GoModFmt")

  " the title information was introduced with 7.4-2200
  " https://github.com/vim/vim/commit/d823fa910cca43fec3c31c030ee908a14c272640
  if has('patch-7.4.2200')
    " clean up previous list
    if l:listtype == "quickfix"
      let l:list_title = getqflist({'title': 1})
    else
      let l:list_title = getloclist(0, {'title': 1})
    endif
  else
    " can't check the title, so assume that the list was for go fmt.
    let l:list_title = {'title': 'Format'}
  endif

  if has_key(l:list_title, "title") && l:list_title['title'] == "Format"
    call go#list#Clean(l:listtype)
  endif
endfunction

" parse_errors parses the given errors and returns a list of parsed errors
function! s:parse_errors(filename, content) abort
  let splitted = split(a:content, '\n')

  " list of errors to be put into location list
  let errors = []
  for line in splitted
    let tokens = matchlist(line, '^\(.\{-}\):\(\d\+\):\s*\(.*\)')
    if !empty(tokens)
      call add(errors,{
            \"filename": a:filename,
            \"lnum":     tokens[2],
            \"text":     tokens[3],
            \ })
    endif
  endfor

  return errors
endfunction

" show_errors opens a location list and shows the given errors. If the given
" errors is empty, it closes the the location list
function! s:show_errors(errors) abort
  let l:listtype = go#list#Type("GoModFmt")
  if !empty(a:errors)
    call go#list#Populate(l:listtype, a:errors, 'Format')
    call go#util#EchoError("GoModFmt returned error")
  endif

  " this closes the window if there are no errors or it opens
  " it if there is any
  call go#list#Window(l:listtype, len(a:errors))
endfunction

function! go#mod#ToggleModFmtAutoSave() abort
  if go#config#ModFmtAutosave()
    call go#config#SetModFmtAutosave(0)
    call go#util#EchoProgress("auto mod fmt disabled")
    return
  end

  call go#config#SetModFmtAutosave(1)
  call go#util#EchoProgress("auto mod fmt enabled")
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
