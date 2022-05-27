" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" fmt.vim: Vim command to format Go files with gofmt (and gofmt compatible
" toorls, such as goimports).

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

"  we have those problems :
"  http://stackoverflow.com/questions/12741977/prevent-vim-from-updating-its-undo-tree
"  http://stackoverflow.com/questions/18532692/golang-formatter-and-vim-how-to-destroy-history-record?rq=1
"
"  The below function is an improved version that aims to fix all problems.
"  it doesn't undo changes and break undo history.  If you are here reading
"  this and have VimL experience, please look at the function for
"  improvements, patches are welcome :)
function! go#fmt#Format(withGoimport) abort
  let l:bin_name = go#config#FmtCommand()
  if a:withGoimport == 1
    let l:mode = go#config#ImportsMode()
    if l:mode == 'gopls'
      if !go#config#GoplsEnabled()
        call go#util#EchoError("go_imports_mode is 'gopls', but gopls is disabled")
        return
      endif
      call go#lsp#Imports()
      return
    endif

    let l:bin_name = 'goimports'
  endif

  if l:bin_name == 'gopls'
    if !go#config#GoplsEnabled()
      call go#util#EchoError("go_fmt_command is 'gopls', but gopls is disabled")
      return
    endif
    call go#lsp#Format()
    return
  endif

  if go#config#FmtExperimental()
    " Using winsaveview to save/restore cursor state has the problem of
    " closing folds on save:
    "   https://github.com/fatih/vim-go/issues/502
    " One fix is to use mkview instead. Unfortunately, this sometimes causes
    " other bad side effects:
    "   https://github.com/fatih/vim-go/issues/728
    " and still closes all folds if foldlevel>0:
    "   https://github.com/fatih/vim-go/issues/732
    let l:curw = {}
    try
      mkview!
    catch
      let l:curw = winsaveview()
    endtry

    " save our undo file to be restored after we are done. This is needed to
    " prevent an additional undo jump due to BufWritePre auto command and also
    " restore 'redo' history because it's getting being destroyed every
    " BufWritePre
    let tmpundofile = tempname()
    exe 'wundo! ' . tmpundofile
  else
    " Save cursor position and many other things.
    let l:curw = winsaveview()
  endif

  " Write current unsaved buffer to a temp file
  let l:tmpname = tempname() . '.go'
  call writefile(go#util#GetLines(), l:tmpname)
  if go#util#IsWin()
    let l:tmpname = tr(l:tmpname, '\', '/')
  endif

  let current_col = col('.')
  let [l:out, l:err] = go#fmt#run(l:bin_name, l:tmpname, expand('%'))
  let line_offset = len(readfile(l:tmpname)) - line('$')
  let l:orig_line = getline('.')

  if l:err == 0
    call go#fmt#update_file(l:tmpname, expand('%'))
  elseif !go#config#FmtFailSilently()
    let l:errors = s:replace_filename(expand('%'), out)
    call go#fmt#ShowErrors(l:errors)
  endif

  " We didn't use the temp file, so clean up
  call delete(l:tmpname)

  if go#config#FmtExperimental()
    " restore our undo history
    silent! exe 'rundo ' . tmpundofile
    call delete(tmpundofile)

    " Restore our cursor/windows positions, folds, etc.
    if empty(l:curw)
      silent! loadview
    else
      call winrestview(l:curw)
    endif
  else
    " Restore our cursor/windows positions.
    call winrestview(l:curw)
  endif

  " be smart and jump to the line the new statement was added/removed and
  " adjust the column within the line
  let l:lineno = line('.') + line_offset
  call cursor(l:lineno, current_col + (len(getline(l:lineno)) - len(l:orig_line)))

  " Syntax highlighting breaks less often.
  syntax sync fromstart
endfunction

" update_file updates the target file with the given formatted source
function! go#fmt#update_file(source, target)
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

  call go#lsp#DidChange(expand(a:target, ':p'))

  let &fileformat = old_fileformat
  let &syntax = &syntax

  call go#fmt#CleanErrors()
endfunction

" run runs the gofmt/goimport command for the given source file and returns
" the output of the executed command. Target is the real file to be formatted.
function! go#fmt#run(bin_name, source, target)
  let l:cmd = s:fmt_cmd(a:bin_name, a:source, a:target)
  if empty(l:cmd)
    return
  endif
  return go#util#Exec(l:cmd)
endfunction

" fmt_cmd returns the command to run as a list.
function! s:fmt_cmd(bin_name, source, target)
  let l:cmd = [a:bin_name, '-w']

  " add the options for binary (if any). go_fmt_options was by default of type
  " string, however to allow customization it's now a dictionary of binary
  " name mapping to options.
  let opts = go#config#FmtOptions()
  if type(opts) == type({})
    let opts = has_key(opts, a:bin_name) ? opts[a:bin_name] : ""
  endif
  call extend(cmd, split(opts, " "))
  if a:bin_name is# 'goimports'
    call extend(cmd, ["-srcdir", a:target])
  endif

  call add(cmd, a:source)
  return cmd
endfunction

" replace_filename replaces the filename on each line of content with
" a:filename.
function! s:replace_filename(filename, content) abort
  let l:errors = split(a:content, '\n')

  let l:errors = map(l:errors, printf('substitute(v:val, ''^.\{-}:'', ''%s:'', '''')', a:filename))
  return join(l:errors, "\n")
endfunction

function! go#fmt#CleanErrors() abort
  let l:listtype = go#list#Type("GoFmt")

  " clean up previous list
  if l:listtype == "quickfix"
    let l:list_title = getqflist({'title': 1})
  else
    let l:list_title = getloclist(0, {'title': 1})
  endif

  if has_key(l:list_title, 'title') && (l:list_title['title'] == 'Format' || l:list_title['title'] == 'GoMetaLinterAutoSave')
    call go#list#Clean(l:listtype)
  endif
endfunction

" show_errors opens a location list and shows the given errors. If errors is
" empty, it closes the the location list.
function! go#fmt#ShowErrors(errors) abort
  let l:errorformat = '%f:%l:%c:\ %m'
  let l:listtype = go#list#Type("GoFmt")

  call go#list#ParseFormat(l:listtype, l:errorformat, a:errors, 'Format', 0)
  let l:errors = go#list#Get(l:listtype)

  " this closes the window if there are no errors or it opens
  " it if there are any.
  call go#list#Window(l:listtype, len(l:errors))
endfunction

function! go#fmt#ToggleFmtAutoSave() abort
  if go#config#FmtAutosave()
    call go#config#SetFmtAutosave(0)
    call go#util#EchoProgress("auto fmt disabled")
    return
  end

  call go#config#SetFmtAutosave(1)
  call go#util#EchoProgress("auto fmt enabled")
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
