" pathogen.vim - path option manipulation
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      2.0

" Install in ~/.vim/autoload (or ~\vimfiles\autoload).
"
" For management of individually installed plugins in ~/.vim/bundle (or
" ~\vimfiles\bundle), adding `call pathogen#infect()` to your .vimrc
" prior to `filetype plugin indent on` is the only other setup necessary.
"
" The API is documented inline below.  For maximum ease of reading,
" :set foldmethod=marker

if exists("g:loaded_pathogen") || &cp
  finish
endif
let g:loaded_pathogen = 1

" Point of entry for basic default usage.  Give a directory name to invoke
" pathogen#runtime_append_all_bundles() (defaults to "bundle"), or a full path
" to invoke pathogen#runtime_prepend_subdirectories().  Afterwards,
" pathogen#cycle_filetype() is invoked.
function! pathogen#infect(...) abort " {{{1
  let source_path = a:0 ? a:1 : 'bundle'
  if source_path =~# '[\\/]'
    call pathogen#runtime_prepend_subdirectories(source_path)
  else
    call pathogen#runtime_append_all_bundles(source_path)
  endif
  call pathogen#cycle_filetype()
endfunction " }}}1

" Split a path into a list.
function! pathogen#split(path) abort " {{{1
  if type(a:path) == type([]) | return a:path | endif
  let split = split(a:path,'\\\@<!\%(\\\\\)*\zs,')
  return map(split,'substitute(v:val,''\\\([\\,]\)'',''\1'',"g")')
endfunction " }}}1

" Convert a list to a path.
function! pathogen#join(...) abort " {{{1
  if type(a:1) == type(1) && a:1
    let i = 1
    let space = ' '
  else
    let i = 0
    let space = ''
  endif
  let path = ""
  while i < a:0
    if type(a:000[i]) == type([])
      let list = a:000[i]
      let j = 0
      while j < len(list)
        let escaped = substitute(list[j],'[,'.space.']\|\\[\,'.space.']\@=','\\&','g')
        let path .= ',' . escaped
        let j += 1
      endwhile
    else
      let path .= "," . a:000[i]
    endif
    let i += 1
  endwhile
  return substitute(path,'^,','','')
endfunction " }}}1

" Convert a list to a path with escaped spaces for 'path', 'tag', etc.
function! pathogen#legacyjoin(...) abort " {{{1
  return call('pathogen#join',[1] + a:000)
endfunction " }}}1

" Remove duplicates from a list.
function! pathogen#uniq(list) abort " {{{1
  let i = 0
  let seen = {}
  while i < len(a:list)
    if (a:list[i] ==# '' && exists('empty')) || has_key(seen,a:list[i])
      call remove(a:list,i)
    elseif a:list[i] ==# ''
      let i += 1
      let empty = 1
    else
      let seen[a:list[i]] = 1
      let i += 1
    endif
  endwhile
  return a:list
endfunction " }}}1

" \ on Windows unless shellslash is set, / everywhere else.
function! pathogen#separator() abort " {{{1
  return !exists("+shellslash") || &shellslash ? '/' : '\'
endfunction " }}}1

" Convenience wrapper around glob() which returns a list.
function! pathogen#glob(pattern) abort " {{{1
  let files = split(glob(a:pattern),"\n")
  return map(files,'substitute(v:val,"[".pathogen#separator()."/]$","","")')
endfunction "}}}1

" Like pathogen#glob(), only limit the results to directories.
function! pathogen#glob_directories(pattern) abort " {{{1
  return filter(pathogen#glob(a:pattern),'isdirectory(v:val)')
endfunction "}}}1

" Turn filetype detection off and back on again if it was already enabled.
function! pathogen#cycle_filetype() " {{{1
  if exists('g:did_load_filetypes')
    filetype off
    filetype on
  endif
endfunction " }}}1

" Checks if a bundle is 'disabled'. A bundle is considered 'disabled' if
" its 'basename()' is included in g:pathogen_disabled[]' or ends in a tilde.
function! pathogen#is_disabled(path) " {{{1
  if a:path =~# '\~$'
    return 1
  elseif !exists("g:pathogen_disabled")
    return 0
  endif
  let sep = pathogen#separator()
  return index(g:pathogen_disabled, strpart(a:path, strridx(a:path, sep)+1)) != -1
endfunction "}}}1

" Prepend all subdirectories of path to the rtp, and append all 'after'
" directories in those subdirectories.
function! pathogen#runtime_prepend_subdirectories(path) " {{{1
  let sep    = pathogen#separator()
  let before = filter(pathogen#glob_directories(a:path.sep."*"), '!pathogen#is_disabled(v:val)')
  let after  = filter(pathogen#glob_directories(a:path.sep."*".sep."after"), '!pathogen#is_disabled(v:val[0:-7])')
  let rtp = pathogen#split(&rtp)
  let path = expand(a:path)
  call filter(rtp,'v:val[0:strlen(path)-1] !=# path')
  let &rtp = pathogen#join(pathogen#uniq(before + rtp + after))
  return &rtp
endfunction " }}}1

" For each directory in rtp, check for a subdirectory named dir.  If it
" exists, add all subdirectories of that subdirectory to the rtp, immediately
" after the original directory.  If no argument is given, 'bundle' is used.
" Repeated calls with the same arguments are ignored.
function! pathogen#runtime_append_all_bundles(...) " {{{1
  let sep = pathogen#separator()
  let name = a:0 ? a:1 : 'bundle'
  if "\n".s:done_bundles =~# "\\M\n".name."\n"
    return ""
  endif
  let s:done_bundles .= name . "\n"
  let list = []
  for dir in pathogen#split(&rtp)
    if dir =~# '\<after$'
      let list +=  filter(pathogen#glob_directories(substitute(dir,'after$',name,'').sep.'*[^~]'.sep.'after'), '!pathogen#is_disabled(v:val[0:-7])') + [dir]
    else
      let list +=  [dir] + filter(pathogen#glob_directories(dir.sep.name.sep.'*[^~]'), '!pathogen#is_disabled(v:val)')
    endif
  endfor
  let &rtp = pathogen#join(pathogen#uniq(list))
  return 1
endfunction

let s:done_bundles = ''
" }}}1

" Invoke :helptags on all non-$VIM doc directories in runtimepath.
function! pathogen#helptags() " {{{1
  let sep = pathogen#separator()
  for dir in pathogen#split(&rtp)
    if (dir.sep)[0 : strlen($VIMRUNTIME)] !=# $VIMRUNTIME.sep && filewritable(dir.sep.'doc') == 2 && !empty(filter(split(glob(dir.sep.'doc'.sep.'*'),"\n>"),'!isdirectory(v:val)')) && (!filereadable(dir.sep.'doc'.sep.'tags') || filewritable(dir.sep.'doc'.sep.'tags'))
      helptags `=dir.'/doc'`
    endif
  endfor
endfunction " }}}1

command! -bar Helptags :call pathogen#helptags()

" Like findfile(), but hardcoded to use the runtimepath.
function! pathogen#runtime_findfile(file,count) "{{{1
  let rtp = pathogen#join(1,pathogen#split(&rtp))
  return fnamemodify(findfile(a:file,rtp,a:count),':p')
endfunction " }}}1

" Backport of fnameescape().
function! pathogen#fnameescape(string) " {{{1
  if exists('*fnameescape')
    return fnameescape(a:string)
  elseif a:string ==# '-'
    return '\-'
  else
    return substitute(escape(a:string," \t\n*?[{`$\\%#'\"|!<"),'^[+>]','\\&','')
  endif
endfunction " }}}1

function! s:find(count,cmd,file,lcd) " {{{1
  let rtp = pathogen#join(1,pathogen#split(&runtimepath))
  let file = pathogen#runtime_findfile(a:file,a:count)
  if file ==# ''
    return "echoerr 'E345: Can''t find file \"".a:file."\" in runtimepath'"
  elseif a:lcd
    let path = file[0:-strlen(a:file)-2]
    execute 'lcd `=path`'
    return a:cmd.' '.pathogen#fnameescape(a:file)
  else
    return a:cmd.' '.pathogen#fnameescape(file)
  endif
endfunction " }}}1

function! s:Findcomplete(A,L,P) " {{{1
  let sep = pathogen#separator()
  let cheats = {
        \'a': 'autoload',
        \'d': 'doc',
        \'f': 'ftplugin',
        \'i': 'indent',
        \'p': 'plugin',
        \'s': 'syntax'}
  if a:A =~# '^\w[\\/]' && has_key(cheats,a:A[0])
    let request = cheats[a:A[0]].a:A[1:-1]
  else
    let request = a:A
  endif
  let pattern = substitute(request,'\'.sep,'*'.sep,'g').'*'
  let found = {}
  for path in pathogen#split(&runtimepath)
    let path = expand(path, ':p')
    let matches = split(glob(path.sep.pattern),"\n")
    call map(matches,'isdirectory(v:val) ? v:val.sep : v:val')
    call map(matches,'expand(v:val, ":p")[strlen(path)+1:-1]')
    for match in matches
      let found[match] = 1
    endfor
  endfor
  return sort(keys(found))
endfunction " }}}1

command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Ve       :execute s:find(<count>,'edit<bang>',<q-args>,0)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vedit    :execute s:find(<count>,'edit<bang>',<q-args>,0)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vopen    :execute s:find(<count>,'edit<bang>',<q-args>,1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vsplit   :execute s:find(<count>,'split',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vvsplit  :execute s:find(<count>,'vsplit',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vtabedit :execute s:find(<count>,'tabedit',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vpedit   :execute s:find(<count>,'pedit',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vread    :execute s:find(<count>,'read',<q-args>,<bang>1)

" vim:set ft=vim ts=8 sw=2 sts=2:
