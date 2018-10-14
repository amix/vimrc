" pathogen.vim - path option manipulation
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      2.3

" Install in ~/.vim/autoload (or ~\vimfiles\autoload).
"
" For management of individually installed plugins in ~/.vim/bundle (or
" ~\vimfiles\bundle), adding `execute pathogen#infect()` to the top of your
" .vimrc is the only other setup necessary.
"
" The API is documented inline below.

if exists("g:loaded_pathogen") || &cp
  finish
endif
let g:loaded_pathogen = 1

" Point of entry for basic default usage.  Give a relative path to invoke
" pathogen#interpose() (defaults to "bundle/{}"), or an absolute path to invoke
" pathogen#surround().  Curly braces are expanded with pathogen#expand():
" "bundle/{}" finds all subdirectories inside "bundle" inside all directories
" in the runtime path.
function! pathogen#infect(...) abort
  for path in a:0 ? filter(reverse(copy(a:000)), 'type(v:val) == type("")') : ['bundle/{}']
    if path =~# '^\%({\=[$~\\/]\|{\=\w:[\\/]\).*[{}*]'
      call pathogen#surround(path)
    elseif path =~# '^\%([$~\\/]\|\w:[\\/]\)'
      call s:warn('Change pathogen#infect('.string(path).') to pathogen#infect('.string(path.'/{}').')')
      call pathogen#surround(path . '/{}')
    elseif path =~# '[{}*]'
      call pathogen#interpose(path)
    else
      call s:warn('Change pathogen#infect('.string(path).') to pathogen#infect('.string(path.'/{}').')')
      call pathogen#interpose(path . '/{}')
    endif
  endfor
  call pathogen#cycle_filetype()
  if pathogen#is_disabled($MYVIMRC)
    return 'finish'
  endif
  return ''
endfunction

" Split a path into a list.
function! pathogen#split(path) abort
  if type(a:path) == type([]) | return a:path | endif
  if empty(a:path) | return [] | endif
  let split = split(a:path,'\\\@<!\%(\\\\\)*\zs,')
  return map(split,'substitute(v:val,''\\\([\\,]\)'',''\1'',"g")')
endfunction

" Convert a list to a path.
function! pathogen#join(...) abort
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
endfunction

" Convert a list to a path with escaped spaces for 'path', 'tag', etc.
function! pathogen#legacyjoin(...) abort
  return call('pathogen#join',[1] + a:000)
endfunction

" Turn filetype detection off and back on again if it was already enabled.
function! pathogen#cycle_filetype() abort
  if exists('g:did_load_filetypes')
    filetype off
    filetype on
  endif
endfunction

" Check if a bundle is disabled.  A bundle is considered disabled if its
" basename or full name is included in the list g:pathogen_disabled.
function! pathogen#is_disabled(path) abort
  if a:path =~# '\~$'
    return 1
  endif
  let sep = pathogen#slash()
  let blacklist = map(
        \ get(g:, 'pathogen_blacklist', get(g:, 'pathogen_disabled', [])) +
        \ pathogen#split($VIMBLACKLIST),
        \ 'substitute(v:val, "[\\/]$", "", "")')
  return index(blacklist, fnamemodify(a:path, ':t')) != -1 || index(blacklist, a:path) != -1
endfunction "}}}1

" Prepend the given directory to the runtime path and append its corresponding
" after directory.  Curly braces are expanded with pathogen#expand().
function! pathogen#surround(path) abort
  let sep = pathogen#slash()
  let rtp = pathogen#split(&rtp)
  let path = fnamemodify(a:path, ':p:?[\\/]\=$??')
  let before = filter(pathogen#expand(path), '!pathogen#is_disabled(v:val)')
  let after = filter(reverse(pathogen#expand(path.sep.'after')), '!pathogen#is_disabled(v:val[0:-7])')
  call filter(rtp, 'index(before + after, v:val) == -1')
  let &rtp = pathogen#join(before, rtp, after)
  return &rtp
endfunction

" For each directory in the runtime path, add a second entry with the given
" argument appended.  Curly braces are expanded with pathogen#expand().
function! pathogen#interpose(name) abort
  let sep = pathogen#slash()
  let name = a:name
  if has_key(s:done_bundles, name)
    return ""
  endif
  let s:done_bundles[name] = 1
  let list = []
  for dir in pathogen#split(&rtp)
    if dir =~# '\<after$'
      let list += reverse(filter(pathogen#expand(dir[0:-6].name.sep.'after'), '!pathogen#is_disabled(v:val[0:-7])')) + [dir]
    else
      let list += [dir] + filter(pathogen#expand(dir.sep.name), '!pathogen#is_disabled(v:val)')
    endif
  endfor
  let &rtp = pathogen#join(pathogen#uniq(list))
  return 1
endfunction

let s:done_bundles = {}

" Invoke :helptags on all non-$VIM doc directories in runtimepath.
function! pathogen#helptags() abort
  let sep = pathogen#slash()
  for glob in pathogen#split(&rtp)
    for dir in map(split(glob(glob), "\n"), 'v:val.sep."/doc/".sep')
      if (dir)[0 : strlen($VIMRUNTIME)] !=# $VIMRUNTIME.sep && filewritable(dir) == 2 && !empty(split(glob(dir.'*.txt'))) && (!filereadable(dir.'tags') || filewritable(dir.'tags'))
        silent! execute 'helptags' pathogen#fnameescape(dir)
      endif
    endfor
  endfor
endfunction

command! -bar Helptags :call pathogen#helptags()

" Execute the given command.  This is basically a backdoor for --remote-expr.
function! pathogen#execute(...) abort
  for command in a:000
    execute command
  endfor
  return ''
endfunction

" Section: Unofficial

function! pathogen#is_absolute(path) abort
  return a:path =~# (has('win32') ? '^\%([\\/]\|\w:\)[\\/]\|^[~$]' : '^[/~$]')
endfunction

" Given a string, returns all possible permutations of comma delimited braced
" alternatives of that string.  pathogen#expand('/{a,b}/{c,d}') yields
" ['/a/c', '/a/d', '/b/c', '/b/d'].  Empty braces are treated as a wildcard
" and globbed.  Actual globs are preserved.
function! pathogen#expand(pattern) abort
  if a:pattern =~# '{[^{}]\+}'
    let [pre, pat, post] = split(substitute(a:pattern, '\(.\{-\}\){\([^{}]\+\)}\(.*\)', "\\1\001\\2\001\\3", ''), "\001", 1)
    let found = map(split(pat, ',', 1), 'pre.v:val.post')
    let results = []
    for pattern in found
      call extend(results, pathogen#expand(pattern))
    endfor
    return results
  elseif a:pattern =~# '{}'
    let pat = matchstr(a:pattern, '^.*{}[^*]*\%($\|[\\/]\)')
    let post = a:pattern[strlen(pat) : -1]
    return map(split(glob(substitute(pat, '{}', '*', 'g')), "\n"), 'v:val.post')
  else
    return [a:pattern]
  endif
endfunction

" \ on Windows unless shellslash is set, / everywhere else.
function! pathogen#slash() abort
  return !exists("+shellslash") || &shellslash ? '/' : '\'
endfunction

function! pathogen#separator() abort
  return pathogen#slash()
endfunction

" Convenience wrapper around glob() which returns a list.
function! pathogen#glob(pattern) abort
  let files = split(glob(a:pattern),"\n")
  return map(files,'substitute(v:val,"[".pathogen#slash()."/]$","","")')
endfunction "}}}1

" Like pathogen#glob(), only limit the results to directories.
function! pathogen#glob_directories(pattern) abort
  return filter(pathogen#glob(a:pattern),'isdirectory(v:val)')
endfunction "}}}1

" Remove duplicates from a list.
function! pathogen#uniq(list) abort
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
endfunction

" Backport of fnameescape().
function! pathogen#fnameescape(string) abort
  if exists('*fnameescape')
    return fnameescape(a:string)
  elseif a:string ==# '-'
    return '\-'
  else
    return substitute(escape(a:string," \t\n*?[{`$\\%#'\"|!<"),'^[+>]','\\&','')
  endif
endfunction

" Like findfile(), but hardcoded to use the runtimepath.
function! pathogen#runtime_findfile(file,count) abort "{{{1
  let rtp = pathogen#join(1,pathogen#split(&rtp))
  let file = findfile(a:file,rtp,a:count)
  if file ==# ''
    return ''
  else
    return fnamemodify(file,':p')
  endif
endfunction

" Section: Deprecated

function! s:warn(msg) abort
  echohl WarningMsg
  echomsg a:msg
  echohl NONE
endfunction

" Prepend all subdirectories of path to the rtp, and append all 'after'
" directories in those subdirectories.  Deprecated.
function! pathogen#runtime_prepend_subdirectories(path) abort
  call s:warn('Change pathogen#runtime_prepend_subdirectories('.string(a:path).') to pathogen#infect('.string(a:path.'/{}').')')
  return pathogen#surround(a:path . pathogen#slash() . '{}')
endfunction

function! pathogen#incubate(...) abort
  let name = a:0 ? a:1 : 'bundle/{}'
  call s:warn('Change pathogen#incubate('.(a:0 ? string(a:1) : '').') to pathogen#infect('.string(name).')')
  return pathogen#interpose(name)
endfunction

" Deprecated alias for pathogen#interpose().
function! pathogen#runtime_append_all_bundles(...) abort
  if a:0
    call s:warn('Change pathogen#runtime_append_all_bundles('.string(a:1).') to pathogen#infect('.string(a:1.'/{}').')')
  else
    call s:warn('Change pathogen#runtime_append_all_bundles() to pathogen#infect()')
  endif
  return pathogen#interpose(a:0 ? a:1 . '/{}' : 'bundle/{}')
endfunction

if exists(':Vedit')
  finish
endif

let s:vopen_warning = 0

function! s:find(count,cmd,file,lcd)
  let rtp = pathogen#join(1,pathogen#split(&runtimepath))
  let file = pathogen#runtime_findfile(a:file,a:count)
  if file ==# ''
    return "echoerr 'E345: Can''t find file \"".a:file."\" in runtimepath'"
  endif
  if !s:vopen_warning
    let s:vopen_warning = 1
    let warning = '|echohl WarningMsg|echo "Install scriptease.vim to continue using :V'.a:cmd.'"|echohl NONE'
  else
    let warning = ''
  endif
  if a:lcd
    let path = file[0:-strlen(a:file)-2]
    execute 'lcd `=path`'
    return a:cmd.' '.pathogen#fnameescape(a:file) . warning
  else
    return a:cmd.' '.pathogen#fnameescape(file) . warning
  endif
endfunction

function! s:Findcomplete(A,L,P)
  let sep = pathogen#slash()
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
  let pattern = substitute(request,'/\|\'.sep,'*'.sep,'g').'*'
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
endfunction

command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Ve       :execute s:find(<count>,'edit<bang>',<q-args>,0)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vedit    :execute s:find(<count>,'edit<bang>',<q-args>,0)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vopen    :execute s:find(<count>,'edit<bang>',<q-args>,1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vsplit   :execute s:find(<count>,'split',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vvsplit  :execute s:find(<count>,'vsplit',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vtabedit :execute s:find(<count>,'tabedit',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vpedit   :execute s:find(<count>,'pedit',<q-args>,<bang>1)
command! -bar -bang -range=1 -nargs=1 -complete=customlist,s:Findcomplete Vread    :execute s:find(<count>,'read',<q-args>,<bang>1)

" vim:set et sw=2 foldmethod=expr foldexpr=getline(v\:lnum)=~'^\"\ Section\:'?'>1'\:getline(v\:lnum)=~#'^fu'?'a1'\:getline(v\:lnum)=~#'^endf'?'s1'\:'=':
