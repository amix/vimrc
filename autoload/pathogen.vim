" pathogen.vim - path option manipulation
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      2.4

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
" pathogen#interpose() or an absolute path to invoke pathogen#surround().
" Curly braces are expanded with pathogen#expand(): "bundle/{}" finds all
" subdirectories inside "bundle" inside all directories in the runtime path.
" If no arguments are given, defaults "bundle/{}", and also "pack/{}/start/{}"
" on versions of Vim without native package support.
function! pathogen#infect(...) abort
  if a:0
    let paths = filter(reverse(copy(a:000)), 'type(v:val) == type("")')
  else
    let paths = ['bundle/{}', 'pack/{}/start/{}']
  endif
  if has('packages')
    call filter(paths, 'v:val !~# "^pack/[^/]*/start/[^/]*$"')
  endif
  let static = '^\%([$~\\/]\|\w:[\\/]\)[^{}*]*$'
  for path in filter(copy(paths), 'v:val =~# static')
    call pathogen#surround(path)
  endfor
  for path in filter(copy(paths), 'v:val !~# static')
    if path =~# '^\%([$~\\/]\|\w:[\\/]\)'
      call pathogen#surround(path)
    else
      call pathogen#interpose(path)
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
" basename or full name is included in the list g:pathogen_blacklist or the
" comma delimited environment variable $VIMBLACKLIST.
function! pathogen#is_disabled(path) abort
  if a:path =~# '\~$'
    return 1
  endif
  let sep = pathogen#slash()
  let blacklist = get(g:, 'pathogen_blacklist', get(g:, 'pathogen_disabled', [])) + pathogen#split($VIMBLACKLIST)
  if !empty(blacklist)
    call map(blacklist, 'substitute(v:val, "[\\/]$", "", "")')
  endif
  return index(blacklist, fnamemodify(a:path, ':t')) != -1 || index(blacklist, a:path) != -1
endfunction

" Prepend the given directory to the runtime path and append its corresponding
" after directory.  Curly braces are expanded with pathogen#expand().
function! pathogen#surround(path) abort
  let sep = pathogen#slash()
  let rtp = pathogen#split(&rtp)
  let path = fnamemodify(a:path, ':s?[\\/]\=$??')
  let before = filter(pathogen#expand(path), '!pathogen#is_disabled(v:val)')
  let after = filter(reverse(pathogen#expand(path, sep.'after')), '!pathogen#is_disabled(v:val[0 : -7])')
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
      let list += reverse(filter(pathogen#expand(dir[0 : -6].name, sep.'after'), '!pathogen#is_disabled(v:val[0 : -7])')) + [dir]
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
function! pathogen#expand(pattern, ...) abort
  let after = a:0 ? a:1 : ''
  let pattern = substitute(a:pattern, '^[~$][^\/]*', '\=expand(submatch(0))', '')
  if pattern =~# '{[^{}]\+}'
    let [pre, pat, post] = split(substitute(pattern, '\(.\{-\}\){\([^{}]\+\)}\(.*\)', "\\1\001\\2\001\\3", ''), "\001", 1)
    let found = map(split(pat, ',', 1), 'pre.v:val.post')
    let results = []
    for pattern in found
      call extend(results, pathogen#expand(pattern))
    endfor
  elseif pattern =~# '{}'
    let pat = matchstr(pattern, '^.*{}[^*]*\%($\|[\\/]\)')
    let post = pattern[strlen(pat) : -1]
    let results = map(split(glob(substitute(pat, '{}', '*', 'g')), "\n"), 'v:val.post')
  else
    let results = [pattern]
  endif
  let vf = pathogen#slash() . 'vimfiles'
  call map(results, 'v:val =~# "\\*" ? v:val.after : isdirectory(v:val.vf.after) ? v:val.vf.after : isdirectory(v:val.after) ? v:val.after : ""')
  return filter(results, '!empty(v:val)')
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
endfunction

" Like pathogen#glob(), only limit the results to directories.
function! pathogen#glob_directories(pattern) abort
  return filter(pathogen#glob(a:pattern),'isdirectory(v:val)')
endfunction

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
function! pathogen#runtime_findfile(file,count) abort
  let rtp = pathogen#join(1,pathogen#split(&rtp))
  let file = findfile(a:file,rtp,a:count)
  if file ==# ''
    return ''
  else
    return fnamemodify(file,':p')
  endif
endfunction

" vim:set et sw=2 foldmethod=expr foldexpr=getline(v\:lnum)=~'^\"\ Section\:'?'>1'\:getline(v\:lnum)=~#'^fu'?'a1'\:getline(v\:lnum)=~#'^endf'?'s1'\:'=':
