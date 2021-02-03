" fugitive.vim - A Git wrapper so awesome, it should be illegal
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      3.2
" GetLatestVimScripts: 2975 1 :AutoInstall: fugitive.vim

if exists('g:loaded_fugitive')
  finish
endif
let g:loaded_fugitive = 1

function! FugitiveGitDir(...) abort
  if !a:0 || type(a:1) == type(0) && a:1 < 0
    if exists('g:fugitive_event')
      return g:fugitive_event
    endif
    let dir = get(b:, 'git_dir', '')
    if empty(dir) && (empty(bufname('')) || &buftype =~# '^\%(nofile\|acwrite\|quickfix\|prompt\)$')
      return FugitiveExtractGitDir(getcwd())
    endif
    return dir
  elseif type(a:1) == type(0)
    return getbufvar(a:1, 'git_dir')
  elseif type(a:1) == type('')
    return substitute(s:Slash(a:1), '/$', '', '')
  else
    return ''
  endif
endfunction

" FugitiveReal() takes a fugitive:// URL and returns the corresponding path in
" the work tree.  This may be useful to get a cleaner path for inclusion in
" the statusline, for example.  Note that the file and its parent directories
" are not guaranteed to exist.
"
" This is intended as an abstract API to be used on any "virtual" path.  For a
" buffer named foo://bar, check for a function named FooReal(), and if it
" exists, call FooReal("foo://bar").
function! FugitiveReal(...) abort
  let file = a:0 ? a:1 : @%
  if file =~# '^\a\a\+:' || a:0 > 1
    return call('fugitive#Real', [file] + a:000[1:-1])
  elseif file =~# '^/\|^\a:\|^$'
    return file
  else
    return fnamemodify(file, ':p' . (file =~# '[\/]$' ? '' : ':s?[\/]$??'))
  endif
endfunction

" FugitiveFind() takes a Fugitive object and returns the appropriate Vim
" buffer name.  You can use this to generate Fugitive URLs ("HEAD:README") or
" to get the absolute path to a file in the Git dir (".git/HEAD"), the common
" dir (".git/config"), or the work tree (":(top)Makefile").
"
" An optional second argument provides the Git dir, or the buffer number of a
" buffer with a Git dir.  The default is the current buffer.
function! FugitiveFind(...) abort
  return fugitive#Find(a:0 ? a:1 : bufnr(''), FugitiveGitDir(a:0 > 1 ? a:2 : -1))
endfunction

function! FugitivePath(...) abort
  if a:0 > 1
    return fugitive#Path(a:1, a:2, FugitiveGitDir(a:0 > 2 ? a:3 : -1))
  else
    return FugitiveReal(a:0 ? a:1 : @%)
  endif
endfunction

" FugitiveParse() takes a fugitive:// URL and returns a 2 element list
" containing an object name ("commit:file") and the Git dir.  It's effectively
" the inverse of FugitiveFind().
function! FugitiveParse(...) abort
  let path = s:Slash(a:0 ? a:1 : @%)
  if path !~# '^fugitive:'
    return ['', '']
  endif
  let vals = matchlist(path, '\c^fugitive:\%(//\)\=\(.\{-\}\)\%(//\|::\)\(\x\{40,\}\|[0-3]\)\(/.*\)\=$')
  if len(vals)
    return [(vals[2] =~# '^.$' ? ':' : '') . vals[2] . substitute(vals[3], '^/', ':', ''), vals[1]]
  endif
  let v:errmsg = 'fugitive: invalid Fugitive URL ' . path
  throw v:errmsg
endfunction

" FugitivePrepare() constructs a Git command string which can be executed with
" functions like system() and commands like :!.  Integer arguments will be
" treated as buffer numbers, and the appropriate relative path inserted in
" their place.
"
" If the first argument is a string that looks like a path or an empty string,
" it will be used as the Git dir.  If it's a buffer number, the Git dir for
" that buffer will be used.  The default is the current buffer.
function! FugitivePrepare(...) abort
  return call('fugitive#Prepare', a:000)
endfunction

function! FugitiveConfig(...) abort
  if a:0 == 2 && type(a:2) != type({})
    return fugitive#Config(a:1, FugitiveGitDir(a:2))
  elseif a:0 == 1 && a:1 !~# '^[[:alnum:]-]\+\.'
    return fugitive#Config(FugitiveGitDir(a:1))
  else
    return call('fugitive#Config', a:000)
  endif
endfunction

" Retrieve a Git configuration value.  An optional second argument provides
" the Git dir as with FugitiveFind().  Pass a blank string to limit to the
" global config.
function! FugitiveConfigGet(name, ...) abort
  return call('FugitiveConfig', [a:name] + a:000)
endfunction

" Like FugitiveConfigGet(), but return a list of all values.
function! FugitiveConfigGetAll(name, ...) abort
  if a:0 && type(a:1) ==# type({})
    let config = a:1
  else
    let config = fugitive#Config(FugitiveGitDir(a:0 ? a:1 : -1))
  endif
  let name = substitute(a:name, '^[^.]\+\|[^.]\+$', '\L&', 'g')
  return copy(get(config, name, []))
endfunction

function! FugitiveRemoteUrl(...) abort
  return fugitive#RemoteUrl(a:0 ? a:1 : '', FugitiveGitDir(a:0 > 1 ? a:2 : -1))
endfunction

function! FugitiveHead(...) abort
  let dir = FugitiveGitDir(a:0 > 1 ? a:2 : -1)
  if empty(dir)
    return ''
  endif
  return fugitive#Head(a:0 ? a:1 : 0, dir)
endfunction

function! FugitiveStatusline(...) abort
  if !exists('b:git_dir')
    return ''
  endif
  return fugitive#Statusline()
endfunction

function! FugitiveCommonDir(...) abort
  let dir = FugitiveGitDir(a:0 ? a:1 : -1)
  if empty(dir)
    return ''
  endif
  return fugitive#CommonDir(dir)
endfunction

function! FugitiveWorkTree(...) abort
  return s:Tree(FugitiveGitDir(a:0 ? a:1 : -1))
endfunction

function! FugitiveIsGitDir(path) abort
  let path = substitute(a:path, '[\/]$', '', '') . '/'
  return len(a:path) && getfsize(path.'HEAD') > 10 && (
        \ isdirectory(path.'objects') && isdirectory(path.'refs') ||
        \ getftype(path.'commondir') ==# 'file')
endfunction

let s:worktree_for_dir = {}
let s:dir_for_worktree = {}
function! s:Tree(path) abort
  let dir = a:path
  if dir =~# '/\.git$'
    return len(dir) ==# 5 ? '/' : dir[0:-6]
  elseif dir ==# ''
    return ''
  endif
  if !has_key(s:worktree_for_dir, dir)
    let s:worktree_for_dir[dir] = ''
    let config_file = dir . '/config'
    if filereadable(config_file)
      let config = readfile(config_file,'',10)
      call filter(config,'v:val =~# "^\\s*worktree *="')
      if len(config) == 1
        let worktree = FugitiveVimPath(matchstr(config[0], '= *\zs.*'))
      endif
    elseif filereadable(dir . '/gitdir')
      let worktree = fnamemodify(FugitiveVimPath(readfile(dir . '/gitdir')[0]), ':h')
      if worktree ==# '.'
        unlet! worktree
      endif
    endif
    if exists('worktree')
      let s:worktree_for_dir[dir] = s:Slash(resolve(worktree))
      let s:dir_for_worktree[s:worktree_for_dir[dir]] = dir
    endif
  endif
  if s:worktree_for_dir[dir] =~# '^\.'
    return simplify(dir . '/' . s:worktree_for_dir[dir])
  else
    return s:worktree_for_dir[dir]
  endif
endfunction

function! s:CeilingDirectories() abort
  if !exists('s:ceiling_directories')
    let s:ceiling_directories = []
    let resolve = 1
    for dir in split($GIT_CEILING_DIRECTORIES, has('win32') ? ';' : ':', 1)
      if empty(dir)
        let resolve = 0
      elseif resolve
        call add(s:ceiling_directories, resolve(dir))
      else
        call add(s:ceiling_directories, dir)
      endif
    endfor
  endif
  return s:ceiling_directories + get(g:, 'ceiling_directories', [])
endfunction

function! FugitiveExtractGitDir(path) abort
  let path = s:Slash(a:path)
  if path =~# '^fugitive:'
    return matchstr(path, '\C^fugitive:\%(//\)\=\zs.\{-\}\ze\%(//\|::\|$\)')
  elseif empty(path)
    return ''
  elseif isdirectory(path)
    let path = fnamemodify(path, ':p:s?/$??')
  else
    let path = fnamemodify(path, ':p:h:s?/$??')
  endif
  let pre = substitute(matchstr(path, '^\a\a\+\ze:'), '^.', '\u&', '')
  if len(pre) && exists('*' . pre . 'Real')
    let path = s:Slash({pre}Real(path))
  endif
  let root = resolve(path)
  if root !=# path
    silent! exe (haslocaldir() ? 'lcd' : exists(':tcd') && haslocaldir(-1) ? 'tcd' : 'cd') '.'
  endif
  let previous = ""
  let env_git_dir = len($GIT_DIR) ? s:Slash(simplify(fnamemodify(FugitiveVimPath($GIT_DIR), ':p:s?[\/]$??'))) : ''
  call s:Tree(env_git_dir)
  while root !=# previous
    if root =~# '\v^//%([^/]+/?)?$'
      break
    endif
    if index(s:CeilingDirectories(), root) >= 0
      break
    endif
    if root ==# $GIT_WORK_TREE && FugitiveIsGitDir(env_git_dir)
      return env_git_dir
    elseif has_key(s:dir_for_worktree, root)
      return s:dir_for_worktree[root]
    endif
    let dir = substitute(root, '[\/]$', '', '') . '/.git'
    let type = getftype(dir)
    if type ==# 'dir' && FugitiveIsGitDir(dir)
      return dir
    elseif type ==# 'link' && FugitiveIsGitDir(dir)
      return resolve(dir)
    elseif type !=# '' && filereadable(dir)
      let line = get(readfile(dir, '', 1), 0, '')
      let file_dir = s:Slash(FugitiveVimPath(matchstr(line, '^gitdir: \zs.*')))
      if file_dir !~# '^/\|^\a:' && FugitiveIsGitDir(root . '/' . file_dir)
        return simplify(root . '/' . file_dir)
      elseif len(file_dir) && FugitiveIsGitDir(file_dir)
        return file_dir
      endif
    elseif FugitiveIsGitDir(root)
      return root
    endif
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile
  return ''
endfunction

function! FugitiveDetect(path) abort
  if exists('b:git_dir') && b:git_dir =~# '^$\|/$\|^fugitive:'
    unlet b:git_dir
  endif
  if !exists('b:git_dir')
    let dir = FugitiveExtractGitDir(a:path)
    if dir !=# ''
      let b:git_dir = dir
    endif
  endif
  if !exists('b:git_dir') || !exists('#User#Fugitive')
    return ''
  endif
  if v:version >= 704 || (v:version == 703 && has('patch442'))
    doautocmd <nomodeline> User Fugitive
  elseif &modelines > 0
    let modelines = &modelines
    try
      set modelines=0
      doautocmd User Fugitive
    finally
      let &modelines = modelines
    endtry
  else
    doautocmd User Fugitive
  endif
  return ''
endfunction

function! FugitiveVimPath(path) abort
  if exists('+shellslash') && !&shellslash
    return tr(a:path, '/', '\')
  else
    return a:path
  endif
endfunction

function! FugitiveGitPath(path) abort
  return s:Slash(a:path)
endfunction

function! s:Slash(path) abort
  if exists('+shellslash')
    return tr(a:path, '\', '/')
  else
    return a:path
  endif
endfunction

function! s:ProjectionistDetect() abort
  let file = s:Slash(get(g:, 'projectionist_file', ''))
  let dir = FugitiveExtractGitDir(file)
  let base = matchstr(file, '^fugitive://.\{-\}//\x\+')
  if empty(base)
    let base = s:Tree(dir)
  endif
  if len(base)
    if exists('+shellslash') && !&shellslash
      let base = tr(base, '/', '\')
    endif
    let file = FugitiveCommonDir(dir) . '/info/projections.json'
    if filereadable(file)
      call projectionist#append(base, file)
    endif
  endif
endfunction

if v:version + has('patch061') < 703
  runtime! autoload/fugitive.vim
endif
let g:io_fugitive = {
      \ 'simplify': function('fugitive#simplify'),
      \ 'resolve': function('fugitive#resolve'),
      \ 'getftime': function('fugitive#getftime'),
      \ 'getfsize': function('fugitive#getfsize'),
      \ 'getftype': function('fugitive#getftype'),
      \ 'filereadable': function('fugitive#filereadable'),
      \ 'filewritable': function('fugitive#filewritable'),
      \ 'isdirectory': function('fugitive#isdirectory'),
      \ 'getfperm': function('fugitive#getfperm'),
      \ 'setfperm': function('fugitive#setfperm'),
      \ 'readfile': function('fugitive#readfile'),
      \ 'writefile': function('fugitive#writefile'),
      \ 'glob': function('fugitive#glob'),
      \ 'delete': function('fugitive#delete'),
      \ 'Real': function('FugitiveReal')}

augroup fugitive
  autocmd!

  autocmd BufNewFile,BufReadPost * call FugitiveDetect(expand('<amatch>:p'))
  autocmd FileType           netrw call FugitiveDetect(fnamemodify(get(b:, 'netrw_curdir', expand('<amatch>')), ':p'))

  autocmd FileType git
        \ if len(FugitiveGitDir()) |
        \   call fugitive#MapJumps() |
        \   call fugitive#MapCfile() |
        \ endif
  autocmd FileType gitcommit
        \ if len(FugitiveGitDir()) |
        \   call fugitive#MapCfile('fugitive#MessageCfile()') |
        \ endif
  autocmd FileType git,gitcommit
        \ if len(FugitiveGitDir()) && &foldtext ==# 'foldtext()' |
        \    setlocal foldtext=fugitive#Foldtext() |
        \ endif
  autocmd FileType fugitive
        \ if len(FugitiveGitDir()) |
        \   call fugitive#MapCfile('fugitive#StatusCfile()') |
        \ endif
  autocmd FileType gitrebase
        \ let &l:include = '^\%(pick\|squash\|edit\|reword\|fixup\|drop\|[pserfd]\)\>' |
        \ if len(FugitiveGitDir()) |
        \   let &l:includeexpr = 'v:fname =~# ''^\x\{4,\}$'' ? FugitiveFind(v:fname) : ' .
        \   (len(&l:includeexpr) ? &l:includeexpr : 'v:fname') |
        \ endif |
        \ let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') . '|setl inex= inc='

  autocmd BufReadCmd index{,.lock}
        \ if FugitiveIsGitDir(expand('<amatch>:p:h')) |
        \   let b:git_dir = s:Slash(expand('<amatch>:p:h')) |
        \   exe fugitive#BufReadStatus() |
        \ elseif filereadable(expand('<amatch>')) |
        \   silent doautocmd BufReadPre |
        \   keepalt read <amatch> |
        \   1delete_ |
        \   silent doautocmd BufReadPost |
        \ else |
        \   silent doautocmd BufNewFile |
        \ endif

  autocmd BufReadCmd    fugitive://*//*             exe fugitive#BufReadCmd() |
        \ if &path =~# '^\.\%(,\|$\)' |
        \   let &l:path = substitute(&path, '^\.,\=', '', '') |
        \ endif
  autocmd BufWriteCmd   fugitive://*//[0-3]/*       exe fugitive#BufWriteCmd()
  autocmd FileReadCmd   fugitive://*//*             exe fugitive#FileReadCmd()
  autocmd FileWriteCmd  fugitive://*//[0-3]/*       exe fugitive#FileWriteCmd()
  if exists('##SourceCmd')
    autocmd SourceCmd     fugitive://*//*    nested exe fugitive#SourceCmd()
  endif

  autocmd User Flags call Hoist('buffer', function('FugitiveStatusline'))

  autocmd User ProjectionistDetect call s:ProjectionistDetect()
augroup END

let s:addr_other = has('patch-8.1.560') ? '-addr=other' : ''
let s:addr_tabs  = has('patch-7.4.542') ? '-addr=tabs' : ''
let s:addr_wins  = has('patch-7.4.542') ? '-addr=windows' : ''

if exists(':G') != 2
  command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#Complete G   exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)
endif
command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#Complete Git exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)

if exists(':Gstatus') !=# 2
  exe 'command! -bang -bar     -range=-1' s:addr_other 'Gstatus exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
endif

for s:cmd in ['Commit', 'Revert', 'Merge', 'Rebase', 'Pull', 'Push', 'Fetch', 'Blame']
  if exists(':G' . tolower(s:cmd)) != 2
    exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#' . s:cmd . 'Complete G' . tolower(s:cmd) 'exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", "' . tolower(s:cmd) . ' " . <q-args>)'
  endif
endfor
unlet s:cmd

exe "command! -bar -bang -nargs=? -complete=customlist,fugitive#CdComplete Gcd  exe fugitive#Cd(<q-args>, 0)"
exe "command! -bar -bang -nargs=? -complete=customlist,fugitive#CdComplete Glcd exe fugitive#Cd(<q-args>, 1)"

exe 'command! -bang -nargs=? -range=-1' s:addr_wins '-complete=customlist,fugitive#GrepComplete Ggrep  exe fugitive#GrepCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bang -nargs=? -range=-1' s:addr_wins '-complete=customlist,fugitive#GrepComplete Gcgrep exe fugitive#GrepCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bang -nargs=? -range=-1' s:addr_wins '-complete=customlist,fugitive#GrepComplete Glgrep exe fugitive#GrepCommand(0, <count> > 0 ? <count> : 0, +"<range>", <bang>0, "<mods>", <q-args>)'

exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete Glog  :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "")'
exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete Gclog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "c")'
exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete GcLog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "c")'
exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete Gllog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "l")'
exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete GlLog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "l")'

exe 'command! -bar -bang -nargs=*                          -complete=customlist,fugitive#EditComplete   Ge       exe fugitive#Open("edit<bang>", 0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=*                          -complete=customlist,fugitive#EditComplete   Gedit    exe fugitive#Open("edit<bang>", 0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=*                          -complete=customlist,fugitive#ReadComplete   Gpedit   exe fugitive#Open("pedit", <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=* -range=-1' s:addr_other '-complete=customlist,fugitive#ReadComplete   Gsplit   exe fugitive#Open((<count> > 0 ? <count> : "").(<count> ? "split" : "edit"), <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=* -range=-1' s:addr_other '-complete=customlist,fugitive#ReadComplete   Gvsplit  exe fugitive#Open((<count> > 0 ? <count> : "").(<count> ? "vsplit" : "edit!"), <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=* -range=-1' s:addr_tabs  '-complete=customlist,fugitive#ReadComplete   Gtabedit exe fugitive#Open((<count> >= 0 ? <count> : "")."tabedit", <bang>0, "<mods>", <q-args>, [<f-args>])'

if exists(':Gr') != 2
  exe 'command! -bar -bang -nargs=* -range=-1                -complete=customlist,fugitive#ReadComplete   Gr     exe fugitive#ReadCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
endif
exe 'command! -bar -bang -nargs=* -range=-1                -complete=customlist,fugitive#ReadComplete   Gread    exe fugitive#ReadCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'

exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gdiffsplit  exe fugitive#Diffsplit(1, <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Ghdiffsplit exe fugitive#Diffsplit(0, <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gvdiffsplit exe fugitive#Diffsplit(0, <bang>0, "vert <mods>", <q-args>, [<f-args>])'

exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gw     exe fugitive#WriteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gwrite exe fugitive#WriteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gwq    exe fugitive#WqCommand(   <line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'

exe 'command! -bar -bang -nargs=0 -complete=customlist,fugitive#CompleteObject GRemove exe fugitive#RemoveCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=0 -complete=customlist,fugitive#CompleteObject GDelete exe fugitive#DeleteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#CompleteObject GMove   exe fugitive#MoveCommand(  <line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#RenameComplete GRename exe fugitive#RenameCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
if exists(':Gremove') != 2
  exe 'command! -bar -bang -nargs=0 -complete=customlist,fugitive#CompleteObject Gremove exe fugitive#RemoveCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
endif
if exists(':Gdelete') != 2
  exe 'command! -bar -bang -nargs=0 -complete=customlist,fugitive#CompleteObject Gdelete exe fugitive#DeleteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
endif
if exists(':Gmove') != 2
  exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#CompleteObject Gmove   exe fugitive#MoveCommand(  <line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
endif
if exists(':Grename') != 2
  exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#RenameComplete Grename exe fugitive#RenameCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
endif

exe 'command! -bar -bang -range=-1 -nargs=* -complete=customlist,fugitive#CompleteObject GBrowse exe fugitive#BrowseCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
if exists(':Gbrowse') != 2
  exe 'command! -bar -bang -range=-1 -nargs=* -complete=customlist,fugitive#CompleteObject Gbrowse exe fugitive#BrowseCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>, [<f-args>])'
endif

if get(g:, 'fugitive_no_maps')
  finish
endif

let s:nowait = v:version >= 704 ? '<nowait>' : ''

function! s:Map(mode, lhs, rhs, ...) abort
  for mode in split(a:mode, '\zs')
    let flags = (a:0 ? a:1 : '') . (a:rhs =~# '<Plug>' ? '' : '<script>')
    let head = a:lhs
    let tail = ''
    let keys = get(g:, mode.'remap', {})
    if type(keys) == type([])
      return
    endif
    while !empty(head)
      if has_key(keys, head)
        let head = keys[head]
        if empty(head)
          return
        endif
        break
      endif
      let tail = matchstr(head, '<[^<>]*>$\|.$') . tail
      let head = substitute(head, '<[^<>]*>$\|.$', '', '')
    endwhile
    if flags !~# '<unique>' || empty(mapcheck(head.tail, mode))
      exe mode.'map' s:nowait flags head.tail a:rhs
    endif
  endfor
endfunction

call s:Map('c', '<C-R><C-G>', 'fnameescape(fugitive#Object(@%))', '<expr>')
call s:Map('n', 'y<C-G>', ':<C-U>call setreg(v:register, fugitive#Object(@%))<CR>', '<silent>')
