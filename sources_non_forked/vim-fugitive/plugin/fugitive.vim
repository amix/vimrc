" fugitive.vim - A Git wrapper so awesome, it should be illegal
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      2.4
" GetLatestVimScripts: 2975 1 :AutoInstall: fugitive.vim

if exists('g:loaded_fugitive')
  finish
endif
let g:loaded_fugitive = 1

function! s:shellslash(path) abort
  if &shell =~? 'cmd' || exists('+shellslash') && !&shellslash
    return tr(a:path, '\', '/')
  else
    return a:path
  endif
endfunction

function! FugitiveIsGitDir(path) abort
  let path = substitute(a:path, '[\/]$', '', '') . '/'
  return getfsize(path.'HEAD') > 10 && (
        \ isdirectory(path.'objects') && isdirectory(path.'refs') ||
        \ getftype(path.'commondir') ==# 'file')
endfunction

let s:worktree_for_dir = {}
let s:dir_for_worktree = {}
function! FugitiveTreeForGitDir(git_dir) abort
  let dir = substitute(s:shellslash(a:git_dir), '/$', '', '')
  if dir =~# '/\.git$'
    return len(dir) ==# 5 ? '/' : dir[0:-6]
  endif
  if !has_key(s:worktree_for_dir, dir)
    let s:worktree_for_dir[dir] = ''
    let config_file = dir . '/config'
    if filereadable(config_file)
      let config = readfile(config_file,'',10)
      call filter(config,'v:val =~# "^\\s*worktree *="')
      if len(config) == 1
        let worktree = matchstr(config[0], '= *\zs.*')
      endif
    elseif filereadable(dir . '/gitdir')
      let worktree = fnamemodify(readfile(dir . '/gitdir')[0], ':h')
      if worktree ==# '.'
        unlet! worktree
      endif
    endif
    if exists('worktree')
      let s:worktree_for_dir[dir] = worktree
      let s:dir_for_worktree[s:worktree_for_dir[dir]] = dir
    endif
  endif
  if s:worktree_for_dir[dir] =~# '^\.'
    return simplify(dir . '/' . s:worktree_for_dir[dir])
  else
    return s:worktree_for_dir[dir]
  endif
endfunction

function! FugitiveExtractGitDir(path) abort
  let path = s:shellslash(a:path)
  if path =~# '^fugitive:'
    return matchstr(path, '\C^fugitive:\%(//\)\=\zs.\{-\}\ze\%(//\|::\|$\)')
  elseif isdirectory(path)
    let path = fnamemodify(path, ':p:s?/$??')
  else
    let path = fnamemodify(path, ':p:h:s?/$??')
  endif
  let root = resolve(path)
  if root !=# path
    silent! exe haslocaldir() ? 'lcd .' : 'cd .'
  endif
  let previous = ""
  while root !=# previous
    if root =~# '\v^//%([^/]+/?)?$'
      break
    endif
    if index(split($GIT_CEILING_DIRECTORIES, ':'), root) >= 0
      break
    endif
    if root ==# $GIT_WORK_TREE && FugitiveIsGitDir($GIT_DIR)
      return simplify(fnamemodify($GIT_DIR, ':p:s?[\/]$??'))
    endif
    if FugitiveIsGitDir($GIT_DIR)
      call FugitiveTreeForGitDir(simplify(fnamemodify($GIT_DIR, ':p:s?[\/]$??')))
      if has_key(s:dir_for_worktree, root)
        return s:dir_for_worktree[root]
      endif
    endif
    let dir = substitute(root, '[\/]$', '', '') . '/.git'
    let type = getftype(dir)
    if type ==# 'dir' && FugitiveIsGitDir(dir)
      return dir
    elseif type ==# 'link' && FugitiveIsGitDir(dir)
      return resolve(dir)
    elseif type !=# '' && filereadable(dir)
      let line = get(readfile(dir, '', 1), 0, '')
      if line =~# '^gitdir: \.' && FugitiveIsGitDir(root.'/'.line[8:-1])
        return simplify(root.'/'.line[8:-1])
      elseif line =~# '^gitdir: ' && FugitiveIsGitDir(line[8:-1])
        return line[8:-1]
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
  if exists('b:git_dir')
    return fugitive#Init()
  endif
endfunction

function! FugitiveStatusline(...) abort
  if !exists('b:git_dir')
    return ''
  endif
  return fugitive#Statusline()
endfunction

function! FugitiveHead(...) abort
  if !exists('b:git_dir')
    return ''
  endif
  return fugitive#repo().head(a:0 ? a:1 : 0)
endfunction

function! FugitivePath(...) abort
  let file = fnamemodify(a:0 ? a:1 : @%, ':p')
  if file =~? '^fugitive:'
    return fugitive#Path(file)
  else
    return file
  endif
endfunction

function! FugitiveReal(...) abort
  return call('FugitivePath', a:000)
endfunction

augroup fugitive
  autocmd!

  autocmd BufNewFile,BufReadPost * call FugitiveDetect(expand('%:p'))
  autocmd FileType           netrw call FugitiveDetect(fnamemodify(get(b:, 'netrw_curdir', @%), ':p'))
  autocmd User NERDTreeInit,NERDTreeNewRoot
        \ if exists('b:NERDTree.root.path.str') |
        \   call FugitiveDetect(b:NERDTree.root.path.str()) |
        \ endif
  autocmd VimEnter * if expand('<amatch>')==''|call FugitiveDetect(getcwd())|endif
  autocmd CmdWinEnter * call FugitiveDetect(expand('#:p'))

  autocmd FileType git
        \ if exists('b:git_dir') |
        \  call fugitive#MapJumps() |
        \ endif
  autocmd FileType git,gitcommit,gitrebase
        \ if exists('b:git_dir') |
        \   call fugitive#MapCfile() |
        \ endif

  autocmd BufReadCmd  index{,.lock}
        \ if FugitiveIsGitDir(expand('<amatch>:p:h')) |
        \   exe fugitive#BufReadStatus() |
        \ elseif filereadable(expand('<amatch>')) |
        \   read <amatch> |
        \   1delete |
        \ endif
  autocmd FileReadCmd fugitive://**//[0-3]/**          exe fugitive#FileRead()
  autocmd BufReadCmd  fugitive://**//[0-3]/**          exe fugitive#BufReadIndex()
  autocmd BufWriteCmd fugitive://**//[0-3]/**          exe fugitive#BufWriteIndex()
  autocmd BufReadCmd  fugitive://**//[0-9a-f][0-9a-f]* exe fugitive#BufReadObject()
  autocmd FileReadCmd fugitive://**//[0-9a-f][0-9a-f]* exe fugitive#FileRead()

  autocmd User Flags call Hoist('buffer', function('FugitiveStatusline'))
augroup END
