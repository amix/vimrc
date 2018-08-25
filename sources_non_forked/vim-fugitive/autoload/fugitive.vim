" Location:     autoload/fugitive.vim
" Maintainer:   Tim Pope <http://tpo.pe/>

if exists('g:autoloaded_fugitive')
  finish
endif
let g:autoloaded_fugitive = 1

if !exists('g:fugitive_git_executable')
  let g:fugitive_git_executable = 'git'
endif

" Section: Utility

function! s:function(name) abort
  return function(substitute(a:name,'^s:',matchstr(expand('<sfile>'), '<SNR>\d\+_'),''))
endfunction

function! s:sub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'')
endfunction

function! s:gsub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'g')
endfunction

function! s:Uniq(list) abort
  let i = 0
  let seen = {}
  while i < len(a:list)
    let str = string(a:list[i])
    if has_key(seen, str)
      call remove(a:list, i)
    else
      let seen[str] = 1
      let i += 1
    endif
  endwhile
  return a:list
endfunction

function! s:winshell() abort
  return has('win32') && &shellcmdflag !~# '^-'
endfunction

function! s:shellesc(arg) abort
  if a:arg =~ '^[A-Za-z0-9_/.-]\+$'
    return a:arg
  elseif s:winshell()
    return '"'.s:gsub(s:gsub(a:arg, '"', '""'), '\%', '"%"').'"'
  else
    return shellescape(a:arg)
  endif
endfunction

function! s:fnameescape(file) abort
  if exists('*fnameescape')
    return fnameescape(a:file)
  else
    return escape(a:file," \t\n*?[{`$\\%#'\"|!<")
  endif
endfunction

function! s:throw(string) abort
  let v:errmsg = 'fugitive: '.a:string
  throw v:errmsg
endfunction

function! s:warn(str) abort
  echohl WarningMsg
  echomsg a:str
  echohl None
  let v:warningmsg = a:str
endfunction

function! s:Slash(path) abort
  if exists('+shellslash')
    return tr(a:path, '\', '/')
  else
    return a:path
  endif
endfunction

function! s:PlatformSlash(path) abort
  if exists('+shellslash') && !&shellslash
    return tr(a:path, '/', '\')
  else
    return a:path
  endif
endfunction

function! s:Resolve(path) abort
  let path = resolve(a:path)
  if has('win32')
    let path = s:PlatformSlash(fnamemodify(fnamemodify(path, ':h'), ':p') . fnamemodify(path, ':t'))
  endif
  return path
endfunction

function! s:cpath(path, ...) abort
  if exists('+fileignorecase') && &fileignorecase
    let path = s:PlatformSlash(tolower(a:path))
  else
    let path = s:PlatformSlash(a:path)
  endif
  return a:0 ? path ==# s:cpath(a:1) : path
endfunction

let s:executables = {}

function! s:executable(binary) abort
  if !has_key(s:executables, a:binary)
    let s:executables[a:binary] = executable(a:binary)
  endif
  return s:executables[a:binary]
endfunction

function! s:map(mode, lhs, rhs, ...) abort
  let flags = (a:0 ? a:1 : '') . (a:rhs =~# '<Plug>' ? '' : '<script>')
  let head = a:lhs
  let tail = ''
  let keys = get(g:, a:mode.'remap', {})
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
  if flags !~# '<unique>' || empty(mapcheck(head.tail, a:mode))
    exe a:mode.'map <buffer>' flags head.tail a:rhs
    if a:0 > 1
      let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') .
            \ '|sil! exe "' . a:mode . 'unmap <buffer> ' . head.tail . '"'
    endif
  endif
endfunction

function! s:System(cmd) abort
  try
    return system(a:cmd)
  catch /^Vim\%((\a\+)\)\=:E484:/
    let opts = ['shell', 'shellcmdflag', 'shellredir', 'shellquote', 'shellxquote', 'shellxescape', 'shellslash']
    call filter(opts, 'exists("+".v:val) && !empty(eval("&".v:val))')
    call map(opts, 'v:val."=".eval("&".v:val)')
    call s:throw('failed to run `' . a:cmd . '` with ' . join(opts, ' '))
  endtry
endfunction

" Section: Git

function! s:UserCommand() abort
  return get(g:, 'fugitive_git_command', g:fugitive_git_executable)
endfunction

function! s:Prepare(dir, ...) abort
  if type(a:dir) == type([])
    let args = ['--git-dir=' . (a:0 ? a:1 : get(b:, 'git_dir', ''))] + a:dir
  else
    let args = ['--git-dir=' . a:dir] + (a:000)
  endif
  return g:fugitive_git_executable . ' ' . join(map(args, 's:shellesc(v:val)'))
endfunction

let s:git_versions = {}
function! fugitive#GitVersion(...) abort
  if !has_key(s:git_versions, g:fugitive_git_executable)
    let s:git_versions[g:fugitive_git_executable] = matchstr(system(g:fugitive_git_executable.' --version'), "\\S\\+\\ze\n")
  endif
  return s:git_versions[g:fugitive_git_executable]
endfunction

let s:commondirs = {}
function! fugitive#CommonDir(dir) abort
  if empty(a:dir)
    return ''
  endif
  if !has_key(s:commondirs, a:dir)
    if getfsize(a:dir . '/HEAD') < 10
      let s:commondirs[a:dir] = ''
    elseif filereadable(a:dir . '/commondir')
      let dir = get(readfile(a:dir . '/commondir', 1), 0, '')
      if dir =~# '^/\|^\a:/'
        let s:commondirs[a:dir] = dir
      else
        let s:commondirs[a:dir] = simplify(a:dir . '/' . dir)
      endif
    else
      let s:commondirs[a:dir] = a:dir
    endif
  endif
  return s:commondirs[a:dir]
endfunction

function! s:Tree(...) abort
  return FugitiveTreeForGitDir(a:0 ? a:1 : get(b:, 'git_dir', ''))
endfunction

function! s:PreparePathArgs(cmd, dir) abort
  if fugitive#GitVersion() !~# '^[01]\.'
    call insert(a:cmd, '--literal-pathspecs')
  endif
  let split = index(a:cmd, '--')
  let tree = s:Tree(a:dir)
  if empty(tree) || split < 0
    return a:cmd
  endif
  for i in range(split + 1, len(a:cmd) - 1)
    let a:cmd[i] = fugitive#Path(a:cmd[i], './', a:dir)
  endfor
  return a:cmd
endfunction

function! s:TreeChomp(...) abort
  let args = copy(type(a:1) == type([]) ? a:1 : a:000)
  let dir = a:0 > 1 && type(a:1) == type([]) ? a:2 : b:git_dir
  call s:PreparePathArgs(args, dir)
  let tree = s:Tree(dir)
  let pre = ''
  if empty(tree)
    let args = ['--git-dir=' . dir] + args
  elseif s:cpath(tree) !=# s:cpath(getcwd())
    if fugitive#GitVersion() =~# '^[01]\.'
      let pre = 'cd ' . s:shellesc(tree) . (s:winshell() ? ' & ' : '; ')
    else
      let args = ['-C', tree] + args
    endif
  endif
  return s:sub(s:System(pre . g:fugitive_git_executable . ' ' .
        \ join(map(args, 's:shellesc(v:val)'))), '\n$', '')
endfunction

function! fugitive#Prepare(cmd, ...) abort
  let dir = a:0 ? a:1 : get(b:, 'git_dir', '')
  let tree = s:Tree(dir)
  let args = type(a:cmd) == type([]) ? join(map(s:PreparePathArgs(copy(a:cmd), dir), 's:shellesc(v:val)')) : a:cmd
  let pre = ''
  if empty(tree) || (type(a:cmd) == type([]) && index(a:cmd, '--') == len(a:cmd) - 1)
    let args = s:shellesc('--git-dir=' . dir) . ' ' . args
  elseif fugitive#GitVersion() =~# '^[01]\.'
    let pre = 'cd ' . s:shellesc(tree) . (s:winshell() ? ' & ' : '; ')
  else
    let args = '-C ' . s:shellesc(tree) . ' ' . args
  endif
  return pre . g:fugitive_git_executable . ' ' . args
endfunction

function! fugitive#Head(...) abort
  let dir = a:0 > 1 ? a:2 : get(b:, 'git_dir', '')
  if empty(dir) || !filereadable(dir . '/HEAD')
    return ''
  endif
  let head = readfile(dir . '/HEAD')[0]
  if head =~# '^ref: '
    return substitute(head, '\C^ref: \%(refs/\%(heads/\|remotes/\|tags/\)\=\)\=', '', '')
  elseif head =~# '^\x\{40\}$'
    let len = a:0 ? a:1 : 0
    return len < 0 ? head : len ? head[0:len-1] : ''
  else
    return ''
  endif
endfunction

function! fugitive#RevParse(rev, ...) abort
  let hash = system(s:Prepare(a:0 ? a:1 : b:git_dir, 'rev-parse', '--verify', a:rev, '--'))[0:-2]
  if !v:shell_error && hash =~# '^\x\{40\}$'
    return hash
  endif
  call s:throw('rev-parse '.a:rev.': '.hash)
endfunction

function! fugitive#Config(name, ...) abort
  let cmd = s:Prepare(a:0 ? a:1 : get(b:, 'git_dir', ''), 'config', '--get', a:name)
  let out = matchstr(system(cmd), "[^\r\n]*")
  return v:shell_error ? '' : out
endfunction

function! s:Remote(dir) abort
  let head = FugitiveHead(0, a:dir)
  let remote = len(head) ? fugitive#Config('branch.' . head . '.remote') : ''
  let i = 10
  while remote ==# '.' && i > 0
    let head = matchstr(fugitive#Config('branch.' . head . '.merge'), 'refs/heads/\zs.*')
    let remote = len(head) ? fugitive#Config('branch.' . head . '.remote') : ''
    let i -= 1
  endwhile
  return remote =~# '^\.\=$' ? 'origin' : remote
endfunction

function! fugitive#RemoteUrl(...) abort
  let dir = a:0 > 1 ? a:2 : get(b:, 'git_dir', '')
  let remote = !a:0 || a:1 =~# '^\.\=$' ? s:Remote(dir) : a:1
  if fugitive#GitVersion() =~# '^[01]\.\|^2\.[0-6]\.'
    return fugitive#Config('remote.' . remote . '.url')
  endif
  let cmd = s:Prepare(dir, 'remote', 'get-url', remote, '--')
  let out = substitute(system(cmd), "\n$", '', '')
  return v:shell_error ? '' : out
endfunction

" Section: Repository Object

function! s:add_methods(namespace, method_names) abort
  for name in a:method_names
    let s:{a:namespace}_prototype[name] = s:function('s:'.a:namespace.'_'.name)
  endfor
endfunction

let s:commands = []
function! s:command(definition) abort
  let s:commands += [a:definition]
endfunction

function! s:define_commands() abort
  for command in s:commands
    exe 'command! -buffer '.command
  endfor
endfunction

let s:repo_prototype = {}
let s:repos = {}

function! fugitive#repo(...) abort
  let dir = a:0 ? a:1 : (exists('b:git_dir') && b:git_dir !=# '' ? b:git_dir : FugitiveExtractGitDir(expand('%:p')))
  if dir !=# ''
    if has_key(s:repos, dir)
      let repo = get(s:repos, dir)
    else
      let repo = {'git_dir': dir}
      let s:repos[dir] = repo
    endif
    return extend(repo, s:repo_prototype, 'keep')
  endif
  call s:throw('not a git repository: '.expand('%:p'))
endfunction

function! s:repo_dir(...) dict abort
  return join([self.git_dir]+a:000,'/')
endfunction

function! s:repo_tree(...) dict abort
  let dir = s:Tree(self.git_dir)
  if dir ==# ''
    call s:throw('no work tree')
  else
    return join([dir]+a:000,'/')
  endif
endfunction

function! s:repo_bare() dict abort
  if self.dir() =~# '/\.git$'
    return 0
  else
    return s:Tree(self.git_dir) ==# ''
  endif
endfunction

function! s:repo_route(object) dict abort
  return fugitive#Route(a:object, self.git_dir)
endfunction

function! s:repo_translate(rev) dict abort
  return s:Slash(fugitive#Route(substitute(a:rev, '^/', ':(top)', ''), self.git_dir))
endfunction

function! s:repo_head(...) dict abort
  return fugitive#Head(a:0 ? a:1 : 0, self.git_dir)
endfunction

call s:add_methods('repo',['dir','tree','bare','route','translate','head'])

function! s:repo_git_command(...) dict abort
  let git = s:UserCommand() . ' --git-dir='.s:shellesc(self.git_dir)
  return git.join(map(copy(a:000),'" ".s:shellesc(v:val)'),'')
endfunction

function! s:repo_git_chomp(...) dict abort
  return s:sub(s:System(s:Prepare(a:000, self.git_dir)), '\n$', '')
endfunction

function! s:repo_git_chomp_in_tree(...) dict abort
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let dir = getcwd()
  try
    execute cd s:fnameescape(self.tree())
    return call(self.git_chomp, a:000, self)
  finally
    execute cd s:fnameescape(dir)
  endtry
endfunction

function! s:repo_rev_parse(rev) dict abort
  return fugitive#RevParse(a:rev, self.git_dir)
endfunction

call s:add_methods('repo',['git_command','git_chomp','git_chomp_in_tree','rev_parse'])

function! s:repo_superglob(base) dict abort
  return map(fugitive#Complete(a:base, self.git_dir), 'substitute(v:val, ''\\\(.\)'', ''\1'', "g")')
endfunction

call s:add_methods('repo',['superglob'])

function! s:repo_config(name) dict abort
  return fugitive#Config(a:name, self.git_dir)
endfunction

function! s:repo_user() dict abort
  let username = self.config('user.name')
  let useremail = self.config('user.email')
  return username.' <'.useremail.'>'
endfunction

call s:add_methods('repo',['config', 'user'])

" Section: File API

function! s:DirCommitFile(path) abort
  let vals = matchlist(s:Slash(a:path), '\c^fugitive:\%(//\)\=\(.\{-\}\)\%(//\|::\)\(\x\{40\}\|[0-3]\)\(/.*\)\=$')
  if empty(vals)
    return ['', '', '']
  endif
  return vals[1:3]
endfunction

function! s:DirRev(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  return [dir, (commit =~# '^.$' ? ':' : '') . commit . substitute(file, '^/', ':', '')]
endfunction

function! s:Owner(path, ...) abort
  let dir = a:0 ? a:1 : get(b:, 'git_dir', '')
  if empty(dir)
    return ''
  endif
  let [pdir, commit, file] = s:DirCommitFile(a:path)
  if s:cpath(dir, pdir) && commit =~# '^\x\{40\}$'
    return commit
  endif
  let path = fnamemodify(a:path, ':p')
  if s:cpath(dir . '/', path[0 : len(dir)]) && a:path =~# 'HEAD$'
    return strpart(path, len(dir) + 1)
  endif
  let refs = fugitive#CommonDir(dir) . '/refs'
  if s:cpath(refs . '/', path[0 : len(refs)]) && path !~# '[\/]$'
    return strpart(path, len(refs) - 4)
  endif
  return ''
endfunction

function! fugitive#Real(url) abort
  if empty(a:url)
    return ''
  endif
  let [dir, commit, file] = s:DirCommitFile(a:url)
  if len(dir)
    let tree = s:Tree(dir)
    return s:PlatformSlash((len(tree) ? tree : dir) . file)
  endif
  let pre = substitute(matchstr(a:url, '^\a\a\+\ze:'), '^.', '\u&', '')
  if len(pre) && pre !=? 'fugitive' && exists('*' . pre . 'Real')
    let url = {pre}Real(a:url)
  else
    let url = fnamemodify(a:url, ':p' . (a:url =~# '[\/]$' ? '' : ':s?[\/]$??'))
  endif
  return s:PlatformSlash(empty(url) ? a:url : url)
endfunction

function! fugitive#Path(url, ...) abort
  if !a:0 || empty(a:url)
    return fugitive#Real(a:url)
  endif
  let url = s:Slash(fnamemodify(a:url, ':p'))
  if url =~# '/$' && s:Slash(a:url) !~# '/$'
    let url = url[0:-2]
  endif
  let dir = a:0 > 1 ? a:2 : get(b:, 'git_dir', '')
  let tree = s:Tree(dir)
  let [argdir, commit, file] = s:DirCommitFile(a:url)
  if len(argdir) && s:cpath(argdir) !=# s:cpath(dir)
    let file = ''
  elseif len(dir) && s:cpath(url[0 : len(dir)]) ==# s:cpath(dir . '/')
    let file = '/.git'.url[strlen(dir) : -1]
  elseif len(tree) && s:cpath(url[0 : len(tree)]) ==# s:cpath(tree . '/')
    let file = url[len(tree) : -1]
  elseif s:cpath(url) ==# s:cpath(tree) || len(argdir) && empty(file)
    let file = '/'
  endif
  if empty(file) && a:1 =~# '^$\|^[.:]/$'
    return s:Slash(fugitive#Real(a:url))
  endif
  return substitute(file, '^/', a:1, '')
endfunction

function! s:Relative(...) abort
  return fugitive#Path(@%, a:0 ? a:1 : ':(top)')
endfunction

function! fugitive#Route(object, ...) abort
  if a:object =~# '^[~$]'
    let prefix = matchstr(a:object, '^[~$]\i*')
    let owner = expand(prefix)
    return s:PlatformSlash((len(owner) ? owner : prefix) . strpart(a:object, len(prefix)))
  elseif s:Slash(a:object) =~# '^$\|^/\|^\%(\a\a\+:\).*\%(//\|::\)' . (has('win32') ? '\|^\a:/' : '')
    return s:PlatformSlash(a:object)
  elseif s:Slash(a:object) =~# '^\.\.\=\%(/\|$\)'
    return s:PlatformSlash(simplify(getcwd() . '/' . a:object))
  endif
  let dir = a:0 ? a:1 : get(b:, 'git_dir', '')
  if empty(dir)
    let file = matchstr(a:object, '^\%(:\d:\|[^:]*:\)\zs.*', '', '')
    let dir = FugitiveExtractGitDir(file)
    if empty(dir)
      return fnamemodify(len(file) ? file : a:object, ':p')
    endif
  endif
  let rev = s:Slash(a:object)
  let tree = s:Tree(dir)
  let base = len(tree) ? tree : 'fugitive://' . dir . '//0'
  if rev ==# '.git'
    let f = len(tree) ? tree . '/.git' : dir
  elseif rev =~# '^\.git/'
    let f = substitute(rev, '^\.git', '', '')
    let cdir = fugitive#CommonDir(dir)
    if f =~# '^/\.\./\.\.\%(/\|$\)'
      let f = simplify(len(tree) ? tree . f[3:-1] : dir . f)
    elseif f =~# '^/\.\.\%(/\|$\)'
      let f = base . f[3:-1]
    elseif cdir !=# dir && (
          \ f =~# '^/\%(config\|hooks\|info\|logs/refs\|objects\|refs\|worktrees\)\%(/\|$\)' ||
          \ f !~# '^/logs$\|/\w*HEAD$' && getftime(dir . f) < 0 && getftime(cdir . f) >= 0)
      let f = simplify(cdir . f)
    else
      let f = simplify(dir . f)
    endif
  elseif rev ==# ':/'
    let f = base
  elseif rev =~# '^\.\%(/\|$\)'
    let f = base . rev[1:-1]
  elseif rev =~# '^::\%(/\|\a\+\:\)'
    let f = rev[2:-1]
  elseif rev =~# '^::\.\.\=\%(/\|$\)'
    let f = simplify(getcwd() . '/' . rev[2:-1])
  elseif rev =~# '^::'
    let f = base . '/' . rev[2:-1]
  elseif rev =~# '^:\%([0-3]:\)\=\.\.\=\%(/\|$\)\|^:[0-3]:\%(/\|\a\+:\)'
    let f = rev =~# '^:\%([0-3]:\)\=\.' ? simplify(getcwd() . '/' . matchstr(rev, '\..*')) : rev[3:-1]
    if s:cpath(base . '/', (f . '/')[0 : len(base)])
      let f = 'fugitive://' . dir . '//' . +matchstr(rev, '^:\zs\d\ze:') . '/' . strpart(f, len(base) + 1)
    else
      let altdir = FugitiveExtractGitDir(f)
      if len(altdir) && !s:cpath(dir, altdir)
        return fugitive#Route(a:object, altdir)
      endif
    endif
  elseif rev =~# '^:[0-3]:'
    let f = 'fugitive://' . dir . '//' . rev[1] . '/' . rev[3:-1]
  elseif rev ==# ':'
    if $GIT_INDEX_FILE =~# '/[^/]*index[^/]*\.lock$' && s:cpath(fnamemodify($GIT_INDEX_FILE,':p')[0:strlen(dir)]) ==# s:cpath(dir . '/') && filereadable($GIT_INDEX_FILE)
      let f = fnamemodify($GIT_INDEX_FILE, ':p')
    else
      let f = dir . '/index'
    endif
  elseif rev =~# '^:(\%(top\|top,literal\|literal,top\|literal\))'
    let f = base . '/' . matchstr(rev, ')\zs.*')
  elseif rev =~# '^:/\@!'
    let f = 'fugitive://' . dir . '//0/' . rev[1:-1]
  else
    if rev =~# 'HEAD$\|^refs/' && rev !~# ':'
      let cdir = rev =~# '^refs/' ? fugitive#CommonDir(dir) : dir
      if filereadable(cdir . '/' . rev)
        let f = simplify(cdir . '/' . rev)
      endif
    endif
    if !exists('f')
      let commit = substitute(matchstr(rev, '^[^:]\+\|^:.*'), '^@\%($|[^~]\)\@=', 'HEAD', '')
      let file = substitute(matchstr(rev, '^[^:]\+\zs:.*'), '^:', '/', '')
      if file =~# '^/\.\.\=\%(/\|$\)\|^//\|^/\a\+:'
        let file = file =~# '^/\.' ? simplify(getcwd() . file) : file[1:-1]
        if s:cpath(base . '/', (file . '/')[0 : len(base)])
          let file = '/' . strpart(file, len(base) + 1)
        else
          let altdir = FugitiveExtractGitDir(file)
          if len(altdir) && !s:cpath(dir, altdir)
            return fugitive#Route(a:object, altdir)
          endif
          return file
        endif
      endif
      if commit !~# '^[0-9a-f]\{40\}$'
        let commit = system(s:Prepare(dir, 'rev-parse', '--verify', commit, '--'))[0:-2]
        let commit = v:shell_error ? '' : commit
      endif
      if len(commit)
        let f = 'fugitive://' . dir . '//' . commit . file
      else
        let f = base . '/' . substitute(rev, '^:/:\=\|^[^:]\+:', '', '')
      endif
    endif
  endif
  return s:PlatformSlash(f)
endfunction

function! s:Generate(rev, ...) abort
  let dir = a:0 ? a:1 : get(b:, 'git_dir', '')
  let tree = s:Tree(dir)
  let object = a:rev
  if a:rev =~# '^/\.git\%(/\|$\)'
    let object = a:rev[1:-1]
  elseif a:rev =~# '^/' && len(tree) && getftime(tree . a:rev) >= 0 && getftime(a:rev) < 0
    let object = ':(top)' . a:rev[1:-1]
  endif
  return fugitive#Route(object, dir)
endfunction

function! s:RemoveDot(path, ...) abort
  if a:path !~# '^\./'
    return a:path
  endif
  let dir = a:0 ? a:1 : get(b:, 'git_dir', '')
  let cdir = fugitive#CommonDir(dir)
  if len(filter(['', '/tags', '/heads', '/remotes'], 'getftime(cdir . "/refs" . v:val . a:path[1:-1]) >= 0')) ||
        \ a:path =~# 'HEAD$' && filereadable(dir . a:path[1:-1]) ||
        \ a:path =~# '^\./refs/' && filereadable(cdir . a:path[1:-1])
    return a:path
  endif
  return a:path[2:-1]
endfunction

function! fugitive#Object(...) abort
  let dir = a:0 > 1 ? a:2 : get(b:, 'git_dir', '')
  let [fdir, rev] = s:DirRev(a:0 ? a:1 : @%)
  if s:cpath(dir) !=# s:cpath(fdir)
    let rev = ''
  endif
  let tree = s:Tree(dir)
  if empty(rev) && empty(tree)
  elseif empty(rev)
    let rev = fugitive#Path(a:0 ? a:1 : @%, './', dir)
    let cdir = fugitive#CommonDir(dir)
    if rev =~# '^\./\.git/refs/\%(tags\|heads\|remotes\)/.\|^\./\.git/\w*HEAD$'
      let rev = rev[7:-1]
    elseif s:cpath(cdir . '/refs/', rev[0 : len(cdir)])
      let rev = strpart(rev, len(cdir)+1)
    elseif rev =~# '^\./.git\%(/\|$\)'
      return fnamemodify(a:0 ? a:1 : @%, ':p')
    endif
  endif
  if rev !~# '^\.\%(/\|$\)' || s:cpath(getcwd(), tree)
    return rev
  else
    return tree . rev[1:-1]
  endif
endfunction

function! s:Expand(rev) abort
  if a:rev =~# '^:[0-3]$'
    let file = a:rev . s:Relative(':')
  elseif a:rev =~# '^-'
    let file = 'HEAD^{}' . a:rev[1:-1] . s:Relative(':')
  elseif a:rev =~# '^@{'
    let file = 'HEAD' . a:rev. s:Relative(':')
  elseif a:rev =~# '^[~^]/\@!'
    let commit = substitute(s:DirCommitFile(@%)[1], '^\d\=$', 'HEAD', '')
    let file = commit . a:rev . s:Relative(':')
  else
    let file = a:rev
  endif
  return s:sub(substitute(file,
        \ '\([%#]\)$\|\\\([[:punct:]]\)','\=len(submatch(2)) ? submatch(2) : fugitive#Path(expand(submatch(1)))','g'),
        \ '\.\@<=/$','')
endfunction

function! s:ShellExpand(cmd) abort
  return substitute(a:cmd, '\\\@<![%#]:\@!', '\=s:RemoveDot(fugitive#Path(expand(submatch(0)), "./"))', 'g')
endfunction

let s:trees = {}
let s:indexes = {}
function! s:TreeInfo(dir, commit) abort
  let git = s:Prepare(a:dir)
  if a:commit =~# '^:\=[0-3]$'
    let index = get(s:indexes, a:dir, [])
    let newftime = getftime(a:dir . '/index')
    if get(index, 0, -1) < newftime
      let out = system(git . ' ls-files --stage')
      let s:indexes[a:dir] = [newftime, {'0': {}, '1': {}, '2': {}, '3': {}}]
      if v:shell_error
        return [{}, -1]
      endif
      for line in split(out, "\n")
        let [info, filename] = split(line, "\t")
        let [mode, sha, stage] = split(info, '\s\+')
        let s:indexes[a:dir][1][stage][filename] = [newftime, mode, 'blob', sha, -2]
        while filename =~# '/'
          let filename = substitute(filename, '/[^/]*$', '', '')
          let s:indexes[a:dir][1][stage][filename] = [newftime, '040000', 'tree', '', 0]
        endwhile
      endfor
    endif
    return [get(s:indexes[a:dir][1], a:commit[-1:-1], {}), newftime]
  elseif a:commit =~# '^\x\{40\}$'
    if !has_key(s:trees, a:dir)
      let s:trees[a:dir] = {}
    endif
    if !has_key(s:trees[a:dir], a:commit)
      let ftime = +system(git . ' log -1 --pretty=format:%ct ' . a:commit)
      if v:shell_error
        let s:trees[a:dir][a:commit] = [{}, -1]
        return s:trees[a:dir][a:commit]
      endif
      let s:trees[a:dir][a:commit] = [{}, +ftime]
      let out = system(git . ' ls-tree -rtl --full-name ' . a:commit)
      if v:shell_error
        return s:trees[a:dir][a:commit]
      endif
      for line in split(out, "\n")
        let [info, filename] = split(line, "\t")
        let [mode, type, sha, size] = split(info, '\s\+')
        let s:trees[a:dir][a:commit][0][filename] = [ftime, mode, type, sha, +size, filename]
      endfor
    endif
    return s:trees[a:dir][a:commit]
  endif
  return [{}, -1]
endfunction

function! s:PathInfo(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  if empty(dir) || !get(g:, 'fugitive_file_api', 1)
    return [-1, '000000', '', '', -1]
  endif
  let path = substitute(file[1:-1], '/*$', '', '')
  let [tree, ftime] = s:TreeInfo(dir, commit)
  let entry = empty(path) ? [ftime, '040000', 'tree', '', -1] : get(tree, path, [])
  if empty(entry) || file =~# '/$' && entry[1] !=# 'tree'
    return [-1, '000000', '', '', -1]
  else
    return entry
  endif
endfunction

function! fugitive#simplify(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  if empty(dir)
    return ''
  endif
  if file =~# '/\.\.\%(/\|$\)'
    let tree = s:Tree(dir)
    if len(tree)
      let path = simplify(tree . file)
      if strpart(path . '/', 0, len(tree) + 1) !=# tree . '/'
        return s:PlatformSlash(path)
      endif
    endif
  endif
  return s:PlatformSlash('fugitive://' . simplify(dir) . '//' . commit . simplify(file))
endfunction

function! fugitive#resolve(url) abort
  let url = fugitive#simplify(a:url)
  if url =~? '^fugitive:'
    return url
  else
    return resolve(url)
  endif
endfunction

function! fugitive#getftime(url) abort
  return s:PathInfo(a:url)[0]
endfunction

function! fugitive#getfsize(url) abort
  let entry = s:PathInfo(a:url)
  if entry[4] == -2 && entry[2] ==# 'blob' && len(entry[3])
    let dir = s:DirCommitFile(a:url)[0]
    let size = +system(s:Prepare(dir, 'cat-file', '-s', entry[3], '--'))
    let entry[4] = v:shell_error ? -1 : size
  endif
  return entry[4]
endfunction

function! fugitive#getftype(url) abort
  return get({'tree': 'dir', 'blob': 'file'}, s:PathInfo(a:url)[2], '')
endfunction

function! fugitive#filereadable(url) abort
  return s:PathInfo(a:url)[2] ==# 'blob'
endfunction

function! fugitive#filewritable(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  if commit !~# '^\d$' || !filewritable(dir . '/index')
    return 0
  endif
  return s:PathInfo(a:url)[2] ==# 'blob' ? 1 : 2
endfunction

function! fugitive#isdirectory(url) abort
  return s:PathInfo(a:url)[2] ==# 'tree'
endfunction

function! fugitive#getfperm(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  let perm = getfperm(dir)
  let fperm = s:PathInfo(a:url)[1]
  if fperm ==# '040000'
    let fperm = '000755'
  endif
  if fperm !~# '[15]'
    let perm = tr(perm, 'x', '-')
  endif
  if fperm !~# '[45]$'
    let perm = tr(perm, 'rw', '--')
  endif
  if commit !~# '^\d$'
    let perm = tr(perm, 'w', '-')
  endif
  return perm ==# '---------' ? '' : perm
endfunction

function! fugitive#setfperm(url, perm) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  let entry = s:PathInfo(a:url)
  let perm = fugitive#getfperm(a:url)
  if commit !~# '^\d$' || entry[2] !=# 'blob' ||
      \ substitute(perm, 'x', '-', 'g') !=# substitute(a:perm, 'x', '-', 'g')
    return -2
  endif
  call system(s:Prepare(dir, 'update-index', '--index-info'),
        \ (a:perm =~# 'x' ? '000755 ' : '000644 ') . entry[3] . ' ' . commit . "\t" . file[1:-1])
  return v:shell_error ? -1 : 0
endfunction

function! s:TempCmd(out, cmd) abort
  let prefix = ''
  try
    let cmd = (type(a:cmd) == type([]) ? call('s:Prepare', a:cmd) : a:cmd)
    let redir = ' > ' . a:out
    if s:winshell()
      let cmd_escape_char = &shellxquote == '(' ?  '^' : '^^^'
      return s:System('cmd /c "' . prefix . s:gsub(cmd, '[<>]', cmd_escape_char . '&') . redir . '"')
    elseif &shell =~# 'fish'
      return s:System(' begin;' . prefix . cmd . redir . ';end ')
    else
      return s:System(' (' . prefix . cmd . redir . ') ')
    endif
  endtry
endfunction

if !exists('s:blobdirs')
  let s:blobdirs = {}
endif
function! s:BlobTemp(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  if empty(file)
    return ''
  endif
  if !has_key(s:blobdirs, dir)
    let s:blobdirs[dir] = tempname()
  endif
  let tempfile = s:blobdirs[dir] . '/' . commit . file
  let tempparent = fnamemodify(tempfile, ':h')
  if !isdirectory(tempparent)
    call mkdir(tempparent, 'p')
  endif
  if commit =~# '^\d$' || !filereadable(tempfile)
    let rev = s:DirRev(a:url)[1]
    let command = s:Prepare(dir, 'cat-file', 'blob', rev, '--')
    call s:TempCmd(tempfile, command)
    if v:shell_error
      call delete(tempfile)
      return ''
    endif
  endif
  return s:Resolve(tempfile)
endfunction

function! fugitive#readfile(url, ...) abort
  let entry = s:PathInfo(a:url)
  if entry[2] !=# 'blob'
    return []
  endif
  let temp = s:BlobTemp(a:url)
  if empty(temp)
    return []
  endif
  return call('readfile', [temp] + a:000)
endfunction

function! fugitive#writefile(lines, url, ...) abort
  let url = type(a:url) ==# type('') ? a:url : ''
  let [dir, commit, file] = s:DirCommitFile(url)
  let entry = s:PathInfo(url)
  if commit =~# '^\d$' && entry[2] !=# 'tree'
    let temp = tempname()
    if a:0 && a:1 =~# 'a' && entry[2] ==# 'blob'
      call writefile(fugitive#readfile(url, 'b'), temp, 'b')
    endif
    call call('writefile', [a:lines, temp] + a:000)
    let hash = system(s:Prepare(dir, 'hash-object', '-w', temp))[0:-2]
    let mode = len(entry[1]) ? entry[1] : '100644'
    if !v:shell_error && hash =~# '^\x\{40\}$'
      call system(s:Prepare(dir, 'update-index', '--index-info'),
            \ mode . ' ' . hash . ' ' . commit . "\t" . file[1:-1])
      if !v:shell_error
        return 0
      endif
    endif
  endif
  return call('writefile', [a:lines, a:url] + a:000)
endfunction

let s:globsubs = {
      \ '/**/': '/\%([^./][^/]*/\)*',
      \ '/**': '/\%([^./][^/]\+/\)*[^./][^/]*',
      \ '**/': '[^/]*\%(/[^./][^/]*\)*',
      \ '**': '.*',
      \ '/*': '/[^/.][^/]*',
      \ '*': '[^/]*',
      \ '?': '[^/]'}
function! fugitive#glob(url, ...) abort
  let [dirglob, commit, glob] = s:DirCommitFile(a:url)
  let append = matchstr(glob, '/*$')
  let glob = substitute(glob, '/*$', '', '')
  let pattern = '^' . substitute(glob, '/\=\*\*/\=\|/\=\*\|[.?\^$]', '\=get(s:globsubs, submatch(0), "\\" . submatch(0))', 'g')[1:-1] . '$'
  let results = []
  for dir in dirglob =~# '[*?]' ? split(glob(dirglob), "\n") : [dirglob]
    if empty(dir) || !get(g:, 'fugitive_file_api', 1) || !filereadable(dir . '/HEAD')
      continue
    endif
    let files = items(s:TreeInfo(dir, commit)[0])
    if len(append)
      call filter(files, 'v:val[1][2] ==# "tree"')
    endif
    call map(files, 'v:val[0]')
    call filter(files, 'v:val =~# pattern')
    let prepend = 'fugitive://' . dir . '//' . substitute(commit, '^:', '', '') . '/'
    call sort(files)
    call map(files, 's:PlatformSlash(prepend . v:val . append)')
    call extend(results, files)
  endfor
  if a:0 > 1 && a:2
    return results
  else
    return join(results, "\n")
  endif
endfunction

function! fugitive#delete(url, ...) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  if a:0 && len(a:1) || commit !~# '^\d$'
    return -1
  endif
  let entry = s:PathInfo(a:url)
  if entry[2] !=# 'blob'
    return -1
  endif
  call system(s:Prepare(dir, 'update-index', '--index-info'),
        \ '000000 0000000000000000000000000000000000000000 ' . commit . "\t" . file[1:-1])
  return v:shell_error ? -1 : 0
endfunction

" Section: Buffer Object

let s:buffer_prototype = {}

function! fugitive#buffer(...) abort
  let buffer = {'#': bufnr(a:0 ? a:1 : '%')}
  call extend(buffer, s:buffer_prototype, 'keep')
  if buffer.getvar('git_dir') !=# ''
    return buffer
  endif
  call s:throw('not a git repository: '.bufname(buffer['#']))
endfunction

function! s:buffer_getvar(var) dict abort
  return getbufvar(self['#'],a:var)
endfunction

function! s:buffer_getline(lnum) dict abort
  return get(getbufline(self['#'], a:lnum), 0, '')
endfunction

function! s:buffer_repo() dict abort
  return fugitive#repo(self.getvar('git_dir'))
endfunction

function! s:buffer_type(...) dict abort
  if !empty(self.getvar('fugitive_type'))
    let type = self.getvar('fugitive_type')
  elseif fnamemodify(self.spec(),':p') =~# '\.git/refs/\|\.git/\w*HEAD$'
    let type = 'head'
  elseif self.getline(1) =~ '^tree \x\{40\}$' && self.getline(2) == ''
    let type = 'tree'
  elseif self.getline(1) =~ '^\d\{6\} \w\{4\} \x\{40\}\>\t'
    let type = 'tree'
  elseif self.getline(1) =~ '^\d\{6\} \x\{40\}\> \d\t'
    let type = 'index'
  elseif isdirectory(self.spec())
    let type = 'directory'
  elseif self.spec() == ''
    let type = 'null'
  else
    let type = 'file'
  endif
  if a:0
    return !empty(filter(copy(a:000),'v:val ==# type'))
  else
    return type
  endif
endfunction

if has('win32')

  function! s:buffer_spec() dict abort
    let bufname = bufname(self['#'])
    let retval = ''
    for i in split(bufname,'[^:]\zs\\')
      let retval = fnamemodify((retval==''?'':retval.'\').i,':.')
    endfor
    return s:Slash(fnamemodify(retval,':p'))
  endfunction

else

  function! s:buffer_spec() dict abort
    let bufname = bufname(self['#'])
    return s:Slash(bufname == '' ? '' : fnamemodify(bufname,':p'))
  endfunction

endif

function! s:buffer_name() dict abort
  return self.spec()
endfunction

function! s:buffer_commit() dict abort
  return matchstr(self.spec(),'^fugitive:\%(//\)\=.\{-\}\%(//\|::\)\zs\w*')
endfunction

function! s:buffer_relative(...) dict abort
  let rev = matchstr(self.spec(),'^fugitive:\%(//\)\=.\{-\}\%(//\|::\)\zs.*')
  if rev != ''
    let rev = s:sub(rev,'\w*','')
  elseif s:cpath(self.spec()[0 : len(self.repo().dir())]) ==#
        \ s:cpath(self.repo().dir() . '/')
    let rev = '/.git'.self.spec()[strlen(self.repo().dir()) : -1]
  elseif !self.repo().bare() &&
        \ s:cpath(self.spec()[0 : len(self.repo().tree())]) ==#
        \ s:cpath(self.repo().tree() . '/')
    let rev = self.spec()[strlen(self.repo().tree()) : -1]
  endif
  return s:sub(s:sub(rev,'.\zs/$',''),'^/',a:0 ? a:1 : '')
endfunction

function! s:buffer_path(...) dict abort
  if a:0
    return self.relative(a:1)
  endif
  return self.relative()
endfunction

call s:add_methods('buffer',['getvar','getline','repo','type','spec','name','commit','path','relative'])

" Section: Completion

function! s:GlobComplete(lead, pattern) abort
  if v:version >= 704
    let results = glob(a:lead . a:pattern, 0, 1)
  else
    let results = split(glob(a:lead . a:pattern), "\n")
  endif
  call map(results, 'v:val !~# "/$" && isdirectory(v:val) ? v:val."/" : v:val')
  call map(results, 'v:val[ strlen(a:lead) : -1 ]')
  return results
endfunction

function! fugitive#PathComplete(base, ...) abort
  let dir = a:0 == 1 ? a:1 : get(b:, 'git_dir', '')
  let tree = s:Tree(dir) . '/'
  let strip = '^\%(:/:\=\|:(top)\|:(top,literal)\|:(literal,top)\|:(literal)\)'
  let base = substitute(a:base, strip, '', '')
  if base =~# '^\.git/'
    let pattern = s:gsub(base[5:-1], '/', '*&').'*'
    let matches = s:GlobComplete(dir . '/', pattern)
    let cdir = fugitive#CommonDir(dir)
    if len(cdir) && s:cpath(dir) !=# s:cpath(cdir)
      call extend(matches, s:GlobComplete(cdir . '/', pattern))
    endif
    call s:Uniq(matches)
    call map(matches, "'.git/' . v:val")
  elseif base =~# '^\~/'
    let matches = map(s:GlobComplete(expand('~/'), base[2:-1] . '*'), '"~/" . v:val')
  elseif a:base =~# '^/\|^\a\+:\|^\.\.\=/\|^:(literal)'
    let matches = s:GlobComplete('', base . '*')
  elseif len(tree) > 1
    let matches = s:GlobComplete(tree, s:gsub(base, '/', '*&').'*')
  else
    let matches = []
  endif
  call map(matches, 's:fnameescape(s:Slash(matchstr(a:base, strip) . v:val))')
  return matches
endfunction

function! fugitive#Complete(base, ...) abort
  let dir = a:0 == 1 ? a:1 : get(b:, 'git_dir', '')
  let cwd = a:0 == 1 ? s:Tree(dir) : getcwd()
  let tree = s:Tree(dir) . '/'
  let subdir = ''
  if len(tree) > 1 && s:cpath(tree, cwd[0 : len(tree) - 1])
    let subdir = strpart(cwd, len(tree)) . '/'
  endif

  if a:base =~# '^\.\=/\|^:(' || a:base !~# ':'
    let results = []
    if a:base =~# '^refs/'
      let results += map(s:GlobComplete(fugitive#CommonDir(dir) . '/', a:base . '*'), 's:Slash(v:val)')
    elseif a:base !~# '^\.\=/\|^:('
      let heads = ['HEAD', 'ORIG_HEAD', 'FETCH_HEAD', 'MERGE_HEAD', 'refs/']
      let heads += sort(split(s:TreeChomp(["rev-parse","--symbolic","--branches","--tags","--remotes"], dir),"\n"))
      if filereadable(fugitive#CommonDir(dir) . '/refs/stash')
        let heads += ["stash"]
        let heads += sort(split(s:TreeChomp(["stash","list","--pretty=format:%gd"], dir),"\n"))
      endif
      call filter(heads,'v:val[ 0 : strlen(a:base)-1 ] ==# a:base')
      let results += heads
    endif
    call map(results, 's:fnameescape(v:val)')
    if !empty(tree)
      let results += a:0 == 1 ? fugitive#PathComplete(a:base, dir) : fugitive#PathComplete(a:base)
    endif
    return results

  elseif a:base =~# '^:'
    let entries = split(s:TreeChomp(['ls-files','--stage'], dir),"\n")
    if a:base =~# ':\./'
      call map(entries, 'substitute(v:val, "\\M\t\\zs" . subdir, "./", "")')
    endif
    call map(entries,'s:sub(v:val,".*(\\d)\\t(.*)",":\\1:\\2")')
    if a:base !~# '^:[0-3]\%(:\|$\)'
      call filter(entries,'v:val[1] == "0"')
      call map(entries,'v:val[2:-1]')
    endif

  else
    let tree = matchstr(a:base, '.*[:/]')
    let entries = split(s:TreeChomp(['ls-tree', substitute(tree,  ':\zs\./', '\=subdir', '')], dir),"\n")
    call map(entries,'s:sub(v:val,"^04.*\\zs$","/")')
    call map(entries,'tree.s:sub(v:val,".*\t","")')

  endif
  call filter(entries, 'v:val[ 0 : strlen(a:base)-1 ] ==# a:base')
  return map(entries, 's:fnameescape(v:val)')
endfunction

" Section: Buffer auto-commands

function! s:ReplaceCmd(cmd) abort
  let temp = tempname()
  let err = s:TempCmd(temp, a:cmd)
  if v:shell_error
    call s:throw((len(err) ? err : filereadable(temp) ? join(readfile(temp), ' ') : 'unknown error running ' . a:cmd))
  endif
  let temp = s:Resolve(temp)
  let fn = expand('%:p')
  silent exe 'doau BufReadPre '.s:fnameescape(fn)
  silent exe 'keepalt file '.temp
  try
    silent noautocmd edit!
  finally
    try
      silent exe 'keepalt file '.s:fnameescape(fn)
    catch /^Vim\%((\a\+)\)\=:E302:/
    endtry
    call delete(temp)
    if s:cpath(fnamemodify(bufname('$'), ':p'), temp)
      silent execute 'bwipeout '.bufnr('$')
    endif
    silent exe 'doau BufReadPost '.s:fnameescape(fn)
  endtry
endfunction

function! fugitive#BufReadStatus() abort
  let amatch = s:Slash(expand('%:p'))
  if !exists('b:fugitive_display_format')
    let b:fugitive_display_format = filereadable(expand('%').'.lock')
  endif
  let b:fugitive_display_format = b:fugitive_display_format % 2
  let b:fugitive_type = 'index'
  try
    let dir = fnamemodify(amatch, ':h')
    setlocal noro ma nomodeline
    let prefix = ''
    if s:cpath(fnamemodify($GIT_INDEX_FILE !=# '' ? $GIT_INDEX_FILE : b:git_dir . '/index', ':p')) !=# s:cpath(amatch)
      if s:winshell()
        let old_index = $GIT_INDEX_FILE
      else
        let prefix = 'env GIT_INDEX_FILE='.s:shellesc(amatch).' '
      endif
    endif
    if b:fugitive_display_format
      let cmd = ['ls-files', '--stage']
    elseif fugitive#GitVersion() =~# '^0\|^1\.[1-7]\.'
      let cmd = ['status']
    else
      let cmd = [
            \ '-c', 'status.displayCommentPrefix=true',
            \ '-c', 'color.status=false',
            \ '-c', 'status.short=false',
            \ 'status']
    endif
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    let cwd = getcwd()
    let cmd_str = prefix . s:Prepare(cmd, dir)
    try
      if exists('old_index')
        let $GIT_INDEX_FILE = amatch
      endif
      execute cd s:fnameescape(s:Tree(dir))
      call s:ReplaceCmd(cmd_str)
    finally
      if exists('old_index')
        let $GIT_INDEX_FILE = old_index
      endif
      execute cd s:fnameescape(cwd)
    endtry
    if b:fugitive_display_format
      if &filetype !=# 'git'
        set filetype=git
      endif
      set nospell
    else
      if &filetype !=# 'gitcommit'
        set filetype=gitcommit
      endif
      set foldtext=fugitive#Foldtext()
    endif
    setlocal readonly nomodifiable nomodified noswapfile
    if &bufhidden ==# ''
      setlocal bufhidden=delete
    endif
    call fugitive#MapJumps()
    let nowait = v:version >= 704 ? '<nowait>' : ''
    nunmap   <buffer>          P
    nunmap   <buffer>          ~
    nnoremap <buffer> <silent> <C-N> :<C-U>execute <SID>StageNext(v:count1)<CR>
    nnoremap <buffer> <silent> <C-P> :<C-U>execute <SID>StagePrevious(v:count1)<CR>
    exe "nnoremap <buffer> <silent>" nowait "- :<C-U>silent execute <SID>StageToggle(line('.'),line('.')+v:count1-1)<CR>"
    exe "xnoremap <buffer> <silent>" nowait "- :<C-U>silent execute <SID>StageToggle(line(\"'<\"),line(\"'>\"))<CR>"
    nnoremap <buffer> <silent> a :<C-U>let b:fugitive_display_format += 1<Bar>exe fugitive#BufReadStatus()<CR>
    nnoremap <buffer> <silent> i :<C-U>let b:fugitive_display_format -= 1<Bar>exe fugitive#BufReadStatus()<CR>
    nnoremap <buffer> <silent> C :<C-U>Gcommit<CR>:echohl WarningMsg<Bar>echo ':Gstatus C is deprecated in favor of cc'<Bar>echohl NONE<CR>
    nnoremap <buffer> <silent> cA :<C-U>Gcommit --amend --reuse-message=HEAD<CR>:echohl WarningMsg<Bar>echo ':Gstatus cA is deprecated in favor of ce'<Bar>echohl NONE<CR>
    nnoremap <buffer> <silent> ca :<C-U>Gcommit --amend<CR>
    nnoremap <buffer> <silent> cc :<C-U>Gcommit<CR>
    nnoremap <buffer> <silent> ce :<C-U>Gcommit --amend --no-edit<CR>
    nnoremap <buffer> <silent> cw :<C-U>Gcommit --amend --only<CR>
    nnoremap <buffer> <silent> cva :<C-U>Gcommit -v --amend<CR>
    nnoremap <buffer> <silent> cvc :<C-U>Gcommit -v<CR>
    nnoremap <buffer> <silent> D :<C-U>execute <SID>StageDiff('Gdiff')<CR>
    nnoremap <buffer> <silent> dd :<C-U>execute <SID>StageDiff('Gdiff')<CR>
    nnoremap <buffer> <silent> dh :<C-U>execute <SID>StageDiff('Gsdiff')<CR>
    nnoremap <buffer> <silent> ds :<C-U>execute <SID>StageDiff('Gsdiff')<CR>
    nnoremap <buffer> <silent> dp :<C-U>execute <SID>StageDiffEdit()<CR>
    nnoremap <buffer> <silent> dv :<C-U>execute <SID>StageDiff('Gvdiff')<CR>
    nnoremap <buffer> <silent> p :<C-U>execute <SID>StagePatch(line('.'),line('.')+v:count1-1)<CR>
    xnoremap <buffer> <silent> p :<C-U>execute <SID>StagePatch(line("'<"),line("'>"))<CR>
    nnoremap <buffer> <silent> P :<C-U>execute <SID>StagePatch(line('.'),line('.')+v:count1-1)<CR>
    xnoremap <buffer> <silent> P :<C-U>execute <SID>StagePatch(line("'<"),line("'>"))<CR>
    nnoremap <buffer> <silent> q :<C-U>if bufnr('$') == 1<Bar>quit<Bar>else<Bar>bdelete<Bar>endif<CR>
    nnoremap <buffer> <silent> r :<C-U>edit<CR>
    nnoremap <buffer> <silent> R :<C-U>edit<CR>
    nnoremap <buffer> <silent> U :<C-U>execute <SID>StageUndo()<CR>
    nnoremap <buffer>          . : <C-R>=<SID>fnameescape(<SID>StatusCfile())<CR><Home>
    nnoremap <buffer> <silent> g?   :help fugitive-:Gstatus<CR>
    nnoremap <buffer> <silent> <F1> :help fugitive-:Gstatus<CR>
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
endfunction

function! fugitive#FileReadCmd(...) abort
  let amatch = a:0 ? a:1 : expand('<amatch>')
  let [dir, rev] = s:DirRev(amatch)
  let line = a:0 > 1 ? a:2 : line("'[")
  if empty(dir)
    return 'noautocmd ' . line . 'read ' . s:fnameescape(amatch)
  endif
  if rev !~# ':'
    let cmd = s:Prepare(dir, 'log', '--pretty=format:%B', '-1', rev, '--')
  else
    let cmd = s:Prepare(dir, 'cat-file', '-p', rev, '--')
  endif
  return line . 'read !' . escape(cmd, '!#%')
endfunction

function! fugitive#FileWriteCmd(...) abort
  let tmp = tempname()
  let amatch = a:0 ? a:1 : expand('<amatch>')
  let autype = a:0 > 1 ? 'Buf' : 'File'
  if exists('#' . autype . 'WritePre')
    execute 'doautocmd ' . autype . 'WritePre ' . s:fnameescape(amatch)
  endif
  try
    let [dir, commit, file] = s:DirCommitFile(amatch)
    if commit !~# '^[0-3]$' || !v:cmdbang && (line("'[") != 1 || line("']") != line('$'))
      return "noautocmd '[,']write" . (v:cmdbang ? '!' : '') . ' ' . s:fnameescape(amatch)
    endif
    silent execute "'[,']write !".s:Prepare(dir, 'hash-object', '-w', '--stdin', '--').' > '.tmp
    let sha1 = readfile(tmp)[0]
    let old_mode = matchstr(system(s:Prepare(dir, 'ls-files', '--stage', '--', '.' . file)), '^\d\+')
    if empty(old_mode)
      let old_mode = executable(s:Tree(dir) . file) ? '100755' : '100644'
    endif
    let info = old_mode.' '.sha1.' '.commit."\t".file[1:-1]
    let error = system(s:Prepare(dir, 'update-index', '--index-info'), info . "\n")
    if v:shell_error == 0
      setlocal nomodified
      if exists('#' . autype . 'WritePost')
        execute 'doautocmd ' . autype . 'WritePost ' . s:fnameescape(amatch)
      endif
      call fugitive#ReloadStatus()
      return ''
    else
      return 'echoerr '.string('fugitive: '.error)
    endif
  finally
    call delete(tmp)
  endtry
endfunction

function! fugitive#BufReadCmd(...) abort
  let amatch = a:0 ? a:1 : expand('<amatch>')
  try
    let [dir, rev] = s:DirRev(amatch)
    if empty(dir)
      return 'echo "Invalid Fugitive URL"'
    endif
    if rev =~# '^:\d$'
      let b:fugitive_type = 'stage'
    else
      let b:fugitive_type = system(s:Prepare(dir, 'cat-file', '-t', rev))[0:-2]
      if v:shell_error && rev =~# '^:0'
        let sha = system(s:Prepare(dir, 'write-tree', '--prefix=' . rev[3:-1]))[0:-2]
        let b:fugitive_type = 'tree'
      endif
      if v:shell_error
        unlet b:fugitive_type
        if rev =~# '^:\d:'
          let &readonly = !filewritable(dir . '/index')
          return 'silent doautocmd BufNewFile '.s:fnameescape(amatch)
        else
          setlocal readonly nomodifiable
          return ''
        endif
      elseif b:fugitive_type !~# '^\%(tag\|commit\|tree\|blob\)$'
        return "echoerr ".string("fugitive: unrecognized git type '".b:fugitive_type."'")
      endif
      if !exists('b:fugitive_display_format') && b:fugitive_type != 'blob'
        let b:fugitive_display_format = +getbufvar('#','fugitive_display_format')
      endif
    endif

    if b:fugitive_type !=# 'blob'
      setlocal nomodeline
    endif

    setlocal noreadonly modifiable
    let pos = getpos('.')
    silent keepjumps %delete_
    setlocal endofline

    try
      if b:fugitive_type ==# 'tree'
        let b:fugitive_display_format = b:fugitive_display_format % 2
        if b:fugitive_display_format
          call s:ReplaceCmd([dir, 'ls-tree', exists('sha') ? sha : rev])
        else
          if !exists('sha')
            let sha = system(s:Prepare(dir, 'rev-parse', '--verify', rev))[0:-2]
          endif
          call s:ReplaceCmd([dir, 'show', '--no-color', sha])
        endif
      elseif b:fugitive_type ==# 'tag'
        let b:fugitive_display_format = b:fugitive_display_format % 2
        if b:fugitive_display_format
          call s:ReplaceCmd([dir, 'cat-file', b:fugitive_type, rev])
        else
          call s:ReplaceCmd([dir, 'cat-file', '-p', rev])
        endif
      elseif b:fugitive_type ==# 'commit'
        let b:fugitive_display_format = b:fugitive_display_format % 2
        if b:fugitive_display_format
          call s:ReplaceCmd([dir, 'cat-file', b:fugitive_type, rev])
        else
          call s:ReplaceCmd([dir, 'show', '--no-color', '--pretty=format:tree%x20%T%nparent%x20%P%nauthor%x20%an%x20<%ae>%x20%ad%ncommitter%x20%cn%x20<%ce>%x20%cd%nencoding%x20%e%n%n%s%n%n%b', rev])
          keepjumps call search('^parent ')
          if getline('.') ==# 'parent '
            silent keepjumps delete_
          else
            silent exe 'keepjumps s/\m\C\%(^parent\)\@<! /\rparent /e' . (&gdefault ? '' : 'g')
          endif
          keepjumps let lnum = search('^encoding \%(<unknown>\)\=$','W',line('.')+3)
          if lnum
            silent keepjumps delete_
          end
          silent keepjumps 1,/^diff --git\|\%$/g/\r$/s///
          keepjumps 1
        endif
      elseif b:fugitive_type ==# 'stage'
        call s:ReplaceCmd([dir, 'ls-files', '--stage'])
      elseif b:fugitive_type ==# 'blob'
        call s:ReplaceCmd([dir, 'cat-file', b:fugitive_type, rev])
        setlocal nomodeline
      endif
    finally
      keepjumps call setpos('.',pos)
      setlocal nomodified noswapfile
      if rev !~# '^:.:'
        setlocal nomodifiable
      else
        let &modifiable = b:fugitive_type !=# 'tree'
      endif
      let &readonly = !&modifiable || !filewritable(dir . '/index')
      if &bufhidden ==# ''
        setlocal bufhidden=delete
      endif
      if b:fugitive_type !=# 'blob'
        setlocal filetype=git foldmethod=syntax
        nnoremap <buffer> <silent> a :<C-U>let b:fugitive_display_format += v:count1<Bar>exe fugitive#BufReadCmd(@%)<CR>
        nnoremap <buffer> <silent> i :<C-U>let b:fugitive_display_format -= v:count1<Bar>exe fugitive#BufReadCmd(@%)<CR>
      else
        call fugitive#MapJumps()
      endif
    endtry

    return ''
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
endfunction

function! fugitive#BufWriteCmd(...) abort
  return fugitive#FileWriteCmd(a:0 ? a:1 : expand('<amatch>'), 1)
endfunction

function! fugitive#SourceCmd(...) abort
  let amatch = a:0 ? a:1 : expand('<amatch>')
  let temp = s:BlobTemp(amatch)
  if empty(temp)
    return 'noautocmd source ' . s:fnameescape(amatch)
  endif
  if !exists('g:virtual_scriptnames')
    let g:virtual_scriptnames = {}
  endif
  let g:virtual_scriptnames[temp] = amatch
  return 'source ' . s:fnameescape(temp)
endfunction

" Section: Temp files

if !exists('s:temp_files')
  let s:temp_files = {}
endif

function! s:SetupTemp(file) abort
  if has_key(s:temp_files, s:cpath(a:file))
    let dict = s:temp_files[s:cpath(a:file)]
    let b:git_dir = dict.dir
    call extend(b:, {'fugitive_type': 'temp'}, 'keep')
    if has_key(dict, 'filetype') && dict.filetype !=# &l:filetype
      let &l:filetype = dict.filetype
    endif
    setlocal foldmarker=<<<<<<<,>>>>>>>
    setlocal bufhidden=delete nobuflisted
    setlocal buftype=nowrite
    nnoremap <buffer> <silent> q    :<C-U>bdelete<CR>
    if getline(1) !~# '^diff '
      setlocal nomodifiable
    endif
    call FugitiveDetect(a:file)
  endif
  return ''
endfunction

augroup fugitive_temp
  autocmd!
  autocmd BufNewFile,BufReadPost * exe s:SetupTemp(expand('<amatch>:p'))
augroup END

" Section: :Git

call s:command("-bang -nargs=? -complete=customlist,s:GitComplete Git :execute s:Git(<bang>0,'<mods>',<q-args>)")

function! s:Git(bang, mods, args) abort
  if a:bang
    return s:Edit('edit', 1, a:mods, a:args)
  endif
  let git = s:UserCommand()
  if has('gui_running') && !has('win32')
    let git .= ' --no-pager'
  endif
  let args = matchstr(a:args,'\v\C.{-}%($|\\@<!%(\\\\)*\|)@=')
  let after = matchstr(a:args, '\v\C\\@<!%(\\\\)*\zs\|.*')
  let tree = s:Tree()
  if has('win32')
    let after = '|call fugitive#ReloadStatus()' . after
  endif
  if exists(':terminal') && has('nvim') && !get(g:, 'fugitive_force_bang_command')
    if len(@%)
      -tabedit %
    else
      -tabnew
    endif
    execute 'lcd' fnameescape(tree)
    let exec = escape(git . ' ' . s:ShellExpand(args), '#%')
    return 'exe ' . string('terminal ' . exec) . after
  else
    let cmd = "exe '!'.escape(" . string(git) . " . ' ' . s:ShellExpand(" . string(args) . "),'!#%')"
    if s:cpath(tree) !=# s:cpath(getcwd())
      let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
      let cmd = 'try|' . cd . ' ' . tree . '|' . cmd . '|finally|' . cd . ' ' . s:fnameescape(getcwd()) . '|endtry'
    endif
    return cmd . after
  endif
endfunction

let s:exec_paths = {}
function! s:Subcommands() abort
  if !has_key(s:exec_paths, g:fugitive_git_executable)
    let s:exec_paths[g:fugitive_git_executable] = s:sub(system(g:fugitive_git_executable.' --exec-path'),'\n$','')
  endif
  let exec_path = s:exec_paths[g:fugitive_git_executable]
  return map(split(glob(exec_path.'/git-*'),"\n"),'s:sub(v:val[strlen(exec_path)+5 : -1],"\\.exe$","")')
endfunction

let s:aliases = {}
function! s:Aliases() abort
  if !has_key(s:aliases, b:git_dir)
    let s:aliases[b:git_dir] = {}
    let lines = split(s:TreeChomp('config','-z','--get-regexp','^alias[.]'),"\1")
    for line in v:shell_error ? [] : lines
      let s:aliases[b:git_dir][matchstr(line, '\.\zs.\{-}\ze\n')] = matchstr(line, '\n\zs.*')
    endfor
  endif
  return s:aliases[b:git_dir]
endfunction

function! s:GitComplete(A, L, P) abort
  let pre = strpart(a:L, 0, a:P)
  if pre !~# ' [[:alnum:]-]\+ '
    let cmds = s:Subcommands()
    return filter(sort(cmds+keys(s:Aliases())), 'strpart(v:val, 0, strlen(a:A)) ==# a:A')
  elseif pre =~# ' -- '
    return fugitive#PathComplete(a:A, b:git_dir)
  else
    return fugitive#Complete(a:A, b:git_dir)
  endif
endfunction

" Section: :Gcd, :Glcd

function! s:DirComplete(A, L, P) abort
  return filter(fugitive#PathComplete(a:A), 'v:val =~# "/$"')
endfunction

function! s:DirArg(path) abort
  let path = substitute(a:path, '^:/:\=\|^:(\%(top\|top,literal\|literal,top\|literal\))', '', '')
  if path =~# '^/\|^\a\+:\|^\.\.\=\%(/\|$\)'
    return path
  else
    return (empty(s:Tree()) ? b:git_dir : s:Tree()) . '/' . path
  endif
endfunction

call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Gcd  :exe 'cd<bang>'  s:fnameescape(s:DirArg(<q-args>))")
call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Glcd :exe 'lcd<bang>' s:fnameescape(s:DirArg(<q-args>))")

" Section: :Gstatus

call s:command("-bar -bang -range=-1 Gstatus :execute s:Status(<bang>0, <count>, '<mods>')")
augroup fugitive_status
  autocmd!
  if !has('win32')
    autocmd FocusGained,ShellCmdPost * call fugitive#ReloadStatus()
    autocmd BufDelete term://* call fugitive#ReloadStatus()
  endif
augroup END

function! s:Status(bang, count, mods) abort
  try
    exe (a:mods ==# '<mods>' ? '' : a:mods) 'Gpedit :'
    wincmd P
    setlocal foldmethod=syntax foldlevel=1 buftype=nowrite
    nnoremap <buffer> <silent> q    :<C-U>bdelete<CR>
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
  return ''
endfunction

function! fugitive#ReloadStatus() abort
  if exists('s:reloading_status')
    return
  endif
  try
    let s:reloading_status = 1
    let mytab = tabpagenr()
    for tab in [mytab] + range(1,tabpagenr('$'))
      for winnr in range(1,tabpagewinnr(tab,'$'))
        if getbufvar(tabpagebuflist(tab)[winnr-1],'fugitive_type') ==# 'index'
          execute 'tabnext '.tab
          if winnr != winnr()
            execute winnr.'wincmd w'
            let restorewinnr = 1
          endif
          try
            if !&modified
              call fugitive#BufReadStatus()
            endif
          finally
            if exists('restorewinnr')
              wincmd p
            endif
            execute 'tabnext '.mytab
          endtry
        endif
      endfor
    endfor
  finally
    unlet! s:reloading_status
  endtry
endfunction

function! fugitive#reload_status() abort
  return fugitive#ReloadStatus()
endfunction

function! s:stage_info(lnum) abort
  let filename = matchstr(getline(a:lnum),'^.\=\t\zs.\{-\}\ze\%( ([^()[:digit:]]\+)\)\=$')
  let lnum = a:lnum
  if has('multi_byte_encoding')
    let colon = '\%(:\|\%uff1a\)'
  else
    let colon = ':'
  endif
  while lnum && getline(lnum) !~# colon.'$'
    let lnum -= 1
  endwhile
  if !lnum
    return ['', '']
  elseif (getline(lnum+1) =~# '^.\= .*\<git \%(reset\|rm --cached\) ' && getline(lnum+2) ==# '#') || getline(lnum) =~# '^\%(. \)\=Changes to be committed:$'
    return [matchstr(filename, colon.' *\zs.*'), 'staged']
  elseif (getline(lnum+1) =~# '^.\= .*\<git add ' && getline(lnum+2) ==# '#' && getline(lnum+3) !~# colon.'  ') || getline(lnum) =~# '^\(. \)\=Untracked files:$'
    return [filename, 'untracked']
  elseif getline(lnum+2) =~# '^.\= .*\<git checkout ' || getline(lnum) =~# '\%(. \)\=Changes not staged for commit:$'
    return [matchstr(filename, colon.' *\zs.*'), 'unstaged']
  elseif getline(lnum+2) =~# '^.\= .*\<git \%(add\|rm\)' || getline(lnum) =~# '\%(. \)\=Unmerged paths:$'
    return [matchstr(filename, colon.' *\zs.*'), 'unmerged']
  else
    return ['', 'unknown']
  endif
endfunction

function! s:StageNext(count) abort
  for i in range(a:count)
    call search('^.\=\t.*','W')
  endfor
  return '.'
endfunction

function! s:StagePrevious(count) abort
  if line('.') == 1 && exists(':CtrlP') && get(g:, 'ctrl_p_map') =~? '^<c-p>$'
    return 'CtrlP '.fnameescape(s:Tree())
  else
    for i in range(a:count)
      call search('^.\=\t.*','Wbe')
    endfor
    return '.'
  endif
endfunction

function! s:StageReloadSeek(target,lnum1,lnum2) abort
  let jump = a:target
  let f = matchstr(getline(a:lnum1-1),'^.\=\t\%([[:alpha:] ]\+: *\|.*\%uff1a *\)\=\zs.*')
  if f !=# '' | let jump = f | endif
  let f = matchstr(getline(a:lnum2+1),'^.\=\t\%([[:alpha:] ]\+: *\|.*\%uff1a *\)\=\zs.*')
  if f !=# '' | let jump = f | endif
  silent! edit!
  1
  redraw
  call search('^.\=\t\%([[:alpha:] ]\+: *\|.*\%uff1a *\)\=\V'.jump.'\%( ([^()[:digit:]]\+)\)\=\$','W')
endfunction

function! s:StageUndo() abort
  let [filename, section] = s:stage_info(line('.'))
  if empty(filename)
    return ''
  endif
  let hash = s:TreeChomp('hash-object', '-w', './' . filename)
  if !empty(hash)
    if section ==# 'untracked'
      call s:TreeChomp('clean', '-f', './' . filename)
    elseif section ==# 'unmerged'
      call s:TreeChomp('rm', './' . filename)
    elseif section ==# 'unstaged'
      call s:TreeChomp('checkout', './' . filename)
    else
      call s:TreeChomp('checkout', 'HEAD^{}', './' . filename)
    endif
    call s:StageReloadSeek(filename, line('.'), line('.'))
    let @" = hash
    return 'checktime|redraw|echomsg ' .
          \ string('To restore, :Git cat-file blob '.hash[0:6].' > '.filename)
  endif
endfunction

function! s:StageDiff(diff) abort
  let [filename, section] = s:stage_info(line('.'))
  if filename ==# '' && section ==# 'staged'
    return 'Git! diff --no-ext-diff --cached'
  elseif filename ==# ''
    return 'Git! diff --no-ext-diff'
  elseif filename =~# ' -> '
    let [old, new] = split(filename,' -> ')
    execute 'Gedit '.s:fnameescape(':0:'.new)
    return a:diff.' HEAD:'.s:fnameescape(old)
  elseif section ==# 'staged'
    execute 'Gedit '.s:fnameescape(':0:'.filename)
    return a:diff.' -'
  else
    execute 'Gedit '.s:fnameescape('/'.filename)
    return a:diff
  endif
endfunction

function! s:StageDiffEdit() abort
  let [filename, section] = s:stage_info(line('.'))
  let arg = (filename ==# '' ? '.' : filename)
  if section ==# 'staged'
    return 'Git! diff --no-ext-diff --cached '.s:shellesc(arg)
  elseif section ==# 'untracked'
    call s:TreeChomp('add', '--intent-to-add', './' . arg)
    if arg ==# '.'
      silent! edit!
      1
      if !search('^.*:\n.*\n.\= .*"git checkout \|^\%(# \)=Changes not staged for commit:$','W')
        call search(':$','W')
      endif
    else
      call s:StageReloadSeek(arg,line('.'),line('.'))
    endif
    return ''
  else
    return 'Git! diff --no-ext-diff '.s:shellesc(arg)
  endif
endfunction

function! s:StageToggle(lnum1,lnum2) abort
  if a:lnum1 == 1 && a:lnum2 == 1
    return 'Gedit /.git|call search("^index$", "wc")'
  endif
  try
    let output = ''
    for lnum in range(a:lnum1,a:lnum2)
      let [filename, section] = s:stage_info(lnum)
      if getline('.') =~# ':$'
        if section ==# 'staged'
          call s:TreeChomp('reset','-q')
          silent! edit!
          1
          if !search('^.*:\n.\= .*"git add .*\n#\n\|^\%(. \)\=Untracked files:$','W')
            call search(':$','W')
          endif
          return ''
        elseif section ==# 'unstaged'
          call s:TreeChomp('add','-u')
          silent! edit!
          1
          if !search('^.*:\n\.\= .*"git add .*\n#\n\|^\%( \)=Untracked files:$','W')
            call search(':$','W')
          endif
          return ''
        else
          call s:TreeChomp('add', '.')
          silent! edit!
          1
          call search(':$','W')
          return ''
        endif
      endif
      if filename ==# ''
        continue
      endif
      execute lnum
      if section ==# 'staged'
        if filename =~ ' -> '
          let files_to_unstage = split(filename,' -> ')
        else
          let files_to_unstage = [filename]
        endif
        let filename = files_to_unstage[-1]
        let cmd = ['reset', '-q'] + map(copy(files_to_unstage), '"./" . v:val')
      elseif getline(lnum) =~# '^.\=\tdeleted:'
        let cmd = ['rm', './' . filename]
      elseif getline(lnum) =~# '^.\=\tmodified:'
        let cmd = ['add', './' . filename]
      else
        let cmd = ['add','-A', './' . filename]
      endif
      if !exists('first_filename')
        let first_filename = filename
      endif
      let output .= call('s:TreeChomp', cmd)."\n"
    endfor
    if exists('first_filename')
      call s:StageReloadSeek(first_filename,a:lnum1,a:lnum2)
    endif
    echo s:sub(s:gsub(output,'\n+','\n'),'\n$','')
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
  return 'checktime'
endfunction

function! s:StagePatch(lnum1,lnum2) abort
  let add = []
  let reset = []

  for lnum in range(a:lnum1,a:lnum2)
    let [filename, section] = s:stage_info(lnum)
    if getline('.') =~# ':$' && section ==# 'staged'
      return 'Git reset --patch'
    elseif getline('.') =~# ':$' && section ==# 'unstaged'
      return 'Git add --patch'
    elseif getline('.') =~# ':$' && section ==# 'untracked'
      return 'Git add -N .'
    elseif filename ==# ''
      continue
    endif
    if !exists('first_filename')
      let first_filename = filename
    endif
    execute lnum
    if filename =~ ' -> '
      let reset += [split(filename,' -> ')[1]]
    elseif section ==# 'staged'
      let reset += [filename]
    elseif getline(lnum) !~# '^.\=\tdeleted:'
      let add += [filename]
    endif
  endfor
  try
    if !empty(add)
      execute "Git add --patch -- ".join(map(add,'s:shellesc(v:val)'))
    endif
    if !empty(reset)
      execute "Git reset --patch -- ".join(map(reset,'s:shellesc(v:val)'))
    endif
    if exists('first_filename')
      silent! edit!
      1
      redraw
      call search('^.\=\t\%([[:alpha:] ]\+: *\)\=\V'.first_filename.'\%( ([^()[:digit:]]\+)\)\=\$','W')
    endif
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
  return 'checktime'
endfunction

" Section: :Gcommit

call s:command("-nargs=? -complete=customlist,s:CommitComplete Gcommit :execute s:Commit('<mods>', <q-args>)")

function! s:Commit(mods, args, ...) abort
  let mods = s:gsub(a:mods ==# '<mods>' ? '' : a:mods, '<tab>', '-tab')
  let dir = a:0 ? a:1 : b:git_dir
  let tree = s:Tree(dir)
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  let msgfile = dir . '/COMMIT_EDITMSG'
  let outfile = tempname()
  let errorfile = tempname()
  try
    let guioptions = &guioptions
    try
      if &guioptions =~# '!'
        setglobal guioptions-=!
      endif
      execute cd s:fnameescape(tree)
      if s:winshell()
        let command = ''
        let old_editor = $GIT_EDITOR
        let $GIT_EDITOR = 'false'
      else
        let command = 'env GIT_EDITOR=false '
      endif
      let args = s:ShellExpand(a:args)
      let command .= s:UserCommand() . ' commit ' . args
      if &shell =~# 'csh'
        noautocmd silent execute '!('.escape(command, '!#%').' > '.outfile.') >& '.errorfile
      elseif a:args =~# '\%(^\| \)-\%(-interactive\|p\|-patch\)\>'
        noautocmd execute '!'.command.' 2> '.errorfile
      else
        noautocmd silent execute '!'.command.' > '.outfile.' 2> '.errorfile
      endif
      let error = v:shell_error
    finally
      execute cd s:fnameescape(cwd)
      let &guioptions = guioptions
    endtry
    if !has('gui_running')
      redraw!
    endif
    if !error
      if filereadable(outfile)
        for line in readfile(outfile)
          echo line
        endfor
      endif
      return ''
    else
      let errors = readfile(errorfile)
      let error = get(errors,-2,get(errors,-1,'!'))
      if error =~# 'false''\=\.$'
        let args = s:gsub(args,'%(%(^| )-- )@<!%(^| )@<=%(-[esp]|--edit|--interactive|--patch|--signoff)%($| )','')
        let args = s:gsub(args,'%(%(^| )-- )@<!%(^| )@<=%(-c|--reedit-message|--reuse-message|-F|--file|-m|--message)%(\s+|\=)%(''[^'']*''|"%(\\.|[^"])*"|\\.|\S)*','')
        let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
        let cwd = getcwd()
        let args = s:sub(args, '\ze -- |$', ' --no-edit --no-interactive --no-signoff')
        let args = '-F '.s:shellesc(msgfile).' '.args
        if args !~# '\%(^\| \)--cleanup\>'
          let args = '--cleanup=strip '.args
        endif
        if bufname('%') == '' && line('$') == 1 && getline(1) == '' && !&mod
          execute mods 'keepalt edit' s:fnameescape(msgfile)
        elseif a:args =~# '\%(^\| \)-\w*v' || mods =~# '\<tab\>'
          execute mods 'keepalt -tabedit' s:fnameescape(msgfile)
        elseif get(b:, 'fugitive_type', '') ==# 'index'
          execute mods 'keepalt edit' s:fnameescape(msgfile)
          execute (search('^#','n')+1).'wincmd+'
          setlocal nopreviewwindow
        else
          execute mods 'keepalt split' s:fnameescape(msgfile)
        endif
        let b:fugitive_commit_arguments = args
        setlocal bufhidden=wipe filetype=gitcommit
        return '1'
      elseif error ==# '!'
        return 'Gstatus'
      else
        call s:throw(empty(error)?join(errors, ' '):error)
      endif
    endif
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  finally
    if exists('old_editor')
      let $GIT_EDITOR = old_editor
    endif
    call delete(outfile)
    call delete(errorfile)
    call fugitive#ReloadStatus()
  endtry
endfunction

function! s:CommitComplete(A,L,P) abort
  if a:A =~# '^--fixup=\|^--squash='
    let commits = split(s:TreeChomp('log', '--pretty=format:%s', '@{upstream}..'), "\n")
    if !v:shell_error
      let pre = matchstr(a:A, '^--\w*=') . ':/^'
      return map(commits, 'pre . tr(v:val, "\\ !^$*?[]()''\"`&;<>|#", "....................")')
    endif
  elseif a:A =~ '^-' || type(a:A) == type(0) " a:A is 0 on :Gcommit -<Tab>
    let args = ['-C', '-F', '-a', '-c', '-e', '-i', '-m', '-n', '-o', '-q', '-s', '-t', '-u', '-v', '--all', '--allow-empty', '--amend', '--author=', '--cleanup=', '--dry-run', '--edit', '--file=', '--fixup=', '--include', '--interactive', '--message=', '--no-verify', '--only', '--quiet', '--reedit-message=', '--reuse-message=', '--signoff', '--squash=', '--template=', '--untracked-files', '--verbose']
    return filter(args,'v:val[0 : strlen(a:A)-1] ==# a:A')
  else
    return fugitive#PathComplete(a:A, b:git_dir)
  endif
  return []
endfunction

function! s:FinishCommit() abort
  let args = getbufvar(+expand('<abuf>'),'fugitive_commit_arguments')
  if !empty(args)
    call setbufvar(+expand('<abuf>'),'fugitive_commit_arguments','')
    return s:Commit('', args, getbufvar(+expand('<abuf>'),'git_dir'))
  endif
  return ''
endfunction

" Section: :Gmerge, :Grebase, :Gpull

call s:command("-nargs=? -bang -complete=custom,s:RevisionComplete Gmerge " .
      \ "execute s:Merge('merge', <bang>0, <q-args>)")
call s:command("-nargs=? -bang -complete=custom,s:RevisionComplete Grebase " .
      \ "execute s:Merge('rebase', <bang>0, <q-args>)")
call s:command("-nargs=? -bang -complete=custom,s:RemoteComplete Gpull " .
      \ "execute s:Merge('pull --progress', <bang>0, <q-args>)")

function! s:RevisionComplete(A, L, P) abort
  return s:TreeChomp('rev-parse', '--symbolic', '--branches', '--tags', '--remotes')
        \ . "\nHEAD\nFETCH_HEAD\nMERGE_HEAD\nORIG_HEAD"
endfunction

function! s:RemoteComplete(A, L, P) abort
  let remote = matchstr(a:L, ' \zs\S\+\ze ')
  if !empty(remote)
    let matches = split(s:TreeChomp('ls-remote', remote), "\n")
    call filter(matches, 'v:val =~# "\t" && v:val !~# "{"')
    call map(matches, 's:sub(v:val, "^.*\t%(refs/%(heads/|tags/)=)=", "")')
  else
    let matches = split(s:TreeChomp('remote'), "\n")
  endif
  return join(matches, "\n")
endfunction

function! fugitive#Cwindow() abort
  if &buftype == 'quickfix'
    cwindow
  else
    botright cwindow
    if &buftype == 'quickfix'
      wincmd p
    endif
  endif
endfunction

let s:common_efm = ''
      \ . '%+Egit:%.%#,'
      \ . '%+Eusage:%.%#,'
      \ . '%+Eerror:%.%#,'
      \ . '%+Efatal:%.%#,'
      \ . '%-G%.%#%\e[K%.%#,'
      \ . '%-G%.%#%\r%.%\+'

function! s:Merge(cmd, bang, args) abort
  if a:cmd =~# '^rebase' && ' '.a:args =~# ' -i\| --interactive\| --edit-todo'
    return 'echoerr "git rebase --interactive not supported"'
  endif
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  let [mp, efm] = [&l:mp, &l:efm]
  let had_merge_msg = filereadable(b:git_dir . '/MERGE_MSG')
  try
    let &l:errorformat = ''
          \ . '%-Gerror:%.%#false''.,'
          \ . '%-G%.%# ''git commit'' %.%#,'
          \ . '%+Emerge:%.%#,'
          \ . s:common_efm . ','
          \ . '%+ECannot %.%#: You have unstaged changes.,'
          \ . '%+ECannot %.%#: Your index contains uncommitted changes.,'
          \ . '%+EThere is no tracking information for the current branch.,'
          \ . '%+EYou are not currently on a branch. Please specify which,'
          \ . 'CONFLICT (%m): %f deleted in %.%#,'
          \ . 'CONFLICT (%m): Merge conflict in %f,'
          \ . 'CONFLICT (%m): Rename \"%f\"->%.%#,'
          \ . 'CONFLICT (%m): Rename %.%#->%f %.%#,'
          \ . 'CONFLICT (%m): There is a directory with name %f in %.%#,'
          \ . '%+ECONFLICT %.%#,'
          \ . '%+EKONFLIKT %.%#,'
          \ . '%+ECONFLIT %.%#,'
          \ . "%+EXUNG \u0110\u1ed8T %.%#,"
          \ . "%+E\u51b2\u7a81 %.%#,"
          \ . 'U%\t%f'
    if a:cmd =~# '^merge' && empty(a:args) &&
          \ (had_merge_msg || isdirectory(b:git_dir . '/rebase-apply') ||
          \  !empty(s:TreeChomp('diff-files', '--diff-filter=U')))
      let &l:makeprg = g:fugitive_git_executable.' diff-files --name-status --diff-filter=U'
    else
      let &l:makeprg = s:sub(s:UserCommand() . ' ' . a:cmd .
            \ (a:args =~# ' \%(--no-edit\|--abort\|-m\)\>' || a:cmd =~# '^rebase' ? '' : ' --edit') .
            \ ' ' . a:args, ' *$', '')
    endif
    if !empty($GIT_EDITOR) || has('win32')
      let old_editor = $GIT_EDITOR
      let $GIT_EDITOR = 'false'
    else
      let &l:makeprg = 'env GIT_EDITOR=false ' . &l:makeprg
    endif
    execute cd fnameescape(s:Tree())
    silent noautocmd make!
  catch /^Vim\%((\a\+)\)\=:E211/
    let err = v:exception
  finally
    redraw!
    let [&l:mp, &l:efm] = [mp, efm]
    if exists('old_editor')
      let $GIT_EDITOR = old_editor
    endif
    execute cd fnameescape(cwd)
  endtry
  call fugitive#ReloadStatus()
  if empty(filter(getqflist(),'v:val.valid'))
    if !had_merge_msg && filereadable(b:git_dir . '/MERGE_MSG')
      cclose
      return 'Gcommit --no-status -n -t '.s:shellesc(b:git_dir . '/MERGE_MSG')
    endif
  endif
  let qflist = getqflist()
  let found = 0
  for e in qflist
    if !empty(e.bufnr)
      let found = 1
      let e.pattern = '^<<<<<<<'
    endif
  endfor
  call fugitive#Cwindow()
  if found
    call setqflist(qflist, 'r')
    if !a:bang
      return 'cfirst'
    endif
  endif
  return exists('err') ? 'echoerr '.string(err) : ''
endfunction

" Section: :Ggrep, :Glog

if !exists('g:fugitive_summary_format')
  let g:fugitive_summary_format = '%s'
endif

function! s:GrepComplete(A, L, P) abort
  if strpart(a:L, 0, a:P) =~# ' -- '
    return fugitive#PathComplete(a:A, b:git_dir)
  else
    return fugitive#Complete(a:A, b:git_dir)
  endif
endfunction

call s:command("-bang -nargs=? -complete=customlist,s:GrepComplete Ggrep :execute s:Grep('grep',<bang>0,<q-args>)")
call s:command("-bang -nargs=? -complete=customlist,s:GrepComplete Glgrep :execute s:Grep('lgrep',<bang>0,<q-args>)")
call s:command("-bar -bang -nargs=* -range=-1 -complete=customlist,s:GrepComplete Glog :call s:Log('grep',<bang>0,<line1>,<count>,<q-args>)")
call s:command("-bar -bang -nargs=* -range=-1 -complete=customlist,s:GrepComplete Gllog :call s:Log('lgrep',<bang>0,<line1>,<count>,<q-args>)")

function! s:Grep(cmd,bang,arg) abort
  let grepprg = &grepprg
  let grepformat = &grepformat
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let dir = getcwd()
  try
    execute cd s:fnameescape(s:Tree())
    let &grepprg = s:UserCommand() . ' --no-pager grep -n --no-color'
    let &grepformat = '%f:%l:%m,%m %f match%ts,%f'
    exe a:cmd.'! '.escape(s:ShellExpand(matchstr(a:arg, '\v\C.{-}%($|[''" ]\@=\|)@=')), '|#%')
    let list = a:cmd =~# '^l' ? getloclist(0) : getqflist()
    for entry in list
      if bufname(entry.bufnr) =~ ':'
        let entry.filename = s:Generate(bufname(entry.bufnr))
        unlet! entry.bufnr
        let changed = 1
      elseif a:arg =~# '\%(^\| \)--cached\>'
        let entry.filename = s:Generate(':0:'.bufname(entry.bufnr))
        unlet! entry.bufnr
        let changed = 1
      endif
    endfor
    if a:cmd =~# '^l' && exists('changed')
      call setloclist(0, list, 'r')
    elseif exists('changed')
      call setqflist(list, 'r')
    endif
    if !a:bang && !empty(list)
      return (a:cmd =~# '^l' ? 'l' : 'c').'first'.matchstr(a:arg,'\v\C[''" ]\zs\|.*')
    else
      return matchstr(a:arg,'\v\C[''" ]\|\zs.*')
    endif
  finally
    let &grepprg = grepprg
    let &grepformat = grepformat
    execute cd s:fnameescape(dir)
  endtry
endfunction

function! s:Log(cmd, bang, line1, line2, ...) abort
  let args = ' ' . join(a:000, ' ')
  let before = substitute(args, ' --\S\@!.*', '', '')
  let after = strpart(args, len(before))
  let path = s:Relative('/')
  if path =~# '^/\.git\%(/\|$\)' || len(after)
    let path = ''
  endif
  let relative = s:Relative('')
  if before !~# '\s[^[:space:]-]'
    let owner = s:Owner(@%)
    if len(owner)
      let before .= ' ' . s:shellesc(owner)
    endif
  endif
  if relative =~# '^\.git\%(/\|$\)'
    let relative = ''
  endif
  if len(relative) && a:line2 > 0
    let before .= ' -L ' . s:shellesc(a:line1 . ',' . a:line2 . ':' . relative)
  elseif len(relative) && (empty(after) || a:line2 == 0)
    let after = (len(after) > 3 ? after : ' -- ') . relative
  endif
  let grepformat = &grepformat
  let grepprg = &grepprg
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let dir = getcwd()
  try
    execute cd s:fnameescape(s:Tree())
    let &grepprg = escape(s:UserCommand() . ' --no-pager log --no-color ' .
          \ s:shellesc('--pretty=format:fugitive://'.b:git_dir.'//%H'.path.'::'.g:fugitive_summary_format), '%#')
    let &grepformat = '%Cdiff %.%#,%C--- %.%#,%C+++ %.%#,%Z@@ -%\d%\+\,%\d%\+ +%l\,%\d%\+ @@,%-G-%.%#,%-G+%.%#,%-G %.%#,%A%f::%m,%-G%.%#'
    exe a:cmd . (a:bang ? '! ' : ' ') . s:ShellExpand(before . after)
  finally
    let &grepformat = grepformat
    let &grepprg = grepprg
    execute cd s:fnameescape(dir)
  endtry
endfunction

" Section: :Gedit, :Gpedit, :Gsplit, :Gvsplit, :Gtabedit, :Gread

function! s:UsableWin(nr) abort
  return a:nr && !getwinvar(a:nr, '&previewwindow') &&
        \ index(['nofile','help','quickfix'], getbufvar(winbufnr(a:nr), '&buftype')) < 0
endfunction

function! s:EditParse(args) abort
  let pre = []
  let args = copy(a:args)
  while !empty(args) && args[0] =~# '^+'
    call add(pre, ' ' . escape(remove(args, 0), ' |"'))
  endwhile
  if len(args)
    let file = join(args)
  elseif empty(expand('%'))
    let file = ':'
  elseif empty(s:DirCommitFile(@%)[1]) && s:Relative('./') !~# '^\./\.git\>'
    let file = ':0:%'
  else
    let file = '%'
  endif
  return [s:Expand(file), join(pre)]
endfunction

function! s:BlurStatus() abort
  if &previewwindow && get(b:,'fugitive_type', '') ==# 'index'
    let winnrs = filter([winnr('#')] + range(1, winnr('$')), 's:UsableWin(v:val)')
    if len(winnrs)
      exe winnrs[0].'wincmd w'
    elseif winnr('$') == 1
      let tabs = (&go =~# 'e' || !has('gui_running')) && &stal && (tabpagenr('$') >= &stal)
      execute 'rightbelow' (&lines - &previewheight - &cmdheight - tabs - 1 - !!&laststatus).'new'
    else
      rightbelow new
    endif
    if &diff
      let mywinnr = winnr()
      for winnr in range(winnr('$'),1,-1)
        if winnr != mywinnr && getwinvar(winnr,'&diff')
          execute winnr.'wincmd w'
          close
          if winnr('$') > 1
            wincmd p
          endif
        endif
      endfor
      diffoff!
    endif
  endif
endfunction

function! s:Edit(cmd, bang, mods, args, ...) abort
  let mods = a:mods ==# '<mods>' ? '' : a:mods

  if a:bang
    let temp = tempname()
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    let cwd = getcwd()
    try
      execute cd s:fnameescape(s:Tree())
      let git = s:UserCommand()
      let args = s:ShellExpand(a:args)
      silent! execute '!' . escape(git . ' --no-pager ' . args, '!#%') .
            \ (&shell =~# 'csh' ? ' >& ' . temp : ' > ' . temp . ' 2>&1')
    finally
      execute cd s:fnameescape(cwd)
    endtry
    let temp = s:Resolve(temp)
    let s:temp_files[s:cpath(temp)] = { 'dir': b:git_dir, 'filetype': 'git' }
    if a:cmd ==# 'edit'
      call s:BlurStatus()
    endif
    silent execute mods a:cmd temp
    call fugitive#ReloadStatus()
    return 'redraw|echo ' . string(':!' . git . ' ' . args)
  endif

  let [file, pre] = s:EditParse(a:000)
  try
    let file = s:Generate(file)
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
  if file !~# '^\a\a\+:'
    let file = s:sub(file, '/$', '')
  endif
  if a:cmd ==# 'edit'
    call s:BlurStatus()
  endif
  return mods . ' ' . a:cmd . pre . ' ' . s:fnameescape(file)
endfunction

function! s:Read(count, line1, line2, range, bang, mods, args, ...) abort
  let mods = a:mods ==# '<mods>' ? '' : a:mods
  let after = a:line2
  if a:count < 0
    let delete = 'silent 1,' . line('$') . 'delete_|'
    let after = line('$')
  elseif a:range == 2
    let delete = 'silent ' . a:line1 . ',' . a:line2 . 'delete_|'
  else
    let delete = ''
  endif
  if a:bang
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    let cwd = getcwd()
    try
      execute cd s:fnameescape(s:Tree())
      let git = s:UserCommand()
      let args = s:ShellExpand(a:args)
      silent execute mods after.'read!' escape(git . ' --no-pager ' . args, '!#%')
    finally
      execute cd s:fnameescape(cwd)
    endtry
    execute delete . 'diffupdate'
    call fugitive#ReloadStatus()
    return 'redraw|echo '.string(':!'.git.' '.args)
  endif
  let [file, pre] = s:EditParse(a:000)
  try
    let file = s:Generate(file)
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
  if file =~# '^fugitive:' && after is# 0
    return 'exe ' .string(mods . ' ' . fugitive#FileReadCmd(file, 0, pre)) . '|diffupdate'
  endif
  return mods . ' ' . after . 'read' . pre . ' ' . s:fnameescape(file) . '|' . delete . 'diffupdate' . (a:count < 0 ? '|' . line('.') : '')
endfunction

function! s:EditRunComplete(A,L,P) abort
  if a:L =~# '^\w\+!'
    return s:GitComplete(a:A, a:L, a:P)
  else
    return fugitive#Complete(a:A, a:L, a:P)
  endif
endfunction

call s:command("-bar -bang -nargs=*           -complete=customlist,fugitive#Complete Ge       execute s:Edit('edit<bang>', 0, '<mods>', <q-args>, <f-args>)")
call s:command("-bar -bang -nargs=*           -complete=customlist,fugitive#Complete Gedit    execute s:Edit('edit<bang>', 0, '<mods>', <q-args>, <f-args>)")
call s:command("-bar -bang -nargs=*           -complete=customlist,s:EditRunComplete Gpedit   execute s:Edit('pedit', <bang>0, '<mods>', <q-args>, <f-args>)")
call s:command("-bar -bang -nargs=* -range=0  -complete=customlist,s:EditRunComplete Gsplit   execute s:Edit((<count> ? <count> : '').'split', <bang>0, '<mods>', <q-args>, <f-args>)")
call s:command("-bar -bang -nargs=* -range=0  -complete=customlist,s:EditRunComplete Gvsplit  execute s:Edit((<count> ? <count> : '').'vsplit', <bang>0, '<mods>', <q-args>, <f-args>)")
call s:command("-bar -bang -nargs=* -range=0  -complete=customlist,s:EditRunComplete" . (has('patch-7.4.542') ? ' -addr=tabs' : '') . " Gtabedit execute s:Edit((<count> ? <count> : '').'tabedit', <bang>0, '<mods>', <q-args>, <f-args>)")
call s:command("-bar -bang -nargs=* -range=-1 -complete=customlist,s:EditRunComplete Gread execute s:Read(<count>, <line1>, <line2>, +'<range>', <bang>0, '<mods>', <q-args>, <f-args>)")

" Section: :Gwrite, :Gwq

call s:command("-bar -bang -nargs=* -complete=customlist,fugitive#Complete Gwrite :execute s:Write(<bang>0,<f-args>)")
call s:command("-bar -bang -nargs=* -complete=customlist,fugitive#Complete Gw :execute s:Write(<bang>0,<f-args>)")
call s:command("-bar -bang -nargs=* -complete=customlist,fugitive#Complete Gwq :execute s:Wq(<bang>0,<f-args>)")

function! s:Write(force,...) abort
  if exists('b:fugitive_commit_arguments')
    return 'write|bdelete'
  elseif expand('%:t') == 'COMMIT_EDITMSG' && $GIT_INDEX_FILE != ''
    return 'wq'
  elseif get(b:, 'fugitive_type', '') ==# 'index'
    return 'Gcommit'
  elseif &buftype ==# 'nowrite' && getline(4) =~# '^+++ '
    let filename = getline(4)[6:-1]
    setlocal buftype=
    silent write
    setlocal buftype=nowrite
    if matchstr(getline(2),'index [[:xdigit:]]\+\.\.\zs[[:xdigit:]]\{7\}') ==# fugitive#RevParse(':0:'.filename)[0:6]
      let err = s:TreeChomp('apply', '--cached', '--reverse', '--', expand('%:p'))
    else
      let err = s:TreeChomp('apply', '--cached', '--', expand('%:p'))
    endif
    if err !=# ''
      let v:errmsg = split(err,"\n")[0]
      return 'echoerr v:errmsg'
    elseif a:force
      return 'bdelete'
    else
      return 'Gedit '.fnameescape(filename)
    endif
  endif
  let mytab = tabpagenr()
  let mybufnr = bufnr('')
  let file = a:0 ? s:Generate(s:Expand(join(a:000, ' '))) : fugitive#Real(@%)
  if empty(file)
    return 'echoerr '.string('fugitive: cannot determine file path')
  endif
  if file =~# '^fugitive:'
    return 'write' . (a:force ? '! ' : ' ') . s:fnameescape(file)
  endif
  let always_permitted = s:cpath(fugitive#Real(@%), file) && s:DirCommitFile(@%)[1] =~# '^0\=$'
  if !always_permitted && !a:force && (len(s:TreeChomp('diff', '--name-status', 'HEAD', '--', file)) || len(s:TreeChomp('ls-files', '--others', '--', file)))
    let v:errmsg = 'fugitive: file has uncommitted changes (use ! to override)'
    return 'echoerr v:errmsg'
  endif
  let treebufnr = 0
  for nr in range(1,bufnr('$'))
    if fnamemodify(bufname(nr),':p') ==# file
      let treebufnr = nr
    endif
  endfor

  if treebufnr > 0 && treebufnr != bufnr('')
    let temp = tempname()
    silent execute '%write '.temp
    for tab in [mytab] + range(1,tabpagenr('$'))
      for winnr in range(1,tabpagewinnr(tab,'$'))
        if tabpagebuflist(tab)[winnr-1] == treebufnr
          execute 'tabnext '.tab
          if winnr != winnr()
            execute winnr.'wincmd w'
            let restorewinnr = 1
          endif
          try
            let lnum = line('.')
            let last = line('$')
            silent execute '$read '.temp
            silent execute '1,'.last.'delete_'
            silent write!
            silent execute lnum
            let did = 1
          finally
            if exists('restorewinnr')
              wincmd p
            endif
            execute 'tabnext '.mytab
          endtry
        endif
      endfor
    endfor
    if !exists('did')
      call writefile(readfile(temp,'b'),file,'b')
    endif
  else
    execute 'write! '.s:fnameescape(file)
  endif

  if a:force
    let error = s:TreeChomp('add', '--force', '--', file)
  else
    let error = s:TreeChomp('add', '--', file)
  endif
  if v:shell_error
    let v:errmsg = 'fugitive: '.error
    return 'echoerr v:errmsg'
  endif
  if s:cpath(fugitive#Real(@%), file) && s:DirCommitFile(@%)[1] =~# '^\d$'
    set nomodified
  endif

  let one = s:Generate(':1:'.file)
  let two = s:Generate(':2:'.file)
  let three = s:Generate(':3:'.file)
  for nr in range(1,bufnr('$'))
    let name = fnamemodify(bufname(nr), ':p')
    if bufloaded(nr) && !getbufvar(nr,'&modified') && (name ==# one || name ==# two || name ==# three)
      execute nr.'bdelete'
    endif
  endfor

  unlet! restorewinnr
  let zero = s:Generate(':0:'.file)
  silent execute 'doautocmd BufWritePost' s:fnameescape(zero)
  for tab in range(1,tabpagenr('$'))
    for winnr in range(1,tabpagewinnr(tab,'$'))
      let bufnr = tabpagebuflist(tab)[winnr-1]
      let bufname = fnamemodify(bufname(bufnr), ':p')
      if bufname ==# zero && bufnr != mybufnr
        execute 'tabnext '.tab
        if winnr != winnr()
          execute winnr.'wincmd w'
          let restorewinnr = 1
        endif
        try
          let lnum = line('.')
          let last = line('$')
          silent execute '$read '.s:fnameescape(file)
          silent execute '1,'.last.'delete_'
          silent execute lnum
          set nomodified
          diffupdate
        finally
          if exists('restorewinnr')
            wincmd p
          endif
          execute 'tabnext '.mytab
        endtry
        break
      endif
    endfor
  endfor
  call fugitive#ReloadStatus()
  return 'checktime'
endfunction

function! s:Wq(force,...) abort
  let bang = a:force ? '!' : ''
  if exists('b:fugitive_commit_arguments')
    return 'wq'.bang
  endif
  let result = call(s:function('s:Write'),[a:force]+a:000)
  if result =~# '^\%(write\|wq\|echoerr\)'
    return s:sub(result,'^write','wq')
  else
    return result.'|quit'.bang
  endif
endfunction

augroup fugitive_commit
  autocmd!
  autocmd VimLeavePre,BufDelete COMMIT_EDITMSG execute s:sub(s:FinishCommit(), '^echoerr (.*)', 'echohl ErrorMsg|echo \1|echohl NONE')
augroup END

" Section: :Gpush, :Gfetch

call s:command("-nargs=? -bang -complete=custom,s:RemoteComplete Gpush  execute s:Dispatch('<bang>', 'push '.<q-args>)")
call s:command("-nargs=? -bang -complete=custom,s:RemoteComplete Gfetch execute s:Dispatch('<bang>', 'fetch '.<q-args>)")

function! s:Dispatch(bang, args)
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  let [mp, efm, cc] = [&l:mp, &l:efm, get(b:, 'current_compiler', '')]
  try
    let b:current_compiler = 'git'
    let &l:errorformat = s:common_efm
    execute cd fnameescape(s:Tree())
    let &l:makeprg = substitute(s:UserCommand() . ' ' . a:args, '\s\+$', '', '')
    if exists(':Make') == 2
      noautocmd Make
    else
      silent noautocmd make!
      redraw!
      return 'call fugitive#Cwindow()'
    endif
    return ''
  finally
    let [&l:mp, &l:efm, b:current_compiler] = [mp, efm, cc]
    if empty(cc) | unlet! b:current_compiler | endif
    execute cd fnameescape(cwd)
  endtry
endfunction

" Section: :Gdiff

call s:command("-bang -bar -nargs=* -complete=customlist,fugitive#Complete Gdiff :execute s:Diff('',<bang>0,<f-args>)")
call s:command("-bang -bar -nargs=* -complete=customlist,fugitive#Complete Gvdiff :execute s:Diff('keepalt vert ',<bang>0,<f-args>)")
call s:command("-bang -bar -nargs=* -complete=customlist,fugitive#Complete Gsdiff :execute s:Diff('keepalt ',<bang>0,<f-args>)")

augroup fugitive_diff
  autocmd!
  autocmd BufWinLeave *
        \ if s:can_diffoff(+expand('<abuf>')) && s:diff_window_count() == 2 |
        \   call s:diffoff_all(getbufvar(+expand('<abuf>'), 'git_dir')) |
        \ endif
  autocmd BufWinEnter *
        \ if s:can_diffoff(+expand('<abuf>')) && s:diff_window_count() == 1 |
        \   call s:diffoff() |
        \ endif
augroup END

function! s:can_diffoff(buf) abort
  return getwinvar(bufwinnr(a:buf), '&diff') &&
        \ !empty(getbufvar(a:buf, 'git_dir')) &&
        \ !empty(getwinvar(bufwinnr(a:buf), 'fugitive_diff_restore'))
endfunction

function! fugitive#CanDiffoff(buf) abort
  return s:can_diffoff(a:buf)
endfunction

function! s:diff_modifier(count) abort
  let fdc = matchstr(&diffopt, 'foldcolumn:\zs\d\+')
  if &diffopt =~# 'horizontal' && &diffopt !~# 'vertical'
    return 'keepalt '
  elseif &diffopt =~# 'vertical'
    return 'keepalt vert '
  elseif winwidth(0) <= a:count * ((&tw ? &tw : 80) + (empty(fdc) ? 2 : fdc))
    return 'keepalt '
  else
    return 'keepalt vert '
  endif
endfunction

function! s:diff_window_count() abort
  let c = 0
  for nr in range(1,winnr('$'))
    let c += getwinvar(nr,'&diff')
  endfor
  return c
endfunction

function! s:diff_restore() abort
  let restore = 'setlocal nodiff noscrollbind'
        \ . ' scrollopt=' . &l:scrollopt
        \ . (&l:wrap ? ' wrap' : ' nowrap')
        \ . ' foldlevel=999'
        \ . ' foldmethod=' . &l:foldmethod
        \ . ' foldcolumn=' . &l:foldcolumn
        \ . ' foldlevel=' . &l:foldlevel
        \ . (&l:foldenable ? ' foldenable' : ' nofoldenable')
  if has('cursorbind')
    let restore .= (&l:cursorbind ? ' ' : ' no') . 'cursorbind'
  endif
  return restore
endfunction

function! s:diffthis() abort
  if !&diff
    let w:fugitive_diff_restore = s:diff_restore()
    diffthis
  endif
endfunction

function! s:diffoff() abort
  if exists('w:fugitive_diff_restore')
    execute w:fugitive_diff_restore
    unlet w:fugitive_diff_restore
  else
    diffoff
  endif
endfunction

function! s:diffoff_all(dir) abort
  let curwin = winnr()
  for nr in range(1,winnr('$'))
    if getwinvar(nr,'&diff')
      if nr != winnr()
        execute nr.'wincmd w'
        let restorewinnr = 1
      endif
      if exists('b:git_dir') && b:git_dir ==# a:dir
        call s:diffoff()
      endif
    endif
  endfor
  execute curwin.'wincmd w'
endfunction

function! s:CompareAge(mine, theirs) abort
  let scores = {':0': 1, ':1': 2, ':2': 3, ':': 4, ':3': 5}
  let mine = substitute(a:mine, '^:', '', '')
  let theirs = substitute(a:theirs, '^:', '', '')
  let my_score    = get(scores, ':'.mine, 0)
  let their_score = get(scores, ':'.theirs, 0)
  if my_score || their_score
    return my_score < their_score ? -1 : my_score != their_score
  elseif mine ==# theirs
    return 0
  endif
  let base = s:TreeChomp('merge-base', mine, theirs)
  if base ==# mine
    return -1
  elseif base ==# theirs
    return 1
  endif
  let my_time    = +s:TreeChomp('log', '--max-count=1', '--pretty=format:%at', a:mine, '--')
  let their_time = +s:TreeChomp('log', '--max-count=1', '--pretty=format:%at', a:theirs, '--')
  return my_time < their_time ? -1 : my_time != their_time
endfunction

function! s:Diff(vert,keepfocus,...) abort
  let args = copy(a:000)
  let post = ''
  if get(args, 0) =~# '^+'
    let post = remove(args, 0)[1:-1]
  endif
  let vert = empty(a:vert) ? s:diff_modifier(2) : a:vert
  let commit = s:DirCommitFile(@%)[1]
  let back = exists('*win_getid') ? 'call win_gotoid(' . win_getid() . ')' : 'wincmd p'
  if exists(':DiffGitCached')
    return 'DiffGitCached'
  elseif (empty(args) || args[0] ==# ':') && commit =~# '^[0-1]\=$' && !empty(s:TreeChomp('ls-files', '--unmerged', '--', expand('%:p')))
    if v:shell_error
      return 'echoerr ' . string("fugitive: error determining merge status of the current buffer")
    endif
    let vert = empty(a:vert) ? s:diff_modifier(3) : a:vert
    let nr = bufnr('')
    execute 'leftabove '.vert.'split' s:fnameescape(s:Generate(s:Relative(':2:')))
    execute 'nnoremap <buffer> <silent> dp :diffput '.nr.'<Bar>diffupdate<CR>'
    let nr2 = bufnr('')
    call s:diffthis()
    exe back
    execute 'rightbelow '.vert.'split' s:fnameescape(s:Generate(s:Relative(':3:')))
    execute 'nnoremap <buffer> <silent> dp :diffput '.nr.'<Bar>diffupdate<CR>'
    let nr3 = bufnr('')
    call s:diffthis()
    exe back
    call s:diffthis()
    execute 'nnoremap <buffer> <silent> d2o :diffget '.nr2.'<Bar>diffupdate<CR>'
    execute 'nnoremap <buffer> <silent> d3o :diffget '.nr3.'<Bar>diffupdate<CR>'
    return post
  elseif len(args)
    let arg = join(args, ' ')
    if arg ==# ''
      return post
    elseif arg ==# '/'
      let file = s:Relative()
    elseif arg ==# ':'
      let file = s:Relative(':0:')
    elseif arg =~# '^:/.'
      try
        let file = fugitive#RevParse(arg).s:Relative(':')
      catch /^fugitive:/
        return 'echoerr v:errmsg'
      endtry
    else
      let file = s:Expand(arg)
    endif
    if file !~# ':' && file !~# '^/' && s:TreeChomp('cat-file','-t',file) =~# '^\%(tag\|commit\)$'
      let file = file.s:Relative(':')
    endif
  else
    let file = empty(commit) ? s:Relative(':0:') : s:Relative()
  endif
  try
    let spec = s:Generate(file)
    let restore = s:diff_restore()
    if exists('+cursorbind')
      setlocal cursorbind
    endif
    let w:fugitive_diff_restore = restore
    if s:CompareAge(commit, s:DirCommitFile(spec)[1]) < 0
      execute 'rightbelow '.vert.'diffsplit '.s:fnameescape(spec)
    else
      execute 'leftabove '.vert.'diffsplit '.s:fnameescape(spec)
    endif
    let &l:readonly = &l:readonly
    redraw
    let w:fugitive_diff_restore = restore
    let winnr = winnr()
    if getwinvar('#', '&diff')
      exe back
      if !a:keepfocus
        call feedkeys(winnr."\<C-W>w", 'n')
      endif
    endif
    return post
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
endfunction

" Section: :Gmove, :Gremove

function! s:Move(force, rename, destination) abort
  if a:destination =~# '^\.\.\=\%(/\|$\)'
    let destination = simplify(getcwd() . '/' . a:destination)
  elseif a:destination =~# '^\a\+:\|^/'
    let destination = a:destination
  elseif a:destination =~# '^:/:\='
    let destination = s:Tree() . substitute(a:destination, '^:/:\=', '', '')
  elseif a:destination =~# '^:(\%(top\|top,literal\|literal,top\))'
    let destination = s:Tree() . matchstr(a:destination, ')\zs.*')
  elseif a:destination =~# '^:(literal)'
    let destination = simplify(getcwd() . '/' . matchstr(a:destination, ')\zs.*'))
  elseif a:rename
    let destination = expand('%:p:s?[\/]$??:h') . '/' . a:destination
  else
    let destination = s:Tree() . '/' . a:destination
  endif
  let destination = s:Slash(destination)
  if isdirectory(@%)
    setlocal noswapfile
  endif
  let message = call('s:TreeChomp', ['mv'] + (a:force ? ['-f'] : []) + ['--', expand('%:p'), destination])
  if v:shell_error
    let v:errmsg = 'fugitive: '.message
    return 'echoerr v:errmsg'
  endif
  if isdirectory(destination)
    let destination = fnamemodify(s:sub(destination,'/$','').'/'.expand('%:t'),':.')
  endif
  call fugitive#ReloadStatus()
  if empty(s:DirCommitFile(@%)[1])
    if isdirectory(destination)
      return 'keepalt edit '.s:fnameescape(destination)
    else
      return 'keepalt saveas! '.s:fnameescape(destination)
    endif
  else
    return 'file '.s:fnameescape(s:Generate(':0:'.destination))
  endif
endfunction

function! s:RenameComplete(A,L,P) abort
  if a:A =~# '^[.:]\=/'
    return fugitive#PathComplete(a:A)
  else
    let pre = s:Slash(fnamemodify(expand('%:p:s?[\/]$??'), ':h')) . '/'
    return map(fugitive#PathComplete(pre.a:A), 'strpart(v:val, len(pre))')
  endif
endfunction

function! s:Remove(after, force) abort
  if s:DirCommitFile(@%)[1] ==# ''
    let cmd = ['rm']
  elseif s:DirCommitFile(@%)[1] ==# '0'
    let cmd = ['rm','--cached']
  else
    let v:errmsg = 'fugitive: rm not supported here'
    return 'echoerr v:errmsg'
  endif
  if a:force
    let cmd += ['--force']
  endif
  let message = call('s:TreeChomp', cmd + ['--', expand('%:p')])
  if v:shell_error
    let v:errmsg = 'fugitive: '.s:sub(message,'error:.*\zs\n\(.*-f.*',' (add ! to force)')
    return 'echoerr '.string(v:errmsg)
  else
    call fugitive#ReloadStatus()
    return a:after . (a:force ? '!' : '')
  endif
endfunction

augroup fugitive_remove
  autocmd!
  autocmd User Fugitive if s:DirCommitFile(@%)[1] =~# '^0\=$' |
        \ exe "command! -buffer -bar -bang -nargs=1 -complete=customlist,fugitive#PathComplete Gmove :execute s:Move(<bang>0,0,<q-args>)" |
        \ exe "command! -buffer -bar -bang -nargs=1 -complete=customlist,s:RenameComplete Grename :execute s:Move(<bang>0,1,<q-args>)" |
        \ exe "command! -buffer -bar -bang Gremove :execute s:Remove('edit',<bang>0)" |
        \ exe "command! -buffer -bar -bang Gdelete :execute s:Remove('bdelete',<bang>0)" |
        \ endif
augroup END

" Section: :Gblame

function! s:Keywordprg() abort
  let args = ' --git-dir='.escape(b:git_dir,"\\\"' ")
  if has('gui_running') && !has('win32')
    return s:UserCommand() . ' --no-pager' . args . ' log -1'
  else
    return s:UserCommand() . args . ' show'
  endif
endfunction

augroup fugitive_blame
  autocmd!
  autocmd FileType fugitiveblame setlocal nomodeline | if exists('b:git_dir') | let &l:keywordprg = s:Keywordprg() | endif
  autocmd Syntax fugitiveblame call s:BlameSyntax()
  autocmd User Fugitive
        \ if get(b:, 'fugitive_type') =~# '^\%(file\|blob\|blame\)$' || filereadable(@%) |
        \   exe "command! -buffer -bar -bang -range=0 -nargs=* Gblame :execute s:Blame(<bang>0,<line1>,<line2>,<count>,'<mods>',[<f-args>])" |
        \ endif
  autocmd ColorScheme,GUIEnter * call s:RehighlightBlame()
  autocmd BufWinLeave * execute getwinvar(+bufwinnr(+expand('<abuf>')), 'fugitive_leave')
augroup END

function! s:linechars(pattern) abort
  let chars = strlen(s:gsub(matchstr(getline('.'), a:pattern), '.', '.'))
  if exists('*synconcealed') && &conceallevel > 1
    for col in range(1, chars)
      let chars -= synconcealed(line('.'), col)[0]
    endfor
  endif
  return chars
endfunction

function! s:Blame(bang, line1, line2, count, mods, args) abort
  if exists('b:fugitive_blamed_bufnr')
    return 'bdelete'
  endif
  try
    if empty(s:Relative('/'))
      call s:throw('file or blob required')
    endif
    if filter(copy(a:args),'v:val !~# "^\\%(--root\|--show-name\\|-\\=\\%([ltfnsew]\\|[MC]\\d*\\)\\+\\)$"') != []
      call s:throw('unsupported option')
    endif
    call map(a:args,'s:sub(v:val,"^\\ze[^-]","-")')
    let cmd = ['--no-pager', 'blame', '--show-number']
    if a:count
      let cmd += ['-L', a:line1 . ',' . a:line1]
    endif
    let cmd += a:args
    if s:DirCommitFile(@%)[1] =~# '\D\|..'
      let cmd += [s:DirCommitFile(@%)[1]]
    else
      let cmd += ['--contents', '-']
    endif
    let cmd += ['--', s:Relative('')]
    let basecmd = escape(s:Prepare(cmd), '!#%')
    try
      let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
      let tree = s:Tree()
      if len(tree) && s:cpath(tree) !=# s:cpath(getcwd())
        let cwd = getcwd()
        execute cd s:fnameescape(tree)
      endif
      let error = tempname()
      let temp = error.'.fugitiveblame'
      if &shell =~# 'csh'
        silent! execute '%write !('.basecmd.' > '.temp.') >& '.error
      else
        silent! execute '%write !'.basecmd.' > '.temp.' 2> '.error
      endif
      if exists('l:cwd')
        execute cd s:fnameescape(cwd)
        unlet cwd
      endif
      if v:shell_error
        call s:throw(join(readfile(error),"\n"))
      endif
      if a:count
        let edit = substitute(a:mods, '^<mods>$', '', '') . get(['edit', 'split', 'pedit'], a:line2 - a:line1, ' split')
        return s:BlameCommit(edit, get(readfile(temp), 0, ''))
      else
        for winnr in range(winnr('$'),1,-1)
          call setwinvar(winnr, '&scrollbind', 0)
          if exists('+cursorbind')
            call setwinvar(winnr, '&cursorbind', 0)
          endif
          if getbufvar(winbufnr(winnr), 'fugitive_blamed_bufnr')
            execute winbufnr(winnr).'bdelete'
          endif
        endfor
        let bufnr = bufnr('')
        let restore = 'call setwinvar(bufwinnr('.bufnr.'),"&scrollbind",0)'
        if exists('+cursorbind')
          let restore .= '|call setwinvar(bufwinnr('.bufnr.'),"&cursorbind",0)'
        endif
        if &l:wrap
          let restore .= '|call setwinvar(bufwinnr('.bufnr.'),"&wrap",1)'
        endif
        if &l:foldenable
          let restore .= '|call setwinvar(bufwinnr('.bufnr.'),"&foldenable",1)'
        endif
        setlocal scrollbind nowrap nofoldenable
        if exists('+cursorbind')
          setlocal cursorbind
        endif
        let top = line('w0') + &scrolloff
        let current = line('.')
        let temp = s:Resolve(temp)
        let s:temp_files[s:cpath(temp)] = { 'dir': b:git_dir, 'filetype': 'fugitiveblame', 'args': cmd, 'bufnr': bufnr }
        exe 'keepalt leftabove vsplit '.temp
        let b:fugitive_blamed_bufnr = bufnr
        let b:fugitive_type = 'blame'
        let w:fugitive_leave = restore
        let b:fugitive_blame_arguments = join(a:args,' ')
        execute top
        normal! zt
        execute current
        if exists('+cursorbind')
          setlocal cursorbind
        endif
        setlocal nomodified nomodifiable nonumber scrollbind nowrap foldcolumn=0 nofoldenable winfixwidth filetype=fugitiveblame buftype=nowrite
        if exists('+concealcursor')
          setlocal concealcursor=nc conceallevel=2
        endif
        if exists('+relativenumber')
          setlocal norelativenumber
        endif
        execute "vertical resize ".(s:linechars('.\{-\}\ze\s\+\d\+)')+1)
        nnoremap <buffer> <silent> <F1> :help fugitive-:Gblame<CR>
        nnoremap <buffer> <silent> g?   :help fugitive-:Gblame<CR>
        nnoremap <buffer> <silent> q    :exe substitute(bufwinnr(b:fugitive_blamed_bufnr).' wincmd w<Bar>'.bufnr('').'bdelete','^-1','','')<CR>
        nnoremap <buffer> <silent> gq   :exe substitute(bufwinnr(b:fugitive_blamed_bufnr).' wincmd w<Bar>'.bufnr('').'bdelete<Bar>if expand("%:p") =~# "^fugitive:[\\/][\\/]"<Bar>Gedit<Bar>endif','^-1','','')<CR>
        nnoremap <buffer> <silent> <CR> :<C-U>exe <SID>BlameCommit("exe 'norm q'<Bar>edit")<CR>
        nnoremap <buffer> <silent> -    :<C-U>exe <SID>BlameJump('')<CR>
        nnoremap <buffer> <silent> P    :<C-U>exe <SID>BlameJump('^'.v:count1)<CR>
        nnoremap <buffer> <silent> ~    :<C-U>exe <SID>BlameJump('~'.v:count1)<CR>
        nnoremap <buffer> <silent> i    :<C-U>exe <SID>BlameCommit("exe 'norm q'<Bar>edit")<CR>
        nnoremap <buffer> <silent> o    :<C-U>exe <SID>BlameCommit((&splitbelow ? "botright" : "topleft")." split")<CR>
        nnoremap <buffer> <silent> O    :<C-U>exe <SID>BlameCommit("tabedit")<CR>
        nnoremap <buffer> <silent> p    :<C-U>exe <SID>Edit((&splitbelow ? "botright" : "topleft").' pedit', 0, '', matchstr(getline('.'), '\x\+'), matchstr(getline('.'), '\x\+'))<CR>
        nnoremap <buffer> <silent> A    :<C-u>exe "vertical resize ".(<SID>linechars('.\{-\}\ze [0-9:/+-][0-9:/+ -]* \d\+)')+1+v:count)<CR>
        nnoremap <buffer> <silent> C    :<C-u>exe "vertical resize ".(<SID>linechars('^\S\+')+1+v:count)<CR>
        nnoremap <buffer> <silent> D    :<C-u>exe "vertical resize ".(<SID>linechars('.\{-\}\ze\d\ze\s\+\d\+)')+1-v:count)<CR>
        redraw
        syncbind
      endif
    finally
      if exists('l:cwd')
        execute cd s:fnameescape(cwd)
      endif
    endtry
    return ''
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
endfunction

function! s:BlameCommit(cmd, ...) abort
  let line = a:0 ? a:1 : getline('.')
  if line =~# '^0\{4,40\} '
    return 'echoerr ' . string('Not Committed Yet')
  endif
  let cmd = s:Edit(a:cmd, 0, '', matchstr(line, '\x\+'), matchstr(line, '\x\+'))
  if cmd =~# '^echoerr'
    return cmd
  endif
  let lnum = matchstr(line, ' \zs\d\+\ze\s\+[([:digit:]]')
  let path = matchstr(line, '^\^\=\x\+\s\+\zs.\{-\}\ze\s*\d\+ ')
  if path ==# ''
    let path = fugitive#Path(a:0 ? @% : bufname(b:fugitive_blamed_bufnr), '')
  endif
  execute cmd
  if a:cmd ==# 'pedit'
    return ''
  endif
  if search('^diff .* b/\M'.escape(path,'\').'$','W')
    call search('^+++')
    let head = line('.')
    while search('^@@ \|^diff ') && getline('.') =~# '^@@ '
      let top = +matchstr(getline('.'),' +\zs\d\+')
      let len = +matchstr(getline('.'),' +\d\+,\zs\d\+')
      if lnum >= top && lnum <= top + len
        let offset = lnum - top
        if &scrolloff
          +
          normal! zt
        else
          normal! zt
          +
        endif
        while offset > 0 && line('.') < line('$')
          +
          if getline('.') =~# '^[ +]'
            let offset -= 1
          endif
        endwhile
        return 'normal! zv'
      endif
    endwhile
    execute head
    normal! zt
  endif
  return ''
endfunction

function! s:BlameJump(suffix) abort
  let commit = matchstr(getline('.'),'^\^\=\zs\x\+')
  if commit =~# '^0\+$'
    let commit = ':0'
  endif
  let lnum = matchstr(getline('.'),' \zs\d\+\ze\s\+[([:digit:]]')
  let path = matchstr(getline('.'),'^\^\=\x\+\s\+\zs.\{-\}\ze\s*\d\+ ')
  if path ==# ''
    let path = fugitive#Path(bufname(b:fugitive_blamed_bufnr), '')
  endif
  let args = b:fugitive_blame_arguments
  let offset = line('.') - line('w0')
  let bufnr = bufnr('%')
  let winnr = bufwinnr(b:fugitive_blamed_bufnr)
  if winnr > 0
    exe winnr.'wincmd w'
  endif
  execute 'Gedit' s:fnameescape(commit . a:suffix . ':' . path)
  execute lnum
  if winnr > 0
    exe bufnr.'bdelete'
  endif
  if exists(':Gblame')
    execute 'Gblame '.args
    execute lnum
    let delta = line('.') - line('w0') - offset
    if delta > 0
      execute 'normal! '.delta."\<C-E>"
    elseif delta < 0
      execute 'normal! '.(-delta)."\<C-Y>"
    endif
    syncbind
  endif
  return ''
endfunction

let s:hash_colors = {}

function! s:BlameSyntax() abort
  let b:current_syntax = 'fugitiveblame'
  let conceal = has('conceal') ? ' conceal' : ''
  let arg = exists('b:fugitive_blame_arguments') ? b:fugitive_blame_arguments : ''
  syn match FugitiveblameBoundary "^\^"
  syn match FugitiveblameBlank                      "^\s\+\s\@=" nextgroup=FugitiveblameAnnotation,fugitiveblameOriginalFile,FugitiveblameOriginalLineNumber skipwhite
  syn match FugitiveblameHash       "\%(^\^\=\)\@<=\<\x\{7,40\}\>" nextgroup=FugitiveblameAnnotation,FugitiveblameOriginalLineNumber,fugitiveblameOriginalFile skipwhite
  syn match FugitiveblameUncommitted "\%(^\^\=\)\@<=\<0\{7,40\}\>" nextgroup=FugitiveblameAnnotation,FugitiveblameOriginalLineNumber,fugitiveblameOriginalFile skipwhite
  syn region FugitiveblameAnnotation matchgroup=FugitiveblameDelimiter start="(" end="\%( \d\+\)\@<=)" contained keepend oneline
  syn match FugitiveblameTime "[0-9:/+-][0-9:/+ -]*[0-9:/+-]\%( \+\d\+)\)\@=" contained containedin=FugitiveblameAnnotation
  exec 'syn match FugitiveblameLineNumber         " *\d\+)\@=" contained containedin=FugitiveblameAnnotation'.conceal
  exec 'syn match FugitiveblameOriginalFile       " \%(\f\+\D\@<=\|\D\@=\f\+\)\%(\%(\s\+\d\+\)\=\s\%((\|\s*\d\+)\)\)\@=" contained nextgroup=FugitiveblameOriginalLineNumber,FugitiveblameAnnotation skipwhite'.(arg =~# 'f' ? '' : conceal)
  exec 'syn match FugitiveblameOriginalLineNumber " *\d\+\%(\s(\)\@=" contained nextgroup=FugitiveblameAnnotation skipwhite'.(arg =~# 'n' ? '' : conceal)
  exec 'syn match FugitiveblameOriginalLineNumber " *\d\+\%(\s\+\d\+)\)\@=" contained nextgroup=FugitiveblameShort skipwhite'.(arg =~# 'n' ? '' : conceal)
  syn match FugitiveblameShort              " \d\+)" contained contains=FugitiveblameLineNumber
  syn match FugitiveblameNotCommittedYet "(\@<=Not Committed Yet\>" contained containedin=FugitiveblameAnnotation
  hi def link FugitiveblameBoundary           Keyword
  hi def link FugitiveblameHash               Identifier
  hi def link FugitiveblameUncommitted        Ignore
  hi def link FugitiveblameTime               PreProc
  hi def link FugitiveblameLineNumber         Number
  hi def link FugitiveblameOriginalFile       String
  hi def link FugitiveblameOriginalLineNumber Float
  hi def link FugitiveblameShort              FugitiveblameDelimiter
  hi def link FugitiveblameDelimiter          Delimiter
  hi def link FugitiveblameNotCommittedYet    Comment
  let seen = {}
  for lnum in range(1, line('$'))
    let hash = matchstr(getline(lnum), '^\^\=\zs\x\{6\}')
    if hash ==# '' || hash ==# '000000' || has_key(seen, hash)
      continue
    endif
    let seen[hash] = 1
    if &t_Co > 16 && get(g:, 'CSApprox_loaded') && !empty(findfile('autoload/csapprox/per_component.vim', escape(&rtp, ' ')))
          \ && empty(get(s:hash_colors, hash))
      let [s, r, g, b; __] = map(matchlist(hash, '\(\x\x\)\(\x\x\)\(\x\x\)'), 'str2nr(v:val,16)')
      let color = csapprox#per_component#Approximate(r, g, b)
      if color == 16 && &background ==# 'dark'
        let color = 8
      endif
      let s:hash_colors[hash] = ' ctermfg='.color
    else
      let s:hash_colors[hash] = ''
    endif
    exe 'syn match FugitiveblameHash'.hash.'       "\%(^\^\=\)\@<='.hash.'\x\{1,34\}\>" nextgroup=FugitiveblameAnnotation,FugitiveblameOriginalLineNumber,fugitiveblameOriginalFile skipwhite'
  endfor
  call s:RehighlightBlame()
endfunction

function! s:RehighlightBlame() abort
  for [hash, cterm] in items(s:hash_colors)
    if !empty(cterm) || has('gui_running') || has('termguicolors') && &termguicolors
      exe 'hi FugitiveblameHash'.hash.' guifg=#'.hash.get(s:hash_colors, hash, '')
    else
      exe 'hi link FugitiveblameHash'.hash.' Identifier'
    endif
  endfor
endfunction

" Section: :Gbrowse

call s:command("-bar -bang -range=0 -nargs=* -complete=customlist,fugitive#Complete Gbrowse :execute s:Browse(<bang>0,<line1>,<count>,<f-args>)")

let s:redirects = {}

function! s:Browse(bang,line1,count,...) abort
  try
    let validremote = '\.\|\.\=/.*\|[[:alnum:]_-]\+\%(://.\{-\}\)\='
    if a:0
      let remote = matchstr(join(a:000, ' '),'@\zs\%('.validremote.'\)$')
      let rev = substitute(join(a:000, ' '),'@\%('.validremote.'\)$','','')
    else
      let remote = ''
      let rev = ''
    endif
    if rev ==# ''
      let rev = s:DirRev(@%)[1]
    endif
    if rev =~# '^:\=$'
      let expanded = s:Relative()
    else
      let expanded = s:Expand(rev)
    endif
    let cdir = fugitive#CommonDir(b:git_dir)
    for dir in ['tags/', 'heads/', 'remotes/']
      if expanded !~# '^[./]' && filereadable(cdir . '/refs/' . dir . expanded)
        let expanded = '.git/refs/' . dir . expanded
      endif
    endfor
    let full = s:Generate(expanded)
    let commit = ''
    if full =~? '^fugitive:'
      let [dir, commit, path] = s:DirCommitFile(full)
      if commit =~# '^:\=\d$'
        let commit = ''
      endif
      if commit =~ '..'
        let type = s:TreeChomp('cat-file','-t',commit.s:sub(path,'^/',':'))
        let branch = matchstr(expanded, '^[^:]*')
      else
        let type = 'blob'
      endif
      let path = path[1:-1]
    elseif empty(s:Tree())
      let path = '.git/' . full[strlen(b:git_dir)+1:-1]
      let type = ''
    else
      let path = full[strlen(s:Tree())+1:-1]
      if path =~# '^\.git/'
        let type = ''
      elseif isdirectory(full)
        let type = 'tree'
      else
        let type = 'blob'
      endif
    endif
    if type ==# 'tree' && !empty(path)
      let path = s:sub(path, '/\=$', '/')
    endif
    if path =~# '^\.git/.*HEAD$' && filereadable(b:git_dir . '/' . path[5:-1])
      let body = readfile(b:git_dir . '/' . path[5:-1])[0]
      if body =~# '^\x\{40\}$'
        let commit = body
        let type = 'commit'
        let path = ''
      elseif body =~# '^ref: refs/'
        let path = '.git/' . matchstr(body,'ref: \zs.*')
      endif
    endif

    let merge = ''
    if path =~# '^\.git/refs/remotes/.'
      if empty(remote)
        let remote = matchstr(path, '^\.git/refs/remotes/\zs[^/]\+')
        let branch = matchstr(path, '^\.git/refs/remotes/[^/]\+/\zs.\+')
      else
        let merge = matchstr(path, '^\.git/refs/remotes/[^/]\+/\zs.\+')
        let path = '.git/refs/heads/'.merge
      endif
    elseif path =~# '^\.git/refs/heads/.'
      let branch = path[16:-1]
    elseif !exists('branch')
      let branch = FugitiveHead()
    endif
    if !empty(branch)
      let r = fugitive#Config('branch.'.branch.'.remote')
      let m = fugitive#Config('branch.'.branch.'.merge')[11:-1]
      if r ==# '.' && !empty(m)
        let r2 = fugitive#Config('branch.'.m.'.remote')
        if r2 !~# '^\.\=$'
          let r = r2
          let m = fugitive#Config('branch.'.m.'.merge')[11:-1]
        endif
      endif
      if empty(remote)
        let remote = r
      endif
      if r ==# '.' || r ==# remote
        let merge = m
        if path =~# '^\.git/refs/heads/.'
          let path = '.git/refs/heads/'.merge
        endif
      endif
    endif

    let line1 = a:count > 0 ? a:line1 : 0
    let line2 = a:count > 0 ? a:count : 0
    if empty(commit) && path !~# '^\.git/'
      if a:line1 && !a:count && !empty(merge)
        let commit = merge
      else
        let commit = ''
        if len(merge)
          let remotehead = cdir . '/refs/remotes/' . remote . '/' . merge
          let commit = filereadable(remotehead) ? get(readfile(remotehead), 0, '') : ''
          if a:count && !a:0 && commit =~# '^\x\{40\}$'
            let blame_list = tempname()
            call writefile([commit, ''], blame_list, 'b')
            let blame_in = tempname()
            silent exe '%write' blame_in
            let blame = split(s:TreeChomp('blame', '--contents', blame_in, '-L', a:line1.','.a:count, '-S', blame_list, '-s', '--show-number', './' . path), "\n")
            if !v:shell_error
              let blame_regex = '^\^\x\+\s\+\zs\d\+\ze\s'
              if get(blame, 0) =~# blame_regex && get(blame, -1) =~# blame_regex
                let line1 = +matchstr(blame[0], blame_regex)
                let line2 = +matchstr(blame[-1], blame_regex)
              else
                call s:throw("Can't browse to uncommitted change")
              endif
            endif
          endif
        endif
      endif
      if empty(commit)
        let commit = readfile(b:git_dir . '/HEAD', '', 1)[0]
      endif
      let i = 0
      while commit =~# '^ref: ' && i < 10
        let commit = readfile(cdir . '/' . commit[5:-1], '', 1)[0]
        let i -= 1
      endwhile
    endif

    if empty(remote)
      let remote = '.'
    endif
    let raw = fugitive#RemoteUrl(remote)
    if empty(raw)
      let raw = remote
    endif

    if raw =~# '^https\=://' && s:executable('curl')
      if !has_key(s:redirects, raw)
        let s:redirects[raw] = matchstr(system('curl -I ' .
              \ s:shellesc(raw . '/info/refs?service=git-upload-pack')),
              \ 'Location: \zs\S\+\ze/info/refs?')
      endif
      if len(s:redirects[raw])
        let raw = s:redirects[raw]
      endif
    endif

    let opts = {
          \ 'dir': b:git_dir,
          \ 'repo': fugitive#repo(),
          \ 'remote': raw,
          \ 'revision': 'No longer provided',
          \ 'commit': commit,
          \ 'path': path,
          \ 'type': type,
          \ 'line1': line1,
          \ 'line2': line2}

    for Handler in get(g:, 'fugitive_browse_handlers', [])
      let url = call(Handler, [copy(opts)])
      if !empty(url)
        break
      endif
    endfor

    if empty(url)
      call s:throw("No Gbrowse handler found for '".raw."'")
    endif

    let url = s:gsub(url, '[ <>]', '\="%".printf("%02X",char2nr(submatch(0)))')
    if a:bang
      if has('clipboard')
        let @+ = url
      endif
      return 'echomsg '.string(url)
    elseif exists(':Browse') == 2
      return 'echomsg '.string(url).'|Browse '.url
    else
      if !exists('g:loaded_netrw')
        runtime! autoload/netrw.vim
      endif
      if exists('*netrw#BrowseX')
        return 'echomsg '.string(url).'|call netrw#BrowseX('.string(url).', 0)'
      else
        return 'echomsg '.string(url).'|call netrw#NetrwBrowseX('.string(url).', 0)'
      endif
    endif
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
endfunction

" Section: Go to file

nnoremap <SID>: :<C-U><C-R>=v:count ? v:count : ''<CR>
function! fugitive#MapCfile(...) abort
  exe 'cnoremap <buffer> <expr> <Plug><cfile>' (a:0 ? a:1 : 'fugitive#Cfile()')
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') . '|sil! exe "cunmap <buffer> <Plug><cfile>"'
  if !exists('g:fugitive_no_maps')
    call s:map('n', 'gf',          '<SID>:find <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:map('n', '<C-W>f',     '<SID>:sfind <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:map('n', '<C-W><C-F>', '<SID>:sfind <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:map('n', '<C-W>gf',  '<SID>:tabfind <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:map('c', '<C-R><C-F>', '<Plug><cfile>', '<silent><unique>', 1)
  endif
endfunction

function! s:ContainingCommit() abort
  let commit = s:Owner(@%)
  return empty(commit) ? 'HEAD' : commit
endfunction

function! s:NavigateUp(count) abort
  let rev = substitute(s:DirRev(@%)[1], '^$', ':', 'g')
  let c = a:count
  while c
    if rev =~# ':.*/.'
      let rev = matchstr(rev, '.*\ze/.\+', '')
    elseif rev =~# '.:.'
      let rev = matchstr(rev, '^.[^:]*:')
    elseif rev =~# '^:'
      let rev = 'HEAD^{}'
    elseif rev =~# ':$'
      let rev = rev[0:-2]
    else
      return rev.'~'.c
    endif
    let c -= 1
  endwhile
  return rev
endfunction

function! fugitive#MapJumps(...) abort
  if get(b:, 'fugitive_type', '') ==# 'blob'
    nnoremap <buffer> <silent> <CR>    :<C-U>.Gblame<CR>
  else
    nnoremap <buffer> <silent> <CR>    :<C-U>exe <SID>GF("edit")<CR>
  endif
  if !&modifiable
    let nowait = v:version >= 704 ? '<nowait>' : ''
    if get(b:, 'fugitive_type', '') ==# 'blob'
      nnoremap <buffer> <silent> o     :<C-U>.,.+1Gblame<CR>
      nnoremap <buffer> <silent> S     :<C-U>vertical .,.+1Gblame<CR>
      nnoremap <buffer> <silent> O     :<C-U>tab .,.+1Gblame<CR>
      nnoremap <buffer> <silent> p     :<C-U>.,.+2Gblame<CR>
    else
      nnoremap <buffer> <silent> o     :<C-U>exe <SID>GF("split")<CR>
      nnoremap <buffer> <silent> S     :<C-U>exe <SID>GF("vsplit")<CR>
      nnoremap <buffer> <silent> O     :<C-U>exe <SID>GF("tabedit")<CR>
      nnoremap <buffer> <silent> p     :<C-U>exe <SID>GF("pedit")<CR>
    endif
    exe "nnoremap <buffer> <silent>" nowait  "-     :<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>NavigateUp(v:count1))<Bar> if getline(1) =~# '^tree \x\{40\}$' && empty(getline(2))<Bar>call search('^'.escape(expand('#:t'),'.*[]~\').'/\=$','wc')<Bar>endif<CR>"
    nnoremap <buffer> <silent> P     :<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit().'^'.v:count1.<SID>Relative(':'))<CR>
    nnoremap <buffer> <silent> ~     :<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit().'~'.v:count1.<SID>Relative(':'))<CR>
    nnoremap <buffer> <silent> C     :<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>
    nnoremap <buffer> <silent> cc    :<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>
    nnoremap <buffer> <silent> co    :<C-U>exe 'Gsplit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>
    nnoremap <buffer> <silent> cS    :<C-U>exe 'Gvsplit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>
    nnoremap <buffer> <silent> cO    :<C-U>exe 'Gtabedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>
    nnoremap <buffer> <silent> cp    :<C-U>exe 'Gpedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>
    nmap     <buffer>          .     <SID>: <Plug><cfile><Home>
  endif
endfunction

function! s:StatusCfile(...) abort
  let pre = ''
  let tree = FugitiveTreeForGitDir(b:git_dir)
  let lead = s:cpath(tree, getcwd()) ? './' : tree . '/'
  if getline('.') =~# '^.\=\trenamed:.* -> '
    return lead . matchstr(getline('.'),' -> \zs.*')
  elseif getline('.') =~# '^.\=\t\(\k\| \)\+\p\?: *.'
    return lead . matchstr(getline('.'),': *\zs.\{-\}\ze\%( ([^()[:digit:]]\+)\)\=$')
  elseif getline('.') =~# '^.\=\t.'
    return lead . matchstr(getline('.'),'\t\zs.*')
  elseif getline('.') =~# ': needs merge$'
    return lead . matchstr(getline('.'),'.*\ze: needs merge$')
  elseif getline('.') =~# '^\%(. \)\=Not currently on any branch.$'
    return 'HEAD'
  elseif getline('.') =~# '^\%(. \)\=On branch '
    return 'refs/heads/'.getline('.')[12:]
  elseif getline('.') =~# "^\\%(. \\)\=Your branch .*'"
    return matchstr(getline('.'),"'\\zs\\S\\+\\ze'")
  else
    return ''
  endif
endfunction

function! fugitive#StatusCfile() abort
  let file = s:Generate(s:StatusCfile())
  return empty(file) ? "\<C-R>\<C-F>" : s:fnameescape(file)
endfunction

function! s:cfile() abort
  try
    let myhash = s:DirRev(@%)[1]
    if len(myhash)
      try
        let myhash = fugitive#RevParse(myhash)
      catch /^fugitive:/
        let myhash = ''
      endtry
    endif
    if empty(myhash) && getline(1) =~# '^\%(commit\|tag\) \w'
      let myhash = matchstr(getline(1),'^\w\+ \zs\S\+')
    endif

    let showtree = (getline(1) =~# '^tree ' && getline(2) == "")

    let treebase = substitute(s:DirCommitFile(@%)[1], '^\d$', ':&', '') . ':' .
          \ s:Relative('') . (s:Relative('') =~# '^$\|/$' ? '' : '/')

    if getline('.') =~# '^\d\{6\} \l\{3,8\} \x\{40\}\t'
      return [treebase . s:sub(matchstr(getline('.'),'\t\zs.*'),'/$','')]
    elseif showtree
      return [treebase . s:sub(getline('.'),'/$','')]

    else

      let dcmds = []

      " Index
      if getline('.') =~# '^\d\{6\} \x\{40\} \d\t'
        let ref = matchstr(getline('.'),'\x\{40\}')
        let file = ':'.s:sub(matchstr(getline('.'),'\d\t.*'),'\t',':')
        return [file]
      endif

      if getline('.') =~# '^ref: '
        let ref = strpart(getline('.'),5)

      elseif getline('.') =~# '^commit \x\{40\}\>'
        let ref = matchstr(getline('.'),'\x\{40\}')
        return [ref]

      elseif getline('.') =~# '^parent \x\{40\}\>'
        let ref = matchstr(getline('.'),'\x\{40\}')
        let line = line('.')
        let parent = 0
        while getline(line) =~# '^parent '
          let parent += 1
          let line -= 1
        endwhile
        return [ref]

      elseif getline('.') =~# '^tree \x\{40\}$'
        let ref = matchstr(getline('.'),'\x\{40\}')
        if len(myhash) && fugitive#RevParse(myhash.':') ==# ref
          let ref = myhash.':'
        endif
        return [ref]

      elseif getline('.') =~# '^object \x\{40\}$' && getline(line('.')+1) =~ '^type \%(commit\|tree\|blob\)$'
        let ref = matchstr(getline('.'),'\x\{40\}')
        let type = matchstr(getline(line('.')+1),'type \zs.*')

      elseif getline('.') =~# '^\l\{3,8\} '.myhash.'$'
        let ref = s:DirRev(@%)[1]

      elseif getline('.') =~# '^\l\{3,8\} \x\{40\}\>'
        let ref = matchstr(getline('.'),'\x\{40\}')
        echoerr "warning: unknown context ".matchstr(getline('.'),'^\l*')

      elseif getline('.') =~# '^[+-]\{3\} [abciow12]\=/'
        let ref = getline('.')[4:]

      elseif getline('.') =~# '^[+-]' && search('^@@ -\d\+\%(,\d\+\)\= +\d\+','bnW')
        let type = getline('.')[0]
        let lnum = line('.') - 1
        let offset = 0
        while getline(lnum) !~# '^@@ -\d\+\%(,\d\+\)\= +\d\+'
          if getline(lnum) =~# '^[ '.type.']'
            let offset += 1
          endif
          let lnum -= 1
        endwhile
        let offset += matchstr(getline(lnum), type.'\zs\d\+')
        let ref = getline(search('^'.type.'\{3\} [abciow12]/','bnW'))[4:-1]
        let dcmds = [offset, 'normal!zv']

      elseif getline('.') =~# '^rename from '
        let ref = 'a/'.getline('.')[12:]
      elseif getline('.') =~# '^rename to '
        let ref = 'b/'.getline('.')[10:]

      elseif getline('.') =~# '^@@ -\d\+\%(,\d\+\)\= +\d\+'
        let diff = getline(search('^diff --git \%([abciow12]/.*\|/dev/null\) \%([abciow12]/.*\|/dev/null\)', 'bcnW'))
        let offset = matchstr(getline('.'), '+\zs\d\+')

        let dref = matchstr(diff, '\Cdiff --git \zs\%([abciow12]/.*\|/dev/null\)\ze \%([abciow12]/.*\|/dev/null\)')
        let ref = matchstr(diff, '\Cdiff --git \%([abciow12]/.*\|/dev/null\) \zs\%([abciow12]/.*\|/dev/null\)')
        let dcmd = 'Gdiff! +'.offset

      elseif getline('.') =~# '^diff --git \%([abciow12]/.*\|/dev/null\) \%([abciow12]/.*\|/dev/null\)'
        let dref = matchstr(getline('.'),'\Cdiff --git \zs\%([abciow12]/.*\|/dev/null\)\ze \%([abciow12]/.*\|/dev/null\)')
        let ref = matchstr(getline('.'),'\Cdiff --git \%([abciow12]/.*\|/dev/null\) \zs\%([abciow12]/.*\|/dev/null\)')
        let dcmd = 'Gdiff!'

      elseif getline('.') =~# '^index ' && getline(line('.')-1) =~# '^diff --git \%([abciow12]/.*\|/dev/null\) \%([abciow12]/.*\|/dev/null\)'
        let line = getline(line('.')-1)
        let dref = matchstr(line,'\Cdiff --git \zs\%([abciow12]/.*\|/dev/null\)\ze \%([abciow12]/.*\|/dev/null\)')
        let ref = matchstr(line,'\Cdiff --git \%([abciow12]/.*\|/dev/null\) \zs\%([abciow12]/.*\|/dev/null\)')
        let dcmd = 'Gdiff!'

      elseif line('$') == 1 && getline('.') =~ '^\x\{40\}$'
        let ref = getline('.')

      elseif expand('<cword>') =~# '^\x\{7,40\}\>'
        return [expand('<cword>')]

      else
        let ref = ''
      endif

      let prefixes = {
            \ '1': '',
            \ '2': '',
            \ 'b': ':0:',
            \ 'i': ':0:',
            \ 'o': '',
            \ 'w': ''}

      if len(myhash)
        let prefixes.a = myhash.'^:'
        let prefixes.b = myhash.':'
      endif
      let ref = substitute(ref, '^\(\w\)/', '\=get(prefixes, submatch(1), "HEAD:")', '')
      if exists('dref')
        let dref = substitute(dref, '^\(\w\)/', '\=get(prefixes, submatch(1), "HEAD:")', '')
      endif

      if ref ==# '/dev/null'
        " Empty blob
        let ref = 'e69de29bb2d1d6434b8b29ae775ad8c2e48c5391'
      endif

      if exists('dref')
        return [ref, dcmd . ' ' . s:fnameescape(dref)] + dcmds
      elseif ref != ""
        return [ref] + dcmds
      endif

    endif
    return []
  endtry
endfunction

function! s:GF(mode) abort
  try
    let results = &filetype ==# 'gitcommit' ? [s:StatusCfile()] : s:cfile()
  catch /^fugitive:/
    return 'echoerr v:errmsg'
  endtry
  if len(results) > 1
    return 'G' . a:mode .
          \ ' +' . escape(join(results[1:-1], '|'), '| ') . ' ' .
          \ s:fnameescape(results[0])
  elseif len(results)
    return 'G' . a:mode . ' ' . s:fnameescape(results[0])
  else
    return ''
  endif
endfunction

function! fugitive#Cfile() abort
  let pre = ''
  let results = s:cfile()
  if empty(results)
    let cfile = expand('<cfile>')
    if &includeexpr =~# '\<v:fname\>'
      sandbox let cfile = eval(substitute(&includeexpr, '\C\<v:fname\>', '\=string(cfile)', 'g'))
    endif
    return cfile
  elseif len(results) > 1
    let pre = '+' . join(map(results[1:-1], 'escape(v:val, " ")'), '\|') . ' '
  endif
  return pre . s:fnameescape(s:Generate(results[0]))
endfunction

" Section: Statusline

function! fugitive#Statusline(...) abort
  if !exists('b:git_dir')
    return ''
  endif
  let status = ''
  let commit = s:DirCommitFile(@%)[1]
  if len(commit)
    let status .= ':' . commit[0:7]
  endif
  let status .= '('.FugitiveHead(7).')'
  return '[Git'.status.']'
endfunction

function! fugitive#statusline(...) abort
  return fugitive#Statusline()
endfunction

function! fugitive#head(...) abort
  if !exists('b:git_dir')
    return ''
  endif

  return fugitive#Head(a:0 ? a:1 : 0)
endfunction

" Section: Folding

function! fugitive#Foldtext() abort
  if &foldmethod !=# 'syntax'
    return foldtext()
  endif

  let line_foldstart = getline(v:foldstart)
  if line_foldstart =~# '^diff '
    let [add, remove] = [-1, -1]
    let filename = ''
    for lnum in range(v:foldstart, v:foldend)
      let line = getline(lnum)
      if filename ==# '' && line =~# '^[+-]\{3\} [abciow12]/'
        let filename = line[6:-1]
      endif
      if line =~# '^+'
        let add += 1
      elseif line =~# '^-'
        let remove += 1
      elseif line =~# '^Binary '
        let binary = 1
      endif
    endfor
    if filename ==# ''
      let filename = matchstr(line_foldstart, '^diff .\{-\} [abciow12]/\zs.*\ze [abciow12]/')
    endif
    if filename ==# ''
      let filename = line_foldstart[5:-1]
    endif
    if exists('binary')
      return 'Binary: '.filename
    else
      return (add<10&&remove<100?' ':'') . add . '+ ' . (remove<10&&add<100?' ':'') . remove . '- ' . filename
    endif
  elseif line_foldstart =~# '^# .*:$'
    let lines = getline(v:foldstart, v:foldend)
    call filter(lines, 'v:val =~# "^#\t"')
    cal map(lines, "s:sub(v:val, '^#\t%(modified: +|renamed: +)=', '')")
    cal map(lines, "s:sub(v:val, '^([[:alpha:] ]+): +(.*)', '\\2 (\\1)')")
    return line_foldstart.' '.join(lines, ', ')
  endif
  return foldtext()
endfunction

function! fugitive#foldtext() abort
  return fugitive#Foldtext()
endfunction

augroup fugitive_folding
  autocmd!
  autocmd User Fugitive
        \ if &filetype =~# '^git\%(commit\)\=$' && &foldtext ==# 'foldtext()' |
        \    set foldtext=fugitive#Foldtext() |
        \ endif
augroup END

" Section: Initialization

function! fugitive#Init() abort
  if exists('#User#FugitiveBoot')
    try
      let [save_mls, &modelines] = [&mls, 0]
      doautocmd User FugitiveBoot
    finally
      let &mls = save_mls
    endtry
  endif
  if !exists('g:fugitive_no_maps')
    call s:map('c', '<C-R><C-G>', '<SID>fnameescape(fugitive#Object(@%))', '<expr>')
    call s:map('n', 'y<C-G>', ':<C-U>call setreg(v:register, fugitive#Object(@%))<CR>', '<silent>')
  endif
  if expand('%:p') =~# ':[\/][\/]'
    let &l:path = s:sub(&path, '^\.%(,|$)', '')
  endif
  if stridx(&tags, escape(b:git_dir, ', ')) == -1
    if filereadable(b:git_dir.'/tags')
      let &l:tags = escape(b:git_dir.'/tags', ', ').','.&tags
    endif
    if &filetype !=# '' && filereadable(b:git_dir.'/'.&filetype.'.tags')
      let &l:tags = escape(b:git_dir.'/'.&filetype.'.tags', ', ').','.&tags
    endif
  endif
  try
    let [save_mls, &modelines] = [&mls, 0]
    call s:define_commands()
    doautocmd User Fugitive
  finally
    let &mls = save_mls
  endtry
endfunction

function! fugitive#is_git_dir(path) abort
  return FugitiveIsGitDir(a:path)
endfunction

function! fugitive#extract_git_dir(path) abort
  return FugitiveExtractGitDir(a:path)
endfunction

function! fugitive#detect(path) abort
  return FugitiveDetect(a:path)
endfunction

" Section: End
