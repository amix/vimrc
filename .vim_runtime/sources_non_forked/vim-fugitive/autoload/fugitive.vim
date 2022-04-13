" Location:     autoload/fugitive.vim
" Maintainer:   Tim Pope <http://tpo.pe/>

if exists('g:autoloaded_fugitive')
  finish
endif
let g:autoloaded_fugitive = 1

" Section: Utility

function! s:function(name) abort
  return function(substitute(a:name,'^s:',matchstr(expand('<sfile>'), '.*\zs<SNR>\d\+_'),''))
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

function! s:WinShellEsc(arg) abort
  if type(a:arg) == type([])
    return join(map(copy(a:arg), 's:WinShellEsc(v:val)'))
  elseif a:arg =~# '^[A-Za-z0-9_/:.-]\+$'
    return a:arg
  else
    return '"' . s:gsub(s:gsub(a:arg, '"', '""'), '\%', '"%"') . '"'
  endif
endfunction

function! s:shellesc(arg) abort
  if type(a:arg) == type([])
    return join(map(copy(a:arg), 's:shellesc(v:val)'))
  elseif a:arg =~# '^[A-Za-z0-9_/:.-]\+$'
    return a:arg
  elseif s:winshell()
    return '"' . s:gsub(s:gsub(a:arg, '"', '""'), '\%', '"%"') . '"'
  else
    return shellescape(a:arg)
  endif
endfunction

let s:fnameescape = " \t\n*?[{`$\\%#'\"|!<"
function! s:fnameescape(file) abort
  if type(a:file) == type([])
    return join(map(copy(a:file), 's:fnameescape(v:val)'))
  elseif exists('*fnameescape')
    return fnameescape(a:file)
  else
    return escape(a:file, s:fnameescape)
  endif
endfunction

function! s:throw(string) abort
  throw 'fugitive: '.a:string
endfunction

function! s:VersionCheck() abort
  if v:version < 704
    return 'return ' . string('echoerr "fugitive: Vim 7.4 or newer required"')
  elseif empty(fugitive#GitVersion())
    return 'return ' . string('echoerr "fugitive: cannot execute Git"')
  elseif !fugitive#GitVersion(1, 8, 5)
    return 'return ' . string('echoerr "fugitive: Git 1.8.5 or newer required"')
  else
    return ''
  endif
endfunction

function! s:DirCheck(...) abort
  let vcheck = s:VersionCheck()
  if !empty(vcheck)
    return vcheck
  elseif !empty(a:0 ? s:Dir(a:1) : s:Dir())
    return ''
  elseif empty(bufname(''))
    return 'return ' . string('echoerr "fugitive: working directory does not belong to a Git repository"')
  else
    return 'return ' . string('echoerr "fugitive: file does not belong to a Git repository"')
  endif
endfunction

function! s:Mods(mods, ...) abort
  let mods = substitute(a:mods, '\C<mods>', '', '')
  let mods = mods =~# '\S$' ? mods . ' ' : mods
  if a:0 && mods !~# '\<\%(aboveleft\|belowright\|leftabove\|rightbelow\|topleft\|botright\|tab\)\>'
    let mods = a:1 . ' ' . mods
  endif
  return substitute(mods, '\s\+', ' ', 'g')
endfunction

if exists('+shellslash')
  function! s:Slash(path) abort
    return tr(a:path, '\', '/')
  endfunction
else
  function! s:Slash(path) abort
    return a:path
  endfunction
endif

function! s:Resolve(path) abort
  let path = resolve(a:path)
  if has('win32')
    let path = FugitiveVimPath(fnamemodify(fnamemodify(path, ':h'), ':p') . fnamemodify(path, ':t'))
  endif
  return path
endfunction

function! s:FileIgnoreCase(for_completion) abort
  return (exists('+fileignorecase') && &fileignorecase)
        \ || (a:for_completion && exists('+wildignorecase') && &wildignorecase)
endfunction

function! s:cpath(path, ...) abort
  if s:FileIgnoreCase(0)
    let path = FugitiveVimPath(tolower(a:path))
  else
    let path = FugitiveVimPath(a:path)
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

if !exists('s:temp_scripts')
  let s:temp_scripts = {}
endif
function! s:TempScript(...) abort
  let body = join(a:000, "\n")
  if !has_key(s:temp_scripts, body)
    let s:temp_scripts[body] = tempname() . '.sh'
  endif
  let temp = s:temp_scripts[body]
  if !filereadable(temp)
    call writefile(['#!/bin/sh'] + a:000, temp)
  endif
  return FugitiveGitPath(temp)
endfunction

function! s:DoAutocmd(cmd) abort
  if v:version >= 704 || (v:version == 703 && has('patch442'))
    return 'doautocmd <nomodeline>' . a:cmd
  elseif &modelines > 0
    return 'try|set modelines=0|doautocmd ' . a:cmd . '|finally|set modelines=' . &modelines . '|endtry'
  else
    return 'doautocmd ' . a:cmd
  endif
endfunction

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
      exe mode.'map <buffer>' s:nowait flags head.tail a:rhs
      if a:0 > 1
        let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') .
              \ '|sil! exe "' . mode . 'unmap <buffer> ' . head.tail . '"'
      endif
    endif
  endfor
endfunction

function! fugitive#Autowrite() abort
  if &autowrite || &autowriteall
    try
      if &confirm
        let reconfirm = 1
        setglobal noconfirm
      endif
      silent! wall
    finally
      if exists('reconfirm')
        setglobal confirm
      endif
    endtry
  endif
  return ''
endfunction

" Section: Git

function! s:GitCmd() abort
  if !exists('g:fugitive_git_executable')
    return ['git']
  elseif type(g:fugitive_git_executable) == type([])
    return g:fugitive_git_executable
  else
    let dquote = '"\%([^"]\|""\|\\"\)*"\|'
    let string = g:fugitive_git_executable
    let list = []
    if string =~# '^\w\+='
      call add(list, 'env')
    endif
    while string =~# '\S'
      let arg = matchstr(string, '^\s*\%(' . dquote . '''[^'']*''\|\\.\|[^[:space:] |]\)\+')
      let string = strpart(string, len(arg))
      let arg = substitute(arg, '^\s\+', '', '')
      let arg = substitute(arg,
            \ '\(' . dquote . '''\%(''''\|[^'']\)*''\|\\[' . s:fnameescape . ']\|^\\[>+-]\|!\d*\)\|' . s:expand,
            \ '\=submatch(0)[0] ==# "\\" ? submatch(0)[1] : submatch(0)[1:-2]', 'g')
      call add(list, arg)
    endwhile
    return list
  endif
endfunction

function! s:GitShellCmd() abort
  if !exists('g:fugitive_git_executable')
    return 'git'
  elseif type(g:fugitive_git_executable) == type([])
    return s:shellesc(g:fugitive_git_executable)
  else
    return g:fugitive_git_executable
  endif
endfunction

function! s:UserCommandCwd(dir) abort
  let tree = s:Tree(a:dir)
  return len(tree) ? FugitiveVimPath(tree) : getcwd()
endfunction

function! s:UserCommandList(...) abort
  if !fugitive#GitVersion(1, 8, 5)
    throw 'fugitive: Git 1.8.5 or higher required'
  endif
  if !exists('g:fugitive_git_command')
    let git = s:GitCmd()
  elseif type(g:fugitive_git_command) == type([])
    let git = g:fugitive_git_command
  else
    let git = split(g:fugitive_git_command, '\s\+')
  endif
  let flags = []
  if a:0 && type(a:1) == type({})
    let git = copy(get(a:1, 'git', git))
    let flags = get(a:1, 'flags', flags)
    let dir = a:1.dir
  elseif a:0
    let dir = a:1
  else
    let dir = ''
  endif
  if len(dir)
    let tree = s:Tree(dir)
    if empty(tree)
      call add(git, '--git-dir=' . FugitiveGitPath(dir))
    elseif len(tree) && s:cpath(tree) !=# s:cpath(getcwd())
      call extend(git, ['-C', FugitiveGitPath(tree)])
    endif
  endif
  return git + flags
endfunction

function! s:UserCommand(...) abort
  return s:shellesc(call('s:UserCommandList', a:0 ? [a:1] : []) + (a:0 ? a:2 : []))
endfunction

let s:git_versions = {}
function! fugitive#GitVersion(...) abort
  let git = s:GitShellCmd()
  if !has_key(s:git_versions, git)
    let s:git_versions[git] = matchstr(s:SystemError(git.' --version')[0], '\d[^[:space:]]\+')
  endif
  if !a:0
    return s:git_versions[git]
  endif
  let components = split(s:git_versions[git], '\D\+')
  if empty(components)
    return -1
  endif
  for i in range(len(a:000))
    if a:000[i] > +get(components, i)
      return 0
    elseif a:000[i] < +get(components, i)
      return 1
    endif
  endfor
  return a:000[i] ==# get(components, i)
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
      let cdir = get(readfile(a:dir . '/commondir', 1), 0, '')
      if cdir =~# '^/\|^\a:/'
        let s:commondirs[a:dir] = s:Slash(FugitiveVimPath(cdir))
      else
        let s:commondirs[a:dir] = simplify(a:dir . '/' . cdir)
      endif
    else
      let s:commondirs[a:dir] = a:dir
    endif
  endif
  return s:commondirs[a:dir]
endfunction

function! s:Dir(...) abort
  return a:0 ? FugitiveGitDir(a:1) : FugitiveGitDir()
endfunction

function! s:Tree(...) abort
  return a:0 ? FugitiveWorkTree(a:1) : FugitiveWorkTree()
endfunction

function! s:HasOpt(args, ...) abort
  let args = a:args[0 : index(a:args, '--')]
  let opts = copy(a:000)
  if type(opts[0]) == type([])
    if empty(args) || index(opts[0], args[0]) == -1
      return 0
    endif
    call remove(opts, 0)
  endif
  for opt in opts
    if index(args, opt) != -1
      return 1
    endif
  endfor
endfunction

function! s:PreparePathArgs(cmd, dir, literal) abort
  if a:literal
    call insert(a:cmd, '--literal-pathspecs')
  endif
  let split = index(a:cmd, '--')
  for i in range(split < 0 ? len(a:cmd) : split)
    if type(a:cmd[i]) == type(0)
      let a:cmd[i] = fugitive#Path(bufname(a:cmd[i]), './', a:dir)
    endif
  endfor
  if split < 0
    return a:cmd
  endif
  for i in range(split + 1, len(a:cmd) - 1)
    if type(a:cmd[i]) == type(0)
      let a:cmd[i] = fugitive#Path(bufname(a:cmd[i]), './', a:dir)
    elseif a:literal
      let a:cmd[i] = fugitive#Path(a:cmd[i], './', a:dir)
    endif
  endfor
  return a:cmd
endfunction

let s:prepare_env = {
      \ 'sequence.editor': 'GIT_SEQUENCE_EDITOR',
      \ 'core.editor': 'GIT_EDITOR',
      \ 'core.askpass': 'GIT_ASKPASS',
      \ }
function! fugitive#PrepareDirEnvGitArgv(...) abort
  if !fugitive#GitVersion(1, 8, 5)
    throw 'fugitive: Git 1.8.5 or higher required'
  endif
  let git = s:GitCmd()
  if a:0 && type(a:1) ==# type([])
    let cmd = a:000[1:-1] + a:1
  else
    let cmd = copy(a:000)
  endif
  let env = {}
  let i = 0
  while i < len(cmd)
    if type(cmd[i]) == type({})
      if has_key(cmd[i], 'dir')
        let dir = cmd[i].dir
      endif
      if has_key(cmd[i], 'git')
        let git = cmd[i].git
      endif
      call remove(cmd, i)
    elseif cmd[i] =~# '^$\|[\/.]' && cmd[i] !~# '^-'
      let dir = remove(cmd, i)
    elseif cmd[i] =~# '^--git-dir='
      let dir = remove(cmd, i)[10:-1]
    elseif type(cmd[i]) ==# type(0)
      let dir = s:Dir(remove(cmd, i))
    elseif cmd[i] ==# '-c' && len(cmd) > i + 1
      let key = matchstr(cmd[i+1], '^[^=]*')
      if has_key(s:prepare_env, tolower(key)) || key !~# '\.'
        let var = get(s:prepare_env, tolower(key), key)
        let val = matchstr(cmd[i+1], '=\zs.*')
        let env[var] = val
      endif
      if cmd[i+1] =~# '\.'
        let i += 2
      else
        call remove(cmd, i, i + 1)
      endif
    elseif cmd[i] =~# '^--.*pathspecs$'
      let explicit_pathspec_option = 1
      let i += 1
    elseif cmd[i] !~# '^-'
      break
    else
      let i += 1
    endif
  endwhile
  if !exists('dir')
    let dir = s:Dir()
  endif
  call s:PreparePathArgs(cmd, dir, !exists('explicit_pathspec_option'))
  return [dir, env, git, cmd]
endfunction

function! s:BuildEnvPrefix(env) abort
  let pre = ''
  let env = items(a:env)
  if empty(env)
    return ''
  elseif &shellcmdflag =~# '-Command'
    return join(map(env, '"$Env:" . v:val[0] . " = ''" . substitute(v:val[1], "''", "''''", "g") . "''; "'), '')
  elseif s:winshell()
    return join(map(env, '"set " . substitute(join(v:val, "="), "[&|<>^]", "^^^&", "g") . "& "'), '')
  else
    return 'env ' . s:shellesc(map(env, 'join(v:val, "=")')) . ' '
  endif
endfunction

function! s:JobOpts(cmd, env) abort
  if empty(a:env)
    return [a:cmd, {}]
  elseif has('patch-8.2.0239') || has('nvim-0.5.1') || has('patch-8.1.0902') && !has('nvim') && (!has('win32') || empty(filter(keys(a:env), 'exists("$" . v:val)')))
    return [a:cmd, {'env': a:env}]
  endif
  let envlist = map(items(a:env), 'join(v:val, "=")')
  if !has('win32')
    return [['env'] + envlist + a:cmd, {}]
  else
    let pre = join(map(envlist, '"set " . substitute(v:val, "[&|<>^]", "^^^&", "g") . "& "'), '')
    if len(a:cmd) == 3 && a:cmd[0] ==# 'cmd.exe' && a:cmd[1] ==# '/c'
      return [a:cmd[0:1] + [pre . a:cmd[2]], {}]
    else
      return [['cmd.exe', '/c', pre . s:WinShellEsc(a:cmd)], {}]
    endif
  endif
endfunction

function! s:BuildShell(dir, env, git, args) abort
  let cmd = copy(a:args)
  let tree = s:Tree(a:dir)
  let pre = s:BuildEnvPrefix(a:env)
  if empty(tree) || index(cmd, '--') == len(cmd) - 1
    call insert(cmd, '--git-dir=' . FugitiveGitPath(a:dir))
  else
    call extend(cmd, ['-C', FugitiveGitPath(tree)], 'keep')
  endif
  return pre . join(map(a:git + cmd, 's:shellesc(v:val)'))
endfunction

function! fugitive#Prepare(...) abort
  let [dir, env, git, argv] = call('fugitive#PrepareDirEnvGitArgv', a:000)
  return s:BuildShell(dir, env, git, argv)
endfunction

function! s:SystemError(cmd, ...) abort
  try
    if &shellredir ==# '>' && &shell =~# 'sh\|cmd'
      let shellredir = &shellredir
      if &shell =~# 'csh'
        set shellredir=>&
      else
        set shellredir=>%s\ 2>&1
      endif
    endif
    if exists('+guioptions') && &guioptions =~# '!'
      let guioptions = &guioptions
      set guioptions-=!
    endif
    let out = call('system', [type(a:cmd) ==# type([]) ? fugitive#Prepare(a:cmd) : a:cmd] + a:000)
    return [out, v:shell_error]
  catch /^Vim\%((\a\+)\)\=:E484:/
    let opts = ['shell', 'shellcmdflag', 'shellredir', 'shellquote', 'shellxquote', 'shellxescape', 'shellslash']
    call filter(opts, 'exists("+".v:val) && !empty(eval("&".v:val))')
    call map(opts, 'v:val."=".eval("&".v:val)')
    call s:throw('failed to run `' . a:cmd . '` with ' . join(opts, ' '))
  finally
    if exists('shellredir')
      let &shellredir = shellredir
    endif
    if exists('guioptions')
      let &guioptions = guioptions
    endif
  endtry
endfunction

function! s:ChompError(...) abort
  let [out, exec_error] = s:SystemError(call('fugitive#Prepare', a:000))
  return [s:sub(out, '\n$', ''), exec_error]
endfunction

function! s:ChompDefault(default, ...) abort
  let [out, exec_error] = call('s:ChompError', a:000)
  return exec_error ? a:default : out
endfunction

function! s:LinesError(...) abort
  let [out, exec_error] = call('s:ChompError', a:000)
  return [len(out) && !exec_error ? split(out, "\n", 1) : [], exec_error]
endfunction

function! s:NullError(...) abort
  let [out, exec_error] = s:SystemError(call('fugitive#Prepare', a:000))
  if exec_error
    return [[], substitute(out, "\n$", "", "") : '', exec_error]
  else
    let list = split(out, "\1", 1)
    call remove(list, -1)
    return [list, '', exec_error]
  endif
endfunction

function! s:TreeChomp(...) abort
  let cmd = call('fugitive#Prepare', a:000)
  let [out, exec_error] = s:SystemError(cmd)
  let out = s:sub(out, '\n$', '')
  if !exec_error
    return out
  endif
  throw 'fugitive: error running `' . cmd . '`: ' . out
endfunction

function! s:EchoExec(...) abort
  if s:RunJobs()
    return 'Git ' . s:fnameescape(a:000)
  else
    echo call('s:ChompError', a:000)[0]
    call fugitive#ReloadStatus(-1, 1)
    return 'checktime'
  endif
endfunction

let s:head_cache = {}

function! fugitive#Head(...) abort
  let dir = a:0 > 1 ? a:2 : s:Dir()
  if empty(dir)
    return ''
  endif
  let file = fugitive#Find('.git/HEAD', dir)
  let ftime = getftime(file)
  if ftime == -1
    return ''
  elseif ftime != get(s:head_cache, dir, [-1])[0]
    let s:head_cache[dir] = [ftime, readfile(file)[0]]
  endif
  let head = s:head_cache[dir][1]
  if head =~# '^ref: '
    return substitute(head, '\C^ref: \%(refs/\%(heads/\|remotes/\|tags/\)\=\)\=', '', '')
  elseif head =~# '^\x\{40,\}$'
    let len = a:0 ? a:1 : 0
    return len < 0 ? head : len ? head[0:len-1] : ''
  else
    return ''
  endif
endfunction

function! fugitive#RevParse(rev, ...) abort
  let [hash, exec_error] = s:ChompError([a:0 ? a:1 : s:Dir(), 'rev-parse', '--verify', a:rev, '--'])
  if !exec_error && hash =~# '^\x\{40,\}$'
    return hash
  endif
  throw 'fugitive: rev-parse '.a:rev.': '.hash
endfunction

function! s:ConfigTimestamps(dir, dict) abort
  let files = ['/etc/gitconfig', '~/.gitconfig',
        \ len($XDG_CONFIG_HOME) ? $XDG_CONFIG_HOME . '/git/config' : '~/.config/git/config']
  if len(a:dir)
    call add(files, fugitive#Find('.git/config', a:dir))
  endif
  call extend(files, get(a:dict, 'include.path', []))
  return join(map(files, 'getftime(expand(v:val))'), ',')
endfunction

let s:config = {}
function! fugitive#Config(...) abort
  let name = ''
  let default = get(a:, 3, '')
  if a:0 >= 2 && type(a:2) == type({}) && !has_key(a:2, 'git_dir')
    let name = substitute(a:1, '^[^.]\+\|[^.]\+$', '\L&', 'g')
    return len(a:1) ? get(get(a:2, name, []), 0, default) : a:2
  elseif a:0 >= 2
    let dir = s:Dir(a:2)
    let name = a:1
  elseif a:0 == 1 && type(a:1) == type({}) && !has_key(a:1, 'git_dir')
    return a:1
  elseif a:0 == 1 && type(a:1) == type('') && a:1 =~# '^[[:alnum:]-]\+\.'
    let dir = s:Dir()
    let name = a:1
  elseif a:0 == 1
    let dir = s:Dir(a:1)
  else
    let dir = s:Dir()
  endif
  let name = substitute(name, '^[^.]\+\|[^.]\+$', '\L&', 'g')
  let dir_key = len(dir) ? dir : '_'
  if has_key(s:config, dir_key) && s:config[dir_key][0] ==# s:ConfigTimestamps(dir, s:config[dir_key][1])
    let dict = s:config[dir_key][1]
  else
    let dict = {}
    let [lines, message, exec_error] = s:NullError([dir, 'config', '--list', '-z'])
    if exec_error
      return {}
    endif
    for line in lines
      let key = matchstr(line, "^[^\n]*")
      if !has_key(dict, key)
        let dict[key] = []
      endif
      if len(key) ==# len(line)
        call add(dict[key], 1)
      else
        call add(dict[key], strpart(line, len(key) + 1))
      endif
    endfor
    let s:config[dir_key] = [s:ConfigTimestamps(dir, dict), dict]
    lockvar! dict
  endif
  return len(name) ? get(get(dict, name, []), 0, default) : dict
endfunction

function! fugitive#ConfigGetAll(name, ...) abort
  let config = fugitive#Config(a:0 ? a:1 : s:Dir())
  let name = substitute(a:name, '^[^.]\+\|[^.]\+$', '\L&', 'g')
  return copy(get(config, name, []))
endfunction

function! fugitive#ConfigGetRegexp(pattern, ...) abort
  let config = fugitive#Config(a:0 ? a:1 : s:Dir())
  let filtered = map(filter(copy(config), 'v:key =~# "\\." && v:key =~# a:pattern'), 'copy(v:val)')
  if a:pattern !~# '\\\@<!\%(\\\\\)*\\z[se]'
    return filtered
  endif
  let transformed = {}
  for [k, v] in items(filtered)
    let k = matchstr(k, a:pattern)
    if len(k)
      let transformed[k] = v
    endif
  endfor
  return transformed
endfunction

function! s:Remote(dir) abort
  let head = FugitiveHead(0, a:dir)
  let remote = len(head) ? FugitiveConfigGet('branch.' . head . '.remote', a:dir) : ''
  let i = 10
  while remote ==# '.' && i > 0
    let head = matchstr(FugitiveConfigGet('branch.' . head . '.merge', a:dir), 'refs/heads/\zs.*')
    let remote = len(head) ? FugitiveConfigGet('branch.' . head . '.remote', a:dir) : ''
    let i -= 1
  endwhile
  return remote =~# '^\.\=$' ? 'origin' : remote
endfunction

unlet! s:ssh_aliases
function! fugitive#SshHostAlias(...) abort
  if !exists('s:ssh_aliases')
    let s:ssh_aliases = {}
    if filereadable(expand('~/.ssh/config'))
      let hosts = []
      for line in readfile(expand('~/.ssh/config'))
        let key = matchstr(line, '^\s*\zs\w\+\ze\s')
        let value = matchstr(line, '^\s*\w\+\s\+\zs.*\S')
        if key ==? 'host'
          let hosts = split(value, '\s\+')
        elseif key ==? 'hostname'
          for host in hosts
            if !has_key(s:ssh_aliases, host)
              let s:ssh_aliases[host] = tolower(value)
            endif
          endfor
        endif
      endfor
    endif
  endif
  if a:0
    return get(s:ssh_aliases, a:1, a:1)
  else
    return s:ssh_aliases
  endif
endfunction

let s:redirects = {}

function! fugitive#ResolveRemote(remote) abort
  if a:remote =~# '^https\=://' && s:executable('curl')
    if !has_key(s:redirects, a:remote)
      let s:redirects[a:remote] = matchstr(s:SystemError(
            \ 'curl --disable --silent --max-time 5 -I ' .
            \ s:shellesc(a:remote . '/info/refs?service=git-upload-pack'))[0],
            \ 'Location: \zs\S\+\ze/info/refs?')
    endif
    if len(s:redirects[a:remote])
      return s:redirects[a:remote]
    endif
  endif
  return substitute(a:remote,
        \ '^ssh://\%([^@:/]\+@\)\=\zs[^/:]\+\|^\%([^@:/]\+@\)\=\zs[^/:]\+\ze:/\@!',
        \ '\=fugitive#SshHostAlias(submatch(0))', '')
endfunction

function! fugitive#RemoteUrl(...) abort
  let dir = a:0 > 1 ? a:2 : s:Dir()
  let url = !a:0 || a:1 =~# '^\.\=$' ? s:Remote(dir) : a:1
  if url !~# ':\|^/\|^\.\.\=/'
    if !fugitive#GitVersion(2, 7)
      let url = FugitiveConfigGet('remote.' . url . '.url')
    else
      let url = s:ChompDefault('', [dir, 'remote', 'get-url', url, '--'])
    endif
  endif
  if !get(a:, 3, 0)
    let url = fugitive#ResolveRemote(url)
  endif
  return url
endfunction

" Section: Quickfix

function! s:QuickfixGet(nr, ...) abort
  if a:nr < 0
    return call('getqflist', a:000)
  else
    return call('getloclist', [a:nr] + a:000)
  endif
endfunction

function! s:QuickfixSet(nr, ...) abort
  if a:nr < 0
    return call('setqflist', a:000)
  else
    return call('setloclist', [a:nr] + a:000)
  endif
endfunction

function! s:QuickfixCreate(nr, opts) abort
  if has('patch-7.4.2200')
    call s:QuickfixSet(a:nr, [], ' ', a:opts)
  else
    call s:QuickfixSet(a:nr, [], ' ')
  endif
endfunction

function! s:QuickfixStream(nr, event, title, cmd, first, mods, callback, ...) abort
  let mods = s:Mods(a:mods)
  let opts = {'title': a:title, 'context': {'items': []}}
  call s:QuickfixCreate(a:nr, opts)
  let event = (a:nr < 0 ? 'c' : 'l') . 'fugitive-' . a:event
  silent exe s:DoAutocmd('QuickFixCmdPre ' . event)
  let winnr = winnr()
  exe a:nr < 0 ? 'copen' : 'lopen'
  if winnr != winnr()
    wincmd p
  endif

  let buffer = []
  let lines = split(s:SystemError(s:shellesc(a:cmd))[0], "\n")
  for line in lines
    call extend(buffer, call(a:callback, a:000 + [line]))
    if len(buffer) >= 20
      let contexts = map(copy(buffer), 'get(v:val, "context", {})')
      lockvar contexts
      call extend(opts.context.items, contexts)
      unlet contexts
      call s:QuickfixSet(a:nr, remove(buffer, 0, -1), 'a')
      if mods !~# '\<silent\>'
        redraw
      endif
    endif
  endfor
  call extend(buffer, call(a:callback, a:000 + [0]))
  call extend(opts.context.items, map(copy(buffer), 'get(v:val, "context", {})'))
  lockvar opts.context.items
  call s:QuickfixSet(a:nr, buffer, 'a')

  silent exe s:DoAutocmd('QuickFixCmdPost ' . event)
  if a:first && len(s:QuickfixGet(a:nr))
    call s:BlurStatus()
    return mods . (a:nr < 0 ? 'cfirst' : 'lfirst')
  else
    return 'exe'
  endif
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

" Section: Repository Object

function! s:add_methods(namespace, method_names) abort
  for name in a:method_names
    let s:{a:namespace}_prototype[name] = s:function('s:'.a:namespace.'_'.name)
  endfor
endfunction

let s:repo_prototype = {}
let s:repos = {}

function! fugitive#repo(...) abort
  let dir = a:0 ? s:Dir(a:1) : (len(s:Dir()) ? s:Dir() : FugitiveExtractGitDir(expand('%:p')))
  if dir !=# ''
    if has_key(s:repos, dir)
      let repo = get(s:repos, dir)
    else
      let repo = {'git_dir': dir}
      let s:repos[dir] = repo
    endif
    return extend(repo, s:repo_prototype, 'keep')
  endif
  call s:throw('not a Git repository')
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

function! s:repo_find(object) dict abort
  return fugitive#Find(a:object, self.git_dir)
endfunction

function! s:repo_translate(rev) dict abort
  return s:Slash(fugitive#Find(substitute(a:rev, '^/', ':(top)', ''), self.git_dir))
endfunction

function! s:repo_head(...) dict abort
  return fugitive#Head(a:0 ? a:1 : 0, self.git_dir)
endfunction

call s:add_methods('repo',['dir','tree','bare','find','translate','head'])

function! s:repo_prepare(...) dict abort
  return call('fugitive#Prepare', [self.git_dir] + a:000)
endfunction

function! s:repo_git_command(...) dict abort
  let git = s:GitShellCmd() . ' --git-dir='.s:shellesc(self.git_dir)
  return git.join(map(copy(a:000),'" ".s:shellesc(v:val)'),'')
endfunction

function! s:repo_git_chomp(...) dict abort
  return s:sub(system(FugitivePrepare(a:000, self.git_dir)), '\n$', '')
endfunction

function! s:repo_git_chomp_in_tree(...) dict abort
  return call(self.git_chomp, a:000, self)
endfunction

function! s:repo_rev_parse(rev) dict abort
  return fugitive#RevParse(a:rev, self.git_dir)
endfunction

call s:add_methods('repo',['prepare','git_command','git_chomp','git_chomp_in_tree','rev_parse'])

function! s:repo_superglob(base) dict abort
  return map(fugitive#CompleteObject(a:base, self.git_dir), 'substitute(v:val, ''\\\(.\)'', ''\1'', "g")')
endfunction

call s:add_methods('repo',['superglob'])

function! s:repo_config(name) dict abort
  return FugitiveConfigGet(a:name, self.git_dir)
endfunction

function! s:repo_user() dict abort
  let username = self.config('user.name')
  let useremail = self.config('user.email')
  return username.' <'.useremail.'>'
endfunction

call s:add_methods('repo',['config', 'user'])

" Section: File API

function! s:DirCommitFile(path) abort
  let vals = matchlist(s:Slash(a:path), '\c^fugitive:\%(//\)\=\(.\{-\}\)\%(//\|::\)\(\x\{40,\}\|[0-3]\)\(/.*\)\=$')
  if empty(vals)
    return ['', '', '']
  endif
  return vals[1:3]
endfunction

function! s:DirRev(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  return [dir, (commit =~# '^.$' ? ':' : '') . commit . substitute(file, '^/', ':', '')]
endfunction

let s:merge_heads = ['MERGE_HEAD', 'REBASE_HEAD', 'CHERRY_PICK_HEAD', 'REVERT_HEAD']
function! s:MergeHead(...) abort
  let dir = fugitive#Find('.git/', a:0 ? a:1 : s:Dir())
  for head in s:merge_heads
    if filereadable(dir . head)
      return head
    endif
  endfor
  return ''
endfunction

function! s:Owner(path, ...) abort
  let dir = a:0 ? a:1 : s:Dir()
  if empty(dir)
    return ''
  endif
  let actualdir = fugitive#Find('.git/', dir)
  let [pdir, commit, file] = s:DirCommitFile(a:path)
  if s:cpath(dir, pdir)
    if commit =~# '^\x\{40,\}$'
      return commit
    elseif commit ==# '2'
      return '@'
    elseif commit ==# '0'
      return ''
    endif
    let merge_head = s:MergeHead(dir)
    if empty(merge_head)
      return ''
    endif
    if commit ==# '3'
      return merge_head
    elseif commit ==# '1'
      return s:TreeChomp('merge-base', 'HEAD', merge_head, '--')
    endif
  endif
  let path = fnamemodify(a:path, ':p')
  if s:cpath(actualdir, strpart(path, 0, len(actualdir))) && a:path =~# 'HEAD$'
    return strpart(path, len(actualdir))
  endif
  let refs = fugitive#Find('.git/refs', dir)
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
    return FugitiveVimPath((len(tree) ? tree : dir) . file)
  endif
  let pre = substitute(matchstr(a:url, '^\a\a\+\ze:'), '^.', '\u&', '')
  if len(pre) && pre !=? 'fugitive' && exists('*' . pre . 'Real')
    let url = {pre}Real(a:url)
  else
    let url = fnamemodify(a:url, ':p' . (a:url =~# '[\/]$' ? '' : ':s?[\/]$??'))
  endif
  return FugitiveVimPath(empty(url) ? a:url : url)
endfunction

function! fugitive#Path(url, ...) abort
  if empty(a:url)
    return ''
  endif
  let dir = a:0 > 1 ? a:2 : s:Dir()
  let tree = s:Tree(dir)
  if !a:0
    return fugitive#Real(a:url)
  elseif a:1 =~# '\.$'
    let path = s:Slash(fugitive#Real(a:url))
    let cwd = getcwd()
    let lead = ''
    while s:cpath(tree . '/', (cwd . '/')[0 : len(tree)])
      if s:cpath(cwd . '/', path[0 : len(cwd)])
        if strpart(path, len(cwd) + 1) =~# '^\.git\%(/\|$\)'
          break
        endif
        return a:1[0:-2] . (empty(lead) ? './' : lead) . strpart(path, len(cwd) + 1)
      endif
      let cwd = fnamemodify(cwd, ':h')
      let lead .= '../'
    endwhile
    return a:1[0:-2] . path
  endif
  let url = a:url
  let temp_state = s:TempState(url)
  if has_key(temp_state, 'origin_bufnr')
    let url = bufname(temp_state.origin_bufnr)
  endif
  let url = s:Slash(fnamemodify(url, ':p'))
  if url =~# '/$' && s:Slash(a:url) !~# '/$'
    let url = url[0:-2]
  endif
  let [argdir, commit, file] = s:DirCommitFile(a:url)
  if len(argdir) && s:cpath(argdir) !=# s:cpath(dir)
    let file = ''
  elseif len(dir) && s:cpath(url[0 : len(dir)]) ==# s:cpath(dir . '/')
    let file = '/.git'.url[strlen(dir) : -1]
  elseif len(tree) && s:cpath(url[0 : len(tree)]) ==# s:cpath(tree . '/')
    let file = url[len(tree) : -1]
  elseif s:cpath(url) ==# s:cpath(tree)
    let file = '/'
  endif
  if empty(file) && a:1 =~# '^$\|^[.:]/$'
    return FugitiveGitPath(fugitive#Real(a:url))
  endif
  return substitute(file, '^/', a:1, '')
endfunction

function! s:Relative(...) abort
  return fugitive#Path(@%, a:0 ? a:1 : ':(top)', a:0 > 1 ? a:2 : s:Dir())
endfunction

function! fugitive#Find(object, ...) abort
  if type(a:object) == type(0)
    let name = bufname(a:object)
    return FugitiveVimPath(name =~# '^$\|^/\|^\a\+:' ? name : getcwd() . '/' . name)
  elseif a:object =~# '^[~$]'
    let prefix = matchstr(a:object, '^[~$]\i*')
    let owner = expand(prefix)
    return FugitiveVimPath((len(owner) ? owner : prefix) . strpart(a:object, len(prefix)))
  endif
  let rev = s:Slash(a:object)
  if rev =~# '^$\|^/\|^\%(\a\a\+:\).*\%(//\|::\)' . (has('win32') ? '\|^\a:/' : '')
    return FugitiveVimPath(a:object)
  elseif rev =~# '^\.\.\=\%(/\|$\)'
    return FugitiveVimPath(simplify(getcwd() . '/' . a:object))
  endif
  let dir = a:0 ? a:1 : s:Dir()
  if empty(dir)
    let file = matchstr(a:object, '^\%(:\d:\|[^:]*:\)\zs\%(\.\.\=$\|\.\.\=/.*\|/.*\|\w:/.*\)')
    let dir = FugitiveExtractGitDir(file)
    if empty(dir)
      return ''
    endif
  endif
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
          \ f !~# '^/\%(index$\|index\.lock$\|\w*MSG$\|\w*HEAD$\|logs/\w*HEAD$\|logs$\|rebase-\w\+\)\%(/\|$\)' &&
          \ getftime(FugitiveVimPath(dir . f)) < 0 && getftime(FugitiveVimPath(cdir . f)) >= 0)
      let f = simplify(cdir . f)
    else
      let f = simplify(dir . f)
    endif
  elseif rev ==# ':/'
    let f = tree
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
        return fugitive#Find(a:object, altdir)
      endif
    endif
  elseif rev =~# '^:[0-3]:'
    let f = 'fugitive://' . dir . '//' . rev[1] . '/' . rev[3:-1]
  elseif rev ==# ':'
    if $GIT_INDEX_FILE =~# '/[^/]*index[^/]*\.lock$' && s:cpath(fnamemodify($GIT_INDEX_FILE,':p')[0:strlen(dir)]) ==# s:cpath(dir . '/') && filereadable($GIT_INDEX_FILE)
      let f = fnamemodify($GIT_INDEX_FILE, ':p')
    else
      let f = fugitive#Find('.git/index', dir)
    endif
  elseif rev =~# '^:(\%(top\|top,literal\|literal,top\|literal\))'
    let f = matchstr(rev, ')\zs.*')
    if f=~# '^\.\.\=\%(/\|$\)'
      let f = simplify(getcwd() . '/' . f)
    elseif f !~# '^/\|^\%(\a\a\+:\).*\%(//\|::\)' . (has('win32') ? '\|^\a:/' : '')
      let f = base . '/' . f
    endif
  elseif rev =~# '^:/\@!'
    let f = 'fugitive://' . dir . '//0/' . rev[1:-1]
  else
    if !exists('f')
      let commit = matchstr(rev, '^\%([^:.-]\|\.\.[^/:]\)[^:]*\|^:.*')
      let file = substitute(matchstr(rev, '^\%([^:.-]\|\.\.[^/:]\)[^:]*\zs:.*'), '^:', '/', '')
      if file =~# '^/\.\.\=\%(/\|$\)\|^//\|^/\a\+:'
        let file = file =~# '^/\.' ? simplify(getcwd() . file) : file[1:-1]
        if s:cpath(base . '/', (file . '/')[0 : len(base)])
          let file = '/' . strpart(file, len(base) + 1)
        else
          let altdir = FugitiveExtractGitDir(file)
          if len(altdir) && !s:cpath(dir, altdir)
            return fugitive#Find(a:object, altdir)
          endif
          return file
        endif
      endif
      let commits = split(commit, '\.\.\.-\@!', 1)
      if len(commits) == 2
        call map(commits, 'empty(v:val) ? "@" : v:val')
        let commit = matchstr(s:ChompDefault('', [dir, 'merge-base'] + commits + ['--']), '\<[0-9a-f]\{40,\}\>')
      endif
      if commit !~# '^[0-9a-f]\{40,\}$\|^$'
        let commit = matchstr(s:ChompDefault('', [dir, 'rev-parse', '--verify', commit . (len(file) ? '^{}' : ''), '--']), '\<[0-9a-f]\{40,\}\>')
        if empty(commit) && len(file)
          let commit = repeat('0', 40)
        endif
      endif
      if len(commit)
        let f = 'fugitive://' . dir . '//' . commit . file
      else
        let f = base . '/' . substitute(rev, '^:/:\=\|^[^:]\+:', '', '')
      endif
    endif
  endif
  return FugitiveVimPath(f)
endfunction

function! s:Generate(object, ...) abort
  let dir = a:0 ? a:1 : s:Dir()
  let f = fugitive#Find(a:object, dir)
  if !empty(f)
    return f
  elseif a:object ==# ':/'
    return len(dir) ? FugitiveVimPath('fugitive://' . dir . '//0') : '.'
  endif
  let file = matchstr(a:object, '^\%(:\d:\|[^:]*:\)\zs.*')
  return fnamemodify(FugitiveVimPath(len(file) ? file : a:object), ':p')
endfunction

function! s:DotRelative(path, ...) abort
  let cwd = a:0 ? a:1 : getcwd()
  let path = substitute(a:path, '^[~$]\i*', '\=expand(submatch(0))', '')
  if len(cwd) && s:cpath(cwd . '/', (path . '/')[0 : len(cwd)])
    return '.' . strpart(path, len(cwd))
  endif
  return a:path
endfunction

function! fugitive#Object(...) abort
  let dir = a:0 > 1 ? a:2 : s:Dir()
  let [fdir, rev] = s:DirRev(a:0 ? a:1 : @%)
  if s:cpath(dir) !=# s:cpath(fdir)
    let rev = ''
  endif
  let tree = s:Tree(dir)
  let full = a:0 ? a:1 : s:BufName('%')
  let full = fnamemodify(full, ':p' . (s:Slash(full) =~# '/$' ? '' : ':s?/$??'))
  if empty(rev) && empty(tree)
    return FugitiveGitPath(full)
  elseif empty(rev)
    let rev = fugitive#Path(full, './', dir)
    if rev =~# '^\./.git\%(/\|$\)'
      return FugitiveGitPath(full)
    endif
  endif
  if rev !~# '^\.\%(/\|$\)' || s:cpath(getcwd(), tree)
    return rev
  else
    return FugitiveGitPath(tree . rev[1:-1])
  endif
endfunction

let s:var = '\%(<\%(cword\|cWORD\|cexpr\|cfile\|sfile\|slnum\|afile\|abuf\|amatch' . (has('clientserver') ? '\|client' : '') . '\)>\|%\|#<\=\d\+\|##\=\)'
let s:flag = '\%(:[p8~.htre]\|:g\=s\(.\).\{-\}\1.\{-\}\1\)'
let s:expand = '\%(\(' . s:var . '\)\(' . s:flag . '*\)\(:S\)\=\)'

function! s:BufName(var) abort
  if a:var ==# '%'
    return bufname(get(s:TempState(), 'origin_bufnr', ''))
  elseif a:var =~# '^#\d*$'
    let nr = get(s:TempState(bufname(+a:var[1:-1])), 'origin_bufnr', '')
    return bufname(nr ? nr : +a:var[1:-1])
  else
    return expand(a:var)
  endif
endfunction

function! s:ExpandVarLegacy(str) abort
  if get(g:, 'fugitive_legacy_quoting', 1)
    return substitute(a:str, '\\\ze[%#!]', '', 'g')
  else
    return a:str
  endif
endfunction

function! s:ExpandVar(other, var, flags, esc, ...) abort
  let cwd = a:0 ? a:1 : getcwd()
  if a:other =~# '^\'
    return a:other[1:-1]
  elseif a:other =~# '^'''
    return s:ExpandVarLegacy(substitute(a:other[1:-2], "''", "'", "g"))
  elseif a:other =~# '^"'
    return s:ExpandVarLegacy(substitute(a:other[1:-2], '""', '"', "g"))
  elseif a:other =~# '^!'
    let buffer = s:BufName(len(a:other) > 1 ? '#'. a:other[1:-1] : '%')
    let owner = s:Owner(buffer)
    return len(owner) ? owner : '@'
  elseif a:var ==# '<cfile>'
    let bufname = expand('<cfile>')
    if v:version >= 704 && get(maparg('<Plug><cfile>', 'c', 0, 1), 'expr')
      try
        let bufname = eval(maparg('<Plug><cfile>', 'c'))
        if bufname ==# "\<C-R>\<C-F>"
          let bufname = expand('<cfile>')
        endif
      catch
      endtry
    endif
  elseif a:var =~# '^<'
    let bufname = s:BufName(a:var)
  else
    let bufname = fugitive#Real(s:BufName(a:var))
  endif
  let flags = a:flags
  let file = s:DotRelative(bufname, cwd)
  while len(flags)
    let flag = matchstr(flags, s:flag)
    let flags = strpart(flags, len(flag))
    if flag ==# ':.'
      let file = s:DotRelative(fugitive#Real(file), cwd)
    else
      let file = fnamemodify(file, flag)
    endif
  endwhile
  let file = s:Slash(file)
  if file =~# '^fugitive://'
    let [dir, commit, file_candidate] = s:DirCommitFile(file)
    let tree = s:Tree(dir)
    if len(tree) && len(file_candidate)
      let file = (commit =~# '^.$' ? ':' : '') . commit . ':' .
            \ s:DotRelative(tree . file_candidate)
    elseif empty(file_candidate) && commit !~# '^.$'
      let file = commit
    endif
  endif
  return (len(a:esc) ? shellescape(file) : file)
endfunction

function! s:Expand(rev, ...) abort
  if a:rev =~# '^:[0-3]$'
    let file = len(expand('%')) ? a:rev . ':%' : '%'
  elseif a:rev ==# '>'
    let file = '%'
  elseif a:rev =~# '^>[~^]'
    let file = len(expand('%')) ? '!' . a:rev[1:-1] . ':%' : '%'
  elseif a:rev =~# '^>[> ]\@!'
    let file = len(expand('%')) ? a:rev[1:-1] . ':%' : '%'
  else
    let file = a:rev
  endif
  return substitute(file,
        \ '\(\\[' . s:fnameescape . ']\|^\\[>+-]\|!\d*\)\|' . s:expand,
        \ '\=s:ExpandVar(submatch(1),submatch(2),submatch(3),"", a:0 ? a:1 : getcwd())', 'g')
endfunction

function! fugitive#Expand(object) abort
  return substitute(a:object,
        \ '\(\\[' . s:fnameescape . ']\|^\\[>+-]\|!\d*\)\|' . s:expand,
        \ '\=s:ExpandVar(submatch(1),submatch(2),submatch(3),submatch(5))', 'g')
endfunction

function! s:SplitExpandChain(string, ...) abort
  let list = []
  let string = a:string
  let dquote = '"\%([^"]\|""\|\\"\)*"\|'
  let cwd = a:0 ? a:1 : getcwd()
  while string =~# '\S'
    if string =~# '^\s*|'
      return [list, substitute(string, '^\s*', '', '')]
    endif
    let arg = matchstr(string, '^\s*\%(' . dquote . '''[^'']*''\|\\.\|[^[:space:] |]\)\+')
    let string = strpart(string, len(arg))
    let arg = substitute(arg, '^\s\+', '', '')
    if !exists('seen_separator')
      let arg = substitute(arg, '^\%([^:.][^:]*:\|^:\|^:[0-3]:\)\=\zs\.\.\=\%(/.*\)\=$',
            \ '\=s:DotRelative(s:Slash(simplify(getcwd() . "/" . submatch(0))), cwd)', '')
    endif
    let arg = substitute(arg,
          \ '\(' . dquote . '''\%(''''\|[^'']\)*''\|\\[' . s:fnameescape . ']\|^\\[>+-]\|!\d*\)\|' . s:expand,
          \ '\=s:ExpandVar(submatch(1),submatch(2),submatch(3),submatch(5), cwd)', 'g')
    call add(list, arg)
    if arg ==# '--'
      let seen_separator = 1
    endif
  endwhile
  return [list, '']
endfunction

let s:trees = {}
let s:indexes = {}
function! s:TreeInfo(dir, commit) abort
  if a:commit =~# '^:\=[0-3]$'
    let index = get(s:indexes, a:dir, [])
    let newftime = getftime(fugitive#Find('.git/index', a:dir))
    if get(index, 0, -1) < newftime
      let [lines, exec_error] = s:LinesError([a:dir, 'ls-files', '--stage', '--'])
      let s:indexes[a:dir] = [newftime, {'0': {}, '1': {}, '2': {}, '3': {}}]
      if exec_error
        return [{}, -1]
      endif
      for line in lines
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
  elseif a:commit =~# '^\x\{40,\}$'
    if !has_key(s:trees, a:dir)
      let s:trees[a:dir] = {}
    endif
    if !has_key(s:trees[a:dir], a:commit)
      let [ftime, exec_error] = s:ChompError([a:dir, 'log', '-1', '--pretty=format:%ct', a:commit, '--'])
      if exec_error
        let s:trees[a:dir][a:commit] = [{}, -1]
        return s:trees[a:dir][a:commit]
      endif
      let s:trees[a:dir][a:commit] = [{}, +ftime]
      let [lines, exec_error] = s:LinesError([a:dir, 'ls-tree', '-rtl', '--full-name', a:commit, '--'])
      if exec_error
        return s:trees[a:dir][a:commit]
      endif
      for line in lines
        let [info, filename] = split(line, "\t")
        let [mode, type, sha, size] = split(info, '\s\+')
        let s:trees[a:dir][a:commit][0][filename] = [+ftime, mode, type, sha, +size, filename]
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
  if empty(entry) || file =~# '/$' && entry[2] !=# 'tree'
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
        return FugitiveVimPath(path)
      endif
    endif
  endif
  return FugitiveVimPath('fugitive://' . simplify(dir) . '//' . commit . simplify(file))
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
    let entry[4] = +s:ChompDefault(-1, [dir, 'cat-file', '-s', entry[3]])
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
  if commit !~# '^\d$' || !filewritable(fugitive#Find('.git/index', dir))
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

function! s:UpdateIndex(dir, info) abort
  let info = join(a:info[0:-2]) . "\t" . a:info[-1] . "\n"
  let [error, exec_error] = s:SystemError([a:dir, 'update-index', '--index-info'], info)
  return !exec_error ? '' : len(error) ? error : 'fugitive: unknown update-index error'
endfunction

function! fugitive#setfperm(url, perm) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  let entry = s:PathInfo(a:url)
  let perm = fugitive#getfperm(a:url)
  if commit !~# '^\d$' || entry[2] !=# 'blob' ||
      \ substitute(perm, 'x', '-', 'g') !=# substitute(a:perm, 'x', '-', 'g')
    return -2
  endif
  let error = s:UpdateIndex(dir, [a:perm =~# 'x' ? '000755' : '000644', entry[3], commit, file[1:-1]])
  return len(error) ? -1 : 0
endfunction

function! s:TempCmd(out, cmd) abort
  try
    let cmd = (type(a:cmd) == type([]) ? fugitive#Prepare(a:cmd) : a:cmd)
    let redir = ' > ' . a:out
    if (s:winshell() || &shellcmdflag ==# '-Command') && !has('nvim')
      let cmd_escape_char = &shellxquote == '(' ?  '^' : '^^^'
      return s:SystemError('cmd /c "' . s:gsub(cmd, '[<>%]', cmd_escape_char . '&') . redir . '"')
    elseif &shell =~# 'fish'
      return s:SystemError(' begin;' . cmd . redir . ';end ')
    else
      return s:SystemError(' (' . cmd . redir . ') ')
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
    let exec_error = s:TempCmd(tempfile, [dir, 'cat-file', 'blob', rev])[1]
    if exec_error
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
    let [hash, exec_error] = s:ChompError([dir, 'hash-object', '-w', temp])
    let mode = len(entry[1]) ? entry[1] : '100644'
    if !exec_error && hash =~# '^\x\{40,\}$'
      let error = s:UpdateIndex(dir, [mode, hash, commit, file[1:-1]])
      if empty(error)
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
  let pattern = '^' . substitute(glob, '/\=\*\*/\=\|/\=\*\|[.?\$]\|^^', '\=get(s:globsubs, submatch(0), "\\" . submatch(0))', 'g')[1:-1] . '$'
  let results = []
  for dir in dirglob =~# '[*?]' ? split(glob(dirglob), "\n") : [dirglob]
    if empty(dir) || !get(g:, 'fugitive_file_api', 1) || !filereadable(fugitive#Find('.git/HEAD', dir))
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
    call map(files, 'FugitiveVimPath(prepend . v:val . append)')
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
  let error = s:UpdateIndex(dir, ['000000', '0000000000000000000000000000000000000000', commit, file[1:-1]])
  return len(error) ? -1 : 0
endfunction

" Section: Buffer Object

let s:buffer_prototype = {}

function! fugitive#buffer(...) abort
  let buffer = {'#': bufnr(a:0 ? a:1 : '%')}
  call extend(buffer, s:buffer_prototype, 'keep')
  return buffer
endfunction

function! s:buffer_repo() dict abort
  return fugitive#repo(self['#'])
endfunction

function! s:buffer_type(...) dict abort
  return 'see b:fugitive_type'
endfunction

call s:add_methods('buffer', ['repo', 'type'])

" Section: Completion

function! s:FilterEscape(items, ...) abort
  let items = copy(a:items)
  call map(items, 's:fnameescape(v:val)')
  if a:0 && type(a:1) == type('')
    let cmp = s:FileIgnoreCase(1) ? '==?' : '==#'
    call filter(items, 'strpart(v:val, 0, strlen(a:1)) ' . cmp . ' a:1')
  endif
  return items
endfunction

function! s:GlobComplete(lead, pattern, ...) abort
  if a:lead ==# '/'
    return []
  elseif v:version >= 704
    let results = glob(a:lead . a:pattern, a:0 ? a:1 : 0, 1)
  else
    let results = split(glob(a:lead . a:pattern), "\n")
  endif
  call map(results, 'v:val !~# "/$" && isdirectory(v:val) ? v:val."/" : v:val')
  call map(results, 'v:val[ strlen(a:lead) : -1 ]')
  return results
endfunction

function! fugitive#CompletePath(base, ...) abort
  let dir = a:0 == 1 ? a:1 : a:0 >= 3 ? a:3 : s:Dir()
  let stripped = matchstr(a:base, '^\%(:/:\=\|:(top)\|:(top,literal)\|:(literal,top)\)')
  let base = strpart(a:base, len(stripped))
  if len(stripped) || a:0 < 4
    let root = s:Tree(dir)
  else
    let root = a:4
  endif
  if root !=# '/' && len(root)
    let root .= '/'
  endif
  if empty(stripped)
    let stripped = matchstr(a:base, '^\%(:(literal)\|:\)')
    let base = strpart(a:base, len(stripped))
  endif
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
  elseif a:base =~# '^/\|^\a\+:\|^\.\.\=/'
    let matches = s:GlobComplete('', base . '*')
  elseif len(root)
    let matches = s:GlobComplete(root, s:gsub(base, '/', '*&').'*')
  else
    let matches = []
  endif
  call map(matches, 's:fnameescape(s:Slash(stripped . v:val))')
  return matches
endfunction

function! fugitive#PathComplete(...) abort
  return call('fugitive#CompletePath', a:000)
endfunction

function! s:CompleteHeads(dir) abort
  if empty(a:dir)
    return []
  endif
  let dir = fugitive#Find('.git/', a:dir)
  return sort(filter(['HEAD', 'FETCH_HEAD', 'ORIG_HEAD'] + s:merge_heads, 'filereadable(dir . v:val)')) +
        \ sort(s:LinesError([a:dir, 'rev-parse', '--symbolic', '--branches', '--tags', '--remotes'])[0])
endfunction

function! fugitive#CompleteObject(base, ...) abort
  let dir = a:0 == 1 ? a:1 : a:0 >= 3 ? a:3 : s:Dir()
  let tree = s:Tree(dir)
  let cwd = getcwd()
  let subdir = ''
  if len(tree) && s:cpath(tree . '/', cwd[0 : len(tree)])
    let subdir = strpart(cwd, len(tree) + 1) . '/'
  endif

  if a:base =~# '^\.\=/\|^:(' || a:base !~# ':'
    let results = []
    if a:base =~# '^refs/'
      let results += map(s:GlobComplete(fugitive#CommonDir(dir) . '/', a:base . '*'), 's:Slash(v:val)')
      call map(results, 's:fnameescape(v:val)')
    elseif a:base !~# '^\.\=/\|^:('
      let heads = s:CompleteHeads(dir)
      if filereadable(fugitive#Find('.git/refs/stash', dir))
        let heads += ["stash"]
        let heads += sort(s:LinesError(["stash","list","--pretty=format:%gd"], dir)[0])
      endif
      let results += s:FilterEscape(heads, a:base)
    endif
    let results += a:0 == 1 || a:0 >= 3 ? fugitive#CompletePath(a:base, 0, '', dir, a:0 >= 4 ? a:4 : tree) : fugitive#CompletePath(a:base)
    return results

  elseif a:base =~# '^:'
    let entries = s:LinesError(['ls-files','--stage'], dir)[0]
    if a:base =~# ':\./'
      call map(entries, 'substitute(v:val, "\\M\t\\zs" . subdir, "./", "")')
    endif
    call map(entries,'s:sub(v:val,".*(\\d)\\t(.*)",":\\1:\\2")')
    if a:base !~# '^:[0-3]\%(:\|$\)'
      call filter(entries,'v:val[1] == "0"')
      call map(entries,'v:val[2:-1]')
    endif

  else
    let parent = matchstr(a:base, '.*[:/]')
    let entries = s:LinesError(['ls-tree', substitute(parent,  ':\zs\./', '\=subdir', '')], dir)[0]
    call map(entries,'s:sub(v:val,"^04.*\\zs$","/")')
    call map(entries,'parent.s:sub(v:val,".*\t","")')
  endif
  return s:FilterEscape(entries, a:base)
endfunction

function! s:CompleteSub(subcommand, A, L, P, ...) abort
  let pre = strpart(a:L, 0, a:P)
  if pre =~# ' -- '
    return fugitive#CompletePath(a:A)
  elseif a:A =~# '^-' || a:A is# 0
    return s:FilterEscape(split(s:ChompDefault('', a:subcommand, '--git-completion-helper'), ' '), a:A)
  elseif !a:0
    return fugitive#CompleteObject(a:A, s:Dir())
  elseif type(a:1) == type(function('tr'))
    return call(a:1, [a:A, a:L, a:P] + (a:0 > 1 ? a:2 : []))
  else
    return s:FilterEscape(a:1, a:A)
  endif
endfunction

function! s:CompleteRevision(A, L, P, ...) abort
  return s:FilterEscape(s:CompleteHeads(a:0 ? a:1 : s:Dir()), a:A)
endfunction

function! s:CompleteRemote(A, L, P, ...) abort
  let dir = a:0 ? a:1 : s:Dir()
  let remote = matchstr(a:L, '\u\w*[! ] *.\{-\}\s\@<=\zs[^-[:space:]]\S*\ze ')
  if !empty(remote)
    let matches = s:LinesError([dir, 'ls-remote', remote])[0]
    call filter(matches, 'v:val =~# "\t" && v:val !~# "{"')
    call map(matches, 's:sub(v:val, "^.*\t%(refs/%(heads/|tags/)=)=", "")')
  else
    let matches = s:LinesError([dir, 'remote'])[0]
  endif
  return s:FilterEscape(matches, a:A)
endfunction

" Section: Buffer auto-commands

function! s:ReplaceCmd(cmd) abort
  let temp = tempname()
  let [err, exec_error] = s:TempCmd(temp, a:cmd)
  if exec_error
    call s:throw((len(err) ? err : 'unknown error running ' . a:cmd))
  endif
  setlocal noswapfile
  silent exe 'lockmarks keepalt 0read ++edit' s:fnameescape(temp)
  if &foldenable && foldlevel('$') > 0
    set nofoldenable
    silent keepjumps $delete _
    set foldenable
  else
    silent keepjumps $delete _
  endif
  call delete(temp)
  if s:cpath(fnamemodify(bufname('$'), ':p'), temp)
    silent! execute bufnr('$') . 'bwipeout'
  endif
endfunction

function! s:QueryLog(refspec) abort
  let lines = s:LinesError(['log', '-n', '256', '--pretty=format:%h%x09%s', a:refspec, '--'])[0]
  call map(lines, 'split(v:val, "\t", 1)')
  call map(lines, '{"type": "Log", "commit": v:val[0], "subject": join(v:val[1 : -1], "\t")}')
  return lines
endfunction

function! s:FormatLog(dict) abort
  return a:dict.commit . ' ' . a:dict.subject
endfunction

function! s:FormatRebase(dict) abort
  return a:dict.status . ' ' . a:dict.commit . ' ' . a:dict.subject
endfunction

function! s:FormatFile(dict) abort
  return a:dict.status . ' ' . a:dict.filename
endfunction

function! s:Format(val) abort
  if type(a:val) == type({})
    return s:Format{a:val.type}(a:val)
  elseif type(a:val) == type([])
    return map(copy(a:val), 's:Format(v:val)')
  else
    return '' . a:val
  endif
endfunction

function! s:AddHeader(key, value) abort
  if empty(a:value)
    return
  endif
  let before = 1
  while !empty(getline(before))
    let before += 1
  endwhile
  call append(before - 1, [a:key . ':' . (len(a:value) ? ' ' . a:value : '')])
  if before == 1 && line('$') == 2
    silent keepjumps 2delete _
  endif
endfunction

function! s:AddSection(label, lines, ...) abort
  let note = a:0 ? a:1 : ''
  if empty(a:lines) && empty(note)
    return
  endif
  call append(line('$'), ['', a:label . (len(note) ? ': ' . note : ' (' . len(a:lines) . ')')] + s:Format(a:lines))
endfunction

let s:rebase_abbrevs = {
      \ 'p': 'pick',
      \ 'r': 'reword',
      \ 'e': 'edit',
      \ 's': 'squash',
      \ 'f': 'fixup',
      \ 'x': 'exec',
      \ 'd': 'drop',
      \ 'l': 'label',
      \ 't': 'reset',
      \ 'm': 'merge',
      \ 'b': 'break',
      \ }

function! fugitive#BufReadStatus() abort
  let amatch = s:Slash(expand('%:p'))
  let b:fugitive_type = 'index'
  unlet! b:fugitive_reltime
  try
    silent doautocmd BufReadPre
    let config = fugitive#Config()

    let cmd = [fnamemodify(amatch, ':h')]
    setlocal noro ma nomodeline buftype=nowrite
    if s:cpath(fnamemodify($GIT_INDEX_FILE !=# '' ? $GIT_INDEX_FILE : fugitive#Find('.git/index'), ':p')) !=# s:cpath(amatch)
      let cmd += ['-c', 'GIT_INDEX_FILE=' . amatch]
    endif

    if fugitive#GitVersion(2, 15)
      call add(cmd, '--no-optional-locks')
    endif

    let b:fugitive_files = {'Staged': {}, 'Unstaged': {}}
    let [staged, unstaged, untracked] = [[], [], []]
    let props = {}

    let pull = ''
    if empty(s:Tree())
      let branch = FugitiveHead(0)
      let head = FugitiveHead(11)
    elseif fugitive#GitVersion(2, 11)
      let cmd += ['status', '--porcelain=v2', '-bz']
      let [output, message, exec_error] = s:NullError(cmd)
      if exec_error
        throw 'fugitive: ' . message
      endif

      let i = 0
      while i < len(output)
        let line = output[i]
        let prop = matchlist(line, '# \(\S\+\) \(.*\)')
        if len(prop)
          let props[prop[1]] = prop[2]
        elseif line[0] ==# '?'
          call add(untracked, {'type': 'File', 'status': line[0], 'filename': line[2:-1]})
        elseif line[0] !=# '#'
          if line[0] ==# 'u'
            let file = matchstr(line, '^.\{37\} \x\{40,\} \x\{40,\} \x\{40,\} \zs.*$')
          else
            let file = matchstr(line, '^.\{30\} \x\{40,\} \x\{40,\} \zs.*$')
          endif
          if line[0] ==# '2'
            let i += 1
            let file = matchstr(file, ' \zs.*')
            let files = output[i] . ' -> ' . file
          else
            let files = file
          endif
          let sub = matchstr(line, '^[12u] .. \zs....')
          if line[2] !=# '.'
            call add(staged, {'type': 'File', 'status': line[2], 'filename': files, 'submodule': sub})
          endif
          if line[3] !=# '.'
            let sub = matchstr(line, '^[12u] .. \zs....')
            call add(unstaged, {'type': 'File', 'status': get({'C':'M','M':'?','U':'?'}, matchstr(sub, 'S\.*\zs[CMU]'), line[3]), 'filename': file, 'submodule': sub})
          endif
        endif
        let i += 1
      endwhile
      let branch = substitute(get(props, 'branch.head', '(unknown)'), '\C^(\%(detached\|unknown\))$', '', '')
      if len(branch)
        let head = branch
      elseif has_key(props, 'branch.oid')
        let head = props['branch.oid'][0:10]
      else
        let head = FugitiveHead(11)
      endif
      let pull = get(props, 'branch.upstream', '')
    else " git < 2.11
      let cmd += ['status', '--porcelain', '-bz']
      let [output, message, exec_error] = s:NullError(cmd)
      if exec_error
        throw 'fugitive: ' . message
      endif

      while get(output, 0, '') =~# '^\l\+:'
        call remove(output, 0)
      endwhile
      let head = matchstr(output[0], '^## \zs\S\+\ze\%($\| \[\)')
      if head =~# '\.\.\.'
        let [head, pull] = split(head, '\.\.\.')
        let branch = head
      elseif head ==# 'HEAD' || empty(head)
        let head = FugitiveHead(11)
        let branch = ''
      else
        let branch = head
      endif

      let i = 0
      while i < len(output)
        let line = output[i]
        let file = line[3:-1]
        let files = file
        let i += 1
        if line[2] !=# ' '
          continue
        endif
        if line[0:1] =~# '[RC]'
          let files = output[i] . ' -> ' . file
          let i += 1
        endif
        if line[0] !~# '[ ?!#]'
          call add(staged, {'type': 'File', 'status': line[0], 'filename': files, 'submodule': ''})
        endif
        if line[0:1] ==# '??'
          call add(untracked, {'type': 'File', 'status': line[1], 'filename': files})
        elseif line[1] !~# '[ !#]'
          call add(unstaged, {'type': 'File', 'status': line[1], 'filename': file, 'submodule': ''})
        endif
      endwhile
    endif

    for dict in staged
      let b:fugitive_files['Staged'][dict.filename] = dict
    endfor
    for dict in unstaged
      let b:fugitive_files['Unstaged'][dict.filename] = dict
    endfor

    let pull_type = 'Pull'
    if len(pull)
      let rebase = FugitiveConfigGet('branch.' . branch . '.rebase', config)
      if empty(rebase)
        let rebase = FugitiveConfigGet('pull.rebase', config)
      endif
      if rebase =~# '^\%(true\|yes\|on\|1\|interactive\|merges\|preserve\)$'
        let pull_type = 'Rebase'
      elseif rebase =~# '^\%(false\|no|off\|0\|\)$'
        let pull_type = 'Merge'
      endif
    endif

    let push_remote = FugitiveConfigGet('branch.' . branch . '.pushRemote', config)
    if empty(push_remote)
      let push_remote = FugitiveConfigGet('remote.pushDefault', config)
    endif
    let fetch_remote = FugitiveConfigGet('branch.' . branch . '.remote', config)
    if empty(fetch_remote)
      let fetch_remote = 'origin'
    endif
    if empty(push_remote)
      let push_remote = fetch_remote
    endif

    let push_default = FugitiveConfigGet('push.default', config)
    if empty(push_default)
      let push_default = fugitive#GitVersion(2) ? 'simple' : 'matching'
    endif
    if push_default ==# 'upstream'
      let push = pull
    else
      let push = len(branch) ? (push_remote ==# '.' ? '' : push_remote . '/') . branch : ''
    endif

    if isdirectory(fugitive#Find('.git/rebase-merge/'))
      let rebasing_dir = fugitive#Find('.git/rebase-merge/')
    elseif isdirectory(fugitive#Find('.git/rebase-apply/'))
      let rebasing_dir = fugitive#Find('.git/rebase-apply/')
    endif

    let rebasing = []
    let rebasing_head = 'detached HEAD'
    if exists('rebasing_dir') && filereadable(rebasing_dir . 'git-rebase-todo')
      let rebasing_head = substitute(readfile(rebasing_dir . 'head-name')[0], '\C^refs/heads/', '', '')
      let len = 11
      let lines = readfile(rebasing_dir . 'git-rebase-todo')
      for line in lines
        let hash = matchstr(line, '^[^a-z].*\s\zs[0-9a-f]\{4,\}\ze\.\.')
        if len(hash)
          let len = len(hash)
          break
        endif
      endfor
      if getfsize(rebasing_dir . 'done') > 0
        let done = readfile(rebasing_dir . 'done')
        call map(done, 'substitute(v:val, ''^\l\+\>'', "done", "")')
        let done[-1] = substitute(done[-1], '^\l\+\>', 'stop', '')
        let lines = done + lines
      endif
      call reverse(lines)
      for line in lines
        let match = matchlist(line, '^\(\l\+\)\s\+\(\x\{4,\}\)\s\+\(.*\)')
        if len(match) && match[1] !~# 'exec\|merge\|label'
          call add(rebasing, {'type': 'Rebase', 'status': get(s:rebase_abbrevs, match[1], match[1]), 'commit': strpart(match[2], 0, len), 'subject': match[3]})
        endif
      endfor
    endif

    let diff = {'Staged': [], 'Unstaged': []}
    if len(staged)
      let diff['Staged'] =
          \ s:LinesError(['diff', '--color=never', '--no-ext-diff', '--no-prefix', '--cached'])[0]
    endif
    if len(unstaged)
      let diff['Unstaged'] =
          \ s:LinesError(['diff', '--color=never', '--no-ext-diff', '--no-prefix'])[0]
    endif
    let b:fugitive_diff = diff
    let expanded = get(b:, 'fugitive_expanded', {'Staged': {}, 'Unstaged': {}})
    let b:fugitive_expanded = {'Staged': {}, 'Unstaged': {}}

    silent keepjumps %delete_

    call s:AddHeader('Head', head)
    call s:AddHeader(pull_type, pull)
    if push !=# pull
      call s:AddHeader('Push', push)
    endif
    if empty(s:Tree())
      call s:AddHeader('Bare', 'yes')
    endif
    if get(fugitive#ConfigGetAll('advice.statusHints', config), 0, 'true') !~# '^\%(false\|no|off\|0\|\)$'
      call s:AddHeader('Help', 'g?')
    endif

    call s:AddSection('Rebasing ' . rebasing_head, rebasing)
    call s:AddSection('Untracked', untracked)
    call s:AddSection('Unstaged', unstaged)
    let unstaged_end = len(unstaged) ? line('$') : 0
    call s:AddSection('Staged', staged)
    let staged_end = len(staged) ? line('$') : 0

    if len(pull) && get(props, 'branch.ab') !~# ' -0$'
      call s:AddSection('Unpulled from ' . pull, s:QueryLog(head . '..' . pull))
    endif
    if len(push) && push !=# pull
      call s:AddSection('Unpulled from ' . push, s:QueryLog(head . '..' . push))
    endif
    if len(pull) && push !=# pull
      call s:AddSection('Unpushed to ' . pull, s:QueryLog(pull . '..' . head))
    endif
    if len(push) && !(push ==# pull && get(props, 'branch.ab') =~# '^+0 ')
      call s:AddSection('Unpushed to ' . push, s:QueryLog(push . '..' . head))
    endif

    setlocal nomodified readonly noswapfile
    silent doautocmd BufReadPost
    setlocal nomodifiable
    if &bufhidden ==# ''
      setlocal bufhidden=delete
    endif
    let b:dispatch = '-dir=' . fnameescape(len(s:Tree()) ? s:Tree() : s:Dir()) . ' ' . s:GitShellCmd() . ' fetch --all'
    call fugitive#MapJumps()
    call s:Map('n', '-', ":<C-U>execute <SID>Do('Toggle',0)<CR>", '<silent>')
    call s:Map('x', '-', ":<C-U>execute <SID>Do('Toggle',1)<CR>", '<silent>')
    call s:Map('n', 's', ":<C-U>execute <SID>Do('Stage',0)<CR>", '<silent>')
    call s:Map('x', 's', ":<C-U>execute <SID>Do('Stage',1)<CR>", '<silent>')
    call s:Map('n', 'u', ":<C-U>execute <SID>Do('Unstage',0)<CR>", '<silent>')
    call s:Map('x', 'u', ":<C-U>execute <SID>Do('Unstage',1)<CR>", '<silent>')
    call s:Map('n', 'U', ":exe <SID>EchoExec('reset', '-q')<CR>", '<silent>')
    call s:MapMotion('gu', "exe <SID>StageJump(v:count, 'Untracked', 'Unstaged')")
    call s:MapMotion('gU', "exe <SID>StageJump(v:count, 'Unstaged', 'Untracked')")
    call s:MapMotion('gs', "exe <SID>StageJump(v:count, 'Staged')")
    call s:MapMotion('gp', "exe <SID>StageJump(v:count, 'Unpushed')")
    call s:MapMotion('gP', "exe <SID>StageJump(v:count, 'Unpulled')")
    call s:MapMotion('gr', "exe <SID>StageJump(v:count, 'Rebasing')")
    call s:Map('n', 'C', ":echoerr 'fugitive: C has been removed in favor of cc'<CR>", '<silent>')
    call s:Map('n', 'a', ":<C-U>execute <SID>Do('Toggle',0)<CR>", '<silent>')
    call s:Map('n', 'i', ":<C-U>execute <SID>NextExpandedHunk(v:count1)<CR>", '<silent>')
    call s:Map('n', "=", ":<C-U>execute <SID>StageInline('toggle',line('.'),v:count)<CR>", '<silent>')
    call s:Map('n', "<", ":<C-U>execute <SID>StageInline('hide',  line('.'),v:count)<CR>", '<silent>')
    call s:Map('n', ">", ":<C-U>execute <SID>StageInline('show',  line('.'),v:count)<CR>", '<silent>')
    call s:Map('x', "=", ":<C-U>execute <SID>StageInline('toggle',line(\"'<\"),line(\"'>\")-line(\"'<\")+1)<CR>", '<silent>')
    call s:Map('x', "<", ":<C-U>execute <SID>StageInline('hide',  line(\"'<\"),line(\"'>\")-line(\"'<\")+1)<CR>", '<silent>')
    call s:Map('x', ">", ":<C-U>execute <SID>StageInline('show',  line(\"'<\"),line(\"'>\")-line(\"'<\")+1)<CR>", '<silent>')
    call s:Map('n', 'D', ":echoerr 'fugitive: D has been removed in favor of dd'<CR>", '<silent>')
    call s:Map('n', 'dd', ":<C-U>execute <SID>StageDiff('Gdiffsplit')<CR>", '<silent>')
    call s:Map('n', 'dh', ":<C-U>execute <SID>StageDiff('Ghdiffsplit')<CR>", '<silent>')
    call s:Map('n', 'ds', ":<C-U>execute <SID>StageDiff('Ghdiffsplit')<CR>", '<silent>')
    call s:Map('n', 'dp', ":<C-U>execute <SID>StageDiffEdit()<CR>", '<silent>')
    call s:Map('n', 'dv', ":<C-U>execute <SID>StageDiff('Gvdiffsplit')<CR>", '<silent>')
    call s:Map('n', 'd?', ":<C-U>help fugitive_d<CR>", '<silent>')
    call s:Map('n', 'P', ":<C-U>execute <SID>StagePatch(line('.'),line('.')+v:count1-1)<CR>", '<silent>')
    call s:Map('x', 'P', ":<C-U>execute <SID>StagePatch(line(\"'<\"),line(\"'>\"))<CR>", '<silent>')
    call s:Map('n', 'p', ":<C-U>if v:count<Bar>silent exe <SID>GF('pedit')<Bar>else<Bar>echoerr 'Use = for inline diff, P for :Git add/reset --patch, 1p for :pedit'<Bar>endif<CR>", '<silent>')
    call s:Map('x', 'p', ":<C-U>execute <SID>StagePatch(line(\"'<\"),line(\"'>\"))<CR>", '<silent>')
    call s:Map('n', 'I', ":<C-U>execute <SID>StagePatch(line('.'),line('.'))<CR>", '<silent>')
    call s:Map('x', 'I', ":<C-U>execute <SID>StagePatch(line(\"'<\"),line(\"'>\"))<CR>", '<silent>')
    if empty(mapcheck('q', 'n'))
      nnoremap <buffer> <silent> q :echoerr "fugitive: q removed in favor of gq (or :q)"<CR>
    endif
    call s:Map('n', 'gq', ":<C-U>if bufnr('$') == 1<Bar>quit<Bar>else<Bar>bdelete<Bar>endif<CR>", '<silent>')
    call s:Map('n', 'R', ":echohl WarningMsg<Bar>echo 'Reloading is automatic.  Use :e to force'<Bar>echohl NONE<CR>", '<silent>')
    call s:Map('n', 'g<Bar>', ":<C-U>echoerr 'Changed to X'<CR>", '<silent>')
    call s:Map('x', 'g<Bar>', ":<C-U>echoerr 'Changed to X'<CR>", '<silent>')
    call s:Map('n', 'X', ":<C-U>execute <SID>StageDelete(line('.'), 0, v:count)<CR>", '<silent>')
    call s:Map('x', 'X', ":<C-U>execute <SID>StageDelete(line(\"'<\"), line(\"'>\"), v:count)<CR>", '<silent>')
    call s:Map('n', 'gI', ":<C-U>execute <SID>StageIgnore(line('.'), line('.'), v:count)<CR>", '<silent>')
    call s:Map('x', 'gI', ":<C-U>execute <SID>StageIgnore(line(\"'<\"), line(\"'>\"), v:count)<CR>", '<silent>')
    call s:Map('n', '.', ':<C-U> <C-R>=<SID>StageArgs(0)<CR><Home>')
    call s:Map('x', '.', ':<C-U> <C-R>=<SID>StageArgs(1)<CR><Home>')
    setlocal filetype=fugitive

    for [lnum, section] in [[staged_end, 'Staged'], [unstaged_end, 'Unstaged']]
      while len(getline(lnum))
        let filename = matchstr(getline(lnum), '^[A-Z?] \zs.*')
        if has_key(expanded[section], filename)
          call s:StageInline('show', lnum)
        endif
        let lnum -= 1
      endwhile
    endfor

    let b:fugitive_reltime = reltime()
    return 'silent ' . s:DoAutocmd('User FugitiveIndex')
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
endfunction

function! fugitive#FileReadCmd(...) abort
  let amatch = a:0 ? a:1 : expand('<amatch>')
  let [dir, rev] = s:DirRev(amatch)
  let line = a:0 > 1 ? a:2 : line("'[")
  if empty(dir)
    return 'noautocmd ' . line . 'read ' . s:fnameescape(amatch)
  endif
  if rev !~# ':' && s:ChompDefault('', [dir, 'cat-file', '-t', rev]) =~# '^\%(commit\|tag\)$'
    let cmd = fugitive#Prepare(dir, 'log', '--pretty=format:%B', '-1', rev, '--')
  else
    let cmd = fugitive#Prepare(dir, 'cat-file', '-p', rev)
  endif
  return line . 'read !' . escape(cmd, '!#%')
endfunction

function! fugitive#FileWriteCmd(...) abort
  let tmp = tempname()
  let amatch = a:0 ? a:1 : expand('<amatch>')
  let autype = a:0 > 1 ? 'Buf' : 'File'
  if exists('#' . autype . 'WritePre')
    execute s:DoAutocmd(autype . 'WritePre ' . s:fnameescape(amatch))
  endif
  try
    let [dir, commit, file] = s:DirCommitFile(amatch)
    if commit !~# '^[0-3]$' || !v:cmdbang && (line("'[") != 1 || line("']") != line('$'))
      return "noautocmd '[,']write" . (v:cmdbang ? '!' : '') . ' ' . s:fnameescape(amatch)
    endif
    if exists('+guioptions') && &guioptions =~# '!'
      let guioptions = &guioptions
      set guioptions-=!
    endif
    silent execute "'[,']write !".fugitive#Prepare(dir, 'hash-object', '-w', '--stdin', '--').' > '.tmp
    let sha1 = readfile(tmp)[0]
    let old_mode = matchstr(s:SystemError([dir, 'ls-files', '--stage', '.' . file])[0], '^\d\+')
    if empty(old_mode)
      let old_mode = executable(s:Tree(dir) . file) ? '100755' : '100644'
    endif
    let error = s:UpdateIndex(dir, [old_mode, sha1, commit, file[1:-1]])
    if empty(error)
      setlocal nomodified
      if exists('#' . autype . 'WritePost')
        execute s:DoAutocmd(autype . 'WritePost ' . s:fnameescape(amatch))
      endif
      return ''
    else
      return 'echoerr '.string('fugitive: '.error)
    endif
  finally
    if exists('guioptions')
      let &guioptions = guioptions
    endif
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
    let b:git_dir = dir
    if rev =~# '^:\d$'
      let b:fugitive_type = 'stage'
    else
      let [b:fugitive_type, exec_error] = s:ChompError([dir, 'cat-file', '-t', rev])
      if exec_error && rev =~# '^:0'
        let sha = s:ChompDefault('', dir, 'write-tree', '--prefix=' . rev[3:-1])
        let exec_error = empty(sha)
        let b:fugitive_type = exec_error ? '' : 'tree'
      endif
      if exec_error
        let error = b:fugitive_type
        unlet b:fugitive_type
        setlocal noswapfile
        if empty(&bufhidden)
          setlocal bufhidden=delete
        endif
        if rev =~# '^:\d:'
          let &l:readonly = !filewritable(fugitive#Find('.git/index', dir))
          return 'silent doautocmd BufNewFile'
        else
          setlocal readonly nomodifiable
          return 'silent doautocmd BufNewFile|echo ' . string(error)
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
      if b:fugitive_type !=# 'blob'
        setlocal foldmarker=<<<<<<<<,>>>>>>>>
      endif
      silent exe s:DoAutocmd('BufReadPre')
      if b:fugitive_type ==# 'tree'
        let b:fugitive_display_format = b:fugitive_display_format % 2
        if b:fugitive_display_format
          call s:ReplaceCmd([dir, 'ls-tree', exists('sha') ? sha : rev])
        else
          if !exists('sha')
            let sha = s:TreeChomp(dir, 'rev-parse', '--verify', rev, '--')
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
          call s:ReplaceCmd([dir, '-c', 'diff.noprefix=false', 'show', '--no-color', '-m', '--first-parent', '--pretty=format:tree%x20%T%nparent%x20%P%nauthor%x20%an%x20<%ae>%x20%ad%ncommitter%x20%cn%x20<%ce>%x20%cd%nencoding%x20%e%n%n%s%n%n%b', rev])
          keepjumps 1
          keepjumps call search('^parent ')
          if getline('.') ==# 'parent '
            silent lockmarks keepjumps delete_
          else
            silent exe (exists(':keeppatterns') ? 'keeppatterns' : '') 'keepjumps s/\m\C\%(^parent\)\@<! /\rparent /e' . (&gdefault ? '' : 'g')
          endif
          keepjumps let lnum = search('^encoding \%(<unknown>\)\=$','W',line('.')+3)
          if lnum
            silent lockmarks keepjumps delete_
          end
          silent exe (exists(':keeppatterns') ? 'keeppatterns' : '') 'keepjumps 1,/^diff --git\|\%$/s/\r$//e'
          keepjumps 1
        endif
      elseif b:fugitive_type ==# 'stage'
        call s:ReplaceCmd([dir, 'ls-files', '--stage'])
      elseif b:fugitive_type ==# 'blob'
        call s:ReplaceCmd([dir, 'cat-file', b:fugitive_type, rev])
      endif
    finally
      keepjumps call setpos('.',pos)
      setlocal nomodified noswapfile
      let modifiable = rev =~# '^:.:' && b:fugitive_type !=# 'tree'
      let &l:readonly = !modifiable || !filewritable(fugitive#Find('.git/index', dir))
      if empty(&bufhidden)
        setlocal bufhidden=delete
      endif
      let &l:modifiable = modifiable
      if b:fugitive_type !=# 'blob'
        setlocal filetype=git
        call s:Map('n', 'a', ":<C-U>let b:fugitive_display_format += v:count1<Bar>exe fugitive#BufReadCmd(@%)<CR>", '<silent>')
        call s:Map('n', 'i', ":<C-U>let b:fugitive_display_format -= v:count1<Bar>exe fugitive#BufReadCmd(@%)<CR>", '<silent>')
      endif
      call fugitive#MapJumps()
    endtry

    setlocal modifiable

    return 'silent ' . s:DoAutocmd('BufReadPost') .
          \ (modifiable ? '' : '|setl nomodifiable') . '|silent ' .
          \ s:DoAutocmd('User Fugitive' . substitute(b:fugitive_type, '^\l', '\u&', ''))
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
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

function! s:TempState(...) abort
  return get(s:temp_files, s:cpath(fnamemodify(a:0 ? a:1 : @%, ':p')), {})
endfunction

function! fugitive#Result(...) abort
  if !a:0 && exists('g:fugitive_event')
    return get(g:, 'fugitive_result', {})
  elseif !a:0 || type(a:1) == type('') && a:1 =~# '^-\=$'
    return get(g:, '_fugitive_last_job', {})
  elseif type(a:1) == type(0)
    return s:TempState(bufname(a:1))
  elseif type(a:1) == type('')
    return s:TempState(a:1)
  elseif type(a:1) == type({}) && has_key(a:1, 'file')
    return s:TempState(a:1.file)
  else
    return {}
  endif
endfunction

function! s:TempReadPre(file) abort
  if has_key(s:temp_files, s:cpath(a:file))
    let dict = s:temp_files[s:cpath(a:file)]
    setlocal nomodeline
    if empty(&bufhidden)
      setlocal bufhidden=delete
    endif
    setlocal buftype=nowrite
    setlocal nomodifiable
    let b:git_dir = dict.dir
    if len(dict.dir)
      call extend(b:, {'fugitive_type': 'temp'}, 'keep')
    endif
  endif
endfunction

function! s:TempReadPost(file) abort
  if has_key(s:temp_files, s:cpath(a:file))
    let dict = s:temp_files[s:cpath(a:file)]
    if !has_key(dict, 'job')
      setlocal nobuflisted
    endif
    if get(dict, 'filetype', '') ==# 'git'
      call fugitive#MapJumps()
    endif
    if has_key(dict, 'filetype')
      let &l:filetype = dict.filetype
    endif
    setlocal foldmarker=<<<<<<<<,>>>>>>>>
    if !&modifiable
      if empty(mapcheck('q', 'n'))
        nnoremap <buffer> <silent> q    :<C-U>echoerr "fugitive: q is removed in favor of gq (or :q)"<CR>
      endif
      call s:Map('n', 'gq', ":<C-U>bdelete<CR>", '<silent> <unique>')
    endif
  endif
  return ''
endfunction

function! s:TempDelete(file) abort
  let key = s:cpath(a:file)
  if has_key(s:temp_files, key) && !has_key(s:temp_files[key], 'job') && key !=# s:cpath(get(get(g:, '_fugitive_last_job', {}), 'file', ''))
    call delete(a:file)
    call remove(s:temp_files, key)
  endif
  return ''
endfunction

augroup fugitive_temp
  autocmd!
  autocmd BufReadPre  * exe s:TempReadPre( expand('<amatch>:p'))
  autocmd BufReadPost * exe s:TempReadPost(expand('<amatch>:p'))
  autocmd BufWipeout  * exe s:TempDelete(  expand('<amatch>:p'))
augroup END

" Section: :Git

function! s:AskPassArgs(dir) abort
  if (len($DISPLAY) || len($TERM_PROGRAM) || has('gui_running')) &&
        \ empty($GIT_ASKPASS) && empty($SSH_ASKPASS) && empty(fugitive#ConfigGetAll('core.askpass', a:dir))
    if s:executable(s:ExecPath() . '/git-gui--askpass')
      return ['-c', 'core.askPass=' . s:ExecPath() . '/git-gui--askpass']
    elseif s:executable('ssh-askpass')
      return ['-c', 'core.askPass=ssh-askpass']
    endif
  endif
  return []
endfunction

function! s:RunJobs() abort
  return (exists('*job_start') || exists('*jobstart')) && exists('*bufwinid')
endfunction

function! s:RunSave(state) abort
  let s:temp_files[s:cpath(a:state.file)] = a:state
endfunction

function! s:RunFinished(state, ...) abort
  if has_key(get(g:, '_fugitive_last_job', {}), 'file') && bufnr(g:_fugitive_last_job.file) < 0
    exe s:TempDelete(remove(g:, '_fugitive_last_job').file)
  endif
  let g:_fugitive_last_job = a:state
  let first = join(readfile(a:state.file, '', 2), "\n")
  if get(a:state, 'filetype', '') ==# 'git' && first =~# '\<\([[:upper:][:digit:]_-]\+(\d\+)\).*\1'
    let a:state.filetype = 'man'
  endif
  if !has_key(a:state, 'capture_bufnr')
    return
  endif
  call fugitive#ReloadStatus(a:state, 1)
endfunction

function! s:RunEdit(state, tmp, job) abort
  if get(a:state, 'request', '') !=# 'edit'
    return 0
  endif
  call remove(a:state, 'request')
  let sentinel = a:state.file . '.edit'
  let file = FugitiveVimPath(readfile(sentinel, 1)[0])
  exe substitute(a:state.mods, '\<tab\>', '-tab', 'g') 'keepalt split' s:fnameescape(file)
  set bufhidden=wipe
  let s:edit_jobs[bufnr('')] = [a:state, a:tmp, a:job, sentinel]
  call fugitive#ReloadStatus(a:state.dir, 1)
  return 1
endfunction

function! s:RunReceive(state, tmp, type, job, data, ...) abort
  if a:type ==# 'err' || a:state.pty
    let data = type(a:data) == type([]) ? join(a:data, "\n") : a:data
    let data = a:tmp.escape . data
    let escape = "\033]51;[^\007]*"
    let a:tmp.escape = matchstr(data, escape . '$')
    if len(a:tmp.escape)
      let data = strpart(data, 0, len(data) - len(a:tmp.escape))
    endif
    let cmd = matchstr(data, escape . "\007")[5:-2]
    let data = substitute(data, escape . "\007", '', 'g')
    if cmd =~# '^fugitive:'
      let a:state.request = strpart(cmd, 9)
    endif
    let lines = split(a:tmp.err . data, "\r\\=\n", 1)
    let a:tmp.err = lines[-1]
    let lines[-1] = ''
    call map(lines, 'substitute(v:val, ".*\r", "", "")')
  else
    let lines = type(a:data) == type([]) ? a:data : split(a:data, "\n", 1)
    if len(a:tmp.out)
      let lines[0] = a:tmp.out . lines[0]
    endif
    let a:tmp.out = lines[-1]
    let lines[-1] = ''
  endif
  call writefile(lines, a:state.file, 'ba')
  if has_key(a:tmp, 'echo')
    if !exists('l:data')
      let data = type(a:data) == type([]) ? join(a:data, "\n") : a:data
    endif
    let a:tmp.echo .= data
  endif
  let line_count = a:tmp.line_count
  let a:tmp.line_count += len(lines) - 1
  if !has_key(a:state, 'capture_bufnr') || !bufloaded(a:state.capture_bufnr)
    return
  endif
  call remove(lines, -1)
  try
    call setbufvar(a:state.capture_bufnr, '&modifiable', 1)
    if !line_count && len(lines) > 1000
      let first = remove(lines, 0, 999)
      call setbufline(a:state.capture_bufnr, 1, first)
      redraw
      call setbufline(a:state.capture_bufnr, 1001, lines)
    else
      call setbufline(a:state.capture_bufnr, line_count + 1, lines)
    endif
    call setbufvar(a:state.capture_bufnr, '&modifiable', 0)
    if !getwinvar(bufwinid(a:state.capture_bufnr), '&previewwindow')
      " no-op
    elseif exists('*win_execute')
      call win_execute(bufwinid(a:state.capture_bufnr), '$')
    else
      let winnr = bufwinnr(a:state.capture_bufnr)
      if winnr > 0
        let old_winnr = winnr()
        exe 'noautocmd' winnr.'wincmd w'
        $
        exe 'noautocmd' old_winnr.'wincmd w'
      endif
    endif
  catch
  endtry
endfunction

function! s:RunExit(state, tmp, job, exit_status) abort
  let a:state.exit_status = a:exit_status
  if has_key(a:state, 'job')
    return
  endif
  call s:RunFinished(a:state)
endfunction

function! s:RunClose(state, tmp, job, ...) abort
  if a:0
    call s:RunExit(a:state, a:tmp, a:job, a:1)
  endif
  let noeol = substitute(substitute(a:tmp.err, "\r$", '', ''), ".*\r", '', '') . a:tmp.out
  call writefile([noeol], a:state.file, 'ba')
  call remove(a:state, 'job')
  if has_key(a:state, 'capture_bufnr') && bufloaded(a:state.capture_bufnr)
    if len(noeol)
      call setbufvar(a:state.capture_bufnr, '&modifiable', 1)
      call setbufline(a:state.capture_bufnr, a:tmp.line_count + 1, [noeol])
      call setbufvar(a:state.capture_bufnr, '&eol', 0)
      call setbufvar(a:state.capture_bufnr, '&modifiable', 0)
    endif
    call setbufvar(a:state.capture_bufnr, '&modified', 0)
    call setbufvar(a:state.capture_bufnr, '&buflisted', 0)
    if a:state.filetype !=# getbufvar(a:state.capture_bufnr, '&filetype', '')
      call setbufvar(a:state.capture_bufnr, '&filetype', a:state.filetype)
    endif
  endif
  if !has_key(a:state, 'exit_status')
    return
  endif
  call s:RunFinished(a:state)
endfunction

function! s:RunSend(job, str) abort
  try
    if type(a:job) == type(0)
      call chansend(a:job, a:str)
    else
      call ch_sendraw(a:job, a:str)
    endif
    return len(a:str)
  catch /^Vim\%((\a\+)\)\=:E90[06]:/
    return 0
  endtry
endfunction

function! s:RunCloseIn(job) abort
  try
    if type(a:job) ==# type(0)
      call chanclose(a:job, 'stdin')
    else
      call ch_close_in(a:job)
    endif
    return 1
  catch /^Vim\%((\a\+)\)\=:E90[06]:/
    return 0
  endtry
endfunction

function! s:RunEcho(tmp) abort
  if !has_key(a:tmp, 'echo')
    return
  endif
  let data = a:tmp.echo
  let a:tmp.echo = matchstr(data, "[\r\n]\\+$")
  if len(a:tmp.echo)
    let data = strpart(data, 0, len(data) - len(a:tmp.echo))
  endif
  echon substitute(data, "\r\\ze\n", '', 'g')
endfunction

function! s:RunTick(job) abort
  if type(a:job) == v:t_number
    return jobwait([a:job], 1)[0] == -1
  elseif type(a:job) == 8
    let running = ch_status(a:job) !~# '^closed$\|^failed$' || job_status(a:job) ==# 'run'
    sleep 1m
    return running
  endif
endfunction

if !exists('s:edit_jobs')
  let s:edit_jobs = {}
endif
function! s:RunWait(state, tmp, job, ...) abort
  if a:0 && filereadable(a:1)
    call delete(a:1)
  endif
  try
    while get(a:state, 'request', '') !=# 'edit' && s:RunTick(a:job)
      call s:RunEcho(a:tmp)
      if !get(a:state, 'closed_in')
        let peek = getchar(1)
        if peek != 0 && !(has('win32') && peek == 128)
          let c = getchar()
          let c = type(c) == type(0) ? nr2char(c) : c
          if c ==# "\<C-D>" || c ==# "\<Esc>"
            let a:state.closed_in = 1
            let can_pedit = s:RunCloseIn(a:job) && exists('*setbufline')
            for winnr in range(1, winnr('$'))
              if getwinvar(winnr, '&previewwindow') && getbufvar(winbufnr(winnr), '&modified')
                let can_pedit = 0
              endif
            endfor
            if can_pedit
              if has_key(a:tmp, 'echo')
                call remove(a:tmp, 'echo')
              endif
              call writefile(['fugitive: aborting edit due to background operation.'], a:state.file . '.exit')
              exe (&splitbelow ? 'botright' : 'topleft') 'silent pedit ++ff=unix' fnameescape(a:state.file)
              let a:state.capture_bufnr = bufnr(a:state.file)
              call setbufvar(a:state.capture_bufnr, '&modified', 1)
              let finished = 0
              redraw!
              return ''
            endif
          else
            call s:RunSend(a:job, c)
            if !a:state.pty
              echon c
            endif
          endif
        endif
      endif
    endwhile
    if !has_key(a:state, 'request') && has_key(a:state, 'job') && exists('*job_status') && job_status(a:job) ==# "dead"
      throw 'fugitive: close callback did not fire; this should never happen'
    endif
    call s:RunEcho(a:tmp)
    if has_key(a:tmp, 'echo')
      let a:tmp.echo = substitute(a:tmp.echo, "^\r\\=\n", '', '')
      echo
    endif
    let finished = !s:RunEdit(a:state, a:tmp, a:job)
  finally
    if !exists('finished')
      try
        if a:state.pty && !get(a:state, 'closed_in')
          call s:RunSend(a:job, "\<C-C>")
        elseif type(a:job) == type(0)
          call jobstop(a:job)
        else
          call job_stop(a:job)
        endif
      catch /.*/
      endtry
    elseif finished
      call fugitive#ReloadStatus(a:state, 1)
    endif
  endtry
  return ''
endfunction

if !exists('s:resume_queue')
  let s:resume_queue = []
endif
function! fugitive#Resume() abort
  while len(s:resume_queue)
    if s:resume_queue[0][2] isnot# ''
      try
        call call('s:RunWait', remove(s:resume_queue, 0))
      endtry
    endif
  endwhile
endfunction

function! s:RunBufDelete(bufnr) abort
  let state = s:TempState(bufname(+a:bufnr))
  if has_key(state, 'job')
    try
      if type(state.job) == type(0)
        call jobstop(state.job)
      else
        call job_stop(state.job)
      endif
    catch
    endtry
  endif
  if has_key(s:edit_jobs, a:bufnr) |
    call add(s:resume_queue, remove(s:edit_jobs, a:bufnr))
    call feedkeys(":redraw!|call delete(" . string(s:resume_queue[-1][0].file . '.edit') .
          \ ")|call fugitive#Resume()|silent checktime\r", 'n')
  endif
endfunction

augroup fugitive_job
  autocmd!
  autocmd BufDelete * call s:RunBufDelete(+expand('<abuf>'))
  autocmd VimLeave *
        \ for s:jobbuf in keys(s:edit_jobs) |
        \   call writefile(['Aborting edit due to Vim exit.'], s:edit_jobs[s:jobbuf][0].file . '.exit') |
        \   redraw! |
        \   call call('s:RunWait', remove(s:edit_jobs, s:jobbuf)) |
        \ endfor
augroup END

function! fugitive#PagerFor(argv, ...) abort
  let args = a:argv
  if empty(args)
    return 0
  elseif (args[0] ==# 'help' || get(args, 1, '') ==# '--help') && !s:HasOpt(args, '--web')
    return 1
  endif
  if args[0] ==# 'config' && (s:HasOpt(args, '-e', '--edit') ||
        \   !s:HasOpt(args, '--list', '--get-all', '--get-regexp', '--get-urlmatch')) ||
        \ args[0] =~# '^\%(tag\|branch\)$' && (
        \    s:HasOpt(args, '--edit-description', '--unset-upstream', '-m', '-M', '--move', '-c', '-C', '--copy', '-d', '-D', '--delete') ||
        \   len(filter(args[1:-1], 'v:val =~# "^[^-]\\|^--set-upstream-to="')) &&
        \   !s:HasOpt(args, '--contains', '--no-contains', '--merged', '--no-merged', '--points-at'))
    return 0
  endif
  let config = a:0 ? a:1 : fugitive#Config()
  let value = get(fugitive#ConfigGetAll('pager.' . args[0], config), 0, -1)
  if value =~# '^\%(true\|yes\|on\|1\)$'
    return 1
  elseif value =~# '^\%(false\|no|off\|0\|\)$'
    return 0
  elseif type(value) == type('')
    return value
  elseif args[0] =~# '^\%(branch\|config\|diff\|grep\|log\|range-diff\|shortlog\|show\|tag\|whatchanged\)$' ||
        \ (args[0] ==# 'stash' && get(args, 1, '') ==# 'show') ||
        \ (args[0] ==# 'reflog' && get(args, 1, '') !~# '^\%(expire\|delete\|exists\)$') ||
        \ (args[0] ==# 'am' && s:HasOpt(args, '--show-current-patch'))
    return 1
  else
    return 0
  endif
endfunction

let s:disable_colors = []
for s:colortype in ['advice', 'branch', 'diff', 'grep', 'interactive', 'pager', 'push', 'remote', 'showBranch', 'status', 'transport', 'ui']
  call extend(s:disable_colors, ['-c', 'color.' . s:colortype . '=false'])
endfor
unlet s:colortype
function! fugitive#Command(line1, line2, range, bang, mods, arg) abort
  exe s:VersionCheck()
  let dir = s:Dir()
  let config = copy(fugitive#Config(dir))
  let [args, after] = s:SplitExpandChain(a:arg, s:Tree(dir))
  let flags = []
  let pager = -1
  while len(args)
    if args[0] ==# '-c' && len(args) > 1
      call extend(flags, remove(args, 0, 1))
    elseif args[0] =~# '^-p$\|^--paginate$'
      let pager = 1
      call remove(args, 0)
    elseif args[0] =~# '^-P$\|^--no-pager$'
      let pager = 0
      call remove(args, 0)
    elseif args[0] =~# '^--\%([[:lower:]-]\+-pathspecs\|no-optional-locks\)$'
      call add(flags, remove(args, 0))
    elseif args[0] =~# '^-C$\|^--\%(exec-path=\|git-dir=\|work-tree=\|bare$\)'
      return 'echoerr ' . string('fugitive: ' . args[0] . ' is not supported')
    else
      break
    endif
  endwhile
  if pager is# 0
    call add(flags, '--no-pager')
  endif
  if empty(args) && pager is# -1
    let cmd = s:StatusCommand(a:line1, a:line2, a:range, a:line2, a:bang, a:mods, '', '', [])
    return (empty(cmd) ? 'exe' : cmd) . after
  endif
  let alias = FugitiveConfigGet('alias.' . get(args, 0, ''), config)
  if get(args, 1, '') !=# '--help' && alias !~# '^$\|^!\|[\"'']' && !filereadable(s:ExecPath() . '/git-' . args[0])
        \ && !(has('win32') && filereadable(s:ExecPath() . '/git-' . args[0] . '.exe'))
    call remove(args, 0)
    call extend(args, split(alias, '\s\+'), 'keep')
  endif
  let name = substitute(get(args, 0, ''), '\%(^\|-\)\(\l\)', '\u\1', 'g')
  let git = s:UserCommandList()
  let options = {'git': git, 'dir': dir, 'flags': flags}
  if pager is# -1 && name =~# '^\a\+$' && exists('*s:' . name . 'Subcommand') && get(args, 1, '') !=# '--help'
    try
      let overrides = s:{name}Subcommand(a:line1, a:line2, a:range, a:bang, a:mods, extend({'subcommand': args[0], 'subcommand_args': args[1:-1]}, options))
      if type(overrides) == type('')
        return 'exe ' . string(overrides) . after
      endif
      let args = [get(overrides, 'command', args[0])] + get(overrides, 'insert_args', []) + args[1:-1]
    catch /^fugitive:/
      return 'echoerr ' . string(v:exception)
    endtry
  else
    let overrides = {}
  endif
  let env = get(overrides, 'env', {})
  let i = 0
  while i < len(flags) - 1
    if flags[i] ==# '-c'
      let i += 1
      let config_name = tolower(matchstr(flags[i], '^[^=]\+'))
      if has_key(s:prepare_env, config_name) && flags[i] =~# '=.'
        let env[s:prepare_env[config_name]] = matchstr(flags[i], '=\zs.*')
      endif
      if flags[i] =~# '='
        let config[config_name] = [matchstr(flags[i], '=\zs.*')]
      else
        let config[config_name] = [1]
      endif
    endif
    let i += 1
  endwhile
  let editcmd = a:line2 ? 'split' : 'edit'
  if pager is# 1
    if a:bang && a:line2 >= 0
      let editcmd = 'read'
    elseif a:bang
      let editcmd = 'pedit'
    endif
  elseif pager is# -1
    let pager = fugitive#PagerFor(args, config)
    if a:bang && pager isnot# 1
      return 'echoerr ' .  string('fugitive: :Git! for temp buffer output has been replaced by :Git --paginate')
    endif
  endif
  if (s:HasOpt(args, ['add', 'checkout', 'commit', 'stage', 'stash', 'reset'], '-p', '--patch') ||
        \ s:HasOpt(args, ['add', 'clean', 'stage'], '-i', '--interactive') ||
        \ type(pager) == type('')) && pager isnot# 1
    let mods = substitute(s:Mods(a:mods), '\<tab\>', '-tab', 'g')
    let assign = len(dir) ? '|let b:git_dir = ' . string(dir) : ''
    if has('nvim')
      call fugitive#Autowrite()
      return mods . (a:line2 ? 'split' : 'edit') . ' term://' . s:fnameescape(s:UserCommand(options, args)) . assign . '|startinsert' . after
    elseif has('terminal')
      call fugitive#Autowrite()
      return 'exe ' . string(mods . 'terminal ' . (a:line2 ? '' : '++curwin ') . join(map(s:UserCommandList(options) + args, 's:fnameescape(v:val)'))) . assign . after
    endif
  endif
  if pager is# 1 && editcmd ==# 'read'
    return s:ReadExec(a:line1, a:line2, a:range, a:mods, env, args, options) . after
  endif
  let state = {
        \ 'git': git,
        \ 'flags': flags,
        \ 'args': args,
        \ 'dir': dir,
        \ 'git_dir': dir,
        \ 'cwd': s:UserCommandCwd(dir),
        \ 'filetype': 'git',
        \ 'mods': s:Mods(a:mods),
        \ 'file': s:Resolve(tempname())}
  if pager is# 1
    call extend(env, {'COLUMNS': '' . get(g:, 'fugitive_columns', 80)}, 'keep')
  else
    call extend(env, {'COLUMNS': '' . &columns - 1}, 'keep')
  endif
  if s:RunJobs() && pager isnot# 1
    let state.pty = get(g:, 'fugitive_pty', has('unix') && !has('win32unix') && (has('patch-8.0.0744') || has('nvim')) && fugitive#GitVersion() !~# '\.windows\>')
    if !state.pty
      let args = s:AskPassArgs(dir) + args
    endif
    let tmp = {
          \ 'line_count': 0,
          \ 'err': '',
          \ 'out': '',
          \ 'echo': '',
          \ 'escape': ''}
    let env.FUGITIVE = state.file
    let editor = 'sh ' . s:TempScript(
          \ '[ -f "$FUGITIVE.exit" ] && cat "$FUGITIVE.exit" >&2 && exit 1',
          \ 'echo "$1" > "$FUGITIVE.edit"',
          \ 'printf "\033]51;fugitive:edit\007" >&2',
          \ 'while [ -f "$FUGITIVE.edit" -a ! -f "$FUGITIVE.exit" ]; do sleep 0.05 2>/dev/null || sleep 1; done',
          \ 'exit 0')
    call extend(env, {
          \ 'NO_COLOR': '1',
          \ 'GIT_EDITOR': editor,
          \ 'GIT_SEQUENCE_EDITOR': editor,
          \ 'GIT_MERGE_AUTOEDIT': '1',
          \ 'GIT_PAGER': 'cat',
          \ 'PAGER': 'cat'}, 'keep')
    let args = s:disable_colors + flags + ['-c', 'advice.waitingForEditor=false'] + args
    let argv = s:UserCommandList({'git': git, 'dir': dir}) + args
    let [argv, jobopts] = s:JobOpts(argv, env)
    call fugitive#Autowrite()
    call writefile([], state.file, 'b')
    call s:RunSave(state)
    echo ""
    if exists('*job_start')
      call extend(jobopts, {
            \ 'mode': 'raw',
            \ 'out_cb': function('s:RunReceive', [state, tmp, 'out']),
            \ 'err_cb': function('s:RunReceive', [state, tmp, 'err']),
            \ 'close_cb': function('s:RunClose', [state, tmp]),
            \ 'exit_cb': function('s:RunExit', [state, tmp]),
            \ })
      if state.pty
        let jobopts.pty = 1
      endif
      let job = job_start(argv, jobopts)
    else
      let job = jobstart(argv, extend(jobopts, {
            \ 'pty': state.pty,
            \ 'TERM': 'dumb',
            \ 'on_stdout': function('s:RunReceive', [state, tmp, 'out']),
            \ 'on_stderr': function('s:RunReceive', [state, tmp, 'err']),
            \ 'on_exit': function('s:RunClose', [state, tmp]),
            \ }))
    endif
    let state.job = job
    call add(s:resume_queue, [state, tmp, job])
    return 'call fugitive#Resume()|silent checktime' . after
  elseif pager is# 1
    let pre = s:BuildEnvPrefix(env)
    try
      if exists('+guioptions') && &guioptions =~# '!'
        let guioptions = &guioptions
        set guioptions-=!
      endif
      silent! execute '!' . escape(pre . s:UserCommand({'git': git, 'dir': dir}, s:disable_colors + flags + ['--no-pager'] + args), '!#%') .
            \ (&shell =~# 'csh' ? ' >& ' . s:shellesc(state.file) : ' > ' . s:shellesc(state.file) . ' 2>&1')
      let state.exit_status = v:shell_error
    finally
      if exists('guioptions')
        let &guioptions = guioptions
      endif
    endtry
    redraw!
    call s:RunSave(state)
    call s:RunFinished(state)
    if editcmd ==# 'edit'
      call s:BlurStatus()
    endif
    return state.mods . editcmd . ' ' . s:fnameescape(state.file) .
          \ '|call fugitive#ReloadStatus(fugitive#Result(' . string(state.file) . '), 1)' . after
  elseif has('win32')
    return 'echoerr ' . string('fugitive: Vim 8 with job support required to use :Git on Windows')
  elseif has('gui_running')
    return 'echoerr ' . string('fugitive: Vim 8 with job support required to use :Git in GVim')
  else
    let pre = s:BuildEnvPrefix(env)
    return 'exe ' . string('noautocmd !' . escape(pre . s:UserCommand(options, args), '!#%')) .
          \ '|call fugitive#ReloadStatus(' . string(dir) . ', 1)' .
          \ after
  endif
endfunction

let s:exec_paths = {}
function! s:ExecPath() abort
  let git = s:GitShellCmd()
  if !has_key(s:exec_paths, git)
    let s:exec_paths[git] = s:sub(system(git.' --exec-path'),'\n$','')
  endif
  return s:exec_paths[git]
endfunction

let s:subcommands_before_2_5 = [
      \ 'add', 'am', 'apply', 'archive', 'bisect', 'blame', 'branch', 'bundle',
      \ 'checkout', 'cherry', 'cherry-pick', 'citool', 'clean', 'clone', 'commit', 'config',
      \ 'describe', 'diff', 'difftool', 'fetch', 'format-patch', 'fsck',
      \ 'gc', 'grep', 'gui', 'help', 'init', 'instaweb', 'log',
      \ 'merge', 'mergetool', 'mv', 'notes', 'pull', 'push',
      \ 'rebase', 'reflog', 'remote', 'repack', 'replace', 'request-pull', 'reset', 'revert', 'rm',
      \ 'send-email', 'shortlog', 'show', 'show-branch', 'stash', 'stage', 'status', 'submodule',
      \ 'tag', 'whatchanged',
      \ ]
let s:path_subcommands = {}
function! s:CompletableSubcommands(dir) abort
  let c_exec_path = s:cpath(s:ExecPath())
  if !has_key(s:path_subcommands, c_exec_path)
    if fugitive#GitVersion(2, 18)
      let [lines, exec_error] = s:LinesError(a:dir, '--list-cmds=list-mainporcelain,nohelpers,list-complete')
      call filter(lines, 'v:val =~# "^\\S\\+$"')
      if !exec_error && len(lines)
        let s:path_subcommands[c_exec_path] = lines
      else
        let s:path_subcommands[c_exec_path] = s:subcommands_before_2_5 +
              \ ['maintenance', 'prune', 'range-diff', 'restore', 'sparse-checkout', 'switch', 'worktree']
      endif
    else
      let s:path_subcommands[c_exec_path] = s:subcommands_before_2_5 +
            \ (fugitive#GitVersion(2, 5) ? ['worktree'] : [])
    endif
  endif
  let commands = copy(s:path_subcommands[c_exec_path])
  for path in split($PATH, has('win32') ? ';' : ':')
    if path !~# '^/\|^\a:[\\/]'
      continue
    endif
    let cpath = s:cpath(path)
    if !has_key(s:path_subcommands, cpath)
      let s:path_subcommands[cpath] = filter(map(s:GlobComplete(path.'/git-', '*', 1),'substitute(v:val,"\\.exe$","","")'), 'v:val !~# "--\\|/"')
    endif
    call extend(commands, s:path_subcommands[cpath])
  endfor
  call extend(commands, keys(fugitive#ConfigGetRegexp('^alias\.\zs[^.]\+$', a:dir)))
  let configured = split(FugitiveConfigGet('completion.commands', a:dir), '\s\+')
  let rejected = {}
  for command in configured
    if command =~# '^-.'
      let rejected[strpart(command, 1)] = 1
    endif
  endfor
  call filter(configured, 'v:val !~# "^-"')
  let results = filter(sort(commands + configured), '!has_key(rejected, v:val)')
  if exists('*uniq')
    return uniq(results)
  else
    let i = 1
    while i < len(results)
      if results[i] ==# results[i-1]
        call remove(results, i)
      else
        let i += 1
      endif
    endwhile
    return results
  endif
endfunction

function! fugitive#Complete(lead, ...) abort
  let dir = a:0 == 1 ? a:1 : a:0 >= 3 ? a:3 : s:Dir()
  let root = a:0 >= 4 ? a:4 : s:Tree(s:Dir())
  let pre = a:0 > 1 ? strpart(a:1, 0, a:2) : ''
  let subcmd = matchstr(pre, '\u\w*[! ] *\zs[[:alnum:]-]\+\ze ')
  if empty(subcmd)
    let results = s:CompletableSubcommands(dir)
  elseif a:0 ==# 2 && subcmd =~# '^\%(commit\|revert\|push\|fetch\|pull\|merge\|rebase\)$'
    let cmdline = substitute(a:1, '\u\w*\([! ] *\)' . subcmd, 'G' . subcmd, '')
    let caps_subcmd = substitute(subcmd, '\%(^\|-\)\l', '\u&', 'g')
    return fugitive#{caps_subcmd}Complete(a:lead, cmdline, a:2 + len(cmdline) - len(a:1), dir, root)
  elseif pre =~# ' -- '
    return fugitive#CompletePath(a:lead, a:1, a:2, dir, root)
  elseif a:lead =~# '^-'
    let results = split(s:ChompDefault('', dir, subcmd, '--git-completion-helper'), ' ')
  else
    return fugitive#CompleteObject(a:lead, a:1, a:2, dir, root)
  endif
  return filter(results, 'strpart(v:val, 0, strlen(a:lead)) ==# a:lead')
endfunction

function! fugitive#CompleteForWorkingDir(A, L, P, ...) abort
  let path = a:0 ? a:1 : getcwd()
  return fugitive#Complete(a:A, a:L, a:P, FugitiveExtractGitDir(path), path)
endfunction

" Section: :Gcd, :Glcd

function! fugitive#CdComplete(A, L, P) abort
  return filter(fugitive#CompletePath(a:A), 'v:val =~# "/$"')
endfunction

function! fugitive#Cd(path, ...) abort
  let path = substitute(a:path, '^:/:\=\|^:(\%(top\|top,literal\|literal,top\|literal\))', '', '')
  if path !~# '^/\|^\a\+:\|^\.\.\=\%(/\|$\)'
    let dir = s:Dir()
    exe s:DirCheck(dir)
    let path = (empty(s:Tree(dir)) ? dir : s:Tree(dir)) . '/' . path
  endif
  return (a:0 && a:1 ? 'lcd ' : 'cd ') . s:fnameescape(FugitiveVimPath(path))
endfunction

" Section: :Gstatus

function! s:StatusCommand(line1, line2, range, count, bang, mods, reg, arg, args, ...) abort
  let dir = a:0 ? a:1 : s:Dir()
  exe s:DirCheck(dir)
  try
    let mods = s:Mods(a:mods, &splitbelow ? 'botright' : 'topleft')
    let file = fugitive#Find(':', dir)
    let arg = ' +setl\ foldmarker=<<<<<<<<,>>>>>>>>\|let\ w:fugitive_status=FugitiveGitDir() ' .
          \ s:fnameescape(file)
    for tabnr in [tabpagenr()] + (mods =~# '\<tab\>' ? range(1, tabpagenr('$')) : [])
      let bufs = tabpagebuflist(tabnr)
      for winnr in range(1, tabpagewinnr(tabnr, '$'))
        if s:cpath(file, fnamemodify(bufname(bufs[winnr-1]), ':p'))
          if tabnr == tabpagenr() && winnr == winnr()
            call s:ReloadStatus()
          else
            call s:ExpireStatus(dir)
            exe tabnr . 'tabnext'
            exe winnr . 'wincmd w'
          endif
          let w:fugitive_status = dir
          1
          return ''
        endif
      endfor
    endfor
    if a:count ==# 0
      return mods . 'edit' . (a:bang ? '!' : '') . arg
    elseif a:bang
      return mods . 'pedit' . arg . '|wincmd P'
    else
      return mods . 'keepalt split' . arg
    endif
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  return ''
endfunction

function! s:StageJump(offset, section, ...) abort
  let line = search('^\%(' . a:section . '\)', 'nw')
  if !line && a:0
    let line = search('^\%(' . a:1 . '\)', 'nw')
  endif
  if line
    exe line
    if a:offset
      for i in range(a:offset)
        call search(s:file_commit_pattern . '\|^$', 'W')
        if empty(getline('.')) && a:0 && getline(line('.') + 1) =~# '^\%(' . a:1 . '\)'
          call search(s:file_commit_pattern . '\|^$', 'W')
        endif
        if empty(getline('.'))
          return ''
        endif
      endfor
      call s:StageReveal()
    else
      call s:StageReveal()
      +
    endif
  endif
  return ''
endfunction

function! s:StageSeek(info, fallback) abort
  let info = a:info
  if empty(info.heading)
    return a:fallback
  endif
  let line = search('^' . escape(info.heading, '^$.*[]~\') . ' (\d\+)$', 'wn')
  if !line
    for section in get({'Staged': ['Unstaged', 'Untracked'], 'Unstaged': ['Untracked', 'Staged'], 'Untracked': ['Unstaged', 'Staged']}, info.section, [])
      let line = search('^' . section, 'wn')
      if line
        return line + (info.index > 0 ? 1 : 0)
      endif
    endfor
    return 1
  endif
  let i = 0
  while len(getline(line))
    let filename = matchstr(getline(line), '^[A-Z?] \zs.*')
    if len(filename) &&
          \ ((info.filename[-1:-1] ==# '/' && filename[0 : len(info.filename) - 1] ==# info.filename) ||
          \ (filename[-1:-1] ==# '/' && filename ==# info.filename[0 : len(filename) - 1]) ||
          \ filename ==# info.filename)
      if info.offset < 0
        return line
      else
        if getline(line+1) !~# '^@'
          exe s:StageInline('show', line)
        endif
        if getline(line+1) !~# '^@'
          return line
        endif
        let type = info.sigil ==# '-' ? '-' : '+'
        let offset = -1
        while offset < info.offset
          let line += 1
          if getline(line) =~# '^@'
            let offset = +matchstr(getline(line), type . '\zs\d\+') - 1
          elseif getline(line) =~# '^[ ' . type . ']'
            let offset += 1
          elseif getline(line) !~# '^[ @\+-]'
            return line - 1
          endif
        endwhile
        return line
      endif
    endif
    let commit = matchstr(getline(line), '^\%(\%(\x\x\x\)\@!\l\+\s\+\)\=\zs[0-9a-f]\+')
    if len(commit) && commit ==# info.commit
      return line
    endif
    if i ==# info.index
      let backup = line
    endif
    let i += getline(line) !~# '^[ @\+-]'
    let line += 1
  endwhile
  return exists('backup') ? backup : line - 1
endfunction

function! s:DoAutocmdChanged(dir) abort
  let dir = a:dir is# -2 ? '' : FugitiveGitDir(a:dir)
  if empty(dir) || !exists('#User#FugitiveChanged') || exists('g:fugitive_event')
    return ''
  endif
  try
    let g:fugitive_event = dir
    if type(a:dir) == type({}) && has_key(a:dir, 'args')
      let g:fugitive_result = a:dir
    endif
    exe s:DoAutocmd('User FugitiveChanged')
  finally
    unlet! g:fugitive_event g:fugitive_result
    " Force statusline reload with the buffer's Git dir
    let &ro = &ro
  endtry
  return ''
endfunction

function! s:ReloadStatusBuffer(...) abort
  if get(b:, 'fugitive_type', '') !=# 'index'
    return ''
  endif
  let original_lnum = a:0 ? a:1 : line('.')
  let info = s:StageInfo(original_lnum)
  call fugitive#BufReadStatus()
  call setpos('.', [0, s:StageSeek(info, original_lnum), 1, 0])
  return ''
endfunction

function! s:ReloadStatus(...) abort
  call s:ExpireStatus(-1)
  call s:ReloadStatusBuffer(a:0 ? a:1 : line('.'))
  exe s:DoAutocmdChanged(-1)
  return ''
endfunction

let s:last_time = reltime()
if !exists('s:last_times')
  let s:last_times = {}
endif

function! s:ExpireStatus(bufnr) abort
  if a:bufnr is# -2
    let s:head_cache = {}
    let s:last_time = reltime()
    return ''
  endif
  let dir = s:Dir(a:bufnr)
  if len(dir)
    let s:last_times[s:cpath(dir)] = reltime()
    if has_key(s:head_cache, dir)
      call remove(s:head_cache, dir)
    endif
  endif
  return ''
endfunction

function! s:ReloadWinStatus(...) abort
  if get(b:, 'fugitive_type', '') !=# 'index' || &modified
    return
  endif
  if !exists('b:fugitive_reltime')
    exe s:ReloadStatusBuffer()
    return
  endif
  let t = b:fugitive_reltime
  if reltimestr(reltime(s:last_time, t)) =~# '-\|\d\{10\}\.' ||
        \ reltimestr(reltime(get(s:last_times, s:cpath(s:Dir()), t), t)) =~# '-\|\d\{10\}\.'
    exe s:ReloadStatusBuffer()
  endif
endfunction

function! s:ReloadTabStatus(...) abort
  let mytab = tabpagenr()
  let tab = a:0 ? a:1 : mytab
  let winnr = 1
  while winnr <= tabpagewinnr(tab, '$')
    if getbufvar(tabpagebuflist(tab)[winnr-1], 'fugitive_type') ==# 'index'
      execute 'tabnext '.tab
      if winnr != winnr()
        execute winnr.'wincmd w'
        let restorewinnr = 1
      endif
      try
        call s:ReloadWinStatus()
      finally
        if exists('restorewinnr')
          unlet restorewinnr
          wincmd p
        endif
        execute 'tabnext '.mytab
      endtry
    endif
    let winnr += 1
  endwhile
  unlet! t:fugitive_reload_status
endfunction

function! fugitive#ReloadStatus(...) abort
  call s:ExpireStatus(a:0 ? a:1 : -1)
  if a:0 > 1 ? a:2 : 1
    let t = reltime()
    let t:fugitive_reload_status = t
    for tabnr in exists('*settabvar') ? range(1, tabpagenr('$')) : []
      call settabvar(tabnr, 'fugitive_reload_status', t)
    endfor
    call s:ReloadTabStatus()
  else
    call s:ReloadWinStatus()
    return ''
  endif
  exe s:DoAutocmdChanged(a:0 ? a:1 : -1)
  return ''
endfunction

function! fugitive#EfmDir(...) abort
  let dir = matchstr(a:0 ? a:1 : &errorformat, '\c,%\\&\%(git\|fugitive\)_\=dir=\zs\%(\\.\|[^,]\)*')
  let dir = substitute(dir, '%%', '%', 'g')
  let dir = substitute(dir, '\\\ze[\,]', '', 'g')
  return dir
endfunction

augroup fugitive_status
  autocmd!
  autocmd BufWritePost         * call fugitive#ReloadStatus(-1, 0)
  autocmd ShellCmdPost,ShellFilterPost * nested call fugitive#ReloadStatus(-2, 0)
  autocmd BufDelete * nested
        \ if getbufvar(+expand('<abuf>'), 'buftype') ==# 'terminal' |
        \   if !empty(FugitiveGitDir(+expand('<abuf>'))) |
        \     call fugitive#ReloadStatus(+expand('<abuf>'), 1) |
        \   else |
        \     call fugitive#ReloadStatus(-2, 0) |
        \  endif |
        \ endif
  autocmd QuickFixCmdPost make,lmake,[cl]file,[cl]getfile nested
        \ call fugitive#ReloadStatus(fugitive#EfmDir(), 1)
  if !has('win32')
    autocmd FocusGained        * call fugitive#ReloadStatus(-2, 0)
  endif
  autocmd BufEnter index,index.lock
        \ call s:ReloadWinStatus()
  autocmd TabEnter *
        \ if exists('t:fugitive_reload_status') |
        \    call s:ReloadTabStatus() |
        \ endif
augroup END

function! s:StageInfo(...) abort
  let lnum = a:0 ? a:1 : line('.')
  let sigil = matchstr(getline(lnum), '^[ @\+-]')
  let offset = -1
  if len(sigil)
    let type = sigil ==# '-' ? '-' : '+'
    while lnum > 0 && getline(lnum) !~# '^@'
      if getline(lnum) =~# '^[ '.type.']'
        let offset += 1
      endif
      let lnum -= 1
    endwhile
    let offset += matchstr(getline(lnum), type.'\zs\d\+')
    while getline(lnum) =~# '^[ @\+-]'
      let lnum -= 1
    endwhile
  endif
  let slnum = lnum + 1
  let heading = ''
  let index = 0
  while len(getline(slnum - 1)) && empty(heading)
    let slnum -= 1
    let heading = matchstr(getline(slnum), '^\u\l\+.\{-\}\ze (\d\+)$')
    if empty(heading) && getline(slnum) !~# '^[ @\+-]'
      let index += 1
    endif
  endwhile
  let text = matchstr(getline(lnum), '^[A-Z?] \zs.*')
  return {'section': matchstr(heading, '^\u\l\+'),
        \ 'heading': heading,
        \ 'sigil': sigil,
        \ 'offset': offset,
        \ 'filename': text,
        \ 'relative': reverse(split(text, ' -> ')),
        \ 'paths': map(reverse(split(text, ' -> ')), 's:Tree() . "/" . v:val'),
        \ 'commit': matchstr(getline(lnum), '^\%(\%(\x\x\x\)\@!\l\+\s\+\)\=\zs[0-9a-f]\{4,\}\ze '),
        \ 'status': matchstr(getline(lnum), '^[A-Z?]\ze \|^\%(\x\x\x\)\@!\l\+\ze [0-9a-f]'),
        \ 'submodule': get(get(get(b:fugitive_files, heading, {}), text, {}), 'submodule', ''),
        \ 'index': index}
endfunction

function! s:Selection(arg1, ...) abort
  if a:arg1 ==# 'n'
    let arg1 = line('.')
    let arg2 = -v:count
  elseif a:arg1 ==# 'v'
    let arg1 = line("'<")
    let arg2 = line("'>")
  else
    let arg1 = a:arg1
    let arg2 = a:0 ? a:1 : 0
  endif
  let first = arg1
  if arg2 < 0
    let last = first - arg2 - 1
  elseif arg2 > 0
    let last = arg2
  else
    let last = first
  endif
  while getline(first) =~# '^$\|^[A-Z][a-z]'
    let first += 1
  endwhile
  if first > last || &filetype !=# 'fugitive'
    return []
  endif
  let flnum = first
  while getline(flnum) =~# '^[ @\+-]'
    let flnum -= 1
  endwhile
  let slnum = flnum + 1
  let heading = ''
  let index = 0
  while empty(heading)
    let slnum -= 1
    let heading = matchstr(getline(slnum), '^\u\l\+.\{-\}\ze (\d\+)$')
    if empty(heading) && getline(slnum) !~# '^[ @\+-]'
      let index += 1
    endif
  endwhile
  let results = []
  let template = {
        \ 'heading': heading,
        \ 'section': matchstr(heading, '^\u\l\+'),
        \ 'filename': '',
        \ 'relative': [],
        \ 'paths': [],
        \ 'commit': '',
        \ 'status': '',
        \ 'patch': 0,
        \ 'index': index}
  let line = getline(flnum)
  let lnum = first - (arg1 == flnum ? 0 : 1)
  let root = s:Tree() . '/'
  while lnum <= last
    let heading = matchstr(line, '^\u\l\+\ze.\{-\}\ze (\d\+)$')
    if len(heading)
      let template.heading = heading
      let template.section = matchstr(heading, '^\u\l\+')
      let template.index = 0
    elseif line =~# '^[ @\+-]'
      let template.index -= 1
      if !results[-1].patch
        let results[-1].patch = lnum
      endif
      let results[-1].lnum = lnum
    elseif line =~# '^[A-Z?] '
      let filename = matchstr(line, '^[A-Z?] \zs.*')
      call add(results, extend(deepcopy(template), {
            \ 'lnum': lnum,
            \ 'filename': filename,
            \ 'relative': reverse(split(filename, ' -> ')),
            \ 'paths': map(reverse(split(filename, ' -> ')), 'root . v:val'),
            \ 'status': matchstr(line, '^[A-Z?]'),
            \ }))
    elseif line =~# '^\x\x\x\+ '
      call add(results, extend({
            \ 'lnum': lnum,
            \ 'commit': matchstr(line, '^\x\x\x\+'),
            \ }, template, 'keep'))
    elseif line =~# '^\l\+ \x\x\x\+ '
      call add(results, extend({
            \ 'lnum': lnum,
            \ 'commit': matchstr(line, '^\l\+ \zs\x\x\x\+'),
            \ 'status': matchstr(line, '^\l\+'),
            \ }, template, 'keep'))
    endif
    let lnum += 1
    let template.index += 1
    let line = getline(lnum)
  endwhile
  if len(results) && results[0].patch && arg2 == 0
    while getline(results[0].patch) =~# '^[ \+-]'
      let results[0].patch -= 1
    endwhile
    while getline(results[0].lnum + 1) =~# '^[ \+-]'
      let results[0].lnum += 1
    endwhile
  endif
  return results
endfunction

function! s:StageArgs(visual) abort
  let commits = []
  let paths = []
  for record in s:Selection(a:visual ? 'v' : 'n')
    if len(record.commit)
      call add(commits, record.commit)
    endif
    call extend(paths, record.paths)
  endfor
  if s:cpath(s:Tree(), getcwd())
    call map(paths, 'fugitive#Path(v:val, "./")')
  endif
  return join(map(commits + paths, 's:fnameescape(v:val)'), ' ')
endfunction

function! s:Do(action, visual) abort
  let line = getline('.')
  let reload = 0
  if !a:visual && !v:count && line =~# '^[A-Z][a-z]'
    let header = matchstr(line, '^\S\+\ze:')
    if len(header) && exists('*s:Do' . a:action . header . 'Header')
      let reload = s:Do{a:action}{header}Header(matchstr(line, ': \zs.*')) > 0
    else
      let section = matchstr(line, '^\S\+')
      if exists('*s:Do' . a:action . section . 'Heading')
        let reload = s:Do{a:action}{section}Heading(line) > 0
      endif
    endif
    return reload ? s:ReloadStatus() : ''
  endif
  let selection = s:Selection(a:visual ? 'v' : 'n')
  if empty(selection)
    return ''
  endif
  call filter(selection, 'v:val.section ==# selection[0].section')
  let status = 0
  let err = ''
  try
    for record in selection
      if exists('*s:Do' . a:action . record.section)
        let status = s:Do{a:action}{record.section}(record)
      else
        continue
      endif
      if !status
        return ''
      endif
      let reload = reload || (status > 0)
    endfor
    if status < 0
      execute record.lnum + 1
    endif
    let success = 1
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  finally
    if reload
      execute s:ReloadStatus()
    endif
    if exists('success')
      call s:StageReveal()
    endif
  endtry
  return ''
endfunction

function! s:StageReveal() abort
  exe 'normal! zv'
  let begin = line('.')
  if getline(begin) =~# '^@'
    let end = begin + 1
    while getline(end) =~# '^[ \+-]'
      let end += 1
    endwhile
  elseif getline(begin) =~# '^commit '
    let end = begin
    while end < line('$') && getline(end + 1) !~# '^commit '
      let end += 1
    endwhile
  elseif getline(begin) =~# s:section_pattern
    let end = begin
    while len(getline(end + 1))
      let end += 1
    endwhile
  endif
  if exists('end')
    while line('.') > line('w0') + &scrolloff && end > line('w$')
      execute "normal! \<C-E>"
    endwhile
  endif
endfunction

let s:file_pattern = '^[A-Z?] .\|^diff --'
let s:file_commit_pattern = s:file_pattern . '\|^\%(\l\{3,\} \)\=[0-9a-f]\{4,\} '
let s:item_pattern = s:file_commit_pattern . '\|^@@'

function! s:NextHunk(count) abort
  if &filetype ==# 'fugitive' && getline('.') =~# s:file_pattern
    exe s:StageInline('show')
  endif
  for i in range(a:count)
    if &filetype ==# 'fugitive'
      call search(s:file_pattern . '\|^@', 'W')
      if getline('.') =~# s:file_pattern
        exe s:StageInline('show')
        if getline(line('.') + 1) =~# '^@'
          +
        endif
      endif
    else
      call search('^@@', 'W')
    endif
  endfor
  call s:StageReveal()
  return '.'
endfunction

function! s:PreviousHunk(count) abort
  for i in range(a:count)
    if &filetype ==# 'fugitive'
      let lnum = search(s:file_pattern . '\|^@','Wbn')
      call s:StageInline('show', lnum)
      call search('^? .\|^@','Wb')
    else
      call search('^@@', 'Wb')
    endif
  endfor
  call s:StageReveal()
  return '.'
endfunction

function! s:NextFile(count) abort
  for i in range(a:count)
    exe s:StageInline('hide')
    if !search(s:file_pattern, 'W')
      break
    endif
  endfor
  exe s:StageInline('hide')
  return '.'
endfunction

function! s:PreviousFile(count) abort
  exe s:StageInline('hide')
  for i in range(a:count)
    if !search(s:file_pattern, 'Wb')
      break
    endif
    exe s:StageInline('hide')
  endfor
  return '.'
endfunction

function! s:NextItem(count) abort
  for i in range(a:count)
    if !search(s:item_pattern, 'W') && getline('.') !~# s:item_pattern
      call search('^commit ', 'W')
    endif
  endfor
  call s:StageReveal()
  return '.'
endfunction

function! s:PreviousItem(count) abort
  for i in range(a:count)
    if !search(s:item_pattern, 'Wbe') && getline('.') !~# s:item_pattern
      call search('^commit ', 'Wbe')
    endif
  endfor
  call s:StageReveal()
  return '.'
endfunction

let s:section_pattern = '^[A-Z][a-z][^:]*$'
let s:section_commit_pattern = s:section_pattern . '\|^commit '

function! s:NextSection(count) abort
  let orig = line('.')
  if getline('.') !~# '^commit '
    -
  endif
  for i in range(a:count)
    if !search(s:section_commit_pattern, 'W')
      break
    endif
  endfor
  if getline('.') =~# s:section_commit_pattern
    call s:StageReveal()
    return getline('.') =~# s:section_pattern ? '+' : ':'
  else
    return orig
  endif
endfunction

function! s:PreviousSection(count) abort
  let orig = line('.')
  if getline('.') !~# '^commit '
    -
  endif
  for i in range(a:count)
    if !search(s:section_commit_pattern . '\|\%^', 'bW')
      break
    endif
  endfor
  if getline('.') =~# s:section_commit_pattern || line('.') == 1
    call s:StageReveal()
    return getline('.') =~# s:section_pattern ? '+' : ':'
  else
    return orig
  endif
endfunction

function! s:NextSectionEnd(count) abort
  +
  if empty(getline('.'))
    +
  endif
  for i in range(a:count)
    if !search(s:section_commit_pattern, 'W')
      return '$'
    endif
  endfor
  return search('^.', 'Wb')
endfunction

function! s:PreviousSectionEnd(count) abort
  let old = line('.')
  for i in range(a:count)
    if search(s:section_commit_pattern, 'Wb') <= 1
      exe old
      if i
        break
      else
        return ''
      endif
    endif
    let old = line('.')
  endfor
  return search('^.', 'Wb')
endfunction

function! s:PatchSearchExpr(reverse) abort
  let line = getline('.')
  if col('.') ==# 1 && line =~# '^[+-]'
    if line =~# '^[+-]\{3\} '
      let pattern = '^[+-]\{3\} ' . substitute(escape(strpart(line, 4), '^$.*[]~\'), '^\w/', '\\w/', '') . '$'
    else
      let pattern = '^[+-]\s*' . escape(substitute(strpart(line, 1), '^\s*\|\s*$', '', ''), '^$.*[]~\') . '\s*$'
    endif
    if a:reverse
      return '?' . escape(pattern, '/?') . "\<CR>"
    else
      return '/' . escape(pattern, '/') . "\<CR>"
    endif
  endif
  return a:reverse ? '#' : '*'
endfunction

function! s:StageInline(mode, ...) abort
  if &filetype !=# 'fugitive'
    return ''
  endif
  let lnum1 = a:0 ? a:1 : line('.')
  let lnum = lnum1 + 1
  if a:0 > 1 && a:2 == 0 && lnum1 == 1
    let lnum = line('$') - 1
  elseif a:0 > 1 && a:2 == 0
    let info = s:StageInfo(lnum - 1)
    if empty(info.paths) && len(info.section)
      while len(getline(lnum))
        let lnum += 1
      endwhile
    endif
  elseif a:0 > 1
    let lnum += a:2 - 1
  endif
  while lnum > lnum1
    let lnum -= 1
    while lnum > 0 && getline(lnum) =~# '^[ @\+-]'
      let lnum -= 1
    endwhile
    let info = s:StageInfo(lnum)
    if !has_key(b:fugitive_diff, info.section)
      continue
    endif
    if getline(lnum + 1) =~# '^[ @\+-]'
      let lnum2 = lnum + 1
      while getline(lnum2 + 1) =~# '^[ @\+-]'
        let lnum2 += 1
      endwhile
      if a:mode !=# 'show'
        setlocal modifiable noreadonly
        exe 'silent keepjumps ' . (lnum + 1) . ',' . lnum2 . 'delete _'
        call remove(b:fugitive_expanded[info.section], info.filename)
        setlocal nomodifiable readonly nomodified
      endif
      continue
    endif
    if !has_key(b:fugitive_diff, info.section) || info.status !~# '^[ADMRU]$' || a:mode ==# 'hide'
      continue
    endif
    let mode = ''
    let diff = []
    let index = 0
    let start = -1
    for line in b:fugitive_diff[info.section]
      if mode ==# 'await' && line[0] ==# '@'
        let mode = 'capture'
      endif
      if mode !=# 'head' && line !~# '^[ @\+-]'
        if len(diff)
          break
        endif
        let start = index
        let mode = 'head'
      elseif mode ==# 'head' && line =~# '^diff '
        let start = index
      elseif mode ==# 'head' && substitute(line, "\t$", '', '') ==# '--- ' . info.relative[-1]
        let mode = 'await'
      elseif mode ==# 'head' && substitute(line, "\t$", '', '') ==# '+++ ' . info.relative[0]
        let mode = 'await'
      elseif mode ==# 'capture'
        call add(diff, line)
      elseif line[0] ==# '@'
        let mode = ''
      endif
      let index += 1
    endfor
    if len(diff)
      setlocal modifiable noreadonly
      silent call append(lnum, diff)
      let b:fugitive_expanded[info.section][info.filename] = [start, len(diff)]
      setlocal nomodifiable readonly nomodified
      if foldclosed(lnum+1) > 0
        silent exe (lnum+1) . ',' . (lnum+len(diff)) . 'foldopen!'
      endif
    endif
  endwhile
  return lnum
endfunction

function! s:NextExpandedHunk(count) abort
  for i in range(a:count)
    call s:StageInline('show', line('.'), 1)
    call search(s:file_pattern . '\|^@','W')
  endfor
  return '.'
endfunction

function! s:StageDiff(diff) abort
  let lnum = line('.')
  let info = s:StageInfo(lnum)
  let prefix = info.offset > 0 ? '+' . info.offset : ''
  if info.submodule =~# '^S'
    if info.section ==# 'Staged'
      return 'Git --paginate diff --no-ext-diff --submodule=log --cached -- ' . info.paths[0]
    elseif info.submodule =~# '^SC'
      return 'Git --paginate diff --no-ext-diff --submodule=log -- ' . info.paths[0]
    else
      return 'Git --paginate diff --no-ext-diff --submodule=diff -- ' . info.paths[0]
    endif
  elseif empty(info.paths) && info.section ==# 'Staged'
    return 'Git --paginate diff --no-ext-diff --cached'
  elseif empty(info.paths)
    return 'Git --paginate diff --no-ext-diff'
  elseif len(info.paths) > 1
    execute 'Gedit' . prefix s:fnameescape(':0:' . info.paths[0])
    return a:diff . '! @:'.s:fnameescape(info.paths[1])
  elseif info.section ==# 'Staged' && info.sigil ==# '-'
    execute 'Gedit' prefix s:fnameescape(':0:'.info.paths[0])
    return a:diff . '! :0:%'
  elseif info.section ==# 'Staged'
    execute 'Gedit' prefix s:fnameescape(':0:'.info.paths[0])
    return a:diff . '! @:%'
  elseif info.sigil ==# '-'
    execute 'Gedit' prefix s:fnameescape(':0:'.info.paths[0])
    return a:diff . '! :(top)%'
  else
    execute 'Gedit' prefix s:fnameescape(':(top)'.info.paths[0])
    return a:diff . '!'
  endif
endfunction

function! s:StageDiffEdit() abort
  let info = s:StageInfo(line('.'))
  let arg = (empty(info.paths) ? s:Tree() : info.paths[0])
  if info.section ==# 'Staged'
    return 'Git --paginate diff --no-ext-diff --cached '.s:fnameescape(arg)
  elseif info.status ==# '?'
    call s:TreeChomp('add', '--intent-to-add', '--', arg)
    return s:ReloadStatus()
  else
    return 'Git --paginate diff --no-ext-diff '.s:fnameescape(arg)
  endif
endfunction

function! s:StageApply(info, reverse, extra) abort
  if a:info.status ==# 'R'
    throw 'fugitive: patching renamed file not yet supported'
  endif
  let cmd = ['apply', '-p0', '--recount'] + a:extra
  let info = a:info
  let start = info.patch
  let end = info.lnum
  let lines = getline(start, end)
  if empty(filter(copy(lines), 'v:val =~# "^[+-]"'))
    return -1
  endif
  while getline(end) =~# '^[-+\ ]'
    let end += 1
    if getline(end) =~# '^[' . (a:reverse ? '+' : '-') . '\ ]'
      call add(lines, ' ' . getline(end)[1:-1])
    endif
  endwhile
  while start > 0 && getline(start) !~# '^@'
    let start -= 1
    if getline(start) =~# '^[' . (a:reverse ? '+' : '-') . ' ]'
      call insert(lines, ' ' . getline(start)[1:-1])
    elseif getline(start) =~# '^@'
      call insert(lines, getline(start))
    endif
  endwhile
  if start == 0
    throw 'fugitive: could not find hunk'
  elseif getline(start) !~# '^@@ '
    throw 'fugitive: cannot apply conflict hunk'
  endif
  let i = b:fugitive_expanded[info.section][info.filename][0]
  let head = []
  while get(b:fugitive_diff[info.section], i, '@') !~# '^@'
    let line = b:fugitive_diff[info.section][i]
    if line ==# '--- /dev/null'
      call add(head, '--- ' . get(b:fugitive_diff[info.section], i + 1, '')[4:-1])
    elseif line !~# '^new file '
      call add(head, line)
    endif
    let i += 1
  endwhile
  call extend(lines, head, 'keep')
  let temp = tempname()
  call writefile(lines, temp)
  if a:reverse
    call add(cmd, '--reverse')
  endif
  call extend(cmd, ['--', temp])
  let [output, exec_error] = s:ChompError(cmd)
  if !exec_error
    return 1
  endif
  call s:throw(output)
endfunction

function! s:StageDelete(lnum1, lnum2, count) abort
  let restore = []

  let err = ''
  let did_conflict_err = 0
  try
    for info in s:Selection(a:lnum1, a:lnum2)
      if empty(info.paths)
        continue
      endif
      let sub = get(get(get(b:fugitive_files, info.section, {}), info.filename, {}), 'submodule')
      if sub =~# '^S' && info.status ==# 'M'
        let undo = 'Git checkout ' . fugitive#RevParse('HEAD', FugitiveExtractGitDir(info.paths[0]))[0:10] . ' --'
      elseif sub =~# '^S'
        let err .= '|echoerr ' . string('fugitive: will not touch submodule ' . string(info.relative[0]))
        break
      elseif info.status ==# 'D'
        let undo = 'GRemove'
      elseif info.paths[0] =~# '/$'
        let err .= '|echoerr ' . string('fugitive: will not delete directory ' . string(info.relative[0]))
        break
      else
        let undo = 'Gread ' . s:TreeChomp('hash-object', '-w', '--', info.paths[0])[0:10]
      endif
      if info.patch
        call s:StageApply(info, 1, info.section ==# 'Staged' ? ['--index'] : [])
      elseif sub =~# '^S'
        if info.section ==# 'Staged'
          call s:TreeChomp('reset', '--', info.paths[0])
        endif
        call s:TreeChomp('submodule', 'update', '--', info.paths[0])
      elseif info.status ==# '?'
        call s:TreeChomp('clean', '-f', '--', info.paths[0])
      elseif a:count == 2
        if get(b:fugitive_files['Staged'], info.filename, {'status': ''}).status ==# 'D'
          call delete(FugitiveVimPath(info.paths[0]))
        else
          call s:TreeChomp('checkout', '--ours', '--', info.paths[0])
        endif
      elseif a:count == 3
        if get(b:fugitive_files['Unstaged'], info.filename, {'status': ''}).status ==# 'D'
          call delete(FugitiveVimPath(info.paths[0]))
        else
          call s:TreeChomp('checkout', '--theirs', '--', info.paths[0])
        endif
      elseif info.status =~# '[ADU]' &&
            \ get(b:fugitive_files[info.section ==# 'Staged' ? 'Unstaged' : 'Staged'], info.filename, {'status': ''}).status =~# '[AU]'
        if get(g:, 'fugitive_conflict_x', 0)
          call s:TreeChomp('checkout', info.section ==# 'Unstaged' ? '--ours' : '--theirs', '--', info.paths[0])
        else
          if !did_conflict_err
            let err .= '|echoerr "Use 2X for --ours or 3X for --theirs"'
            let did_conflict_err = 1
          endif
          continue
        endif
      elseif info.status ==# 'U'
        call delete(FugitiveVimPath(info.paths[0]))
      elseif info.status ==# 'A'
        call s:TreeChomp('rm', '-f', '--', info.paths[0])
      elseif info.section ==# 'Unstaged'
        call s:TreeChomp('checkout', '--', info.paths[0])
      else
        call s:TreeChomp('checkout', '@', '--', info.paths[0])
      endif
      if len(undo)
        call add(restore, ':Gsplit ' . s:fnameescape(info.relative[0]) . '|' . undo)
      endif
    endfor
  catch /^fugitive:/
    let err .= '|echoerr ' . string(v:exception)
  endtry
  if empty(restore)
    return err[1:-1]
  endif
  exe s:ReloadStatus()
  call s:StageReveal()
  if len(restore)
    return 'checktime|redraw|echomsg ' . string('To restore, ' . join(restore, '|')) . err
  else
    return 'checktime|redraw' . err
  endif
endfunction

function! s:StageIgnore(lnum1, lnum2, count) abort
  let paths = []
  for info in s:Selection(a:lnum1, a:lnum2)
    call extend(paths, info.relative)
  endfor
  call map(paths, '"/" . v:val')
  if !a:0
    let dir = fugitive#Find('.git/info/')
    if !isdirectory(dir)
      try
        call mkdir(dir)
      catch
      endtry
    endif
  endif
  exe 'Gsplit' (a:count ? '.gitignore' : '.git/info/exclude')
  let last = line('$')
  if last == 1 && empty(getline(1))
    call setline(last, paths)
  else
    call append(last, paths)
    exe last + 1
  endif
  return ''
endfunction

function! s:DoToggleHeadHeader(value) abort
  exe 'edit' s:fnameescape(s:Dir())
  call search('\C^index$', 'wc')
endfunction

function! s:DoToggleHelpHeader(value) abort
  exe 'help fugitive-map'
endfunction

function! s:DoStagePushHeader(value) abort
  let remote = matchstr(a:value, '\zs[^/]\+\ze/')
  if empty(remote)
    let remote = '.'
  endif
  let branch = matchstr(a:value, '\%([^/]\+/\)\=\zs\S\+')
  call feedkeys(':Git push ' . remote . ' ' . branch)
endfunction

function! s:DoTogglePushHeader(value) abort
  return s:DoStagePushHeader(a:value)
endfunction

function! s:DoStageUnpushedHeading(heading) abort
  let remote = matchstr(a:heading, 'to \zs[^/]\+\ze/')
  if empty(remote)
    let remote = '.'
  endif
  let branch = matchstr(a:heading, 'to \%([^/]\+/\)\=\zs\S\+')
  call feedkeys(':Git push ' . remote . ' ' . '@:' . 'refs/heads/' . branch)
endfunction

function! s:DoToggleUnpushedHeading(heading) abort
  return s:DoStageUnpushedHeading(a:heading)
endfunction

function! s:DoStageUnpushed(record) abort
  let remote = matchstr(a:record.heading, 'to \zs[^/]\+\ze/')
  if empty(remote)
    let remote = '.'
  endif
  let branch = matchstr(a:record.heading, 'to \%([^/]\+/\)\=\zs\S\+')
  call feedkeys(':Git push ' . remote . ' ' . a:record.commit . ':' . 'refs/heads/' . branch)
endfunction

function! s:DoToggleUnpushed(record) abort
  return s:DoStageUnpushed(a:record)
endfunction

function! s:DoUnstageUnpulledHeading(heading) abort
  call feedkeys(':Git rebase')
endfunction

function! s:DoToggleUnpulledHeading(heading) abort
  call s:DoUnstageUnpulledHeading(a:heading)
endfunction

function! s:DoUnstageUnpulled(record) abort
  call feedkeys(':Git rebase ' . a:record.commit)
endfunction

function! s:DoToggleUnpulled(record) abort
  call s:DoUnstageUnpulled(a:record)
endfunction

function! s:DoUnstageUnpushed(record) abort
  call feedkeys(':Git -c sequence.editor=true rebase --interactive --autosquash ' . a:record.commit . '^')
endfunction

function! s:DoToggleStagedHeading(...) abort
  call s:TreeChomp('reset', '-q')
  return 1
endfunction

function! s:DoUnstageStagedHeading(heading) abort
  return s:DoToggleStagedHeading(a:heading)
endfunction

function! s:DoToggleUnstagedHeading(...) abort
  call s:TreeChomp('add', '-u')
  return 1
endfunction

function! s:DoStageUnstagedHeading(heading) abort
  return s:DoToggleUnstagedHeading(a:heading)
endfunction

function! s:DoToggleUntrackedHeading(...) abort
  call s:TreeChomp('add', '.')
  return 1
endfunction

function! s:DoStageUntrackedHeading(heading) abort
  return s:DoToggleUntrackedHeading(a:heading)
endfunction

function! s:DoToggleStaged(record) abort
  if a:record.patch
    return s:StageApply(a:record, 1, ['--cached'])
  else
    call s:TreeChomp(['reset', '-q', '--'] + a:record.paths)
    return 1
  endif
endfunction

function! s:DoUnstageStaged(record) abort
  return s:DoToggleStaged(a:record)
endfunction

function! s:DoToggleUnstaged(record) abort
  if a:record.patch
    return s:StageApply(a:record, 0, ['--cached'])
  else
    call s:TreeChomp(['add', '-A', '--'] + a:record.paths)
    return 1
  endif
endfunction

function! s:DoStageUnstaged(record) abort
  return s:DoToggleUnstaged(a:record)
endfunction

function! s:DoUnstageUnstaged(record) abort
  if a:record.status ==# 'A'
    call s:TreeChomp(['reset', '-q', '--'] + a:record.paths)
    return 1
  else
    return -1
  endif
endfunction

function! s:DoToggleUntracked(record) abort
  call s:TreeChomp(['add', '--'] + a:record.paths)
  return 1
endfunction

function! s:DoStageUntracked(record) abort
  return s:DoToggleUntracked(a:record)
endfunction

function! s:StagePatch(lnum1,lnum2) abort
  let add = []
  let reset = []
  let intend = []

  for lnum in range(a:lnum1,a:lnum2)
    let info = s:StageInfo(lnum)
    if empty(info.paths) && info.section ==# 'Staged'
      return 'Git reset --patch'
    elseif empty(info.paths) && info.section ==# 'Unstaged'
      return 'Git add --patch'
    elseif empty(info.paths) && info.section ==# 'Untracked'
      return 'Git add --interactive'
    elseif empty(info.paths)
      continue
    endif
    execute lnum
    if info.section ==# 'Staged'
      let reset += info.relative
    elseif info.section ==# 'Untracked'
      let intend += info.paths
    elseif info.status !~# '^D'
      let add += info.relative
    endif
  endfor
  try
    if !empty(intend)
      call s:TreeChomp(['add', '--intent-to-add', '--'] + intend)
    endif
    if !empty(add)
      execute "Git add --patch -- ".join(map(add,'s:fnameescape(v:val)'))
    endif
    if !empty(reset)
      execute "Git reset --patch -- ".join(map(reset,'s:fnameescape(v:val)'))
    endif
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  return s:ReloadStatus()
endfunction

" Section: :Git commit, :Git revert

function! s:CommitInteractive(line1, line2, range, bang, mods, options, patch) abort
  let status = s:StatusCommand(a:line1, a:line2, a:range, a:line2, a:bang, a:mods, '', '', [], a:options.dir)
  let status = len(status) ? status . '|' : ''
  if a:patch
    return status . 'if search("^Unstaged")|exe "normal >"|exe "+"|endif'
  else
    return status . 'if search("^Untracked\\|^Unstaged")|exe "+"|endif'
  endif
endfunction

function! s:CommitSubcommand(line1, line2, range, bang, mods, options) abort
  let argv = copy(a:options.subcommand_args)
  let i = 0
  while get(argv, i, '--') !=# '--'
    if argv[i] =~# '^-[apzsneiovq].'
      call insert(argv, argv[i][0:1])
      let argv[i+1] = '-' . argv[i+1][2:-1]
    else
      let i += 1
    endif
  endwhile
  if s:HasOpt(argv, '-i', '--interactive')
    return s:CommitInteractive(a:line1, a:line2, a:range, a:bang, a:mods, a:options, 0)
  elseif s:HasOpt(argv, '-p', '--patch')
    return s:CommitInteractive(a:line1, a:line2, a:range, a:bang, a:mods, a:options, 1)
  else
    return {}
  endif
endfunction

function! s:RevertSubcommand(line1, line2, range, bang, mods, options) abort
  return {'insert_args': ['--edit']}
endfunction

function! fugitive#CommitComplete(A, L, P, ...) abort
  let dir = a:0 ? a:1 : s:Dir()
  if a:A =~# '^--fixup=\|^--squash='
    let commits = s:LinesError([dir, 'log', '--pretty=format:%s', '@{upstream}..'])[0]
    let pre = matchstr(a:A, '^--\w*=''\=') . ':/^'
    if pre =~# "'"
      call map(commits, 'pre . string(tr(v:val, "|\"^$*[]", "......."))[1:-1]')
      call filter(commits, 'strpart(v:val, 0, strlen(a:A)) ==# a:A')
      return commits
    else
      return s:FilterEscape(map(commits, 'pre . tr(v:val, "\\ !^$*?[]()''\"`&;<>|#", "....................")'), a:A)
    endif
  else
    return s:CompleteSub('commit', a:A, a:L, a:P, function('fugitive#CompletePath'), a:000)
  endif
  return []
endfunction

function! fugitive#RevertComplete(A, L, P, ...) abort
  return s:CompleteSub('revert', a:A, a:L, a:P, function('s:CompleteRevision'), a:000)
endfunction

" Section: :Git merge, :Git rebase, :Git pull

function! fugitive#MergeComplete(A, L, P, ...) abort
  return s:CompleteSub('merge', a:A, a:L, a:P, function('s:CompleteRevision'), a:000)
endfunction

function! fugitive#RebaseComplete(A, L, P, ...) abort
  return s:CompleteSub('rebase', a:A, a:L, a:P, function('s:CompleteRevision'), a:000)
endfunction

function! fugitive#PullComplete(A, L, P, ...) abort
  return s:CompleteSub('pull', a:A, a:L, a:P, function('s:CompleteRemote'), a:000)
endfunction

function! s:MergeSubcommand(line1, line2, range, bang, mods, options) abort
  let dir = a:options.dir
  if empty(a:options.subcommand_args) && (
        \ filereadable(fugitive#Find('.git/MERGE_MSG', dir)) ||
        \ isdirectory(fugitive#Find('.git/rebase-apply', dir)) ||
        \  !empty(s:TreeChomp(dir, 'diff-files', '--diff-filter=U')))
    return 'echoerr ":Git merge for loading conflicts hase been removed in favor of :Git mergetool"'
  endif
  return {}
endfunction

function! s:RebaseSubcommand(line1, line2, range, bang, mods, options) abort
  let args = a:options.subcommand_args
  if s:HasOpt(args, '--autosquash') && !s:HasOpt(args, '-i', '--interactive')
    return {'env': {'GIT_SEQUENCE_EDITOR': 'true'}, 'insert_args': ['--interactive']}
  endif
  return {}
endfunction

" Section: :Git difftool, :Git mergetool

function! s:ToolItems(state, from, to, offsets, text, ...) abort
  let items = []
  for i in range(len(a:state.diff))
    let diff = a:state.diff[i]
    let path = (i == len(a:state.diff) - 1) ? a:to : a:from
    if empty(path)
      return []
    endif
    let item = {
          \ 'valid': a:0 ? a:1 : 1,
          \ 'filename': diff.filename . FugitiveVimPath(path),
          \ 'lnum': matchstr(get(a:offsets, i), '\d\+'),
          \ 'text': a:text}
    if len(get(diff, 'module', ''))
      let item.module = diff.module . path
    endif
    call add(items, item)
  endfor
  let diff = items[0:-2]
  let items[-1].context = {'diff': items[0:-2]}
  return [items[-1]]
endfunction

function! s:ToolToFrom(str) abort
  if a:str =~# ' => '
    let str = a:str =~# '{.* => .*}' ? a:str : '{' . a:str . '}'
    return [substitute(str, '{.* => \(.*\)}', '\1', ''),
          \ substitute(str, '{\(.*\) => .*}', '\1', '')]
  else
    return [a:str, a:str]
  endif
endfunction

function! s:ToolParse(state, line) abort
  if type(a:line) !=# type('') || a:state.mode ==# 'hunk' && a:line =~# '^[ +-]'
    return []
  elseif a:line =~# '^diff '
    let a:state.mode = 'diffhead'
    let a:state.from = ''
    let a:state.to = ''
  elseif a:state.mode ==# 'diffhead' && a:line =~# '^--- [^/]'
    let a:state.from = a:line[4:-1]
    let a:state.to = a:state.from
  elseif a:state.mode ==# 'diffhead' && a:line =~# '^+++ [^/]'
    let a:state.to = a:line[4:-1]
    if empty(get(a:state, 'from', ''))
      let a:state.from = a:state.to
    endif
  elseif a:line[0] ==# '@'
    let a:state.mode = 'hunk'
    if has_key(a:state, 'from')
      let offsets = split(matchstr(a:line, '^@\+ \zs[-+0-9, ]\+\ze @'), ' ')
      return s:ToolItems(a:state, a:state.from, a:state.to, offsets, matchstr(a:line, ' @@\+ \zs.*'))
    endif
  elseif a:line =~# '^\* Unmerged path .'
    let file = a:line[16:-1]
    return s:ToolItems(a:state, file, file, [], '')
  elseif a:line =~# '^[A-Z]\d*\t.\|^:.*\t.'
    " --raw, --name-status
    let [status; files] = split(a:line, "\t")
    return s:ToolItems(a:state, files[0], files[-1], [], a:state.name_only ? '' : status)
  elseif a:line =~# '^ \S.* |'
    " --stat
    let [_, to, changes; __] = matchlist(a:line, '^ \(.\{-\}\) \+|\zs \(.*\)$')
    let [to, from] = s:ToolToFrom(to)
    return s:ToolItems(a:state, from, to, [], changes)
  elseif a:line =~# '^ *\([0-9.]\+%\) .'
    " --dirstat
    let [_, changes, to; __] = matchlist(a:line, '^ *\([0-9.]\+%\) \(.*\)')
    return s:ToolItems(a:state, to, to, [], changes)
  elseif a:line =~# '^\(\d\+\|-\)\t\(\d\+\|-\)\t.'
    " --numstat
    let [_, add, remove, to; __] = matchlist(a:line, '^\(\d\+\|-\)\t\(\d\+\|-\)\t\(.*\)')
    let [to, from] = s:ToolToFrom(to)
    return s:ToolItems(a:state, from, to, [], add ==# '-' ? 'Binary file' : '+' . add . ' -' . remove, add !=# '-')
  elseif a:state.mode !=# 'diffhead' && a:state.mode !=# 'hunk' && len(a:line) || a:line =~# '^git: \|^usage: \|^error: \|^fatal: '
    return [{'text': a:line}]
  endif
  return []
endfunction

function! s:ToolStream(line1, line2, range, bang, mods, options, args, state) abort
  let i = 0
  let argv = copy(a:args)
  let prompt = 1
  let state = a:state
  while i < len(argv)
    let match = matchlist(argv[i], '^\(-[a-zABDFH-KN-RT-Z]\)\ze\(.*\)')
    if len(match) && len(match[2])
      call insert(argv, match[1])
      let argv[i+1] = '-' . match[2]
      continue
    endif
    let arg = argv[i]
    if arg =~# '^-t$\|^--tool=\|^--tool-help$\|^--help$'
      return {}
    elseif arg =~# '^-y$\|^--no-prompt$'
      let prompt = 0
      call remove(argv, i)
      continue
    elseif arg ==# '--prompt'
      let prompt = 1
      call remove(argv, i)
      continue
    elseif arg =~# '^--\%(no-\)\=\(symlinks\|trust-exit-code\|gui\)$'
      call remove(argv, i)
      continue
    elseif arg ==# '--'
      break
    endif
    let i += 1
  endwhile
  let a:state.mode = 'init'
  let a:state.from = ''
  let a:state.to = ''
  let exec = s:UserCommandList({'git': a:options.git, 'dir': a:options.dir}) + ['-c', 'diff.context=0']
  let exec += a:options.flags + ['--no-pager', 'diff', '--no-ext-diff', '--no-color', '--no-prefix'] + argv
  if prompt
    let title = ':Git ' . s:fnameescape(a:options.flags + [a:options.subcommand] + a:options.subcommand_args)
    return s:QuickfixStream(a:line2, 'difftool', title, exec, !a:bang, a:mods, s:function('s:ToolParse'), a:state)
  else
    let filename = ''
    let cmd = []
    let tabnr = tabpagenr() + 1
    for line in split(s:SystemError(s:shellesc(exec))[0], "\n")
      for item in s:ToolParse(a:state, line)
        if len(get(item, 'filename', '')) && item.filename != filename
          call add(cmd, 'tabedit ' . s:fnameescape(item.filename))
          for i in reverse(range(len(get(item.context, 'diff', []))))
            call add(cmd, (i ? 'rightbelow' : 'leftabove') . ' vert Gdiffsplit! ' . s:fnameescape(item.context.diff[i].filename))
          endfor
          call add(cmd, 'wincmd =')
          let filename = item.filename
        endif
      endfor
    endfor
    return join(cmd, '|') . (empty(cmd) ? '' : '|' . tabnr . 'tabnext')
  endif
endfunction

function! s:MergetoolSubcommand(line1, line2, range, bang, mods, options) abort
  let dir = a:options.dir
  exe s:DirCheck(dir)
  let i = 0
  let prompt = 1
  let cmd = ['diff', '--diff-filter=U']
  let state = {'name_only': 0}
  let state.diff = [{'prefix': ':2:', 'module': ':2:'}, {'prefix': ':3:', 'module': ':3:'}, {'prefix': ':(top)'}]
  call map(state.diff, 'extend(v:val, {"filename": fugitive#Find(v:val.prefix, dir)})')
  return s:ToolStream(a:line1, a:line2, a:range, a:bang, a:mods, a:options, ['--diff-filter=U'] + a:options.subcommand_args, state)
endfunction

function! s:DifftoolSubcommand(line1, line2, range, bang, mods, options) abort
  let dir = a:options.dir
  exe s:DirCheck(dir)
  let i = 0
  let argv = copy(a:options.subcommand_args)
  let commits = []
  let cached = 0
  let reverse = 1
  let prompt = 1
  let state = {'name_only': 0}
  let merge_base_against = {}
  let dash = (index(argv, '--') > i ? ['--'] : [])
  while i < len(argv)
    let match = matchlist(argv[i], '^\(-[a-zABDFH-KN-RT-Z]\)\ze\(.*\)')
    if len(match) && len(match[2])
      call insert(argv, match[1])
      let argv[i+1] = '-' . match[2]
      continue
    endif
    let arg = argv[i]
    if arg ==# '--cached'
      let cached = 1
    elseif arg ==# '-R'
      let reverse = 1
    elseif arg ==# '--name-only'
      let state.name_only = 1
      let argv[0] = '--name-status'
    elseif arg ==# '--'
      break
    elseif arg !~# '^-\|^\.\.\=\%(/\|$\)'
      let parsed = s:LinesError(['rev-parse', '--revs-only', substitute(arg, ':.*', '', '')] + dash)[0]
      call map(parsed, '{"uninteresting": v:val =~# "^\\^", "prefix": substitute(v:val, "^\\^", "", "") . ":"}')
      let merge_base_against = {}
      if arg =~# '\.\.\.' && len(parsed) > 2
        let display = map(split(arg, '\.\.\.', 1), 'empty(v:val) ? "@" : v:val')
        if len(display) == 2
          let parsed[0].module = display[1] . ':'
          let parsed[1].module = display[0] . ':'
        endif
        let parsed[2].module = arg . ':'
        if empty(commits)
          let merge_base_against = parsed[0]
          let parsed = [parsed[2]]
        endif
      elseif arg =~# '\.\.' && len(parsed) == 2
        let display = map(split(arg, '\.\.', 1), 'empty(v:val) ? "@" : v:val')
        if len(display) == 2
          let parsed[0].module = display[0] . ':'
          let parsed[1].module = display[1] . ':'
        endif
      elseif len(parsed) == 1
        let parsed[0].module = arg . ':'
      endif
      call extend(commits, parsed)
    endif
    let i += 1
  endwhile
  if len(merge_base_against)
    call add(commits, merge_base_against)
  endif
  let commits = filter(copy(commits), 'v:val.uninteresting') + filter(commits, '!v:val.uninteresting')
  if cached
    if empty(commits)
      call add(commits, {'prefix': '@:', 'module': '@:'})
    endif
    call add(commits, {'prefix': ':0:', 'module': ':0:'})
  elseif len(commits) < 2
    call add(commits, {'prefix': ':(top)'})
    if len(commits) < 2
      call insert(commits, {'prefix': ':0:', 'module': ':0:'})
    endif
  endif
  if reverse
    let commits = [commits[-1]] + repeat([commits[0]], len(commits) - 1)
    call reverse(commits)
  endif
  if len(commits) > 2
    call add(commits, remove(commits, 0))
  endif
  call map(commits, 'extend(v:val, {"filename": fugitive#Find(v:val.prefix, dir)})')
  let state.diff = commits
  return s:ToolStream(a:line1, a:line2, a:range, a:bang, a:mods, a:options, argv, state)
endfunction

" Section: :Ggrep, :Glog

if !exists('g:fugitive_summary_format')
  let g:fugitive_summary_format = '%s'
endif

function! fugitive#GrepComplete(A, L, P) abort
  return s:CompleteSub('grep', a:A, a:L, a:P)
endfunction

function! fugitive#LogComplete(A, L, P) abort
  return s:CompleteSub('log', a:A, a:L, a:P)
endfunction

function! s:GrepParseLine(prefix, name_only, dir, line) abort
  let entry = {'valid': 1}
  let match = matchlist(a:line, '^\(.\{-\}\):\(\d\+\):\(\d\+:\)\=\(.*\)$')
  if len(match)
    let entry.module = match[1]
    let entry.lnum = +match[2]
    let entry.col = +match[3]
    let entry.text = match[4]
  elseif a:line =~# '^git: \|^usage: \|^error: \|^fatal: '
    return {'text': a:line}
  else
    let entry.module = matchstr(a:line, '\CBinary file \zs.*\ze matches$')
    if len(entry.module)
      let entry.text = 'Binary file'
      let entry.valid = 0
    endif
  endif
  if empty(entry.module) && a:name_only
    let entry.module = a:line
  endif
  if empty(entry.module)
    return {'text': a:line}
  endif
  if entry.module !~# ':'
    let entry.filename = a:prefix . entry.module
  else
    let entry.filename = fugitive#Find(entry.module, a:dir)
  endif
  return entry
endfunction

function! s:GrepSubcommand(line1, line2, range, bang, mods, options) abort
  let dir = a:options.dir
  exe s:DirCheck(dir)
  let listnr = a:line1 == 0 ? a:line1 : a:line2
  let cmd = ['--no-pager', 'grep', '-n', '--no-color', '--full-name']
  let tree = s:Tree(dir)
  let args = a:options.subcommand_args
  if get(args, 0, '') =~# '^-O\|--open-files-in-pager$'
    let args = args[1:-1]
  endif
  let name_only = s:HasOpt(args, '-l', '--files-with-matches', '--name-only', '-L', '--files-without-match')
  if listnr > 0
    exe listnr 'wincmd w'
  else
    call s:BlurStatus()
  endif
  redraw
  call s:QuickfixCreate(listnr, {'title': (listnr < 0 ? ':Git grep ' : ':0Git grep ') . s:fnameescape(args)})
  let tempfile = tempname()
  let event = listnr < 0 ? 'grep-fugitive' : 'lgrep-fugitive'
  silent exe s:DoAutocmd('QuickFixCmdPre ' . event)
  let prefix = FugitiveVimPath(s:HasOpt(args, '--cached') || empty(tree) ?
        \ 'fugitive://' . dir . '//0/' :
        \ s:cpath(getcwd(), tree) ? '' : tree . '/')
  try
    if exists('+guioptions') && &guioptions =~# '!'
      let guioptions = &guioptions
      set guioptions-=!
    endif
    exe '!' . escape(s:UserCommand(a:options, cmd + args), '%#!')
          \ printf(&shellpipe . (&shellpipe =~# '%s' ? '' : ' %s'), s:shellesc(tempfile))
  finally
    if exists('guioptions')
      let &guioptions = guioptions
    endif
  endtry
  let list = map(readfile(tempfile), 's:GrepParseLine(prefix, name_only, dir, v:val)')
  call s:QuickfixSet(listnr, list, 'a')
  silent exe s:DoAutocmd('QuickFixCmdPost ' . event)
  if !has('gui_running')
    redraw
  endif
  if !a:bang && !empty(list)
    return (listnr < 0 ? 'c' : 'l').'first'
  else
    return ''
  endif
endfunction

function! fugitive#GrepCommand(line1, line2, range, bang, mods, arg) abort
  return fugitive#Command(a:line1, a:line2, a:range, a:bang, a:mods,
        \ "grep -O " . (fugitive#GitVersion(2, 19) ? "--column " : "") . a:arg)
endfunction

let s:log_diff_context = '{"filename": fugitive#Find(v:val . from, a:dir), "lnum": get(offsets, v:key), "module": strpart(v:val, 0, len(a:state.base_module)) . from}'

function! s:LogFlushQueue(state, dir) abort
  let queue = remove(a:state, 'queue')
  if a:state.child_found && get(a:state, 'ignore_commit')
    call remove(queue, 0)
  elseif len(queue) && len(a:state.target) && len(get(a:state, 'parents', []))
    let from = substitute(a:state.target, '^/', ':', '')
    let offsets = []
    let queue[0].context.diff = map(copy(a:state.parents), s:log_diff_context)
  endif
  if len(queue) && queue[-1] ==# {'text': ''}
    call remove(queue, -1)
  endif
  return queue
endfunction

function! s:LogParse(state, dir, line) abort
  if a:state.mode ==# 'hunk' && a:line =~# '^[-+ ]'
    return []
  endif
  let list = matchlist(a:line, '^\%(fugitive \(.\{-\}\)\t\|commit \|From \)\=\(\x\{40,\}\)\%( \(.*\)\)\=$')
  if len(list)
    let queue = s:LogFlushQueue(a:state, a:dir)
    let a:state.mode = 'commit'
    let a:state.base = 'fugitive://' . a:dir . '//' . list[2]
    if len(list[1])
      let [a:state.base_module; a:state.parents] = split(list[1], ' ')
    else
      let a:state.base_module = list[2]
      let a:state.parents = []
    endif
    let a:state.message = list[3]
    let a:state.from = ''
    let a:state.to = ''
    let context = {}
    let a:state.queue = [{
          \ 'valid': 1,
          \ 'context': context,
          \ 'filename': a:state.base . a:state.target,
          \ 'module': a:state.base_module . substitute(a:state.target, '^/', ':', ''),
          \ 'text': a:state.message}]
    let a:state.child_found = 0
    return queue
  elseif type(a:line) == type(0)
    return s:LogFlushQueue(a:state, a:dir)
  elseif a:line =~# '^diff'
    let a:state.mode = 'diffhead'
    let a:state.from = ''
    let a:state.to = ''
  elseif a:state.mode ==# 'diffhead' && a:line =~# '^--- \w/'
    let a:state.from = a:line[6:-1]
    let a:state.to = a:state.from
  elseif a:state.mode ==# 'diffhead' && a:line =~# '^+++ \w/'
    let a:state.to = a:line[6:-1]
    if empty(get(a:state, 'from', ''))
      let a:state.from = a:state.to
    endif
  elseif a:line =~# '^@@[^@]*+\d' && len(get(a:state, 'to', '')) && has_key(a:state, 'base')
    let a:state.mode = 'hunk'
    if empty(a:state.target) || a:state.target ==# '/' . a:state.to
      if !a:state.child_found && len(a:state.queue) && a:state.queue[-1] ==# {'text': ''}
        call remove(a:state.queue, -1)
      endif
      let a:state.child_found = 1
      let offsets = map(split(matchstr(a:line, '^@\+ \zs[-+0-9, ]\+\ze @'), ' '), '+matchstr(v:val, "\\d\\+")')
      let context = {}
      if len(a:state.parents)
        let from = ":" . a:state.from
        let context.diff = map(copy(a:state.parents), s:log_diff_context)
      endif
      call add(a:state.queue, {
            \ 'valid': 1,
            \ 'context': context,
            \ 'filename': FugitiveVimPath(a:state.base . '/' . a:state.to),
            \ 'module': a:state.base_module . ':' . a:state.to,
            \ 'lnum': offsets[-1],
            \ 'text': a:state.message . matchstr(a:line, ' @@\+ .\+')})
    endif
  elseif a:state.follow &&
        \ a:line =~# '^ \%(mode change \d\|\%(create\|delete\) mode \d\|\%(rename\|copy\|rewrite\) .* (\d\+%)$\)'
    let rename = matchstr(a:line, '^ rename \zs.* => .*\ze (\d\+%)$')
    if len(rename)
      let rename = rename =~# '{.* => .*}' ? rename : '{' . rename . '}'
      if a:state.target ==# simplify('/' . substitute(rename, '{.* => \(.*\)}', '\1', ''))
        let a:state.target = simplify('/' . substitute(rename, '{\(.*\) => .*}', '\1', ''))
      endif
    endif
    if !get(a:state, 'ignore_summary')
      call add(a:state.queue, {'text': a:line})
    endif
  elseif a:state.mode ==# 'commit' || a:state.mode ==# 'init'
    call add(a:state.queue, {'text': a:line})
  endif
  return []
endfunction

function! fugitive#LogCommand(line1, count, range, bang, mods, args, type) abort
  let dir = s:Dir()
  exe s:DirCheck(dir)
  let listnr = a:type =~# '^l' ? 0 : -1
  let [args, after] = s:SplitExpandChain('log ' . a:args, s:Tree(dir))
  call remove(args, 0)
  let split = index(args, '--')
  if split > 0
    let paths = args[split : -1]
    let args = args[0 : split - 1]
  elseif split == 0
    let paths = args
    let args = []
  else
    let paths = []
  endif
  if a:line1 == 0 && a:count
    let path = fugitive#Path(bufname(a:count), '/', dir)
    let titlepre = ':0,' . a:count
  elseif a:count >= 0
    let path = fugitive#Path(@%, '/', dir)
    let titlepre = a:count == 0 ? ':0,' . bufnr('') : ':'
  else
    let titlepre = ':'
    let path = ''
  endif
  let range = ''
  let extra_args = []
  let extra_paths = []
  let state = {'mode': 'init', 'child_found': 0, 'queue': [], 'follow': 0}
  if path =~# '^/\.git\%(/\|$\)\|^$'
    let path = ''
  elseif a:line1 == 0
    let range = "0," . (a:count ? a:count : bufnr(''))
    let extra_paths = ['.' . path]
    if (empty(paths) || paths ==# ['--']) && !s:HasOpt(args, '--no-follow')
      let state.follow = 1
      if !s:HasOpt(args, '--follow')
        call insert(extra_args, '--follow')
      endif
      if !s:HasOpt(args, '--summary')
        call insert(extra_args, '--summary')
        let state.ignore_summary = 1
      endif
    endif
    let state.ignore_commit = 1
  elseif a:count > 0
    if !s:HasOpt(args, '--merges', '--no-merges')
      call insert(extra_args, '--no-merges')
    endif
    call add(args, '-L' . a:line1 . ',' . a:count . ':' . path[1:-1])
    let state.ignore_commit = 1
  endif
  if len(path) && empty(filter(copy(args), 'v:val =~# "^[^-]"'))
    let owner = s:Owner(@%, dir)
    if len(owner)
      call add(args, owner . (owner =~# '^\x\{40,}' ? '' : '^{}'))
    endif
  endif
  if empty(extra_paths)
    let path = ''
  endif
  if s:HasOpt(args, '-g', '--walk-reflogs')
    let format = "%gd %P\t%H %gs"
  else
    let format = "%h %P\t%H " . g:fugitive_summary_format
  endif
  let cmd = ['--no-pager']
  call extend(cmd, ['-c', 'diff.context=0', '-c', 'diff.noprefix=false', 'log'] +
        \ ['--no-color', '--no-ext-diff', '--pretty=format:fugitive ' . format] +
        \ args + extra_args + paths + extra_paths)
  let state.target = path
  let title = titlepre . (listnr < 0 ? 'Gclog ' : 'Gllog ') . s:fnameescape(args + paths)
  return s:QuickfixStream(listnr, 'log', title, s:UserCommandList(dir) + cmd, !a:bang, a:mods, s:function('s:LogParse'), state, dir) . after
endfunction

" Section: :Gedit, :Gpedit, :Gsplit, :Gvsplit, :Gtabedit, :Gread

function! s:UsableWin(nr) abort
  return a:nr && !getwinvar(a:nr, '&previewwindow') && !getwinvar(a:nr, '&winfixwidth') &&
        \ (empty(getwinvar(a:nr, 'fugitive_status')) || getbufvar(winbufnr(a:nr), 'fugitive_type') !=# 'index') &&
        \ index(['gitrebase', 'gitcommit'], getbufvar(winbufnr(a:nr), '&filetype')) < 0 &&
        \ index(['nofile','help','quickfix', 'terminal'], getbufvar(winbufnr(a:nr), '&buftype')) < 0
endfunction

function! s:ArgSplit(string) abort
  let string = a:string
  let args = []
  while string =~# '\S'
    let arg = matchstr(string, '^\s*\%(\\.\|[^[:space:]]\)\+')
    let string = strpart(string, len(arg))
    let arg = substitute(arg, '^\s\+', '', '')
    call add(args, substitute(arg, '\\\@<!\\ ', ' ', 'g'))
  endwhile
  return args
endfunction

function! s:OpenParse(string, wants_cmd) abort
  let opts = []
  let cmds = []
  let args = s:ArgSplit(a:string)
  while !empty(args)
    if args[0] =~# '^++'
      call add(opts, ' ' . escape(remove(args, 0), ' |"'))
    elseif a:wants_cmd && args[0] =~# '^+'
      call add(cmds, remove(args, 0)[1:-1])
    else
      break
    endif
  endwhile
  if len(args)
    let file = join(args)
    if file ==# '-'
      let result = fugitive#Result()
      if has_key(result, 'file')
        let file = s:fnameescape(result.file)
      else
        throw 'fugitive: no previous command output'
      endif
    endif
  elseif empty(expand('%'))
    let file = ''
  elseif empty(s:DirCommitFile(@%)[1]) && s:Relative('./') !~# '^\./\.git\>'
    let file = '>:0'
  else
    let file = '>'
  endif
  let dir = s:Dir()
  let efile = s:Expand(file)
  let url = s:Generate(efile, dir)

  if a:wants_cmd && file[0] ==# '>' && efile[0] !=# '>' && get(b:, 'fugitive_type', '') isnot# 'tree' && &filetype !=# 'netrw'
    let line = line('.')
    if expand('%:p') !=# url
      let diffcmd = 'diff'
      let from = s:DirRev(@%)[1]
      let to = s:DirRev(url)[1]
      if empty(from) && empty(to)
        let diffcmd = 'diff-files'
        let args = ['--', expand('%:p'), url]
      elseif empty(to)
        let args = [from, '--', url]
      elseif empty(from)
        let args = [to, '--', expand('%:p')]
        let reverse = 1
      else
        let args = [from, to]
      endif
      let [res, exec_error] = s:LinesError([dir, diffcmd, '-U0'] + args)
      if !exec_error
        call filter(res, 'v:val =~# "^@@ "')
        call map(res, 'substitute(v:val, ''[-+]\d\+\zs '', ",1 ", "g")')
        call map(res, 'matchlist(v:val, ''^@@ -\(\d\+\),\(\d\+\) +\(\d\+\),\(\d\+\) @@'')[1:4]')
        if exists('reverse')
          call map(res, 'v:val[2:3] + v:val[0:1]')
        endif
        call filter(res, 'v:val[0] < '.line('.'))
        let hunk = get(res, -1, [0,0,0,0])
        if hunk[0] + hunk[1] > line('.')
          let line = hunk[2] + max([1 - hunk[3], 0])
        else
          let line = hunk[2] + max([hunk[3], 1]) + line('.') - hunk[0] - max([hunk[1], 1])
        endif
      endif
    endif
    call insert(cmds, line)
  endif

  let pre = join(opts, '')
  if len(cmds) > 1
    let pre .= ' +' . escape(join(map(cmds, '"exe ".string(v:val)'), '|'), ' |"')
  elseif len(cmds)
    let pre .= ' +' . escape(cmds[0], ' |"')
  endif
  return [url, pre]
endfunction

function! fugitive#DiffClose() abort
  let mywinnr = winnr()
  for winnr in [winnr('#')] + range(winnr('$'),1,-1)
    if winnr != mywinnr && getwinvar(winnr,'&diff')
      execute winnr.'wincmd w'
      close
      if winnr('$') > 1
        wincmd p
      endif
    endif
  endfor
  diffoff!
endfunction

function! s:BlurStatus() abort
  if (&previewwindow || exists('w:fugitive_status')) && get(b:,'fugitive_type', '') ==# 'index'
    let winnrs = filter([winnr('#')] + range(1, winnr('$')), 's:UsableWin(v:val)')
    if len(winnrs)
      exe winnrs[0].'wincmd w'
    else
      belowright new
    endif
    if &diff
      call fugitive#DiffClose()
    endif
  endif
endfunction

let s:bang_edits = {'split': 'Git', 'vsplit': 'vert Git', 'tabedit': 'tab Git', 'pedit': 'Git!'}
function! fugitive#Open(cmd, bang, mods, arg, args) abort
  exe s:VersionCheck()
  if a:bang
    return 'echoerr ' . string(':G' . a:cmd . '! for temp buffer output has been replaced by :' . get(s:bang_edits, a:cmd, 'Git') . ' --paginate')
  endif

  let mods = s:Mods(a:mods)
  try
    let [file, pre] = s:OpenParse(a:arg, 1)
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  if file !~# '^\a\a\+:' && !(has('win32') && file =~# '^\a:/$')
    let file = substitute(file, '.\zs' . (has('win32') ? '[\/]' : '/') . '$', '', '')
  endif
  if a:cmd ==# 'edit'
    call s:BlurStatus()
  endif
  return mods . a:cmd . pre . ' ' . s:fnameescape(file)
endfunction

function! s:ReadPrepare(line1, count, range, mods) abort
  let mods = s:Mods(a:mods)
  let after = a:count
  if a:count < 0
    let delete = 'silent 1,' . line('$') . 'delete_|'
    let after = line('$')
  elseif a:range == 2
    let delete = 'silent ' . a:line1 . ',' . a:count . 'delete_|'
  else
    let delete = ''
  endif
  if foldlevel(after)
    let pre = after . 'foldopen!|'
  else
    let pre = ''
  endif
  return [pre . 'keepalt ' . mods . after . 'read', delete . 'diffupdate' . (a:count < 0 ? '|' . line('.') : '')]
endfunction

function! s:ReadExec(line1, count, range, mods, env, args, options) abort
  let [read, post] = s:ReadPrepare(a:line1, a:count, a:range, a:mods)
  let env = s:BuildEnvPrefix(extend({'COLUMNS': &tw ? &tw : 80}, a:env))
  silent execute read . '!' escape(env . s:UserCommand(a:options, ['--no-pager'] + a:args), '!#%')
  execute post
  call fugitive#ReloadStatus(a:options.dir, 1)
  return 'redraw|echo '.string(':!'.s:UserCommand(a:options, a:args))
endfunction

function! fugitive#ReadCommand(line1, count, range, bang, mods, arg, args) abort
  exe s:VersionCheck()
  if a:bang
    return 'echoerr ' . string(':Gread! for temp buffer output has been replaced by :{range}Git! --paginate')
  endif
  let [read, post] = s:ReadPrepare(a:line1, a:count, a:range, a:mods)
  try
    let [file, pre] = s:OpenParse(a:arg, 0)
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  if file =~# '^fugitive:' && a:count is# 0
    return 'exe ' .string('keepalt ' . s:Mods(a:mods) . fugitive#FileReadCmd(file, 0, pre)) . '|diffupdate'
  endif
  return read . ' ' . pre . ' ' . s:fnameescape(file) . '|' . post
endfunction

function! fugitive#EditComplete(A, L, P) abort
  if a:A =~# '^>'
    return map(s:FilterEscape(s:CompleteHeads(s:Dir()), a:A[1:-1]), "'>' . v:val")
  else
    return fugitive#CompleteObject(a:A, a:L, a:P)
  endif
endfunction

function! fugitive#ReadComplete(A, L, P) abort
  if a:L =~# '^\w\+!'
    return fugitive#Complete(a:A, a:L, a:P)
  else
    return fugitive#EditComplete(a:A, a:L, a:P)
  endif
endfunction

" Section: :Gwrite, :Gwq

function! fugitive#WriteCommand(line1, line2, range, bang, mods, arg, args) abort
  exe s:VersionCheck()
  if s:cpath(expand('%:p'), fugitive#Find('.git/COMMIT_EDITMSG')) && empty(a:arg)
    return (empty($GIT_INDEX_FILE) ? 'write|bdelete' : 'wq') . (a:bang ? '!' : '')
  elseif get(b:, 'fugitive_type', '') ==# 'index' && empty(a:arg)
    return 'Git commit'
  elseif &buftype ==# 'nowrite' && getline(4) =~# '^[+-]\{3\} '
    return 'echoerr ' . string('fugitive: :Gwrite from :Git diff has been removed in favor of :Git add --edit')
  endif
  let mytab = tabpagenr()
  let mybufnr = bufnr('')
  let args = s:ArgSplit(a:arg)
  let after = ''
  if get(args, 0) =~# '^+'
    let after = '|' . remove(args, 0)[1:-1]
  endif
  try
    let file = len(args) ? s:Generate(s:Expand(join(args, ' '))) : fugitive#Real(@%)
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  if empty(file)
    return 'echoerr '.string('fugitive: cannot determine file path')
  endif
  if file =~# '^fugitive:'
    return 'write' . (a:bang ? '! ' : ' ') . s:fnameescape(file)
  endif
  exe s:DirCheck()
  let always_permitted = s:cpath(fugitive#Real(@%), file) && empty(s:DirCommitFile(@%)[1])
  if !always_permitted && !a:bang && (len(s:TreeChomp('diff', '--name-status', 'HEAD', '--', file)) || len(s:TreeChomp('ls-files', '--others', '--', file)))
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
    silent execute 'keepalt %write '.temp
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
            diffupdate
            let did = 1
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
    if !exists('did')
      call writefile(readfile(temp,'b'),file,'b')
    endif
  else
    execute 'write! '.s:fnameescape(file)
  endif

  if a:bang
    let [error, exec_error] = s:ChompError(['add', '--force', '--', file])
  else
    let [error, exec_error] = s:ChompError(['add', '--', file])
  endif
  if exec_error
    let v:errmsg = 'fugitive: '.error
    return 'echoerr v:errmsg'
  endif
  if s:cpath(fugitive#Real(@%), file) && s:DirCommitFile(@%)[1] =~# '^\d$'
    setlocal nomodified
  endif

  let one = fugitive#Find(':1:'.file)
  let two = fugitive#Find(':2:'.file)
  let three = fugitive#Find(':3:'.file)
  for nr in range(1,bufnr('$'))
    let name = fnamemodify(bufname(nr), ':p')
    if bufloaded(nr) && !getbufvar(nr,'&modified') && (name ==# one || name ==# two || name ==# three)
      execute nr.'bdelete'
    endif
  endfor

  unlet! restorewinnr
  let zero = fugitive#Find(':0:'.file)
  silent exe s:DoAutocmd('BufWritePost ' . s:fnameescape(zero))
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
          setlocal nomodified
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
  call fugitive#ReloadStatus(-1, 1)
  return 'silent checktime' . after
endfunction

function! fugitive#WqCommand(...) abort
  let bang = a:4 ? '!' : ''
  if s:cpath(expand('%:p'), fugitive#Find('.git/COMMIT_EDITMSG'))
    return 'wq'.bang
  endif
  let result = call('fugitive#WriteCommand', a:000)
  if result =~# '^\%(write\|wq\|echoerr\)'
    return s:sub(result,'^write','wq')
  else
    return result.'|quit'.bang
  endif
endfunction

" Section: :Git push, :Git fetch

function! fugitive#PushComplete(A, L, P, ...) abort
  return s:CompleteSub('push', a:A, a:L, a:P, function('s:CompleteRemote'), a:000)
endfunction

function! fugitive#FetchComplete(A, L, P, ...) abort
  return s:CompleteSub('fetch', a:A, a:L, a:P, function('s:CompleteRemote'), a:000)
endfunction

" Section: :Gdiff

augroup fugitive_diff
  autocmd!
  autocmd BufWinLeave * nested
        \ if s:can_diffoff(+expand('<abuf>')) && s:diff_window_count() == 2 |
        \   call s:diffoff_all(s:Dir(+expand('<abuf>'))) |
        \ endif
  autocmd BufWinEnter * nested
        \ if s:can_diffoff(+expand('<abuf>')) && s:diff_window_count() == 1 |
        \   call s:diffoff() |
        \ endif
augroup END

function! s:can_diffoff(buf) abort
  return getwinvar(bufwinnr(a:buf), '&diff') &&
        \ !empty(getwinvar(bufwinnr(a:buf), 'fugitive_diff_restore'))
endfunction

function! fugitive#CanDiffoff(buf) abort
  return s:can_diffoff(bufnr(a:buf))
endfunction

function! s:diff_modifier(count) abort
  let fdc = matchstr(&diffopt, 'foldcolumn:\zs\d\+')
  if &diffopt =~# 'horizontal' && &diffopt !~# 'vertical'
    return ''
  elseif &diffopt =~# 'vertical'
    return 'vertical '
  elseif winwidth(0) <= a:count * ((&tw ? &tw : 80) + (empty(fdc) ? 2 : fdc))
    return ''
  else
    return 'vertical '
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
  if exists('w:fugitive_diff_restore') && v:version < 704
    execute w:fugitive_diff_restore
  endif
  unlet! w:fugitive_diff_restore
  diffoff
endfunction

function! s:diffoff_all(dir) abort
  let curwin = winnr()
  for nr in range(1,winnr('$'))
    if getwinvar(nr, '&diff') && !empty(getwinvar(nr, 'fugitive_diff_restore'))
      if v:version < 704
        if nr != winnr()
          execute nr.'wincmd w'
        endif
        execute w:fugitive_diff_restore
      endif
      call setwinvar(nr, 'fugitive_diff_restore', '')
    endif
  endfor
  if curwin != winnr()
    execute curwin.'wincmd w'
  endif
  diffoff!
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

function! s:IsConflicted() abort
  return len(@%) && !empty(s:ChompDefault('', 'ls-files', '--unmerged', '--', expand('%:p')))
endfunction

function! fugitive#Diffsplit(autodir, keepfocus, mods, arg, args) abort
  exe s:VersionCheck()
  let args = s:ArgSplit(a:arg)
  let post = ''
  if get(args, 0) =~# '^+'
    let post = remove(args, 0)[1:-1]
  endif
  if exists(':DiffGitCached') && empty(args)
    return s:Mods(a:mods) . 'DiffGitCached' . (len(post) ? '|' . post : '')
  endif
  let commit = s:DirCommitFile(@%)[1]
  if a:mods =~# '\<tab\>'
    let mods = substitute(a:mods, '\<tab\>', '', 'g')
    let pre = 'tab split'
  else
    let mods = 'keepalt ' . a:mods
    let pre = ''
  endif
  let back = exists('*win_getid') ? 'call win_gotoid(' . win_getid() . ')' : 'wincmd p'
  if (empty(args) || args[0] ==# ':') && a:keepfocus
    exe s:DirCheck()
    if commit =~# '^1\=$' && s:IsConflicted()
      let parents = [s:Relative(':2:'), s:Relative(':3:')]
    elseif empty(commit)
      let parents = [s:Relative(':0:')]
    elseif commit =~# '^\d\=$'
      let parents = [s:Relative('@:')]
    elseif commit =~# '^\x\x\+$'
      let parents = s:LinesError(['rev-parse', commit . '^@'])[0]
      call map(parents, 's:Relative(v:val . ":")')
    endif
  endif
  try
    if exists('parents') && len(parents) > 1
      exe pre
      let mods = (a:autodir ? s:diff_modifier(len(parents) + 1) : '') . s:Mods(mods, 'leftabove')
      let nr = bufnr('')
      execute mods 'split' s:fnameescape(fugitive#Find(parents[0]))
      call s:Map('n', 'dp', ':diffput '.nr.'<Bar>diffupdate<CR>', '<silent>')
      let nr2 = bufnr('')
      call s:diffthis()
      exe back
      call s:Map('n', 'd2o', ':diffget '.nr2.'<Bar>diffupdate<CR>', '<silent>')
      let mods = substitute(mods, '\Cleftabove\|rightbelow\|aboveleft\|belowright', '\=submatch(0) =~# "f" ? "rightbelow" : "leftabove"', '')
      for i in range(len(parents)-1, 1, -1)
        execute mods 'split' s:fnameescape(fugitive#Find(parents[i]))
        call s:Map('n', 'dp', ':diffput '.nr.'<Bar>diffupdate<CR>', '<silent>')
        let nrx = bufnr('')
        call s:diffthis()
        exe back
        call s:Map('n', 'd' . (i + 2) . 'o', ':diffget '.nrx.'<Bar>diffupdate<CR>', '<silent>')
      endfor
      call s:diffthis()
      if len(parents) > 1
        wincmd =
      endif
      return post
    elseif len(args)
      let arg = join(args, ' ')
      if arg ==# ''
        return post
      elseif arg ==# ':/'
        exe s:DirCheck()
        let file = s:Relative()
      elseif arg ==# ':'
        exe s:DirCheck()
        let file = s:Relative(':0:')
      elseif arg =~# '^:\d$'
        exe s:DirCheck()
        let file = s:Relative(arg . ':')
      elseif arg =~# '^[~^]\d*$'
        return 'echoerr ' . string('fugitive: change ' . arg . ' to !' . arg . ' to diff against ancestor')
      else
        try
          let file = arg =~# '^:/.' ? fugitive#RevParse(arg) . s:Relative(':') : s:Expand(arg)
        catch /^fugitive:/
          return 'echoerr ' . string(v:exception)
        endtry
      endif
    elseif exists('parents') && len(parents)
      let file = parents[-1]
    elseif len(commit)
      let file = s:Relative()
    elseif s:IsConflicted()
      let file = s:Relative(':1:')
      let post = 'echohl WarningMsg|echo "Use :Gdiffsplit! for 3 way diff"|echohl NONE|' . post
    else
      exe s:DirCheck()
      let file = s:Relative(':0:')
    endif
    let spec = s:Generate(file)
    if spec =~# '^fugitive:' && empty(s:DirCommitFile(spec)[2])
      let spec = FugitiveVimPath(spec . s:Relative('/'))
    endif
    exe pre
    let restore = s:diff_restore()
    let w:fugitive_diff_restore = restore
    if len(spec) && s:CompareAge(commit, s:DirCommitFile(spec)[1]) < 0
      let mods = s:Mods(mods, 'rightbelow')
    else
      let mods = s:Mods(mods, 'leftabove')
    endif
    let mods = (a:autodir ? s:diff_modifier(2) : '') . mods
    if &diffopt =~# 'vertical'
      let diffopt = &diffopt
      set diffopt-=vertical
    endif
    execute mods 'diffsplit' s:fnameescape(spec)
    let w:fugitive_diff_restore = restore
    let winnr = winnr()
    if getwinvar('#', '&diff')
      if a:keepfocus
        exe back
      endif
    endif
    return post
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  finally
    if exists('diffopt')
      let &diffopt = diffopt
    endif
  endtry
endfunction

" Section: :GMove, :GRemove

function! s:Move(force, rename, destination) abort
  let dir = s:Dir()
  exe s:DirCheck(dir)
  if s:DirCommitFile(@%)[1] !~# '^0\=$' || empty(@%)
    return 'echoerr ' . string('fugitive: mv not supported for this buffer')
  endif
  if a:destination =~# '^\a\+:\|^/'
    let destination = a:destination
  elseif a:destination =~# '^:/:\='
    let destination = s:Tree(dir) . substitute(a:destination, '^:/:\=', '', '')
  elseif a:destination =~# '^:(\%(top\|top,literal\|literal,top\))'
    let destination = s:Tree(dir) . matchstr(a:destination, ')\zs.*')
  elseif a:destination =~# '^:(literal)'
    let destination = simplify(getcwd() . '/' . matchstr(a:destination, ')\zs.*'))
  elseif a:rename
    let destination = simplify(expand('%:p:s?[\/]$??:h') . '/' . a:destination)
  elseif a:destination =~# '^\.\.\=\%(/\|$\)'
    let destination = simplify(getcwd() . '/' . a:destination)
  else
    let destination = s:Tree(dir) . '/' . a:destination
  endif
  let destination = s:Slash(destination)
  if isdirectory(@%)
    setlocal noswapfile
  endif
  let [message, exec_error] = s:ChompError(['mv'] + (a:force ? ['-f'] : []) + ['--', expand('%:p'), destination], dir)
  if exec_error
    let v:errmsg = 'fugitive: '.message
    return 'echoerr v:errmsg'
  endif
  if isdirectory(destination)
    let destination = fnamemodify(s:sub(destination,'/$','').'/'.expand('%:t'),':.')
  endif
  let reload = '|call fugitive#ReloadStatus(' . string(dir) . ', 1)'
  if empty(s:DirCommitFile(@%)[1])
    if isdirectory(destination)
      return 'keepalt edit '.s:fnameescape(destination) . reload
    else
      return 'keepalt saveas! '.s:fnameescape(destination) . reload
    endif
  else
    return 'file '.s:fnameescape(fugitive#Find(':0:'.destination, dir)) . reload
  endif
endfunction

function! fugitive#RenameComplete(A,L,P) abort
  if a:A =~# '^[.:]\=/'
    return fugitive#CompletePath(a:A)
  else
    let pre = s:Slash(fnamemodify(expand('%:p:s?[\/]$??'), ':h')) . '/'
    return map(fugitive#CompletePath(pre.a:A), 'strpart(v:val, len(pre))')
  endif
endfunction

function! fugitive#MoveCommand(line1, line2, range, bang, mods, arg, args) abort
  return s:Move(a:bang, 0, a:arg)
endfunction

function! fugitive#RenameCommand(line1, line2, range, bang, mods, arg, args) abort
  return s:Move(a:bang, 1, a:arg)
endfunction

function! s:Remove(after, force) abort
  let dir = s:Dir()
  exe s:DirCheck(dir)
  if len(@%) && s:DirCommitFile(@%)[1] ==# ''
    let cmd = ['rm']
  elseif s:DirCommitFile(@%)[1] ==# '0'
    let cmd = ['rm','--cached']
  else
    return 'echoerr ' . string('fugitive: rm not supported for this buffer')
  endif
  if a:force
    let cmd += ['--force']
  endif
  let [message, exec_error] = s:ChompError(cmd + ['--', expand('%:p')], dir)
  if exec_error
    let v:errmsg = 'fugitive: '.s:sub(message,'error:.*\zs\n\(.*-f.*',' (add ! to force)')
    return 'echoerr '.string(v:errmsg)
  else
    return a:after . (a:force ? '!' : ''). '|call fugitive#ReloadStatus(' . string(dir) . ', 1)'
  endif
endfunction

function! fugitive#RemoveCommand(line1, line2, range, bang, mods, arg, args) abort
  return s:Remove('edit', a:bang)
endfunction

function! fugitive#DeleteCommand(line1, line2, range, bang, mods, arg, args) abort
  return s:Remove('bdelete', a:bang)
endfunction

" Section: :Git blame

function! s:Keywordprg() abort
  let args = ' --git-dir='.escape(s:Dir(),"\\\"' ")
  if has('gui_running') && !has('win32')
    return s:GitShellCmd() . ' --no-pager' . args . ' log -1'
  else
    return s:GitShellCmd() . args . ' show'
  endif
endfunction

function! s:linechars(pattern) abort
  let chars = strlen(s:gsub(matchstr(getline('.'), a:pattern), '.', '.'))
  if exists('*synconcealed') && &conceallevel > 1
    for col in range(1, chars)
      let chars -= synconcealed(line('.'), col)[0]
    endfor
  endif
  return chars
endfunction

function! s:BlameBufnr(...) abort
  let state = s:TempState(bufname(a:0 ? a:1 : ''))
  if get(state, 'filetype', '') ==# 'fugitiveblame'
    return get(state, 'origin_bufnr', -1)
  else
    return -1
  endif
endfunction

function! s:BlameCommitFileLnum(...) abort
  let line = a:0 ? a:1 : getline('.')
  let state = a:0 > 1 ? a:2 : s:TempState()
  if get(state, 'filetype', '') !=# 'fugitiveblame'
    return ['', '', 0]
  endif
  let commit = matchstr(line, '^\^\=[?*]*\zs\x\+')
  if commit =~# '^0\+$'
    let commit = ''
  elseif has_key(state, 'blame_reverse_end')
    let commit = get(s:LinesError(state.dir, 'rev-list', '--ancestry-path', '--reverse', commit . '..' . state.blame_reverse_end)[0], 0, '')
  endif
  let lnum = +matchstr(line, ' \zs\d\+\ze \%((\| *\d\+)\)')
  let path = matchstr(line, '^\^\=[?*]*\x* \+\%(\d\+ \+\d\+ \+\)\=\zs.\{-\}\ze\s*\d\+ \%((\| *\d\+)\)')
  if empty(path) && lnum
    let path = get(state, 'blame_file', '')
  endif
  return [commit, path, lnum]
endfunction

function! s:BlameLeave() abort
  let bufwinnr = bufwinnr(s:BlameBufnr())
  if bufwinnr > 0
    let bufnr = bufnr('')
    exe bufwinnr . 'wincmd w'
    return bufnr . 'bdelete'
  endif
  return ''
endfunction

function! s:BlameQuit() abort
  let cmd = s:BlameLeave()
  if empty(cmd)
    return 'bdelete'
  elseif len(s:DirCommitFile(@%)[1])
    return cmd . '|Gedit'
  else
    return cmd
  endif
endfunction

function! fugitive#BlameComplete(A, L, P) abort
  return s:CompleteSub('blame', a:A, a:L, a:P)
endfunction

function! s:BlameSubcommand(line1, count, range, bang, mods, options) abort
  let dir = s:Dir()
  exe s:DirCheck(dir)
  let flags = copy(a:options.subcommand_args)
  let i = 0
  let raw = 0
  let commits = []
  let files = []
  let ranges = []
  if a:line1 > 0 && a:count > 0 && a:range != 1
    call extend(ranges, ['-L', a:line1 . ',' . a:count])
  endif
  while i < len(flags)
    let match = matchlist(flags[i], '^\(-[a-zABDFH-KN-RT-Z]\)\ze\(.*\)')
    if len(match) && len(match[2])
      call insert(flags, match[1])
      let flags[i+1] = '-' . match[2]
      continue
    endif
    let arg = flags[i]
    if arg =~# '^-p$\|^--\%(help\|porcelain\|line-porcelain\|incremental\)$'
      let raw = 1
    elseif arg ==# '--contents' && i + 1 < len(flags)
      call extend(commits, remove(flags, i, i+1))
      continue
    elseif arg ==# '-L' && i + 1 < len(flags)
      call extend(ranges, remove(flags, i, i+1))
      continue
    elseif arg =~# '^--contents='
      call add(commits, remove(flags, i))
      continue
    elseif arg =~# '^-L.'
      call add(ranges, remove(flags, i))
      continue
    elseif arg =~# '^-[GLS]$\|^--\%(date\|encoding\|contents\|ignore-rev\|ignore-revs-file\)$'
      let i += 1
      if i == len(flags)
        echohl ErrorMsg
        echo s:ChompError(['blame', arg])[0]
        echohl NONE
        return ''
      endif
    elseif arg ==# '--'
      if i + 1 < len(flags)
        call extend(files, remove(flags, i + 1, -1))
      endif
      call remove(flags, i)
      break
    elseif arg !~# '^-' && (s:HasOpt(flags, '--not') || arg !~# '^\^')
      if index(flags, '--') >= 0
        call add(commits, remove(flags, i))
        continue
      endif
      if arg =~# '\.\.' && arg !~# '^\.\.\=\%(/\|$\)' && empty(commits)
        call add(commits, remove(flags, i))
        continue
      endif
      try
        let dcf = s:DirCommitFile(fugitive#Find(arg, dir))
        if len(dcf[1]) && empty(dcf[2])
          call add(commits, remove(flags, i))
          continue
        endif
      catch /^fugitive:/
      endtry
      call add(files, remove(flags, i))
      continue
    endif
    let i += 1
  endwhile
  let file = substitute(get(files, 0, get(s:TempState(), 'blame_file', s:Relative('./', dir))), '^\.\%(/\|$\)', '', '')
  if empty(commits) && len(files) > 1
    call add(commits, remove(files, 1))
  endif
  exe s:BlameLeave()
  try
    let cmd = a:options.flags + ['--no-pager', '-c', 'blame.coloring=none', '-c', 'blame.blankBoundary=false', a:options.subcommand, '--show-number']
    call extend(cmd, filter(copy(flags), 'v:val !~# "\\v^%(-b|--%(no-)=color-.*|--progress)$"'))
    if a:count > 0 && empty(ranges)
      let cmd += ['-L', (a:line1 ? a:line1 : line('.')) . ',' . (a:line1 ? a:line1 : line('.'))]
    endif
    call extend(cmd, ranges)
    let tempname = tempname()
    let temp = tempname . (raw ? '' : '.fugitiveblame')
    if len(commits)
      let cmd += commits
    elseif empty(files) && len(matchstr(s:DirCommitFile(@%)[1], '^\x\x\+$'))
      let cmd += [matchstr(s:DirCommitFile(@%)[1], '^\x\x\+$')]
    elseif empty(files) && !s:HasOpt(flags, '--reverse')
      let cmd += ['--contents', tempname . '.in']
      silent execute 'noautocmd keepalt %write ' . s:fnameescape(tempname . '.in')
      let delete_in = 1
    endif
    let basecmd = [{'git': a:options.git, 'dir': dir}] + ['--literal-pathspecs'] + cmd + ['--'] + (len(files) ? files : [file])
    let [err, exec_error] = s:TempCmd(temp, basecmd)
    if exists('delete_in')
      call delete(tempname . '.in')
    endif
    redraw
    try
      if exec_error
        let lines = split(err, "\n")
        if empty(lines)
          let lines = readfile(temp)
        endif
        for i in range(len(lines))
          if lines[i] =~# '^error: \|^fatal: '
            echohl ErrorMsg
            echon lines[i]
            echohl NONE
            break
          else
            echon lines[i]
          endif
          if i != len(lines) - 1
            echon "\n"
          endif
        endfor
        return ''
      endif
      let temp_state = {
            \ 'git': a:options.git,
            \ 'flags': a:options.flags,
            \ 'args': [a:options.subcommand] + a:options.subcommand_args,
            \ 'dir': dir,
            \ 'git_dir': dir,
            \ 'cwd': s:UserCommandCwd(dir),
            \ 'filetype': (raw ? 'git' : 'fugitiveblame'),
            \ 'blame_options': a:options,
            \ 'blame_flags': flags,
            \ 'blame_file': file}
      if s:HasOpt(flags, '--reverse')
        let temp_state.blame_reverse_end = matchstr(get(commits, 0, ''), '\.\.\zs.*')
      endif
      if (a:line1 == 0 || a:range == 1) && a:count > 0
        let edit = s:Mods(a:mods) . get(['edit', 'split', 'pedit', 'vsplit', 'tabedit'], a:count - (a:line1 ? a:line1 : 1), 'split')
        return s:BlameCommit(edit, get(readfile(temp), 0, ''), temp_state)
      else
        let temp = s:Resolve(temp)
        let temp_state.file = temp
        call s:RunSave(temp_state)
        if len(ranges + commits + files) || raw
          let reload = '|call fugitive#ReloadStatus(fugitive#Result(' . string(temp_state.file) . '), 1)'
          let mods = s:Mods(a:mods)
          if a:count != 0
            exe 'silent keepalt' mods 'split' s:fnameescape(temp)
          elseif !&modified || a:bang || &bufhidden ==# 'hide' || (empty(&bufhidden) && &hidden)
            exe 'silent' mods 'edit' . (a:bang ? '! ' : ' ') . s:fnameescape(temp)
          else
            return mods . 'edit ' . s:fnameescape(temp) . reload
          endif
          return reload[1 : -1]
        endif
        if a:mods =~# '\<tab\>'
          silent tabedit %
        endif
        let bufnr = bufnr('')
        let temp_state.origin_bufnr = bufnr
        let restore = []
        let mods = substitute(a:mods, '\<tab\>', '', 'g')
        for winnr in range(winnr('$'),1,-1)
          if getwinvar(winnr, '&scrollbind')
            if !&l:scrollbind
              call setwinvar(winnr, '&scrollbind', 0)
            elseif winnr != winnr() && getwinvar(winnr, '&foldenable')
              call setwinvar(winnr, '&foldenable', 0)
              call add(restore, 'call setwinvar(bufwinnr('.winbufnr(winnr).'),"&foldenable",1)')
            endif
          endif
          if exists('+cursorbind') && !&l:cursorbind && getwinvar(winnr, '&cursorbind')
            call setwinvar(winnr, '&cursorbind', 0)
          endif
          if s:BlameBufnr(winbufnr(winnr)) > 0
            execute winbufnr(winnr).'bdelete'
          endif
        endfor
        let restore_winnr = 'bufwinnr(' . bufnr . ')'
        if !&l:scrollbind
          call add(restore, 'call setwinvar(' . restore_winnr . ',"&scrollbind",0)')
        endif
        if exists('+cursorbind') && !&l:cursorbind
          call add(restore, 'call setwinvar(' . restore_winnr . ',"&cursorbind",0)')
        endif
        if &l:wrap
          call add(restore, 'call setwinvar(' . restore_winnr . ',"&wrap",1)')
        endif
        if &l:foldenable
          call add(restore, 'call setwinvar(' . restore_winnr . ',"&foldenable",1)')
        endif
        setlocal scrollbind nowrap nofoldenable
        if exists('+cursorbind')
          setlocal cursorbind
        endif
        let top = line('w0') + &scrolloff
        let current = line('.')
        exe 'silent keepalt' (a:bang ? s:Mods(mods) . 'split' : s:Mods(mods, 'leftabove') . 'vsplit') s:fnameescape(temp)
        let w:fugitive_leave = join(restore, '|')
        execute top
        normal! zt
        execute current
        if exists('+cursorbind')
          setlocal cursorbind
        endif
        setlocal nonumber scrollbind nowrap foldcolumn=0 nofoldenable winfixwidth
        if exists('+relativenumber')
          setlocal norelativenumber
        endif
        if exists('+signcolumn')
          setlocal signcolumn=no
        endif
        execute "vertical resize ".(s:linechars('.\{-\}\s\+\d\+\ze)')+1)
        redraw
        syncbind
        exe s:DoAutocmdChanged(temp_state)
      endif
    endtry
    return ''
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
endfunction

function! s:BlameCommit(cmd, ...) abort
  let line = a:0 ? a:1 : getline('.')
  let state = a:0 ? a:2 : s:TempState()
  let sigil = has_key(state, 'blame_reverse_end') ? '-' : '+'
  let mods = (s:BlameBufnr() < 0 ? '' : &splitbelow ? "botright " : "topleft ")
  let [commit, path, lnum] = s:BlameCommitFileLnum(line, state)
  if empty(commit) && len(path) && has_key(state, 'blame_reverse_end')
    let path = (len(state.blame_reverse_end) ? state.blame_reverse_end . ':' : ':(top)') . path
    return fugitive#Open(mods . a:cmd, 0, '', '+' . lnum . ' ' . s:fnameescape(path), ['+' . lnum, path])
  endif
  if commit =~# '^0*$'
    return 'echoerr ' . string('fugitive: no commit')
  endif
  if line =~# '^\^' && !has_key(state, 'blame_reverse_end')
    let path = commit . ':' . path
    return fugitive#Open(mods . a:cmd, 0, '', '+' . lnum . ' ' . s:fnameescape(path), ['+' . lnum, path])
  endif
  let cmd = fugitive#Open(mods . a:cmd, 0, '', commit, [commit])
  if cmd =~# '^echoerr'
    return cmd
  endif
  execute cmd
  if a:cmd ==# 'pedit' || empty(path)
    return ''
  endif
  if search('^diff .* b/\M'.escape(path,'\').'$','W')
    call search('^+++')
    let head = line('.')
    while search('^@@ \|^diff ') && getline('.') =~# '^@@ '
      let top = +matchstr(getline('.'),' ' . sigil .'\zs\d\+')
      let len = +matchstr(getline('.'),' ' . sigil . '\d\+,\zs\d\+')
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
          if getline('.') =~# '^[ ' . sigil . ']'
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

function! s:BlameJump(suffix, ...) abort
  let suffix = a:suffix
  let [commit, path, lnum] = s:BlameCommitFileLnum()
  if empty(path)
    return 'echoerr ' . string('fugitive: could not determine filename for blame')
  endif
  if commit =~# '^0*$'
    let commit = '@'
    let suffix = ''
  endif
  let offset = line('.') - line('w0')
  let state = s:TempState()
  let flags = get(state, 'blame_flags', [])
  if a:0 && a:1
    if s:HasOpt(flags, '--reverse')
      call remove(flags, '--reverse')
    else
      call add(flags, '--reverse')
    endif
  endif
  let blame_bufnr = s:BlameBufnr()
  if blame_bufnr > 0
    let bufnr = bufnr('')
    let winnr = bufwinnr(blame_bufnr)
    if winnr > 0
      exe winnr.'wincmd w'
      exe bufnr.'bdelete'
    endif
    execute 'Gedit' s:fnameescape(commit . suffix . ':' . path)
    execute lnum
  endif
  let my_bufnr = bufnr('')
  if blame_bufnr < 0
    let blame_args = flags + [commit . suffix, '--', path]
    let result = s:BlameSubcommand(0, 0, 0, 0, '', extend({'subcommand_args': blame_args}, state.blame_options, 'keep'))
  else
    let blame_args = flags
    let result = s:BlameSubcommand(-1, -1, 0, 0, '', extend({'subcommand_args': blame_args}, state.blame_options, 'keep'))
  endif
  if bufnr('') == my_bufnr
    return result
  endif
  execute result
  execute lnum
  let delta = line('.') - line('w0') - offset
  if delta > 0
    execute 'normal! '.delta."\<C-E>"
  elseif delta < 0
    execute 'normal! '.(-delta)."\<C-Y>"
  endif
  keepjumps syncbind
  redraw
  echo ':Git blame' s:fnameescape(blame_args)
  return ''
endfunction

let s:hash_colors = {}

function! fugitive#BlameSyntax() abort
  let conceal = has('conceal') ? ' conceal' : ''
  let flags = get(s:TempState(), 'blame_flags', [])
  syn spell notoplevel
  syn match FugitiveblameBlank                      "^\s\+\s\@=" nextgroup=FugitiveblameAnnotation,FugitiveblameScoreDebug,FugitiveblameOriginalFile,FugitiveblameOriginalLineNumber skipwhite
  syn match FugitiveblameHash       "\%(^\^\=[?*]*\)\@<=\<\x\{7,\}\>" nextgroup=FugitiveblameAnnotation,FugitiveblameScoreDebug,FugitiveblameOriginalLineNumber,FugitiveblameOriginalFile skipwhite
  if s:HasOpt(flags, '-b') || FugitiveConfigGet('blame.blankBoundary') =~# '^1$\|^true$'
    syn match FugitiveblameBoundaryIgnore "^\^[*?]*\x\{7,\}\>" nextgroup=FugitiveblameAnnotation,FugitiveblameScoreDebug,FugitiveblameOriginalLineNumber,FugitiveblameOriginalFile skipwhite
  else
    syn match FugitiveblameBoundary "^\^"
  endif
  syn match FugitiveblameScoreDebug        " *\d\+\s\+\d\+\s\@=" nextgroup=FugitiveblameAnnotation,FugitiveblameOriginalLineNumber,fugitiveblameOriginalFile contained skipwhite
  syn region FugitiveblameAnnotation matchgroup=FugitiveblameDelimiter start="(" end="\%(\s\d\+\)\@<=)" contained keepend oneline
  syn match FugitiveblameTime "\<[0-9:/+-][0-9:/+ -]*[0-9:/+-]\%(\s\+\d\+)\)\@=" contained containedin=FugitiveblameAnnotation
  exec 'syn match FugitiveblameLineNumber         "\s[[:digit:][:space:]]\{0,' . (len(line('$'))-1). '\}\d)\@=" contained containedin=FugitiveblameAnnotation' conceal
  exec 'syn match FugitiveblameOriginalFile       "\s\%(\f\+\D\@<=\|\D\@=\f\+\)\%(\%(\s\+\d\+\)\=\s\%((\|\s*\d\+)\)\)\@=" contained nextgroup=FugitiveblameOriginalLineNumber,FugitiveblameAnnotation skipwhite' (s:HasOpt(flags, '--show-name', '-f') ? '' : conceal)
  exec 'syn match FugitiveblameOriginalLineNumber "\s*\d\+\%(\s(\)\@=" contained nextgroup=FugitiveblameAnnotation skipwhite' (s:HasOpt(flags, '--show-number', '-n') ? '' : conceal)
  exec 'syn match FugitiveblameOriginalLineNumber "\s*\d\+\%(\s\+\d\+)\)\@=" contained nextgroup=FugitiveblameShort skipwhite' (s:HasOpt(flags, '--show-number', '-n') ? '' : conceal)
  syn match FugitiveblameShort              " \d\+)" contained contains=FugitiveblameLineNumber
  syn match FugitiveblameNotCommittedYet "(\@<=Not Committed Yet\>" contained containedin=FugitiveblameAnnotation
  hi def link FugitiveblameBoundary           Keyword
  hi def link FugitiveblameHash               Identifier
  hi def link FugitiveblameBoundaryIgnore     Ignore
  hi def link FugitiveblameUncommitted        Ignore
  hi def link FugitiveblameScoreDebug         Debug
  hi def link FugitiveblameTime               PreProc
  hi def link FugitiveblameLineNumber         Number
  hi def link FugitiveblameOriginalFile       String
  hi def link FugitiveblameOriginalLineNumber Float
  hi def link FugitiveblameShort              FugitiveblameDelimiter
  hi def link FugitiveblameDelimiter          Delimiter
  hi def link FugitiveblameNotCommittedYet    Comment
  if !get(g:, 'fugitive_dynamic_colors', 1) && !s:HasOpt(flags, '--color-lines') || s:HasOpt(flags, '--no-color-lines')
    return
  endif
  let seen = {}
  for lnum in range(1, line('$'))
    let orig_hash = matchstr(getline(lnum), '^\^\=[*?]*\zs\x\{6\}')
    let hash = orig_hash
    let hash = substitute(hash, '\(\x\)\x', '\=submatch(1).printf("%x", 15-str2nr(submatch(1),16))', 'g')
    let hash = substitute(hash, '\(\x\x\)', '\=printf("%02x", str2nr(submatch(1),16)*3/4+32)', 'g')
    if hash ==# '' || orig_hash ==# '000000' || has_key(seen, hash)
      continue
    endif
    let seen[hash] = 1
    if &t_Co == 256
      let [s, r, g, b; __] = map(matchlist(orig_hash, '\(\x\)\x\(\x\)\x\(\x\)\x'), 'str2nr(v:val,16)')
      let color = 16 + (r + 1) / 3 * 36 + (g + 1) / 3 * 6 + (b + 1) / 3
      if color == 16
        let color = 235
      elseif color == 231
        let color = 255
      endif
      let s:hash_colors[hash] = ' ctermfg='.color
    else
      let s:hash_colors[hash] = ''
    endif
    let pattern = substitute(orig_hash, '^\(\x\)\x\(\x\)\x\(\x\)\x$', '\1\\x\2\\x\3\\x', '') . '*\>'
    exe 'syn match FugitiveblameHash'.hash.'       "\%(^\^\=[*?]*\)\@<='.pattern.'" nextgroup=FugitiveblameAnnotation,FugitiveblameOriginalLineNumber,fugitiveblameOriginalFile skipwhite'
  endfor
  syn match FugitiveblameUncommitted "\%(^\^\=[?*]*\)\@<=\<0\{7,\}\>" nextgroup=FugitiveblameAnnotation,FugitiveblameScoreDebug,FugitiveblameOriginalLineNumber,FugitiveblameOriginalFile skipwhite
  call s:BlameRehighlight()
endfunction

function! s:BlameRehighlight() abort
  for [hash, cterm] in items(s:hash_colors)
    if !empty(cterm) || has('gui_running') || has('termguicolors') && &termguicolors
      exe 'hi FugitiveblameHash'.hash.' guifg=#' . hash . cterm
    else
      exe 'hi link FugitiveblameHash'.hash.' Identifier'
    endif
  endfor
endfunction

function! fugitive#BlameFileType() abort
  setlocal nomodeline
  setlocal foldmethod=manual
  if len(s:Dir())
    let &l:keywordprg = s:Keywordprg()
  endif
  let b:undo_ftplugin = 'setl keywordprg= foldmethod<'
  if exists('+concealcursor')
    setlocal concealcursor=nc conceallevel=2
    let b:undo_ftplugin .= ' concealcursor< conceallevel<'
  endif
  if &modifiable
    return ''
  endif
  call s:Map('n', '<F1>', ':help :Git_blame<CR>', '<silent>')
  call s:Map('n', 'g?',   ':help :Git_blame<CR>', '<silent>')
  if mapcheck('q', 'n') =~# '^$\|bdelete'
    call s:Map('n', 'q',  ':echoerr "fugitive: q removed in favor of gq (or :q)"<CR>', '<silent>')
  endif
  call s:Map('n', 'gq',   ':exe <SID>BlameQuit()<CR>', '<silent>')
  call s:Map('n', '<2-LeftMouse>', ':<C-U>exe <SID>BlameCommit("exe <SID>BlameLeave()<Bar>edit")<CR>', '<silent>')
  call s:Map('n', '<CR>', ':<C-U>exe <SID>BlameCommit("exe <SID>BlameLeave()<Bar>edit")<CR>', '<silent>')
  call s:Map('n', '-',    ':<C-U>exe <SID>BlameJump("")<CR>', '<silent>')
  call s:Map('n', 's',    ':<C-U>exe <SID>BlameJump("")<CR>', '<silent>')
  call s:Map('n', 'u',    ':<C-U>exe <SID>BlameJump("")<CR>', '<silent>')
  call s:Map('n', 'P',    ':<C-U>exe <SID>BlameJump("^".v:count1)<CR>', '<silent>')
  call s:Map('n', '~',    ':<C-U>exe <SID>BlameJump("~".v:count1)<CR>', '<silent>')
  call s:Map('n', 'i',    ':<C-U>exe <SID>BlameCommit("exe <SID>BlameLeave()<Bar>edit")<CR>', '<silent>')
  call s:Map('n', 'o',    ':<C-U>exe <SID>BlameCommit("split")<CR>', '<silent>')
  call s:Map('n', 'O',    ':<C-U>exe <SID>BlameCommit("tabedit")<CR>', '<silent>')
  call s:Map('n', 'p',    ':<C-U>exe <SID>BlameCommit("pedit")<CR>', '<silent>')
  call s:Map('n', '.',    ":<C-U> <C-R>=substitute(<SID>BlameCommitFileLnum()[0],'^$','@','')<CR><Home>")
  call s:Map('n', 'A',    ":<C-u>exe 'vertical resize '.(<SID>linechars('.\\{-\\}\\ze [0-9:/+-][0-9:/+ -]* \\d\\+)')+1+v:count)<CR>", '<silent>')
  call s:Map('n', 'C',    ":<C-u>exe 'vertical resize '.(<SID>linechars('^\\S\\+')+1+v:count)<CR>", '<silent>')
  call s:Map('n', 'D',    ":<C-u>exe 'vertical resize '.(<SID>linechars('.\\{-\\}\\ze\\d\\ze\\s\\+\\d\\+)')+1-v:count)<CR>", '<silent>')
endfunction

augroup fugitive_blame
  autocmd!
  autocmd ColorScheme,GUIEnter * call s:BlameRehighlight()
  autocmd BufWinLeave * execute getwinvar(+bufwinnr(+expand('<abuf>')), 'fugitive_leave')
augroup END

" Section: :GBrowse

function! s:BrowserOpen(url, mods, echo_copy) abort
  let url = substitute(a:url, '[ <>\|"]', '\="%".printf("%02X",char2nr(submatch(0)))', 'g')
  let mods = s:Mods(a:mods)
  if a:echo_copy
    if has('clipboard')
      let @+ = url
    endif
    return 'echo '.string(url)
  elseif exists(':Browse') == 2
    return 'echo '.string(url).'|' . mods . 'Browse '.url
  elseif exists(':OpenBrowser') == 2
    return 'echo '.string(url).'|' . mods . 'OpenBrowser '.url
  else
    if !exists('g:loaded_netrw')
      runtime! autoload/netrw.vim
    endif
    if exists('*netrw#BrowseX')
      return 'echo '.string(url).'|' . mods . 'call netrw#BrowseX('.string(url).', 0)'
    elseif exists('*netrw#NetrwBrowseX')
      return 'echo '.string(url).'|' . mods . 'call netrw#NetrwBrowseX('.string(url).', 0)'
    else
      return 'echoerr ' . string('Netrw not found. Define your own :Browse to use :GBrowse')
    endif
  endif
endfunction

function! fugitive#BrowseCommand(line1, count, range, bang, mods, arg, args) abort
  exe s:VersionCheck()
  let dir = s:Dir()
  try
    let arg = a:arg
    if arg =~# '^++remote='
      let remote = matchstr(arg, '^++remote=\zs\S\+')
      let arg = matchstr(arg, '\s\zs\S.*')
    endif
    let validremote = '\.\|\.\=/.*\|[[:alnum:]_-]\+\%(://.\{-\}\)\='
    if arg ==# '-'
      let remote = ''
      let rev = ''
      let result = fugitive#Result()
      if filereadable(get(result, 'file', ''))
        for line in readfile(result.file, 4096)
          let rev = s:fnameescape(matchstr(line, '\<https\=://[^[:space:]<>]*[^[:space:]<>.,;:"''!?]'))
          if len(rev)
            break
          endif
        endfor
        if empty(rev)
          return 'echoerr ' . string('fugitive: no URL found in output of last :Git')
        endif
      endif
    elseif !exists('l:remote')
      let remote = matchstr(arg, '@\zs\%('.validremote.'\)$')
      let rev = substitute(arg, '@\%('.validremote.'\)$','','')
    else
      let rev = arg
    endif
    if rev =~? '^\a\a\+:[\/][\/]' && rev !~? '^fugitive:'
      let rev = substitute(rev, '\\\@<![#!]\|\\\@<!%\ze\w', '\\&', 'g')
    elseif rev ==# ':'
      let rev = ''
    endif
    let expanded = s:Expand(rev)
    if expanded =~? '^\a\a\+:[\/][\/]' && expanded !~? '^fugitive:'
      return s:BrowserOpen(s:Slash(expanded), a:mods, a:bang)
    endif
    exe s:DirCheck(dir)
    if empty(expanded)
      let bufname = s:BufName('%')
      let expanded = s:DirRev(bufname)[1]
      if empty(expanded)
        let expanded = fugitive#Path(bufname, ':(top)', dir)
      endif
      if a:count > 0 && bufname !=# bufname('')
        let blame = s:BlameCommitFileLnum(getline(a:count))
        if len(blame[0])
          let expanded = blame[0]
        endif
      endif
    endif
    let cdir = FugitiveVimPath(fugitive#CommonDir(dir))
    for subdir in ['tags/', 'heads/', 'remotes/']
      if expanded !~# '^[./]' && filereadable(cdir . '/refs/' . subdir . expanded)
        let expanded = '.git/refs/' . subdir . expanded
      endif
    endfor
    let full = s:Generate(expanded, dir)
    let commit = ''
    if full =~? '^fugitive:'
      let [dir, commit, path] = s:DirCommitFile(full)
      if commit =~# '^:\=\d$'
        let commit = ''
      endif
      if commit =~ '..'
        let type = s:TreeChomp(['cat-file','-t',commit.s:sub(path,'^/',':')], dir)
        let branch = matchstr(expanded, '^[^:]*')
      elseif empty(path) || path ==# '/'
        let type = 'tree'
      else
        let type = 'blob'
      endif
      let path = path[1:-1]
    elseif empty(s:Tree(dir))
      let path = '.git/' . full[strlen(dir)+1:-1]
      let type = ''
    else
      let path = fugitive#Path(full, '/')[1:-1]
      if path =~# '^\.git/'
        let type = ''
      elseif isdirectory(full) || empty(path)
        let type = 'tree'
      else
        let type = 'blob'
      endif
    endif
    if type ==# 'tree' && !empty(path)
      let path = s:sub(path, '/\=$', '/')
    endif
    if path =~# '^\.git/.*HEAD$' && filereadable(dir . '/' . path[5:-1])
      let body = readfile(dir . '/' . path[5:-1])[0]
      if body =~# '^\x\{40,\}$'
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
      let branch = FugitiveHead(0, dir)
    endif
    if !empty(branch)
      let r = FugitiveConfigGet('branch.'.branch.'.remote', dir)
      let m = FugitiveConfigGet('branch.'.branch.'.merge', dir)[11:-1]
      if r ==# '.' && !empty(m)
        let r2 = FugitiveConfigGet('branch.'.m.'.remote', dir)
        if r2 !~# '^\.\=$'
          let r = r2
          let m = FugitiveConfigGet('branch.'.m.'.merge', dir)[11:-1]
        endif
      endif
      if empty(remote)
        let remote = r
      endif
      if r ==# '.' || r ==# remote
        let remote_ref = 'refs/remotes/' . remote . '/' . branch
        if FugitiveConfigGet('push.default', dir) ==# 'upstream' ||
              \ !filereadable(FugitiveFind('.git/' . remote_ref, dir)) && s:ChompError(['rev-parse', '--verify', remote_ref, '--'], dir)[1]
          let merge = m
          if path =~# '^\.git/refs/heads/.'
            let path = '.git/refs/heads/'.merge
          endif
        else
          let merge = branch
        endif
      endif
    endif

    let line1 = a:count > 0 && type ==# 'blob' ? a:line1 : 0
    let line2 = a:count > 0 && type ==# 'blob' ? a:count : 0
    if empty(commit) && path !~# '^\.git/'
      if a:count < 0 && !empty(merge)
        let commit = merge
      else
        let commit = ''
        if len(merge)
          let owner = s:Owner(@%, dir)
          let [commit, exec_error] = s:ChompError(['merge-base', 'refs/remotes/' . remote . '/' . merge, empty(owner) ? '@' : owner, '--'], dir)
          if exec_error
            let commit = ''
          endif
          if line2 > 0 && empty(arg) && commit =~# '^\x\{40,\}$'
            let blame_list = tempname()
            call writefile([commit, ''], blame_list, 'b')
            let blame_in = tempname()
            silent exe '%write' blame_in
            let [blame, exec_error] = s:LinesError(['-c', 'blame.coloring=none', 'blame', '--contents', blame_in, '-L', line1.','.line2, '-S', blame_list, '-s', '--show-number', './' . path], dir)
            if !exec_error
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
        let commit = readfile(fugitive#Find('.git/HEAD', dir), '', 1)[0]
      endif
      let i = 0
      while commit =~# '^ref: ' && i < 10
        let ref_file = cdir . '/' . commit[5:-1]
        if getfsize(ref_file) > 0
          let commit = readfile(ref_file, '', 1)[0]
        else
          let commit = fugitive#RevParse(commit[5:-1], dir)
        endif
        let i -= 1
      endwhile
    endif

    if empty(remote)
      let remote = '.'
    endif
    let raw = fugitive#RemoteUrl(remote, dir)
    if empty(raw)
      let raw = remote
    endif

    let opts = {
          \ 'git_dir': dir,
          \ 'dir': dir,
          \ 'repo': fugitive#repo(dir),
          \ 'remote': raw,
          \ 'revision': 'No longer provided',
          \ 'commit': commit,
          \ 'path': path,
          \ 'type': type,
          \ 'line1': line1,
          \ 'line2': line2}

    let url = ''
    for Handler in get(g:, 'fugitive_browse_handlers', [])
      let url = call(Handler, [copy(opts)])
      if !empty(url)
        break
      endif
    endfor

    if empty(url)
      call s:throw("No GBrowse handler installed for '".raw."'")
    endif

    return s:BrowserOpen(url, a:mods, a:bang)
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
endfunction

" Section: Go to file

let s:ref_header = '\%(Merge\|Rebase\|Upstream\|Pull\|Push\)'

nnoremap <SID>: :<C-U><C-R>=v:count ? v:count : ''<CR>
function! fugitive#MapCfile(...) abort
  exe 'cnoremap <buffer> <expr> <Plug><cfile>' (a:0 ? a:1 : 'fugitive#Cfile()')
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') . '|sil! exe "cunmap <buffer> <Plug><cfile>"'
  if !exists('g:fugitive_no_maps')
    call s:Map('n', 'gf',          '<SID>:find <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:Map('n', '<C-W>f',     '<SID>:sfind <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:Map('n', '<C-W><C-F>', '<SID>:sfind <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:Map('n', '<C-W>gf',  '<SID>:tabfind <Plug><cfile><CR>', '<silent><unique>', 1)
    call s:Map('c', '<C-R><C-F>', '<Plug><cfile>', '<silent><unique>', 1)
  endif
endfunction

function! s:ContainingCommit() abort
  let commit = s:Owner(@%)
  return empty(commit) ? '@' : commit
endfunction

function! s:SquashArgument(...) abort
  if &filetype == 'fugitive'
    let commit = matchstr(getline('.'), '^\%(\%(\x\x\x\)\@!\l\+\s\+\)\=\zs[0-9a-f]\{4,\}\ze \|^' . s:ref_header . ': \zs\S\+')
  elseif has_key(s:temp_files, s:cpath(expand('%:p')))
    let commit = matchstr(getline('.'), '\S\@<!\x\{4,\}\>')
  else
    let commit = s:Owner(@%)
  endif
  return len(commit) && a:0 ? printf(a:1, commit) : commit
endfunction

function! s:RebaseArgument() abort
  return s:SquashArgument(' %s^')
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
      let rev = '@^{}'
    elseif rev =~# ':$'
      let rev = rev[0:-2]
    else
      return rev.'~'.c
    endif
    let c -= 1
  endwhile
  return rev
endfunction

function! s:MapMotion(lhs, rhs) abort
  call s:Map('n', a:lhs, ":<C-U>" . a:rhs . "<CR>", "<silent>")
  call s:Map('o', a:lhs, ":<C-U>" . a:rhs . "<CR>", "<silent>")
  call s:Map('x', a:lhs, ":<C-U>exe 'normal! gv'<Bar>" . a:rhs . "<CR>", "<silent>")
endfunction

function! fugitive#MapJumps(...) abort
  if !&modifiable
    if get(b:, 'fugitive_type', '') ==# 'blob'
      let blame_map = 'Git blame<C-R>=v:count ? " --reverse" : ""<CR><CR>'
      call s:Map('n', '<2-LeftMouse>', ':<C-U>0,1' . blame_map, '<silent>')
      call s:Map('n', '<CR>', ':<C-U>0,1' . blame_map, '<silent>')
      call s:Map('n', 'o',    ':<C-U>0,2' . blame_map, '<silent>')
      call s:Map('n', 'p',    ':<C-U>0,3' . blame_map, '<silent>')
      call s:Map('n', 'gO',   ':<C-U>0,4' . blame_map, '<silent>')
      call s:Map('n', 'O',    ':<C-U>0,5' . blame_map, '<silent>')

      call s:Map('n', 'D', ":echoerr 'fugitive: D has been removed in favor of dd'<CR>", '<silent>')
      call s:Map('n', 'dd', ":<C-U>call fugitive#DiffClose()<Bar>Gdiffsplit!<CR>", '<silent>')
      call s:Map('n', 'dh', ":<C-U>call fugitive#DiffClose()<Bar>Ghdiffsplit!<CR>", '<silent>')
      call s:Map('n', 'ds', ":<C-U>call fugitive#DiffClose()<Bar>Ghdiffsplit!<CR>", '<silent>')
      call s:Map('n', 'dv', ":<C-U>call fugitive#DiffClose()<Bar>Gvdiffsplit!<CR>", '<silent>')
      call s:Map('n', 'd?', ":<C-U>help fugitive_d<CR>", '<silent>')

    else
      call s:Map('n', '<2-LeftMouse>', ':<C-U>exe <SID>GF("edit")<CR>', '<silent>')
      call s:Map('n', '<CR>', ':<C-U>exe <SID>GF("edit")<CR>', '<silent>')
      call s:Map('n', 'o',    ':<C-U>exe <SID>GF("split")<CR>', '<silent>')
      call s:Map('n', 'gO',   ':<C-U>exe <SID>GF("vsplit")<CR>', '<silent>')
      call s:Map('n', 'O',    ':<C-U>exe <SID>GF("tabedit")<CR>', '<silent>')
      call s:Map('n', 'p',    ':<C-U>exe <SID>GF("pedit")<CR>', '<silent>')

      if !exists('g:fugitive_no_maps')
        if exists(':CtrlP') && get(g:, 'ctrl_p_map') =~? '^<c-p>$'
          nnoremap <buffer> <silent> <C-P> :<C-U>execute line('.') == 1 ? 'CtrlP ' . fnameescape(<SID>Tree()) : <SID>PreviousItem(v:count1)<CR>
        else
          nnoremap <buffer> <silent> <C-P> :<C-U>execute <SID>PreviousItem(v:count1)<CR>
        endif
        nnoremap <buffer> <silent> <C-N> :<C-U>execute <SID>NextItem(v:count1)<CR>
      endif
      call s:MapMotion('(', 'exe <SID>PreviousItem(v:count1)')
      call s:MapMotion(')', 'exe <SID>NextItem(v:count1)')
      call s:MapMotion('K', 'exe <SID>PreviousHunk(v:count1)')
      call s:MapMotion('J', 'exe <SID>NextHunk(v:count1)')
      call s:MapMotion('[c', 'exe <SID>PreviousHunk(v:count1)')
      call s:MapMotion(']c', 'exe <SID>NextHunk(v:count1)')
      call s:MapMotion('[/', 'exe <SID>PreviousFile(v:count1)')
      call s:MapMotion(']/', 'exe <SID>NextFile(v:count1)')
      call s:MapMotion('[m', 'exe <SID>PreviousFile(v:count1)')
      call s:MapMotion(']m', 'exe <SID>NextFile(v:count1)')
      call s:MapMotion('[[', 'exe <SID>PreviousSection(v:count1)')
      call s:MapMotion(']]', 'exe <SID>NextSection(v:count1)')
      call s:MapMotion('[]', 'exe <SID>PreviousSectionEnd(v:count1)')
      call s:MapMotion('][', 'exe <SID>NextSectionEnd(v:count1)')
      call s:Map('nxo', '*', '<SID>PatchSearchExpr(0)', '<expr>')
      call s:Map('nxo', '#', '<SID>PatchSearchExpr(1)', '<expr>')
    endif
    call s:Map('n', 'S',    ':<C-U>echoerr "Use gO"<CR>', '<silent>')
    call s:Map('n', 'dq', ":<C-U>call fugitive#DiffClose()<CR>", '<silent>')
    call s:Map('n', '-', ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>NavigateUp(v:count1))<Bar> if getline(1) =~# '^tree \x\{40,\}$' && empty(getline(2))<Bar>call search('^'.escape(expand('#:t'),'.*[]~\').'/\=$','wc')<Bar>endif<CR>", '<silent>')
    call s:Map('n', 'P',     ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit().'^'.v:count1.<SID>Relative(':'))<CR>", '<silent>')
    call s:Map('n', '~',     ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit().'~'.v:count1.<SID>Relative(':'))<CR>", '<silent>')
    call s:Map('n', 'C',     ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>", '<silent>')
    call s:Map('n', 'cp',    ":<C-U>echoerr 'Use gC'<CR>", '<silent>')
    call s:Map('n', 'gC',    ":<C-U>exe 'Gpedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>", '<silent>')
    call s:Map('n', 'gc',    ":<C-U>exe 'Gpedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>", '<silent>')
    call s:Map('n', 'gi',    ":<C-U>exe 'Gsplit' (v:count ? '.gitignore' : '.git/info/exclude')<CR>", '<silent>')
    call s:Map('x', 'gi',    ":<C-U>exe 'Gsplit' (v:count ? '.gitignore' : '.git/info/exclude')<CR>", '<silent>')

    call s:Map('n', 'c<Space>', ':Git commit<Space>')
    call s:Map('n', 'c<CR>', ':Git commit<CR>')
    call s:Map('n', 'cv<Space>', ':tab Git commit -v<Space>')
    call s:Map('n', 'cv<CR>', ':tab Git commit -v<CR>')
    call s:Map('n', 'ca', ':<C-U>Git commit --amend<CR>', '<silent>')
    call s:Map('n', 'cc', ':<C-U>Git commit<CR>', '<silent>')
    call s:Map('n', 'ce', ':<C-U>Git commit --amend --no-edit<CR>', '<silent>')
    call s:Map('n', 'cw', ':<C-U>Git commit --amend --only<CR>', '<silent>')
    call s:Map('n', 'cva', ':<C-U>tab Git commit -v --amend<CR>', '<silent>')
    call s:Map('n', 'cvc', ':<C-U>tab Git commit -v<CR>', '<silent>')
    call s:Map('n', 'cRa', ':<C-U>Git commit --reset-author --amend<CR>', '<silent>')
    call s:Map('n', 'cRe', ':<C-U>Git commit --reset-author --amend --no-edit<CR>', '<silent>')
    call s:Map('n', 'cRw', ':<C-U>Git commit --reset-author --amend --only<CR>', '<silent>')
    call s:Map('n', 'cf', ':<C-U>Git commit --fixup=<C-R>=<SID>SquashArgument()<CR>')
    call s:Map('n', 'cF', ':<C-U><Bar>Git -c sequence.editor=true rebase --interactive --autosquash<C-R>=<SID>RebaseArgument()<CR><Home>Git commit --fixup=<C-R>=<SID>SquashArgument()<CR>')
    call s:Map('n', 'cs', ':<C-U>Git commit --no-edit --squash=<C-R>=<SID>SquashArgument()<CR>')
    call s:Map('n', 'cS', ':<C-U><Bar>Git -c sequence.editor=true rebase --interactive --autosquash<C-R>=<SID>RebaseArgument()<CR><Home>Git commit --no-edit --squash=<C-R>=<SID>SquashArgument()<CR>')
    call s:Map('n', 'cA', ':<C-U>Git commit --edit --squash=<C-R>=<SID>SquashArgument()<CR>')
    call s:Map('n', 'c?', ':<C-U>help fugitive_c<CR>', '<silent>')

    call s:Map('n', 'cr<Space>', ':Git revert<Space>')
    call s:Map('n', 'cr<CR>', ':Git revert<CR>')
    call s:Map('n', 'crc', ':<C-U>Git revert <C-R>=<SID>SquashArgument()<CR><CR>', '<silent>')
    call s:Map('n', 'crn', ':<C-U>Git revert --no-commit <C-R>=<SID>SquashArgument()<CR><CR>', '<silent>')
    call s:Map('n', 'cr?', ':<C-U>help fugitive_cr<CR>', '<silent>')

    call s:Map('n', 'cm<Space>', ':Git merge<Space>')
    call s:Map('n', 'cm<CR>', ':Git merge<CR>')
    call s:Map('n', 'cmt', ':Git mergetool')
    call s:Map('n', 'cm?', ':<C-U>help fugitive_cm<CR>', '<silent>')

    call s:Map('n', 'cz<Space>', ':Git stash<Space>')
    call s:Map('n', 'cz<CR>', ':Git stash<CR>')
    call s:Map('n', 'cza', ':<C-U>Git stash apply --quiet --index stash@{<C-R>=v:count<CR>}<CR>')
    call s:Map('n', 'czA', ':<C-U>Git stash apply --quiet stash@{<C-R>=v:count<CR>}<CR>')
    call s:Map('n', 'czp', ':<C-U>Git stash pop --quiet --index stash@{<C-R>=v:count<CR>}<CR>')
    call s:Map('n', 'czP', ':<C-U>Git stash pop --quiet stash@{<C-R>=v:count<CR>}<CR>')
    call s:Map('n', 'czv', ':<C-U>exe "Gedit" fugitive#RevParse("stash@{" . v:count . "}")<CR>', '<silent>')
    call s:Map('n', 'czw', ':<C-U>Git stash --keep-index<C-R>=v:count > 1 ? " --all" : v:count ? " --include-untracked" : ""<CR><CR>')
    call s:Map('n', 'czz', ':<C-U>Git stash <C-R>=v:count > 1 ? " --all" : v:count ? " --include-untracked" : ""<CR><CR>')
    call s:Map('n', 'cz?', ':<C-U>help fugitive_cz<CR>', '<silent>')

    call s:Map('n', 'co<Space>', ':Git checkout<Space>')
    call s:Map('n', 'co<CR>', ':Git checkout<CR>')
    call s:Map('n', 'coo', ':<C-U>Git checkout <C-R>=substitute(<SID>SquashArgument(),"^$",get(<SID>TempState(),"filetype","") ==# "git" ? expand("<cfile>") : "","")<CR> --<CR>')
    call s:Map('n', 'co?', ':<C-U>help fugitive_co<CR>', '<silent>')

    call s:Map('n', 'cb<Space>', ':Git branch<Space>')
    call s:Map('n', 'cb<CR>', ':Git branch<CR>')
    call s:Map('n', 'cb?', ':<C-U>help fugitive_cb<CR>', '<silent>')

    call s:Map('n', 'r<Space>', ':Git rebase<Space>')
    call s:Map('n', 'r<CR>', ':Git rebase<CR>')
    call s:Map('n', 'ri', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><CR>', '<silent>')
    call s:Map('n', 'rf', ':<C-U>Git -c sequence.editor=true rebase --interactive --autosquash<C-R>=<SID>RebaseArgument()<CR><CR>', '<silent>')
    call s:Map('n', 'ru', ':<C-U>Git rebase --interactive @{upstream}<CR>', '<silent>')
    call s:Map('n', 'rp', ':<C-U>Git rebase --interactive @{push}<CR>', '<silent>')
    call s:Map('n', 'rw', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/reword/e<CR>', '<silent>')
    call s:Map('n', 'rm', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/edit/e<CR>', '<silent>')
    call s:Map('n', 'rd', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/drop/e<CR>', '<silent>')
    call s:Map('n', 'rk', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/drop/e<CR>', '<silent>')
    call s:Map('n', 'rx', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/drop/e<CR>', '<silent>')
    call s:Map('n', 'rr', ':<C-U>Git rebase --continue<CR>', '<silent>')
    call s:Map('n', 'rs', ':<C-U>Git rebase --skip<CR>', '<silent>')
    call s:Map('n', 're', ':<C-U>Git rebase --edit-todo<CR>', '<silent>')
    call s:Map('n', 'ra', ':<C-U>Git rebase --abort<CR>', '<silent>')
    call s:Map('n', 'r?', ':<C-U>help fugitive_r<CR>', '<silent>')

    call s:Map('n', '.',     ":<C-U> <C-R>=<SID>fnameescape(fugitive#Real(@%))<CR><Home>")
    call s:Map('x', '.',     ":<C-U> <C-R>=<SID>fnameescape(fugitive#Real(@%))<CR><Home>")
    call s:Map('n', 'g?',    ":<C-U>help fugitive-map<CR>", '<silent>')
    call s:Map('n', '<F1>',  ":<C-U>help fugitive-map<CR>", '<silent>')
  endif

  let old_browsex = maparg('<Plug>NetrwBrowseX', 'n')
  let new_browsex = substitute(old_browsex, '\Cnetrw#CheckIfRemote(\%(netrw#GX()\)\=)', '0', 'g')
  let new_browsex = substitute(new_browsex, 'netrw#GX()\|expand((exists("g:netrw_gx")? g:netrw_gx : ''<cfile>''))', 'fugitive#GX()', 'g')
  if new_browsex !=# old_browsex
    exe 'nnoremap <silent> <buffer> <Plug>NetrwBrowseX' new_browsex
  endif
endfunction

function! fugitive#GX() abort
  try
    let results = &filetype ==# 'fugitive' ? s:StatusCfile() : &filetype ==# 'git' ? s:cfile() : []
    if len(results) && len(results[0])
      return FugitiveReal(s:Generate(results[0]))
    endif
  catch /^fugitive:/
  endtry
  return expand(get(g:, 'netrw_gx', expand('<cfile>')))
endfunction

function! s:StatusCfile(...) abort
  let tree = s:Tree()
  if empty(tree)
    return ['']
  endif
  let lead = s:cpath(tree, getcwd()) ? './' : tree . '/'
  let info = s:StageInfo()
  let line = getline('.')
  if len(info.sigil) && len(info.section) && len(info.paths)
    if info.section ==# 'Unstaged' && info.sigil !=# '-'
      return [lead . info.relative[0], info.offset, 'normal!zv']
    elseif info.section ==# 'Staged' && info.sigil ==# '-'
      return ['@:' . info.relative[0], info.offset, 'normal!zv']
    else
      return [':0:' . info.relative[0], info.offset, 'normal!zv']
    endif
  elseif len(info.paths)
    return [lead . info.relative[0]]
  elseif len(info.commit)
    return [info.commit]
  elseif line =~# '^' . s:ref_header . ': \|^Head: '
    return [matchstr(line, ' \zs.*')]
  else
    return ['']
  endif
endfunction

function! fugitive#StatusCfile() abort
  let file = fugitive#Find(s:StatusCfile()[0])
  return empty(file) ? fugitive#Cfile() : s:fnameescape(file)
endfunction

function! s:MessageCfile(...) abort
  let tree = s:Tree()
  if empty(tree)
    return ''
  endif
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

function! fugitive#MessageCfile() abort
  let file = fugitive#Find(s:MessageCfile())
  return empty(file) ? fugitive#Cfile() : s:fnameescape(file)
endfunction

function! s:cfile() abort
  if empty(FugitiveGitDir())
    return []
  endif
  try
    let myhash = s:DirRev(@%)[1]
    if len(myhash)
      try
        let myhash = fugitive#RevParse(myhash)
      catch /^fugitive:/
        let myhash = ''
      endtry
    endif
    if empty(myhash) && get(s:TempState(), 'filetype', '') ==# 'git'
      let lnum = line('.')
      while lnum > 0
        if getline(lnum) =~# '^\%(commit\|tag\) \w'
          let myhash = matchstr(getline(lnum),'^\w\+ \zs\S\+')
          break
        endif
        let lnum -= 1
      endwhile
    endif

    let showtree = (getline(1) =~# '^tree ' && getline(2) == "")

    let treebase = substitute(s:DirCommitFile(@%)[1], '^\d$', ':&', '') . ':' .
          \ s:Relative('') . (s:Relative('') =~# '^$\|/$' ? '' : '/')

    if getline('.') =~# '^\d\{6\} \l\{3,8\} \x\{40,\}\t'
      return [treebase . s:sub(matchstr(getline('.'),'\t\zs.*'),'/$','')]
    elseif showtree
      return [treebase . s:sub(getline('.'),'/$','')]

    else

      let dcmds = []

      " Index
      if getline('.') =~# '^\d\{6\} \x\{40,\} \d\t'
        let ref = matchstr(getline('.'),'\x\{40,\}')
        let file = ':'.s:sub(matchstr(getline('.'),'\d\t.*'),'\t',':')
        return [file]
      endif

      if getline('.') =~# '^ref: '
        let ref = strpart(getline('.'),5)

      elseif getline('.') =~# '^commit \x\{40,\}\>'
        let ref = matchstr(getline('.'),'\x\{40,\}')
        return [ref]

      elseif getline('.') =~# '^parent \x\{40,\}\>'
        let ref = matchstr(getline('.'),'\x\{40,\}')
        let line = line('.')
        let parent = 0
        while getline(line) =~# '^parent '
          let parent += 1
          let line -= 1
        endwhile
        return [ref]

      elseif getline('.') =~# '^tree \x\{40,\}$'
        let ref = matchstr(getline('.'),'\x\{40,\}')
        if len(myhash) && fugitive#RevParse(myhash.':') ==# ref
          let ref = myhash.':'
        endif
        return [ref]

      elseif getline('.') =~# '^object \x\{40,\}$' && getline(line('.')+1) =~ '^type \%(commit\|tree\|blob\)$'
        let ref = matchstr(getline('.'),'\x\{40,\}')
        let type = matchstr(getline(line('.')+1),'type \zs.*')

      elseif getline('.') =~# '^\l\{3,8\} '.myhash.'$'
        let ref = s:DirRev(@%)[1]

      elseif getline('.') =~# '^\l\{3,8\} \x\{40,\}\>'
        let ref = matchstr(getline('.'),'\x\{40,\}')
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
        let dcmd = 'Gdiffsplit! +'.offset

      elseif getline('.') =~# '^diff --git \%([abciow12]/.*\|/dev/null\) \%([abciow12]/.*\|/dev/null\)'
        let dref = matchstr(getline('.'),'\Cdiff --git \zs\%([abciow12]/.*\|/dev/null\)\ze \%([abciow12]/.*\|/dev/null\)')
        let ref = matchstr(getline('.'),'\Cdiff --git \%([abciow12]/.*\|/dev/null\) \zs\%([abciow12]/.*\|/dev/null\)')
        let dcmd = 'Gdiffsplit!'

      elseif getline('.') =~# '^index ' && getline(line('.')-1) =~# '^diff --git \%([abciow12]/.*\|/dev/null\) \%([abciow12]/.*\|/dev/null\)'
        let line = getline(line('.')-1)
        let dref = matchstr(line,'\Cdiff --git \zs\%([abciow12]/.*\|/dev/null\)\ze \%([abciow12]/.*\|/dev/null\)')
        let ref = matchstr(line,'\Cdiff --git \%([abciow12]/.*\|/dev/null\) \zs\%([abciow12]/.*\|/dev/null\)')
        let dcmd = 'Gdiffsplit!'

      elseif line('$') == 1 && getline('.') =~ '^\x\{40,\}$'
        let ref = getline('.')

      elseif expand('<cword>') =~# '^\x\{7,\}\>'
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
      let ref = substitute(ref, '^\(\w\)/', '\=get(prefixes, submatch(1), "@:")', '')
      if exists('dref')
        let dref = substitute(dref, '^\(\w\)/', '\=get(prefixes, submatch(1), "@:")', '')
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
    let results = &filetype ==# 'fugitive' ? s:StatusCfile() : &filetype ==# 'gitcommit' ? [s:MessageCfile()] : s:cfile()
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  if len(results) > 1
    return 'G' . a:mode .
          \ ' +' . escape(results[1], ' ') . ' ' .
          \ s:fnameescape(results[0]) . join(map(results[2:-1], '"|" . v:val'), '')
  elseif len(results) && len(results[0])
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
  let dir = s:Dir(bufnr(''))
  if empty(dir)
    return ''
  endif
  let status = ''
  let commit = s:DirCommitFile(@%)[1]
  if len(commit)
    let status .= ':' . commit[0:6]
  endif
  let status .= '('.FugitiveHead(7, dir).')'
  return '[Git'.status.']'
endfunction

function! fugitive#statusline(...) abort
  return fugitive#Statusline()
endfunction

function! fugitive#head(...) abort
  if empty(s:Dir())
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
      return '+-' . v:folddashes . ' ' . (add<10&&remove<100?' ':'') . add . '+ ' . (remove<10&&add<100?' ':'') . remove . '- ' . filename
    endif
  elseif line_foldstart =~# '^@@\+ .* @@'
    return '+-' . v:folddashes . ' ' . line_foldstart
  elseif &filetype ==# 'gitcommit' && line_foldstart =~# '^# .*:$'
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

" Section: Initialization

function! fugitive#Init() abort
  throw 'Third party code is using fugitive#Init() which has been removed. Contact the author if you have a reason to still use it'
endfunction

function! fugitive#is_git_dir(path) abort
  throw 'Third party code is using fugitive#is_git_dir() which has been removed. Change it to FugitiveIsGitDir()'
endfunction

function! fugitive#extract_git_dir(path) abort
  throw 'Third party code is using fugitive#extract_git_dir() which has been removed. Change it to FugitiveExtractGitDir()'
endfunction

function! fugitive#detect(path) abort
  throw 'Third party code is using fugitive#detect() which has been removed. Contact the author if you have a reason to still use it'
endfunction

" Section: End
