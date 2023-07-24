" Location:     autoload/fugitive.vim
" Maintainer:   Tim Pope <http://tpo.pe/>

" The functions contained within this file are for internal use only.  For the
" official API, see the commented functions in plugin/fugitive.vim.

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

function! s:JoinChomp(list) abort
  if empty(a:list[-1])
    return join(a:list[0:-2], "\n")
  else
    return join(a:list, "\n")
  endif
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

function! s:fnameescape(file) abort
  if type(a:file) == type([])
    return join(map(copy(a:file), 's:fnameescape(v:val)'))
  else
    return fnameescape(a:file)
  endif
endfunction

function! fugitive#UrlDecode(str) abort
  return substitute(a:str, '%\(\x\x\)', '\=iconv(nr2char("0x".submatch(1)), "utf-8", "latin1")', 'g')
endfunction

function! s:UrlEncode(str) abort
  return substitute(a:str, '[%#?&;+=\<> [:cntrl:]]', '\=printf("%%%02X", char2nr(submatch(0)))', 'g')
endfunction

function! s:PathUrlEncode(str) abort
  return substitute(a:str, '[%#?[:cntrl:]]', '\=printf("%%%02X", char2nr(submatch(0)))', 'g')
endfunction

function! s:PathJoin(prefix, str) abort
  if a:prefix =~# '://'
    return a:prefix . s:PathUrlEncode(a:str)
  else
    return a:prefix . a:str
  endif
endfunction

function! s:throw(string) abort
  throw 'fugitive: '.a:string
endfunction

function! s:VersionCheck() abort
  if v:version < 704
    return 'return ' . string('echoerr "fugitive: Vim 7.4 or newer required"')
  elseif empty(fugitive#GitVersion())
    let exe = get(s:GitCmd(), 0, '')
    if len(exe) && !executable(exe)
      return 'return ' . string('echoerr "fugitive: cannot find ' . string(exe) . ' in PATH"')
    endif
    return 'return ' . string('echoerr "fugitive: cannot execute Git"')
  elseif !fugitive#GitVersion(1, 8, 5)
    return 'return ' . string('echoerr "fugitive: Git 1.8.5 or newer required"')
  else
    return ''
  endif
endfunction

let s:worktree_error = "core.worktree is required when using an external Git dir"
function! s:DirCheck(...) abort
  let vcheck = s:VersionCheck()
  if !empty(vcheck)
    return vcheck
  endif
  let dir = call('FugitiveGitDir', a:000)
  if !empty(dir) && FugitiveWorkTree(dir, 1) is# 0
    return 'return ' . string('echoerr "fugitive: ' . s:worktree_error . '"')
  elseif !empty(dir)
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
    let default = a:1
    if default ==# 'SpanOrigin'
      if s:OriginBufnr() > 0 && (mods =~# '\<vertical\>' ? &winfixheight : &winfixwidth)
        let default = 'Edge'
      else
        let default = ''
      endif
    endif
    if default ==# 'Edge'
      if mods =~# '\<vertical\>' ? &splitright : &splitbelow
        let mods = 'botright ' . mods
      else
        let mods = 'topleft ' . mods
      endif
    else
      let mods = default . ' ' . mods
    endif
  endif
  return substitute(mods, '\s\+', ' ', 'g')
endfunction

if exists('+shellslash')

  let s:dir_commit_file = '\c^fugitive://\%(/[^/]\@=\)\=\([^?#]\{-1,\}\)//\%(\(\x\{40,\}\|[0-3]\)\(/[^?#]*\)\=\)\=$'

  function! s:Slash(path) abort
    return tr(a:path, '\', '/')
  endfunction

  function! s:VimSlash(path) abort
    return tr(a:path, '\/', &shellslash ? '//' : '\\')
  endfunction

else

  let s:dir_commit_file = '\c^fugitive://\([^?#]\{-\}\)//\%(\(\x\{40,\}\|[0-3]\)\(/[^?#]*\)\=\)\=$'

  function! s:Slash(path) abort
    return a:path
  endfunction

  function! s:VimSlash(path) abort
    return a:path
  endfunction

endif

function! s:AbsoluteVimPath(...) abort
  if a:0 && type(a:1) == type('')
    let path = a:1
  else
    let path = bufname(a:0 && a:1 > 0 ? a:1 : '')
    if getbufvar(a:0 && a:1 > 0 ? a:1 : '', '&buftype') !~# '^\%(nowrite\|acwrite\)\=$'
      return path
    endif
  endif
  if s:Slash(path) =~# '^/\|^\a\+:'
    return path
  else
    return getcwd() . matchstr(getcwd(), '[\\/]') . path
  endif
endfunction

function! s:Resolve(path) abort
  let path = resolve(a:path)
  if has('win32')
    let path = s:VimSlash(fnamemodify(fnamemodify(path, ':h'), ':p') . fnamemodify(path, ':t'))
  endif
  return path
endfunction

function! s:FileIgnoreCase(for_completion) abort
  return (exists('+fileignorecase') && &fileignorecase)
        \ || (a:for_completion && exists('+wildignorecase') && &wildignorecase)
endfunction

function! s:cpath(path, ...) abort
  if s:FileIgnoreCase(0)
    let path = s:VimSlash(tolower(a:path))
  else
    let path = s:VimSlash(a:path)
  endif
  return a:0 ? path ==# s:cpath(a:1) : path
endfunction

let s:quote_chars = {
      \ "\007": 'a', "\010": 'b', "\011": 't', "\012": 'n', "\013": 'v', "\014": 'f', "\015": 'r',
      \ '"': '"', '\': '\'}

let s:unquote_chars = {
      \ 'a': "\007", 'b': "\010", 't': "\011", 'n': "\012", 'v': "\013", 'f': "\014", 'r': "\015",
      \ '"': '"', '\': '\'}

function! s:Quote(string) abort
  let string = substitute(a:string, "[\001-\037\"\\\177]", '\="\\" . get(s:quote_chars, submatch(0), printf("%03o", char2nr(submatch(0))))', 'g')
  if string !=# a:string
    return '"' . string . '"'
  else
    return string
  endif
endfunction

function! fugitive#Unquote(string) abort
  let string = substitute(a:string, "\t*$", '', '')
  if string =~# '^".*"$'
    return substitute(string[1:-2], '\\\(\o\o\o\|.\)', '\=get(s:unquote_chars, submatch(1), iconv(nr2char("0" . submatch(1)), "utf-8", "latin1"))', 'g')
  else
    return string
  endif
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

function! s:DoAutocmd(...) abort
  return join(map(copy(a:000), "'doautocmd <nomodeline>' . v:val"), '|')
endfunction

function! s:Map(mode, lhs, rhs, ...) abort
  let maps = []
  let flags = a:0 && type(a:1) == type('') ? a:1 : ''
  let defer = flags =~# '<unique>'
  let flags = substitute(flags, '<unique>', '', '') . (a:rhs =~# '<Plug>' ? '' : '<script>') . '<nowait>'
  for mode in split(a:mode, '\zs')
    if a:0 <= 1
      call add(maps, mode.'map <buffer>' . substitute(flags, '<unique>', '', '') . ' <Plug>fugitive:' . a:lhs . ' ' . a:rhs)
    endif
    let skip = 0
    let head = a:lhs
    let tail = ''
    let keys = get(g:, mode.'remap', {})
    if type(keys) == type([])
      continue
    endif
    while !empty(head)
      if has_key(keys, head)
        let head = keys[head]
        let skip = empty(head)
        break
      endif
      let tail = matchstr(head, '<[^<>]*>$\|.$') . tail
      let head = substitute(head, '<[^<>]*>$\|.$', '', '')
    endwhile
    if !skip && (!defer || empty(mapcheck(head.tail, mode)))
      call add(maps, mode.'map <buffer>' . flags . ' ' . head.tail . ' ' . a:rhs)
      if a:0 > 1 && a:2
        let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') .
              \ '|sil! exe "' . mode . 'unmap <buffer> ' . head.tail . '"'
      endif
    endif
  endfor
  exe join(maps, '|')
  return ''
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

function! fugitive#Wait(job_or_jobs, ...) abort
  let original = type(a:job_or_jobs) == type([]) ? copy(a:job_or_jobs) : [a:job_or_jobs]
  let jobs = map(copy(original), 'type(v:val) ==# type({}) ? get(v:val, "job", "") : v:val')
  call filter(jobs, 'type(v:val) !=# type("")')
  let timeout_ms = a:0 ? a:1 : -1
  if exists('*jobwait')
    call map(copy(jobs), 'chanclose(v:val, "stdin")')
    call jobwait(jobs, timeout_ms)
    let jobs = map(copy(original), 'type(v:val) ==# type({}) ? get(v:val, "job", "") : v:val')
    call filter(jobs, 'type(v:val) !=# type("")')
    if len(jobs)
      sleep 1m
    endif
  else
    let sleep = has('patch-8.2.2366') ? 'sleep! 1m' : 'sleep 1m'
    for job in jobs
      if ch_status(job) ==# 'open'
        call ch_close_in(job)
      endif
    endfor
    let i = 0
    for job in jobs
      while ch_status(job) !~# '^closed$\|^fail$' || job_status(job) ==# 'run'
        if i == timeout_ms
          break
        endif
        let i += 1
        exe sleep
      endwhile
    endfor
  endif
  return a:job_or_jobs
endfunction

function! s:JobVimExit(dict, callback, temp, job, status) abort
  let a:dict.exit_status = a:status
  let a:dict.stderr = readfile(a:temp . '.err', 'b')
  call delete(a:temp . '.err')
  let a:dict.stdout = readfile(a:temp . '.out', 'b')
  call delete(a:temp . '.out')
  call delete(a:temp . '.in')
  call remove(a:dict, 'job')
  call call(a:callback[0], [a:dict] + a:callback[1:-1])
endfunction

function! s:JobNvimExit(dict, callback, job, data, type) dict abort
  let a:dict.stdout = self.stdout
  let a:dict.stderr = self.stderr
  let a:dict.exit_status = a:data
  call remove(a:dict, 'job')
  call call(a:callback[0], [a:dict] + a:callback[1:-1])
endfunction

function! s:JobExecute(argv, jopts, stdin, callback, ...) abort
  let dict = a:0 ? a:1 : {}
  let cb = len(a:callback) ? a:callback : [function('len')]
  if exists('*jobstart')
    call extend(a:jopts, {
          \ 'stdout_buffered': v:true,
          \ 'stderr_buffered': v:true,
          \ 'on_exit': function('s:JobNvimExit', [dict, cb])})
    try
      let dict.job = jobstart(a:argv, a:jopts)
      if !empty(a:stdin)
        call chansend(dict.job, a:stdin)
        call chanclose(dict.job, 'stdin')
      endif
    catch /^Vim\%((\a\+)\)\=:E475:/
      let [dict.exit_status, dict.stdout, dict.stderr] = [122, [''], ['']]
    endtry
  elseif exists('*ch_close_in')
    let temp = tempname()
    call extend(a:jopts, {
          \ 'out_io': 'file',
          \ 'out_name': temp . '.out',
          \ 'err_io': 'file',
          \ 'err_name': temp . '.err',
          \ 'exit_cb': function('s:JobVimExit', [dict, cb, temp])})
    if a:stdin ==# ['']
      let a:jopts.in_io = 'null'
    elseif !empty(a:stdin)
      let a:jopts.in_io = 'file'
      let a:jopts.in_name = temp . '.in'
      call writefile(a:stdin, a:jopts.in_name, 'b')
    endif
    let dict.job = job_start(a:argv, a:jopts)
    if job_status(dict.job) ==# 'fail'
      let [dict.exit_status, dict.stdout, dict.stderr] = [122, [''], ['']]
      unlet dict.job
    endif
  elseif &shell !~# 'sh' || &shell =~# 'fish\|\%(powershell\|pwsh\)\%(\.exe\)\=$'
    throw 'fugitive: Vim 8 or higher required to use ' . &shell
  else
    let cmd = s:shellesc(a:argv)
    let outfile = tempname()
    try
      if len(a:stdin)
        call writefile(a:stdin, outfile . '.in', 'b')
        let cmd = ' (' . cmd . ' >' . outfile . ' <' . outfile . '.in) '
      else
        let cmd = ' (' . cmd . ' >' . outfile . ') '
      endif
      let dict.stderr = split(system(cmd), "\n", 1)
      let dict.exit_status = v:shell_error
      let dict.stdout = readfile(outfile, 'b')
      call call(cb[0], [dict] + cb[1:-1])
    finally
      call delete(outfile)
      call delete(outfile . '.in')
    endtry
  endif
  if empty(a:callback)
    call fugitive#Wait(dict)
  endif
  return dict
endfunction

function! s:add_methods(namespace, method_names) abort
  for name in a:method_names
    let s:{a:namespace}_prototype[name] = s:function('s:'.a:namespace.'_'.name)
  endfor
endfunction

" Section: Git

let s:run_jobs = (exists('*ch_close_in') || exists('*jobstart')) && exists('*bufwinid')

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
      call add(list, '/usr/bin/env')
    endif
    while string =~# '\S'
      let arg = matchstr(string, '^\s*\%(' . dquote . '''[^'']*''\|\\.\|[^' . "\t" . ' |]\)\+')
      let string = strpart(string, len(arg))
      let arg = substitute(arg, '^\s\+', '', '')
      let arg = substitute(arg,
            \ '\(' . dquote . '''\%(''''\|[^'']\)*''\|\\[' . s:fnameescape . ']\|^\\[>+-]\|' . s:commit_expand . '\)\|' . s:expand,
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
  return len(tree) ? s:VimSlash(tree) : getcwd()
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
    let dir = a:1.git_dir
  elseif a:0
    let dir = s:GitDir(a:1)
  else
    let dir = ''
  endif
  if len(dir)
    let tree = s:Tree(dir)
    if empty(tree)
      call add(git, '--git-dir=' . FugitiveGitPath(dir))
    else
      if !s:cpath(tree . '/.git', dir) || len($GIT_DIR)
        call add(git, '--git-dir=' . FugitiveGitPath(dir))
      endif
      if !s:cpath(tree, getcwd())
        call extend(git, ['-C', FugitiveGitPath(tree)])
      endif
    endif
  endif
  return git + flags
endfunction

let s:git_versions = {}
function! fugitive#GitVersion(...) abort
  let git = s:GitShellCmd()
  if !has_key(s:git_versions, git)
    let s:git_versions[git] = matchstr(get(s:JobExecute(s:GitCmd() + ['--version'], {}, [], [], {}).stdout, 0, ''), '\d[^[:space:]]\+')
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

function! s:Dir(...) abort
  return a:0 ? FugitiveGitDir(a:1) : FugitiveGitDir()
endfunction

function! s:GitDir(...) abort
  return a:0 ? FugitiveGitDir(a:1) : FugitiveGitDir()
endfunction

function! s:InitializeBuffer(repo) abort
  let b:git_dir = s:GitDir(a:repo)
endfunction

function! s:SameRepo(one, two) abort
  let one = s:GitDir(a:one)
  return !empty(one) && one ==# s:GitDir(a:two)
endfunction

if exists('+shellslash')
  function! s:DirUrlPrefix(dir) abort
    let gd = s:GitDir(a:dir)
    return 'fugitive://' . (gd =~# '^[^/]' ? '/' : '') . s:PathUrlEncode(gd) . '//'
  endfunction
else
  function! s:DirUrlPrefix(dir) abort
    return 'fugitive://' . s:PathUrlEncode(s:GitDir(a:dir)) . '//'
  endfunction
endif

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

function! s:PreparePathArgs(cmd, dir, literal, explicit) abort
  if !a:explicit
    call insert(a:cmd, '--literal-pathspecs')
  endif
  let split = index(a:cmd, '--')
  for i in range(split < 0 ? len(a:cmd) : split)
      if type(a:cmd[i]) == type(0)
        if a:literal
          let a:cmd[i] = fugitive#Path(bufname(a:cmd[i]), './', a:dir)
        else
          let a:cmd[i] = fugitive#Path(bufname(a:cmd[i]), ':(top,literal)', a:dir)
        endif
      endif
  endfor
  if split < 0
    return a:cmd
  endif
  for i in range(split + 1, len(a:cmd) - 1)
    if type(a:cmd[i]) == type(0)
      if a:literal
        let a:cmd[i] = fugitive#Path(bufname(a:cmd[i]), './', a:dir)
      else
        let a:cmd[i] = fugitive#Path(bufname(a:cmd[i]), ':(top,literal)', a:dir)
      endif
    elseif !a:explicit
      let a:cmd[i] = fugitive#Path(a:cmd[i], './', a:dir)
    endif
  endfor
  return a:cmd
endfunction

let s:git_index_file_env = {}
function! s:GitIndexFileEnv() abort
  if $GIT_INDEX_FILE =~# '^/\|^\a:' && !has_key(s:git_index_file_env, $GIT_INDEX_FILE)
    let s:git_index_file_env[$GIT_INDEX_FILE] = s:Slash(FugitiveVimPath($GIT_INDEX_FILE))
  endif
  return get(s:git_index_file_env, $GIT_INDEX_FILE, '')
endfunction

function! s:PrepareEnv(env, dir) abort
  if len($GIT_INDEX_FILE) && len(s:Tree(a:dir)) && !has_key(a:env, 'GIT_INDEX_FILE')
    let index_dir = substitute(s:GitIndexFileEnv(), '[^/]\+$', '', '')
    let our_dir = fugitive#Find('.git/', a:dir)
    if !s:cpath(index_dir, our_dir) && !s:cpath(resolve(index_dir), our_dir)
      let a:env['GIT_INDEX_FILE'] = FugitiveGitPath(fugitive#Find('.git/index', a:dir))
    endif
  endif
  if len($GIT_WORK_TREE)
    let a:env['GIT_WORK_TREE'] = '.'
  endif
endfunction

let s:prepare_env = {
      \ 'sequence.editor': 'GIT_SEQUENCE_EDITOR',
      \ 'core.editor': 'GIT_EDITOR',
      \ 'core.askpass': 'GIT_ASKPASS',
      \ }
function! fugitive#PrepareDirEnvGitFlagsArgs(...) abort
  if !fugitive#GitVersion(1, 8, 5)
    throw 'fugitive: Git 1.8.5 or higher required'
  endif
  let git = s:GitCmd()
  if a:0 == 1 && type(a:1) == type({}) && (has_key(a:1, 'fugitive_dir') || has_key(a:1, 'git_dir')) && has_key(a:1, 'flags') && has_key(a:1, 'args')
    let cmd = a:1.flags + a:1.args
    let dir = s:Dir(a:1)
    if has_key(a:1, 'git')
      let git = a:1.git
    endif
    let env = get(a:1, 'env', {})
  else
    let list_args = []
    let cmd = []
    for l:.arg in a:000
      if type(arg) ==# type([])
        call extend(list_args, arg)
      else
        call add(cmd, arg)
      endif
    endfor
    call extend(cmd, list_args)
    let env = {}
  endif
  let autoenv = {}
  let explicit_pathspec_option = 0
  let literal_pathspecs = 1
  let i = 0
  let arg_count = 0
  while i < len(cmd)
    if type(cmd[i]) == type({})
      if has_key(cmd[i], 'fugitive_dir') || has_key(cmd[i], 'git_dir')
        let dir = s:Dir(cmd[i])
      endif
      if has_key(cmd[i], 'git')
        let git = cmd[i].git
      endif
      if has_key(cmd[i], 'env')
        call extend(env, cmd[i].env)
      endif
      call remove(cmd, i)
    elseif cmd[i] =~# '^$\|[\/.]' && cmd[i] !~# '^-'
      let dir = s:Dir(remove(cmd, i))
    elseif cmd[i] =~# '^--git-dir='
      let dir = s:Dir(remove(cmd, i)[10:-1])
    elseif type(cmd[i]) ==# type(0)
      let dir = s:Dir(remove(cmd, i))
    elseif cmd[i] ==# '-c' && len(cmd) > i + 1
      let key = matchstr(cmd[i+1], '^[^=]*')
      if has_key(s:prepare_env, tolower(key))
        let var = s:prepare_env[tolower(key)]
        let val = matchstr(cmd[i+1], '=\zs.*')
        let autoenv[var] = val
      endif
      let i += 2
    elseif cmd[i] =~# '^--.*pathspecs$'
      let literal_pathspecs = (cmd[i] ==# '--literal-pathspecs')
      let explicit_pathspec_option = 1
      let i += 1
    elseif cmd[i] !~# '^-'
      let arg_count = len(cmd) - i
      break
    else
      let i += 1
    endif
  endwhile
  if !exists('dir')
    let dir = s:Dir()
  endif
  call extend(autoenv, env)
  call s:PrepareEnv(autoenv, dir)
  if len($GPG_TTY) && !has_key(autoenv, 'GPG_TTY')
    let autoenv.GPG_TTY = ''
  endif
  call s:PreparePathArgs(cmd, dir, literal_pathspecs, explicit_pathspec_option)
  return [dir, env, extend(autoenv, env), git, cmd[0 : -arg_count-1], arg_count ? cmd[-arg_count : -1] : []]
endfunction

function! s:BuildEnvPrefix(env) abort
  let pre = ''
  let env = items(a:env)
  if empty(env)
    return ''
  elseif &shell =~# '\%(powershell\|pwsh\)\%(\.exe\)\=$'
    return join(map(env, '"$Env:" . v:val[0] . " = ''" . substitute(v:val[1], "''", "''''", "g") . "''; "'), '')
  elseif s:winshell()
    return join(map(env, '"set " . substitute(join(v:val, "="), "[&|<>^]", "^^^&", "g") . "& "'), '')
  else
    return '/usr/bin/env ' . s:shellesc(map(env, 'join(v:val, "=")')) . ' '
  endif
endfunction

function! s:JobOpts(cmd, env) abort
  if empty(a:env)
    return [a:cmd, {}]
  elseif has('patch-8.2.0239') ||
        \ has('nvim') && api_info().version.api_level - api_info().version.api_prerelease >= 7 ||
        \ has('patch-8.0.0902') && !has('nvim') && (!has('win32') || empty(filter(keys(a:env), 'exists("$" . v:val)')))
    return [a:cmd, {'env': a:env}]
  endif
  let envlist = map(items(a:env), 'join(v:val, "=")')
  if !has('win32')
    return [['/usr/bin/env'] + envlist + a:cmd, {}]
  else
    let pre = join(map(envlist, '"set " . substitute(v:val, "[&|<>^]", "^^^&", "g") . "& "'), '')
    if len(a:cmd) == 3 && a:cmd[0] ==# 'cmd.exe' && a:cmd[1] ==# '/c'
      return [a:cmd[0:1] + [pre . a:cmd[2]], {}]
    else
      return [['cmd.exe', '/c', pre . s:WinShellEsc(a:cmd)], {}]
    endif
  endif
endfunction

function! s:PrepareJob(opts) abort
  let dict = {'argv': a:opts.argv}
  if has_key(a:opts, 'env')
    let dict.env = a:opts.env
  endif
  let [argv, jopts] = s:JobOpts(a:opts.argv, get(a:opts, 'env', {}))
  if has_key(a:opts, 'cwd')
    if has('patch-8.0.0902')
      let jopts.cwd = a:opts.cwd
      let dict.cwd = a:opts.cwd
    else
      throw 'fugitive: cwd unsupported'
    endif
  endif
  return [argv, jopts, dict]
endfunction

function! fugitive#PrepareJob(...) abort
  if a:0 == 1 && type(a:1) == type({}) && has_key(a:1, 'argv') && !has_key(a:1, 'args')
    return s:PrepareJob(a:1)
  endif
  let [repo, user_env, exec_env, git, flags, args] = call('fugitive#PrepareDirEnvGitFlagsArgs', a:000)
  let dir = s:GitDir(repo)
  let dict = {'git': git, 'git_dir': dir, 'flags': flags, 'args': args}
  if len(user_env)
    let dict.env = user_env
  endif
  let cmd = flags + args
  let tree = s:Tree(repo)
  if empty(tree) || index(cmd, '--') == len(cmd) - 1
    let dict.cwd = getcwd()
    call extend(cmd, ['--git-dir=' . FugitiveGitPath(dir)], 'keep')
  else
    let dict.cwd = s:VimSlash(tree)
    call extend(cmd, ['-C', FugitiveGitPath(tree)], 'keep')
    if !s:cpath(tree . '/.git', dir) || len($GIT_DIR)
      call extend(cmd, ['--git-dir=' . FugitiveGitPath(dir)], 'keep')
    endif
  endif
  call extend(cmd, git, 'keep')
  return s:JobOpts(cmd, exec_env) + [dict]
endfunction

function! fugitive#Execute(...) abort
  let cb = copy(a:000)
  let cmd = []
  let stdin = []
  while len(cb) && type(cb[0]) !=# type(function('tr'))
    if type(cb[0]) ==# type({}) && has_key(cb[0], 'stdin')
      if type(cb[0].stdin) == type([])
        call extend(stdin, cb[0].stdin)
      elseif type(cb[0].stdin) == type('')
        call extend(stdin, readfile(cb[0].stdin, 'b'))
      endif
      if len(keys(cb[0])) == 1
        call remove(cb, 0)
        continue
      endif
    endif
    call add(cmd, remove(cb, 0))
  endwhile
  let [argv, jopts, dict] = call('fugitive#PrepareJob', cmd)
  return s:JobExecute(argv, jopts, stdin, cb, dict)
endfunction

function! s:BuildShell(dir, env, git, args) abort
  let cmd = copy(a:args)
  let tree = s:Tree(a:dir)
  let pre = s:BuildEnvPrefix(a:env)
  if empty(tree) || index(cmd, '--') == len(cmd) - 1
    call insert(cmd, '--git-dir=' . FugitiveGitPath(a:dir))
  else
    call extend(cmd, ['-C', FugitiveGitPath(tree)], 'keep')
    if !s:cpath(tree . '/.git', a:dir) || len($GIT_DIR)
      call extend(cmd, ['--git-dir=' . FugitiveGitPath(a:dir)], 'keep')
    endif
  endif
  return pre . join(map(a:git + cmd, 's:shellesc(v:val)'))
endfunction

function! s:JobNvimCallback(lines, job, data, type) abort
  let a:lines[-1] .= remove(a:data, 0)
  call extend(a:lines, a:data)
endfunction

function! s:SystemList(cmd) abort
  let exit = []
  if exists('*jobstart')
    let lines = ['']
    let jopts = {
          \ 'on_stdout': function('s:JobNvimCallback', [lines]),
          \ 'on_stderr': function('s:JobNvimCallback', [lines]),
          \ 'on_exit': { j, code, _ -> add(exit, code) }}
    let job = jobstart(a:cmd, jopts)
    call chanclose(job, 'stdin')
    call jobwait([job])
    if empty(lines[-1])
      call remove(lines, -1)
    endif
    return [lines, exit[0]]
  elseif exists('*ch_close_in')
    let lines = []
    let jopts = {
          \ 'out_cb': { j, str -> add(lines, str) },
          \ 'err_cb': { j, str -> add(lines, str) },
          \ 'exit_cb': { j, code -> add(exit, code) }}
    let job = job_start(a:cmd, jopts)
    call ch_close_in(job)
    let sleep = has('patch-8.2.2366') ? 'sleep! 1m' : 'sleep 1m'
    while ch_status(job) !~# '^closed$\|^fail$' || job_status(job) ==# 'run'
      exe sleep
    endwhile
    return [lines, exit[0]]
  else
    let [output, exec_error] = s:SystemError(s:shellesc(a:cmd))
    let lines = split(output, "\n", 1)
    if empty(lines[-1])
      call remove(lines, -1)
    endif
    return [lines, v:shell_error]
  endif
endfunction

function! fugitive#ShellCommand(...) abort
  let [repo, _, env, git, flags, args] = call('fugitive#PrepareDirEnvGitFlagsArgs', a:000)
  return s:BuildShell(s:GitDir(repo), env, git, flags + args)
endfunction

function! s:SystemError(cmd, ...) abort
  let cmd = type(a:cmd) == type([]) ? s:shellesc(a:cmd) : a:cmd
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
    let out = call('system', [cmd] + a:000)
    return [out, v:shell_error]
  catch /^Vim\%((\a\+)\)\=:E484:/
    let opts = ['shell', 'shellcmdflag', 'shellredir', 'shellquote', 'shellxquote', 'shellxescape', 'shellslash']
    call filter(opts, 'exists("+".v:val) && !empty(eval("&".v:val))')
    call map(opts, 'v:val."=".eval("&".v:val)')
    call s:throw('failed to run `' . cmd . '` with ' . join(opts, ' '))
  finally
    if exists('shellredir')
      let &shellredir = shellredir
    endif
    if exists('guioptions')
      let &guioptions = guioptions
    endif
  endtry
endfunction

function! s:ChompStderr(...) abort
  let r = call('fugitive#Execute', a:000)
  return !r.exit_status ? '' : len(r.stderr) > 1 ? s:JoinChomp(r.stderr) : 'unknown Git error' . string(r)
endfunction

function! s:ChompDefault(default, ...) abort
  let r = call('fugitive#Execute', a:000)
  return r.exit_status ? a:default : s:JoinChomp(r.stdout)
endfunction

function! s:LinesError(...) abort
  let r = call('fugitive#Execute', a:000)
  if empty(r.stdout[-1])
    call remove(r.stdout, -1)
  endif
  return [r.exit_status ? [] : r.stdout, r.exit_status]
endfunction

function! s:NullError(cmd) abort
  let r = fugitive#Execute(a:cmd)
  let list = r.exit_status ? [] : split(tr(join(r.stdout, "\1"), "\1\n", "\n\1"), "\1", 1)[0:-2]
  return [list, s:JoinChomp(r.stderr), r.exit_status]
endfunction

function! s:TreeChomp(...) abort
  let r = call('fugitive#Execute', a:000)
  if !r.exit_status
    return s:JoinChomp(r.stdout)
  endif
  throw 'fugitive: error running `' . call('fugitive#ShellCommand', a:000) . '`: ' . s:JoinChomp(r.stderr)
endfunction

function! s:StdoutToFile(out, cmd, ...) abort
  let [argv, jopts, _] = fugitive#PrepareJob(a:cmd)
  let exit = []
  if exists('*jobstart')
    call extend(jopts, {
          \ 'stdout_buffered': v:true,
          \ 'stderr_buffered': v:true,
          \ 'on_exit': { j, code, _ -> add(exit, code) }})
    let job = jobstart(argv, jopts)
    if a:0
      call chansend(job, a:1)
    endif
    call chanclose(job, 'stdin')
    call jobwait([job])
    if len(a:out)
      call writefile(jopts.stdout, a:out, 'b')
    endif
    return [join(jopts.stderr, "\n"), exit[0]]
  elseif exists('*ch_close_in')
    try
      let err = tempname()
      call extend(jopts, {
            \ 'out_io': len(a:out) ? 'file' : 'null',
            \ 'out_name': a:out,
            \ 'err_io': 'file',
            \ 'err_name': err,
            \ 'exit_cb': { j, code -> add(exit, code) }})
      let job = job_start(argv, jopts)
      if a:0
        call ch_sendraw(job, a:1)
      endif
      call ch_close_in(job)
      while ch_status(job) !~# '^closed$\|^fail$' || job_status(job) ==# 'run'
        exe has('patch-8.2.2366') ? 'sleep! 1m' : 'sleep 1m'
      endwhile
      return [join(readfile(err, 'b'), "\n"), exit[0]]
    finally
      call delete(err)
    endtry
  elseif s:winshell() || &shell !~# 'sh' || &shell =~# 'fish\|\%(powershell\|pwsh\)\%(\.exe\)\=$'
    throw 'fugitive: Vim 8 or higher required to use ' . &shell
  else
    let cmd = fugitive#ShellCommand(a:cmd)
    return s:SystemError(' (' . cmd . ' >' . a:out . ') ')
  endif
endfunction

let s:head_cache = {}

function! fugitive#Head(...) abort
  let dir = a:0 > 1 ? a:2 : s:Dir()
  if empty(dir)
    return ''
  endif
  let file = FugitiveActualDir(dir) . '/HEAD'
  let ftime = getftime(file)
  if ftime == -1
    return ''
  elseif ftime != get(s:head_cache, file, [-1])[0]
    let s:head_cache[file] = [ftime, readfile(file)[0]]
  endif
  let head = s:head_cache[file][1]
  let len = a:0 ? a:1 : 0
  if head =~# '^ref: '
    if len < 0
      return strpart(head, 5)
    else
      return substitute(head, '\C^ref: \%(refs/\%(heads/\|remotes/\|tags/\)\=\)\=', '', '')
    endif
  elseif head =~# '^\x\{40,\}$'
    return len < 0 ? head : strpart(head, 0, len)
  else
    return ''
  endif
endfunction

function! fugitive#RevParse(rev, ...) abort
  let hash = s:ChompDefault('', [a:0 ? a:1 : s:Dir(), 'rev-parse', '--verify', a:rev, '--'])
  if hash =~# '^\x\{40,\}$'
    return hash
  endif
  throw 'fugitive: failed to parse revision ' . a:rev
endfunction

" Section: Git config

function! s:ConfigTimestamps(dir, dict) abort
  let files = ['/etc/gitconfig', '~/.gitconfig',
        \ len($XDG_CONFIG_HOME) ? $XDG_CONFIG_HOME . '/git/config' : '~/.config/git/config']
  if len(a:dir)
    call add(files, fugitive#Find('.git/config', a:dir))
  endif
  call extend(files, get(a:dict, 'include.path', []))
  return join(map(files, 'getftime(expand(v:val))'), ',')
endfunction

function! s:ConfigCallback(r, into) abort
  let dict = a:into[1]
  if has_key(dict, 'job')
    call remove(dict, 'job')
  endif
  let lines = a:r.exit_status ? [] : split(tr(join(a:r.stdout, "\1"), "\1\n", "\n\1"), "\1", 1)[0:-2]
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
  let callbacks = remove(dict, 'callbacks')
  lockvar! dict
  let a:into[0] = s:ConfigTimestamps(dict.git_dir, dict)
  for callback in callbacks
    call call(callback[0], [dict] + callback[1:-1])
  endfor
endfunction

let s:config_prototype = {}

let s:config = {}
function! fugitive#ExpireConfig(...) abort
  if !a:0 || a:1 is# 0
    let s:config = {}
  else
    let key = a:1 is# '' ? '_' : s:GitDir(a:0 ? a:1 : -1)
    if len(key) && has_key(s:config, key)
      call remove(s:config, key)
    endif
  endif
endfunction

function! fugitive#Config(...) abort
  let name = ''
  let default = get(a:, 3, '')
  if a:0 && type(a:1) == type(function('tr'))
    let dir = s:Dir()
    let callback = a:000
  elseif a:0 > 1 && type(a:2) == type(function('tr'))
    if type(a:1) == type({}) && has_key(a:1, 'GetAll')
      if has_key(a:1, 'callbacks')
        call add(a:1.callbacks, a:000[1:-1])
      else
        call call(a:2, [a:1] + a:000[2:-1])
      endif
      return a:1
    else
      let dir = s:Dir(a:1)
      let callback = a:000[1:-1]
    endif
  elseif a:0 >= 2 && type(a:2) == type({}) && has_key(a:2, 'GetAll')
    return get(fugitive#ConfigGetAll(a:1, a:2), 0, default)
  elseif a:0 >= 2
    let dir = s:Dir(a:2)
    let name = a:1
  elseif a:0 == 1 && type(a:1) == type({}) && has_key(a:1, 'GetAll')
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
  let git_dir = s:GitDir(dir)
  let dir_key = len(git_dir) ? git_dir : '_'
  let [ts, dict] = get(s:config, dir_key, ['new', {}])
  if !has_key(dict, 'job') && ts !=# s:ConfigTimestamps(git_dir, dict)
    let dict = copy(s:config_prototype)
    let dict.git_dir = git_dir
    let into = ['running', dict]
    let dict.callbacks = []
    let exec = fugitive#Execute([dir, 'config', '--list', '-z', '--'], function('s:ConfigCallback'), into)
    if has_key(exec, 'job')
      let dict.job = exec.job
    endif
    let s:config[dir_key] = into
  endif
  if !exists('l:callback')
    call fugitive#Wait(dict)
  elseif has_key(dict, 'callbacks')
    call add(dict.callbacks, callback)
  else
    call call(callback[0], [dict] + callback[1:-1])
  endif
  return len(name) ? get(fugitive#ConfigGetAll(name, dict), 0, default) : dict
endfunction

function! fugitive#ConfigGetAll(name, ...) abort
  if a:0 && (type(a:name) !=# type('') || a:name !~# '^[[:alnum:]-]\+\.' && type(a:1) ==# type('') && a:1 =~# '^[[:alnum:]-]\+\.')
    let config = fugitive#Config(a:name)
    let name = a:1
  else
    let config = fugitive#Config(a:0 ? a:1 : s:Dir())
    let name = a:name
  endif
  let name = substitute(name, '^[^.]\+\|[^.]\+$', '\L&', 'g')
  call fugitive#Wait(config)
  return name =~# '\.' ? copy(get(config, name, [])) : []
endfunction

function! fugitive#ConfigGetRegexp(pattern, ...) abort
  if type(a:pattern) !=# type('')
    let config = fugitive#Config(a:name)
    let pattern = a:0 ? a:1 : '.*'
  else
    let config = fugitive#Config(a:0 ? a:1 : s:Dir())
    let pattern = a:pattern
  endif
  call fugitive#Wait(config)
  let filtered = map(filter(copy(config), 'v:key =~# "\\." && v:key =~# pattern'), 'copy(v:val)')
  if pattern !~# '\\\@<!\%(\\\\\)*\\z[se]'
    return filtered
  endif
  let transformed = {}
  for [k, v] in items(filtered)
    let k = matchstr(k, pattern)
    if len(k)
      let transformed[k] = v
    endif
  endfor
  return transformed
endfunction

function! s:config_GetAll(name) dict abort
  let name = substitute(a:name, '^[^.]\+\|[^.]\+$', '\L&', 'g')
  call fugitive#Wait(self)
  return name =~# '\.' ? copy(get(self, name, [])) : []
endfunction

function! s:config_Get(name, ...) dict abort
  return get(self.GetAll(a:name), 0, a:0 ? a:1 : '')
endfunction

function! s:config_GetRegexp(pattern) dict abort
  return fugitive#ConfigGetRegexp(self, a:pattern)
endfunction

call s:add_methods('config', ['GetAll', 'Get', 'GetRegexp'])

function! s:RemoteDefault(dir) abort
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

function! s:SshParseHost(value) abort
  let patterns = []
  let negates = []
  for host in split(a:value, '\s\+')
    let pattern = substitute(host, '[\\^$.*~?]', '\=submatch(0) == "*" ? ".*" : submatch(0) == "?" ? "." : "\\" . submatch(0)', 'g')
    if pattern[0] ==# '!'
      call add(negates, '\&\%(^' . pattern[1 : -1] . '$\)\@!')
    else
      call add(patterns, pattern)
    endif
  endfor
  return '^\%(' . join(patterns, '\|') . '\)$' . join(negates, '')
endfunction

function! s:SshParseConfig(into, root, file, ...) abort
  if !filereadable(a:file)
    return a:into
  endif
  let host = a:0 ? a:1 : '^\%(.*\)$'
  for line in readfile(a:file)
    let key = tolower(matchstr(line, '^\s*\zs\w\+\ze\s'))
    let value = matchstr(line, '^\s*\w\+\s\+\zs.*\S')
    if key ==# 'match'
      let host = value ==# 'all' ? '^\%(.*\)$' : ''
    elseif key ==# 'host'
      let host = s:SshParseHost(value)
    elseif key ==# 'include'
      call s:SshParseInclude(a:into, a:root, host, value)
    elseif len(key) && len(host)
      call extend(a:into, {key : []}, 'keep')
      call add(a:into[key], [host, value])
    endif
  endfor
  return a:into
endfunction

function! s:SshParseInclude(into, root, host, value) abort
  for glob in split(a:value)
    if glob !~# '^/'
      let glob = a:root . glob
    endif
    for file in split(glob(glob), "\n")
      call s:SshParseConfig(a:into, a:root, file, a:host)
    endfor
  endfor
endfunction

unlet! s:ssh_config
function! fugitive#SshConfig(host, ...) abort
  if !exists('s:ssh_config')
    let s:ssh_config = {}
    for file in [expand("~/.ssh/config"), "/etc/ssh/ssh_config"]
      call s:SshParseConfig(s:ssh_config, substitute(file, '\w*$', '', ''), file)
    endfor
  endif
  let host_config = {}
  for key in a:0 ? a:1 : keys(s:ssh_config)
    for [host_pattern, value] in get(s:ssh_config, key, [])
      if a:host =~# host_pattern
        let host_config[key] = value
        break
      endif
    endfor
  endfor
  return host_config
endfunction

function! fugitive#SshHostAlias(authority) abort
  let [_, user, host, port; __] = matchlist(a:authority, '^\%(\([^/@]\+\)@\)\=\(.\{-\}\)\%(:\(\d\+\)\)\=$')
  let c = fugitive#SshConfig(host, ['user', 'hostname', 'port'])
  if empty(user)
    let user = get(c, 'user', '')
  endif
  if empty(port)
    let port = get(c, 'port', '')
  endif
  return (len(user) ? user . '@' : '') . get(c, 'hostname', host) . (port =~# '^\%(22\)\=$' ? '' : ':' . port)
endfunction

function! s:CurlResponse(result) abort
  let a:result.headers = {}
  for line in a:result.exit_status ? [] : remove(a:result, 'stdout')
    let header = matchlist(line, '^\([[:alnum:]-]\+\):\s\(.\{-\}\)'. "\r\\=$")
    if len(header)
      let k = tolower(header[1])
      if has_key(a:result.headers, k)
        let a:result.headers[k] .= ', ' . header[2]
      else
        let a:result.headers[k] = header[2]
      endif
    elseif empty(line)
      break
    endif
  endfor
endfunction

let s:remote_headers = {}

function! fugitive#RemoteHttpHeaders(remote) abort
  let remote = type(a:remote) ==# type({}) ? get(a:remote, 'remote', '') : a:remote
  if type(remote) !=# type('') || remote !~# '^https\=://.' || !s:executable('curl')
    return {}
  endif
  let remote = substitute(remote, '#.*', '', '')
  if !has_key(s:remote_headers, remote)
    let url = remote . '/info/refs?service=git-upload-pack'
    let exec = s:JobExecute(
          \ ['curl', '--disable', '--silent', '--max-time', '5', '-X', 'GET', '-I',
          \ url], {}, [], [function('s:CurlResponse')], {})
    call fugitive#Wait(exec)
    let s:remote_headers[remote] = exec.headers
  endif
  return s:remote_headers[remote]
endfunction

function! s:UrlParse(url) abort
  let scp_authority = matchstr(a:url, '^[^:/]\+\ze:\%(//\)\@!')
  if len(scp_authority) && !(has('win32') && scp_authority =~# '^\a:[\/]')
    let url = {'scheme': 'ssh', 'authority': s:UrlEncode(scp_authority), 'hash': '',
          \ 'path': s:UrlEncode(strpart(a:url, len(scp_authority) + 1))}
  elseif empty(a:url)
    let url = {'scheme': '', 'authority': '', 'path': '', 'hash': ''}
  else
    let match = matchlist(a:url, '^\([[:alnum:].+-]\+\)://\([^/]*\)\(/[^#]*\)\=\(#.*\)\=$')
    if empty(match)
      let url = {'scheme': 'file', 'authority': '', 'hash': '',
            \ 'path': s:UrlEncode(a:url)}
    else
      let url = {'scheme': match[1], 'authority': match[2], 'hash': match[4]}
      let url.path = empty(match[3]) ? '/' : match[3]
    endif
  endif
  return url
endfunction

function! s:UrlPopulate(string, into) abort
  let url = a:into
  let url.protocol = substitute(url.scheme, '.\zs$', ':', '')
  let url.user = fugitive#UrlDecode(matchstr(url.authority, '.\{-\}\ze@', '', ''))
  let url.host = substitute(url.authority, '.\{-\}@', '', '')
  let url.hostname = substitute(url.host, ':\d\+$', '', '')
  let url.port = matchstr(url.host, ':\zs\d\+$', '', '')
  let url.origin = substitute(url.scheme, '.\zs$', '://', '') . url.host
  let url.search = matchstr(url.path, '?.*')
  let url.pathname = '/' . matchstr(url.path, '^/\=\zs[^?]*')
  if (url.scheme ==# 'ssh' || url.scheme ==# 'git') && url.path[0:1] ==# '/~'
    let url.path = strpart(url.path, 1)
  endif
  if url.path =~# '^/'
    let url.href = url.scheme . '://' . url.authority . url.path . url.hash
  elseif url.path =~# '^\~'
    let url.href = url.scheme . '://' . url.authority . '/' . url.path . url.hash
  elseif url.scheme ==# 'ssh' && url.authority !~# ':'
    let url.href = url.authority . ':' . url.path . url.hash
  else
    let url.href = a:string
  endif
  let url.path = fugitive#UrlDecode(matchstr(url.path, '^[^?]*'))
  let url.url = matchstr(url.href, '^[^#]*')
endfunction

function! s:RemoteResolve(url, flags) abort
  let remote = s:UrlParse(a:url)
  if remote.scheme =~# '^https\=$' && index(a:flags, ':nohttp') < 0
    let headers = fugitive#RemoteHttpHeaders(a:url)
    let loc = matchstr(get(headers, 'location', ''), '^https\=://.\{-\}\ze/info/refs?')
    if len(loc)
      let remote = s:UrlParse(loc)
    else
      let remote.headers = headers
    endif
  elseif remote.scheme ==# 'ssh'
    let remote.authority = fugitive#SshHostAlias(remote.authority)
  endif
  return remote
endfunction

function! s:ConfigLengthSort(i1, i2) abort
  return len(a:i2[0]) - len(a:i1[0])
endfunction

function! s:RemoteCallback(config, into, flags, cb) abort
  if a:into.remote_name =~# '^\.\=$'
    let a:into.remote_name = s:RemoteDefault(a:config)
  endif
  let url = a:into.remote_name

  if url ==# '.git'
    let url = s:GitDir(a:config)
  elseif url !~# ':\|^/\|^\a:[\/]\|^\.\.\=/'
    let url = FugitiveConfigGet('remote.' . url . '.url', a:config)
  endif
  let instead_of = []
  for [k, vs] in items(fugitive#ConfigGetRegexp('^url\.\zs.\{-\}\ze\.insteadof$', a:config))
    for v in vs
      call add(instead_of, [v, k])
    endfor
  endfor
  call sort(instead_of, 's:ConfigLengthSort')
  for [orig, replacement] in instead_of
    if strpart(url, 0, len(orig)) ==# orig
      let url = replacement . strpart(url, len(orig))
      break
    endif
  endfor
  if index(a:flags, ':noresolve') < 0
    call extend(a:into, s:RemoteResolve(url, a:flags))
  else
    call extend(a:into, s:UrlParse(url))
  endif
  call s:UrlPopulate(url, a:into)
  if len(a:cb)
    call call(a:cb[0], [a:into] + a:cb[1:-1])
  endif
endfunction

function! s:Remote(dir, remote, flags, cb) abort
  let into = {'remote_name': a:remote, 'git_dir': s:GitDir(a:dir)}
  let config = fugitive#Config(a:dir, function('s:RemoteCallback'), into, a:flags, a:cb)
  if len(a:cb)
    return config
  else
    call fugitive#Wait(config)
    return into
  endif
endfunction

function! s:RemoteParseArgs(args) abort
  " Extract ':noresolve' style flags and an optional callback
  let args = []
  let flags = []
  let cb = copy(a:args)
  while len(cb)
    if type(cb[0]) ==# type(function('tr'))
      break
    elseif len(args) > 1 || type(cb[0]) ==# type('') && cb[0] =~# '^:'
      call add(flags, remove(cb, 0))
    else
      call add(args, remove(cb, 0))
    endif
  endwhile

  " From the remaining 0-2 arguments, extract the remote and Git config
  let remote = ''
  if empty(args)
    let dir_or_config = s:Dir()
  elseif len(args) == 1 && type(args[0]) ==# type('') && args[0] !~# '^/\|^\a:[\\/]'
    let dir_or_config = s:Dir()
    let remote = args[0]
  elseif len(args) == 1
    let dir_or_config = args[0]
    if type(args[0]) ==# type({}) && has_key(args[0], 'remote_name')
      let remote = args[0].remote_name
    endif
  elseif type(args[1]) !=# type('') || args[1] =~# '^/\|^\a:[\\/]'
    let dir_or_config = args[1]
    let remote = args[0]
  else
    let dir_or_config = args[0]
    let remote = args[1]
  endif
  return [dir_or_config, remote, flags, cb]
endfunction

function! fugitive#Remote(...) abort
  let [dir_or_config, remote, flags, cb] = s:RemoteParseArgs(a:000)
  return s:Remote(dir_or_config, remote, flags, cb)
endfunction

function! s:RemoteUrlCallback(remote, callback) abort
  return call(a:callback[0], [a:remote.url] + a:callback[1:-1])
endfunction

function! fugitive#RemoteUrl(...) abort
  let [dir_or_config, remote_url, flags, cb] = s:RemoteParseArgs(a:000)
  if len(cb)
    let cb = [function('s:RemoteUrlCallback'), cb]
  endif
  let remote = s:Remote(dir_or_config, remote_url, flags, cb)
  return get(remote, 'url', remote_url)
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

function! s:QuickfixOpen(nr, mods) abort
  let mods = substitute(s:Mods(a:mods), '\<tab\>', '', '')
  return mods . (a:nr < 0 ? 'c' : 'l').'open' . (mods =~# '\<vertical\>' ? ' 20' : '')
endfunction

function! s:QuickfixStream(nr, event, title, cmd, first, mods, callback, ...) abort
  call s:BlurStatus()
  let opts = {'title': a:title, 'context': {'items': []}}
  call s:QuickfixCreate(a:nr, opts)
  let event = (a:nr < 0 ? 'c' : 'l') . 'fugitive-' . a:event
  exe s:DoAutocmd('QuickFixCmdPre ' . event)
  let winnr = winnr()
  exe s:QuickfixOpen(a:nr, a:mods)
  if winnr != winnr()
    wincmd p
  endif

  let buffer = []
  let lines = s:SystemList(a:cmd)[0]
  for line in lines
    call extend(buffer, call(a:callback, a:000 + [line]))
    if len(buffer) >= 20
      let contexts = map(copy(buffer), 'get(v:val, "context", {})')
      lockvar contexts
      call extend(opts.context.items, contexts)
      unlet contexts
      call s:QuickfixSet(a:nr, remove(buffer, 0, -1), 'a')
      if a:mods !~# '\<silent\>'
        redraw
      endif
    endif
  endfor
  call extend(buffer, call(a:callback, a:000 + [0]))
  call extend(opts.context.items, map(copy(buffer), 'get(v:val, "context", {})'))
  lockvar opts.context.items
  call s:QuickfixSet(a:nr, buffer, 'a')

  exe s:DoAutocmd('QuickFixCmdPost ' . event)
  if a:first && len(s:QuickfixGet(a:nr))
    return (a:nr < 0 ? 'cfirst' : 'lfirst')
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

let s:repo_prototype = {}

function! fugitive#repo(...) abort
  let dir = a:0 ? s:GitDir(a:1) : (len(s:GitDir()) ? s:GitDir() : FugitiveExtractGitDir(expand('%:p')))
  if dir !=# ''
    return extend({'git_dir': dir, 'fugitive_dir': dir}, s:repo_prototype, 'keep')
  endif
  throw 'fugitive: not a Git repository'
endfunction

function! s:repo_dir(...) dict abort
  if !a:0
    return self.git_dir
  endif
  throw 'fugitive: fugitive#repo().dir("...") has been replaced by FugitiveFind(".git/...")'
endfunction

function! s:repo_tree(...) dict abort
  let tree = s:Tree(self.git_dir)
  if empty(tree)
    throw 'fugitive: no work tree'
  elseif !a:0
    return tree
  endif
  throw 'fugitive: fugitive#repo().tree("...") has been replaced by FugitiveFind(":(top)...")'
endfunction

function! s:repo_bare() dict abort
  throw 'fugitive: fugitive#repo().bare() has been replaced by !empty(FugitiveWorkTree())'
endfunction

function! s:repo_find(object) dict abort
  throw 'fugitive: fugitive#repo().find(...) has been replaced by FugitiveFind(...)'
endfunction

function! s:repo_translate(rev) dict abort
  throw 'fugitive: fugitive#repo().translate(...) has been replaced by FugitiveFind(...)'
endfunction

function! s:repo_head(...) dict abort
  throw 'fugitive: fugitive#repo().head(...) has been replaced by FugitiveHead(...)'
endfunction

call s:add_methods('repo',['dir','tree','bare','find','translate','head'])

function! s:repo_git_command(...) dict abort
  throw 'fugitive: fugitive#repo().git_command(...) has been replaced by FugitiveShellCommand(...)'
endfunction

function! s:repo_git_chomp(...) dict abort
  silent return substitute(system(fugitive#ShellCommand(a:000, self.git_dir)), '\n$', '', '')
endfunction

function! s:repo_git_chomp_in_tree(...) dict abort
  return call(self.git_chomp, a:000, self)
endfunction

function! s:repo_rev_parse(rev) dict abort
  throw 'fugitive: fugitive#repo().rev_parse(...) has been replaced by FugitiveExecute("rev-parse", "--verify", ...).stdout'
endfunction

call s:add_methods('repo',['git_command','git_chomp','git_chomp_in_tree','rev_parse'])

function! s:repo_config(name) dict abort
  return FugitiveConfigGet(a:name, self.git_dir)
endfunction

call s:add_methods('repo',['config'])

" Section: File API

function! s:DirCommitFile(path) abort
  let vals = matchlist(s:Slash(a:path), s:dir_commit_file)
  if empty(vals)
    return ['', '', '']
  endif
  return [s:Dir(fugitive#UrlDecode(vals[1])), vals[2], empty(vals[2]) ? '/.git/index' : fugitive#UrlDecode(vals[3])]
endfunction

function! s:DirRev(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  return [dir, commit . file ==# '/.git/index' ? ':' : (!empty(dir) && commit =~# '^.$' ? ':' : '') . commit . substitute(file, '^/', ':', '')]
endfunction

function! fugitive#Parse(url) abort
  return reverse(s:DirRev(a:url))
endfunction

let s:merge_heads = ['MERGE_HEAD', 'REBASE_HEAD', 'CHERRY_PICK_HEAD', 'REVERT_HEAD']
function! s:MergeHead(dir) abort
  let dir = fugitive#Find('.git/', a:dir)
  for head in s:merge_heads
    if filereadable(dir . head)
      return head
    endif
  endfor
  return ''
endfunction

function! s:Owner(path, ...) abort
  let dir = a:0 ? s:Dir(a:1) : s:Dir()
  if empty(dir)
    return ''
  endif
  let actualdir = fugitive#Find('.git/', dir)
  let [pdir, commit, file] = s:DirCommitFile(a:path)
  if s:SameRepo(dir, pdir)
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
    return s:VimSlash((len(tree) ? tree : s:GitDir(dir)) . file)
  endif
  let pre = substitute(matchstr(a:url, '^\a\a\+\ze:'), '^.', '\u&', '')
  if len(pre) && pre !=? 'fugitive' && exists('*' . pre . 'Real')
    let url = {pre}Real(a:url)
  else
    let url = fnamemodify(a:url, ':p' . (a:url =~# '[\/]$' ? '' : ':s?[\/]$??'))
  endif
  return s:VimSlash(empty(url) ? a:url : url)
endfunction

function! fugitive#Path(url, ...) abort
  if empty(a:url)
    return ''
  endif
  let repo = call('s:Dir', a:000[1:-1])
  let dir_s = fugitive#Find('.git/', repo)
  let tree = fugitive#Find(':/', repo)
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
  let [argdir, commit, file] = s:DirCommitFile(url)
  if !empty(argdir) && !s:SameRepo(argdir, repo)
    let file = ''
  elseif len(dir_s) && s:cpath(strpart(url, 0, len(dir_s)), dir_s)
    let file = '/.git' . strpart(url, len(dir_s)-1)
  elseif len(tree) && s:cpath(url[0 : len(tree)]) ==# s:cpath(tree . '/')
    let file = url[len(tree) : -1]
  elseif s:cpath(url) ==# s:cpath(tree)
    let file = '/'
  endif
  if empty(file) && a:1 =~# '^$\|^[.:]/$'
    return FugitiveGitPath(fugitive#Real(a:url))
  endif
  return substitute(file, '^/', '\=a:1', '')
endfunction

function! s:Relative(...) abort
  return fugitive#Path(@%, a:0 ? a:1 : ':(top)', a:0 > 1 ? a:2 : s:Dir())
endfunction

function! fugitive#Find(object, ...) abort
  if type(a:object) == type(0)
    let name = bufname(a:object)
    return s:VimSlash(name =~# '^$\|^/\|^\a\+:' ? name : getcwd() . '/' . name)
  elseif a:object =~# '^[~$]'
    let prefix = matchstr(a:object, '^[~$]\i*')
    let owner = expand(prefix)
    return s:VimSlash(FugitiveVimPath((len(owner) ? owner : prefix) . strpart(a:object, len(prefix))))
  endif
  let rev = s:Slash(a:object)
  if rev =~# '^\a\+://' && rev !~# '^fugitive:'
    return rev
  elseif rev =~# '^$\|^/\|^\%(\a\a\+:\).*\%(//\|::\)' . (has('win32') ? '\|^\a:/' : '')
    return s:VimSlash(a:object)
  elseif rev =~# '^\.\.\=\%(/\|$\)'
    return s:VimSlash(simplify(getcwd() . '/' . a:object))
  endif
  let dir = call('s:GitDir', a:000)
  if empty(dir)
    let file = matchstr(a:object, '^\%(:\d:\|[^:]*:\)\zs\%(\.\.\=$\|\.\.\=/.*\|/.*\|\w:/.*\)')
    let dir = FugitiveExtractGitDir(file)
    if empty(dir)
      return ''
    endif
  endif
  let tree = s:Tree(dir)
  let urlprefix = s:DirUrlPrefix(dir)
  let base = len(tree) ? tree : urlprefix . '0'
  if rev ==# '.git'
    let f = len(tree) && len(getftype(tree . '/.git')) ? tree . '/.git' : dir
  elseif rev =~# '^\.git/'
    let f = strpart(rev, 5)
    let fdir = simplify(FugitiveActualDir(dir) . '/')
    let cdir = simplify(FugitiveCommonDir(dir) . '/')
    if f =~# '^\.\./\.\.\%(/\|$\)'
      let f = simplify(len(tree) ? tree . f[2:-1] : fdir . f)
    elseif f =~# '^\.\.\%(/\|$\)'
      let f = s:PathJoin(base, f[2:-1])
    elseif cdir !=# fdir && (
          \ f =~# '^\%(config\|hooks\|info\|logs/refs\|objects\|refs\|worktrees\)\%(/\|$\)' ||
          \ f !~# '^\%(index$\|index\.lock$\|\w*MSG$\|\w*HEAD$\|logs/\w*HEAD$\|logs$\|rebase-\w\+\)\%(/\|$\)' &&
          \ getftime(fdir . f) < 0 && getftime(cdir . f) >= 0)
      let f = simplify(cdir . f)
    else
      let f = simplify(fdir . f)
    endif
  elseif rev ==# ':/'
    let f = tree
  elseif rev =~# '^\.\%(/\|$\)'
    let f = s:PathJoin(base, rev[1:-1])
  elseif rev =~# '^::\%(/\|\a\+\:\)'
    let f = rev[2:-1]
  elseif rev =~# '^::\.\.\=\%(/\|$\)'
    let f = simplify(getcwd() . '/' . rev[2:-1])
  elseif rev =~# '^::'
    let f = s:PathJoin(base, '/' . rev[2:-1])
  elseif rev =~# '^:\%([0-3]:\)\=\.\.\=\%(/\|$\)\|^:[0-3]:\%(/\|\a\+:\)'
    let f = rev =~# '^:\%([0-3]:\)\=\.' ? simplify(getcwd() . '/' . matchstr(rev, '\..*')) : rev[3:-1]
    if s:cpath(base . '/', (f . '/')[0 : len(base)])
      let f = s:PathJoin(urlprefix, +matchstr(rev, '^:\zs\d\ze:') . '/' . strpart(f, len(base) + 1))
    else
      let altdir = FugitiveExtractGitDir(f)
      if len(altdir) && !s:cpath(dir, altdir)
        return fugitive#Find(a:object, altdir)
      endif
    endif
  elseif rev =~# '^:[0-3]:'
    let f = s:PathJoin(urlprefix, rev[1] . '/' . rev[3:-1])
  elseif rev ==# ':'
    let f = urlprefix
  elseif rev =~# '^:(\%(top\|top,literal\|literal,top\|literal\))'
    let f = matchstr(rev, ')\zs.*')
    if f=~# '^\.\.\=\%(/\|$\)'
      let f = simplify(getcwd() . '/' . f)
    elseif f !~# '^/\|^\%(\a\a\+:\).*\%(//\|::\)' . (has('win32') ? '\|^\a:/' : '')
      let f = s:PathJoin(base, '/' . f)
    endif
  elseif rev =~# '^:/\@!'
    let f = s:PathJoin(urlprefix, '0/' . rev[1:-1])
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
        let f = s:PathJoin(urlprefix, commit . file)
      else
        let f = s:PathJoin(base, '/' . substitute(rev, '^:/:\=\|^[^:]\+:', '', ''))
      endif
    endif
  endif
  return s:VimSlash(f)
endfunction

function! s:Generate(object, ...) abort
  let dir = a:0 ? a:1 : s:Dir()
  let f = fugitive#Find(a:object, dir)
  if !empty(f)
    return f
  elseif a:object ==# ':/'
    return len(dir) ? s:VimSlash(s:DirUrlPrefix(dir) . '0') : '.'
  endif
  let file = matchstr(a:object, '^\%(:\d:\|[^:]*:\)\=\zs.*')
  return empty(file) ? '' : fnamemodify(s:VimSlash(file), ':p')
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
  let dir = a:0 > 1 ? s:Dir(a:2) : s:Dir()
  let [fdir, rev] = s:DirRev(a:0 ? a:1 : @%)
  if !s:SameRepo(dir, fdir)
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
let s:commit_expand = '!\\\@!#\=\d*\|!%'

function! s:BufName(var) abort
  if a:var ==# '%'
    return bufname(get(s:TempState(), 'origin_bufnr', ''))
  elseif a:var =~# '^#\d*$'
    let nr = get(s:TempState(+a:var[1:-1]), 'origin_bufnr', '')
    return bufname(nr ? nr : +a:var[1:-1])
  else
    return expand(a:var)
  endif
endfunction

function! s:ExpandVar(other, var, flags, esc, ...) abort
  let cwd = a:0 ? a:1 : getcwd()
  if a:other =~# '^\'
    return a:other[1:-1]
  elseif a:other =~# '^'''
    return substitute(a:other[1:-2], "''", "'", "g")
  elseif a:other =~# '^"'
    return substitute(a:other[1:-2], '""', '"', "g")
  elseif a:other =~# '^[!`]'
    let buffer = s:BufName(a:other =~# '[0-9#]' ? '#' . matchstr(a:other, '\d\+') : '%')
    let owner = s:Owner(buffer)
    return len(owner) ? owner : '@'
  elseif a:other =~# '^\~[~.]$'
    return s:Slash(getcwd())
  elseif len(a:other)
    return expand(a:other)
  elseif a:var ==# '<cfile>'
    let bufnames = [expand('<cfile>')]
    if get(maparg('<Plug><cfile>', 'c', 0, 1), 'expr')
      try
        let bufnames = [eval(maparg('<Plug><cfile>', 'c'))]
        if bufnames[0] ==# "\<C-R>\<C-F>"
          let bufnames = [expand('<cfile>')]
        endif
      catch
      endtry
    endif
  elseif a:var =~# '^<'
    let bufnames = [s:BufName(a:var)]
  elseif a:var ==# '##'
    let bufnames = map(argv(), 'fugitive#Real(v:val)')
  else
    let bufnames = [fugitive#Real(s:BufName(a:var))]
  endif
  let files = []
  for bufname in bufnames
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
    call add(files, len(a:esc) ? shellescape(file) : file)
  endfor
  return join(files, "\1")
endfunction

if has('win32')
  let s:fnameescape = " \t\n*?`%#'\"|!<"
else
  let s:fnameescape = " \t\n*?[{`$\\%#'\"|!<"
endif

function! s:Expand(rev, ...) abort
  if a:rev =~# '^>' && s:Slash(@%) =~# '^fugitive://' && empty(s:DirCommitFile(@%)[1])
    return s:Slash(@%)
  elseif a:rev =~# '^>\=:[0-3]$'
    let file = len(expand('%')) ? a:rev[-2:-1] . ':%' : '%'
  elseif a:rev =~# '^>\%(:\=/\)\=$'
    let file = '%'
  elseif a:rev =~# '^>[> ]\@!' && @% !~# '^fugitive:' && s:Slash(@%) =~# '://\|^$'
    let file = '%'
  elseif a:rev ==# '>:'
    let file = empty(s:DirCommitFile(@%)[0]) ? ':0:%' : '%'
  elseif a:rev =~# '^>[> ]\@!'
    let rev = (a:rev =~# '^>[~^]' ? '!' : '') . a:rev[1:-1]
    let prefix = matchstr(rev, '^\%(\\.\|{[^{}]*}\|[^:]\)*')
    if prefix !=# rev
      let file = rev
    else
      let file = len(expand('%')) ? rev . ':%' : '%'
    endif
  elseif s:Slash(a:rev) =~# '^\a\a\+://'
    let file = substitute(a:rev, '\\\@<!\%(#\a\|%\x\x\)', '\\&', 'g')
  else
    let file = a:rev
  endif
  return substitute(file,
        \ '\(\\[' . s:fnameescape . ']\|^\\[>+-]\|' . s:commit_expand . '\|^\~[~.]\)\|' . s:expand,
        \ '\=tr(s:ExpandVar(submatch(1),submatch(2),submatch(3),"", a:0 ? a:1 : getcwd()), "\1", " ")', 'g')
endfunction

function! fugitive#Expand(object) abort
  return substitute(a:object,
        \ '\(\\[' . s:fnameescape . ']\|^\\[>+-]\|' . s:commit_expand . '\|^\~[~.]\)\|' . s:expand,
        \ '\=tr(s:ExpandVar(submatch(1),submatch(2),submatch(3),submatch(5)), "\1", " ")', 'g')
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
    let arg = matchstr(string, '^\s*\%(' . dquote . '''[^'']*''\|\\.\|[^' . "\t" . ' |]\)\+')
    let string = strpart(string, len(arg))
    let arg = substitute(arg, '^\s\+', '', '')
    if !exists('seen_separator')
      let arg = substitute(arg, '^\%([^:.][^:]*:\|^:\%((literal)\)\=\|^:[0-3]:\)\=\zs\.\.\=\%(/.*\)\=$',
            \ '\=s:DotRelative(s:Slash(simplify(getcwd() . "/" . submatch(0))), cwd)', '')
    endif
    let arg = substitute(arg,
          \ '\(' . dquote . '''\%(''''\|[^'']\)*''\|\\[' . s:fnameescape . ']\|^\\[>+-]\|' . s:commit_expand . '\|^\~[~]\|^\~\w*\|\$\w\+\)\|' . s:expand,
          \ '\=s:ExpandVar(submatch(1),submatch(2),submatch(3),submatch(5), cwd)', 'g')
    call extend(list, split(arg, "\1", 1))
    if arg ==# '--'
      let seen_separator = 1
    endif
  endwhile
  return [list, '']
endfunction

let s:trees = {}
let s:indexes = {}
function! s:TreeInfo(dir, commit) abort
  let key = s:GitDir(a:dir)
  if a:commit =~# '^:\=[0-3]$'
    let index = get(s:indexes, key, [])
    let newftime = getftime(fugitive#Find('.git/index', a:dir))
    if get(index, 0, -2) < newftime
      let [lines, exec_error] = s:LinesError([a:dir, 'ls-files', '--stage', '--'])
      let s:indexes[key] = [newftime, {'0': {}, '1': {}, '2': {}, '3': {}}]
      if exec_error
        return [{}, -1]
      endif
      for line in lines
        let [info, filename] = split(line, "\t")
        let [mode, sha, stage] = split(info, '\s\+')
        let s:indexes[key][1][stage][filename] = [newftime, mode, 'blob', sha, -2]
        while filename =~# '/'
          let filename = substitute(filename, '/[^/]*$', '', '')
          let s:indexes[key][1][stage][filename] = [newftime, '040000', 'tree', '', 0]
        endwhile
      endfor
    endif
    return [get(s:indexes[key][1], a:commit[-1:-1], {}), newftime]
  elseif a:commit =~# '^\x\{40,\}$'
    if !has_key(s:trees, key)
      let s:trees[key] = {}
    endif
    if !has_key(s:trees[key], a:commit)
      let ftime = s:ChompDefault('', [a:dir, 'log', '-1', '--pretty=format:%ct', a:commit, '--'])
      if empty(ftime)
        let s:trees[key][a:commit] = [{}, -1]
        return s:trees[key][a:commit]
      endif
      let s:trees[key][a:commit] = [{}, +ftime]
      let [lines, exec_error] = s:LinesError([a:dir, 'ls-tree', '-rtl', '--full-name', a:commit, '--'])
      if exec_error
        return s:trees[key][a:commit]
      endif
      for line in lines
        let [info, filename] = split(line, "\t")
        let [mode, type, sha, size] = split(info, '\s\+')
        let s:trees[key][a:commit][0][filename] = [+ftime, mode, type, sha, +size, filename]
      endfor
    endif
    return s:trees[key][a:commit]
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
  elseif empty(commit)
    return s:VimSlash(s:DirUrlPrefix(simplify(s:GitDir(dir))))
  endif
  if file =~# '/\.\.\%(/\|$\)'
    let tree = s:Tree(dir)
    if len(tree)
      let path = simplify(tree . file)
      if strpart(path . '/', 0, len(tree) + 1) !=# tree . '/'
        return s:VimSlash(path)
      endif
    endif
  endif
  return s:VimSlash(s:PathJoin(s:DirUrlPrefix(simplify(s:GitDir(dir))), commit . simplify(file)))
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
  let [error, exec_error] = s:StdoutToFile('', [a:dir, 'update-index', '--index-info'], info)
  return !exec_error ? '' : len(error) ? error : 'unknown update-index error'
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

if !exists('s:blobdirs')
  let s:blobdirs = {}
endif
function! s:BlobTemp(url) abort
  let [dir, commit, file] = s:DirCommitFile(a:url)
  if empty(file)
    return ''
  endif
  let key = s:GitDir(dir)
  if !has_key(s:blobdirs, key)
    let s:blobdirs[key] = tempname()
  endif
  let tempfile = s:blobdirs[key] . '/' . commit . file
  let tempparent = fnamemodify(tempfile, ':h')
  if !isdirectory(tempparent)
    call mkdir(tempparent, 'p')
  elseif isdirectory(tempfile)
    if commit =~# '^\d$' && has('patch-7.4.1107')
      call delete(tempfile, 'rf')
    else
      return ''
    endif
  endif
  if commit =~# '^\d$' || !filereadable(tempfile)
    let rev = s:DirRev(a:url)[1]
    let blob_or_filters = fugitive#GitVersion(2, 11) ? '--filters' : 'blob'
    let exec_error = s:StdoutToFile(tempfile, [dir, 'cat-file', blob_or_filters, rev])[1]
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
    let hash = s:ChompDefault('', [dir, '--literal-pathspecs', 'hash-object', '-w', FugitiveGitPath(temp)])
    let mode = entry[1] !=# '000000' ? entry[1] : '100644'
    if hash =~# '^\x\{40,\}$'
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
  let [repo, commit, glob] = s:DirCommitFile(a:url)
  let dirglob = s:GitDir(repo)
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
    let prepend = s:DirUrlPrefix(dir) . substitute(commit, '^:', '', '') . '/'
    call sort(files)
    call map(files, 's:VimSlash(s:PathJoin(prepend, v:val . append))')
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

" Section: Completion

function! s:FilterEscape(items, ...) abort
  let items = copy(a:items)
  call map(items, 'fnameescape(v:val)')
  if !a:0 || type(a:1) != type('')
    let match = ''
  else
    let match = substitute(a:1, '^[+>]\|\\\@<![' . substitute(s:fnameescape, '\\', '', '') . ']', '\\&', 'g')
  endif
  let cmp = s:FileIgnoreCase(1) ? '==?' : '==#'
  return filter(items, 'strpart(v:val, 0, strlen(match)) ' . cmp . ' match')
endfunction

function! s:GlobComplete(lead, pattern, ...) abort
  if a:lead ==# '/'
    return []
  else
    let results = glob(substitute(a:lead . a:pattern, '[\{}]', '\\&', 'g'), a:0 ? a:1 : 0, 1)
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
  if base =~# '^\.git/' && len(dir)
    let pattern = s:gsub(base[5:-1], '/', '*&').'*'
    let fdir = fugitive#Find('.git/', dir)
    let matches = s:GlobComplete(fdir, pattern)
    let cdir = fugitive#Find('.git/refs', dir)[0 : -5]
    if len(cdir) && s:cpath(fdir) !=# s:cpath(cdir)
      call extend(matches, s:GlobComplete(cdir, pattern))
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
  let base = s:Expand(a:base)

  if a:base =~# '^!\d*$' && base !~# '^!'
    return [base]
  elseif base =~# '^\.\=/\|^:(' || base !~# ':'
    let results = []
    if base =~# '^refs/'
      let cdir = fugitive#Find('.git/refs', dir)[0 : -5]
      let results += map(s:GlobComplete(cdir, base . '*'), 's:Slash(v:val)')
      call map(results, 's:fnameescape(v:val)')
    elseif base !~# '^\.\=/\|^:('
      let heads = s:CompleteHeads(dir)
      if filereadable(fugitive#Find('.git/refs/stash', dir))
        let heads += ["stash"]
        let heads += sort(s:LinesError(["stash","list","--pretty=format:%gd"], dir)[0])
      endif
      let results += s:FilterEscape(heads, fnameescape(base))
    endif
    let results += a:0 == 1 || a:0 >= 3 ? fugitive#CompletePath(base, 0, '', dir, a:0 >= 4 ? a:4 : tree) : fugitive#CompletePath(base)
    return results

  elseif base =~# '^:'
    let entries = s:LinesError(['ls-files','--stage'], dir)[0]
    if base =~# ':\./'
      call map(entries, 'substitute(v:val, "\\M\t\\zs" . subdir, "./", "")')
    endif
    call map(entries,'s:sub(v:val,".*(\\d)\\t(.*)",":\\1:\\2")')
    if base !~# '^:[0-3]\%(:\|$\)'
      call filter(entries,'v:val[1] == "0"')
      call map(entries,'v:val[2:-1]')
    endif

  else
    let parent = matchstr(base, '.*[:/]')
    let entries = s:LinesError(['ls-tree', substitute(parent,  ':\zs\./', '\=subdir', '')], dir)[0]
    call map(entries,'s:sub(v:val,"^04.*\\zs$","/")')
    call map(entries,'parent.s:sub(v:val,".*\t","")')
  endif
  return s:FilterEscape(entries, fnameescape(base))
endfunction

function! s:CompleteSub(subcommand, A, L, P, ...) abort
  let pre = strpart(a:L, 0, a:P)
  if pre =~# ' -- '
    return fugitive#CompletePath(a:A)
  elseif a:A =~# '^-' || a:A is# 0
    return s:FilterEscape(split(s:ChompDefault('', [a:subcommand, '--git-completion-helper']), ' '), a:A)
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

augroup fugitive_dummy_events
  autocmd!
  autocmd User Fugitive* "
  autocmd BufWritePre,FileWritePre,FileWritePost * "
  autocmd BufNewFile * "
  autocmd QuickfixCmdPre,QuickfixCmdPost * "
augroup END

function! s:ReplaceCmd(cmd) abort
  let temp = tempname()
  let [err, exec_error] = s:StdoutToFile(temp, a:cmd)
  if exec_error
    throw 'fugitive: ' . (len(err) ? substitute(err, "\n$", '', '') : 'unknown error running ' . string(a:cmd))
  endif
  setlocal noswapfile
  silent exe 'lockmarks keepalt noautocmd 0read ++edit' s:fnameescape(temp)
  if &foldenable && foldlevel('$') > 0
    set nofoldenable
    silent keepjumps $delete _
    set foldenable
  else
    silent keepjumps $delete _
  endif
  call delete(temp)
  if s:cpath(s:AbsoluteVimPath(bufnr('$')), temp)
    silent! noautocmd execute bufnr('$') . 'bwipeout'
  endif
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

function! s:QueryLog(refspec, limit) abort
  let [log, exec_error] = s:LinesError(['log', '-n', '' . a:limit, '--pretty=format:%h%x09%s'] + a:refspec + ['--'])
  call map(log, 'split(v:val, "\t", 1)')
  call map(log, '{"type": "Log", "commit": v:val[0], "subject": join(v:val[1 : -1], "\t")}')
  let result = {'error': exec_error ? 1 : 0, 'overflow': 0, 'entries': log}
  if len(log) == a:limit
    call remove(log, -1)
    let result.overflow = 1
  endif
  return result
endfunction

function! s:QueryLogRange(old, new) abort
  if empty(a:old) || empty(a:new)
    return {'error': 2, 'overflow': 0, 'entries': []}
  endif
  return s:QueryLog([a:old . '..' . a:new], 256)
endfunction

function! s:AddLogSection(label, log) abort
  if empty(a:log.entries)
    return
  endif
  let label = a:label . ' (' . len(a:log.entries) . (a:log.overflow ? '+' : '') . ')'
  call append(line('$'), ['', label] + s:Format(a:log.entries))
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

function! fugitive#BufReadStatus(...) abort
  let amatch = s:Slash(expand('%:p'))
  unlet! b:fugitive_reltime b:fugitive_type
  try
    doautocmd BufReadPre
    let config = fugitive#Config()

    let cmd = [s:Dir()]
    setlocal noreadonly modifiable nomodeline buftype=nowrite
    if amatch !~# '^fugitive:' && s:cpath($GIT_INDEX_FILE !=# '' ? resolve(s:GitIndexFileEnv()) : fugitive#Find('.git/index')) !=# s:cpath(amatch)
      let cmd += [{'env': {'GIT_INDEX_FILE': FugitiveGitPath(amatch)}}]
    endif

    if fugitive#GitVersion(2, 15)
      call add(cmd, '--no-optional-locks')
    endif

    let b:fugitive_files = {'Staged': {}, 'Unstaged': {}}
    let [staged, unstaged, untracked] = [[], [], []]
    let props = {}

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
          call add(untracked, {'type': 'File', 'status': line[0], 'filename': line[2:-1], 'relative': [line[2:-1]]})
        elseif line[0] !=# '#'
          if line[0] ==# 'u'
            let file = matchstr(line, '^.\{37\} \x\{40,\} \x\{40,\} \x\{40,\} \zs.*$')
          else
            let file = matchstr(line, '^.\{30\} \x\{40,\} \x\{40,\} \zs.*$')
          endif
          if line[0] ==# '2'
            let i += 1
            let file = matchstr(file, ' \zs.*')
            let relative = [file, output[i]]
          else
            let relative = [file]
          endif
          let filename = join(reverse(copy(relative)), ' -> ')
          let sub = matchstr(line, '^[12u] .. \zs....')
          if line[2] !=# '.'
            call add(staged, {'type': 'File', 'status': line[2], 'filename': filename, 'relative': relative, 'submodule': sub})
          endif
          if line[3] !=# '.'
            let sub = matchstr(line, '^[12u] .. \zs....')
            call add(unstaged, {'type': 'File', 'status': get({'C':'M','M':'?','U':'?'}, matchstr(sub, 'S\.*\zs[CMU]'), line[3]), 'filename': file, 'relative': [file], 'submodule': sub})
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
        let head = split(head, '\.\.\.')[0]
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
        let i += 1
        if line[2] !=# ' '
          continue
        endif
        if line[0:1] =~# '[RC]'
          let relative = [file, output[i]]
          let i += 1
        else
          let relative = [file]
        endif
        let filename = join(reverse(copy(relative)), ' -> ')
        if line[0] !~# '[ ?!#]'
          call add(staged, {'type': 'File', 'status': line[0], 'filename': filename, 'relative': relative, 'submodule': ''})
        endif
        if line[0:1] ==# '??'
          call add(untracked, {'type': 'File', 'status': line[1], 'filename': filename, 'relative': relative})
        elseif line[1] !~# '[ !#]'
          call add(unstaged, {'type': 'File', 'status': line[1], 'filename': file, 'relative': [file], 'submodule': ''})
        endif
      endwhile
    endif

    let diff = {'Staged': {'stdout': ['']}, 'Unstaged': {'stdout': ['']}}
    if len(staged)
      let diff['Staged'] =
          \ fugitive#Execute(['-c', 'diff.suppressBlankEmpty=false', '-c', 'core.quotePath=false', 'diff', '--color=never', '--no-ext-diff', '--no-prefix', '--cached'], function('len'))
    endif
    if len(unstaged)
      let diff['Unstaged'] =
          \ fugitive#Execute(['-c', 'diff.suppressBlankEmpty=false', '-c', 'core.quotePath=false', 'diff', '--color=never', '--no-ext-diff', '--no-prefix'], function('len'))
    endif

    for dict in staged
      let b:fugitive_files['Staged'][dict.filename] = dict
    endfor
    for dict in unstaged
      let b:fugitive_files['Unstaged'][dict.filename] = dict
    endfor

    let fetch_remote = config.Get('branch.' . branch . '.remote', 'origin')
    let push_remote = config.Get('branch.' . branch . '.pushRemote',
          \ config.Get('remote.pushDefault', fetch_remote))
    if fetch_remote !=# '.' && empty(config.Get('remote.' . fetch_remote . '.fetch'))
      let fetch_remote = ''
    endif
    if push_remote !=# '.' && empty(config.Get('remote.' . push_remote . '.push', config.Get('remote.' . push_remote . '.fetch')))
      let push_remote = ''
    endif

    let pull_type = 'Pull'
    if empty(fetch_remote) || empty(branch)
      let pull_ref = ''
    elseif fetch_remote ==# '.'
      let pull_ref = config.Get('branch.' . branch . '.merge', 'refs/heads/' . branch)
    else
      let pull_ref = substitute(config.Get('branch.' . branch . '.merge', 'refs/heads/' . branch), '^refs/heads/', 'refs/remotes/' . fetch_remote . '/', '')
    endif
    if len(pull_ref)
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

    let push_default = FugitiveConfigGet('push.default', config)
    if empty(push_default)
      let push_default = fugitive#GitVersion(2) ? 'simple' : 'matching'
    endif
    if push_default ==# 'upstream'
      let push_ref = pull_ref
    elseif empty(push_remote) || empty(branch)
      let push_ref = ''
    elseif push_remote ==# '.'
      let push_ref = 'refs/heads/' . branch
    else
      let push_ref = 'refs/remotes/' . push_remote . '/' . branch
    endif

    let push_short = substitute(push_ref, '^refs/\w\+/', '', '')
    let pull_short = substitute(pull_ref, '^refs/\w\+/', '', '')

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

    let b:fugitive_diff = diff
    if get(a:, 1, v:cmdbang)
      unlet! b:fugitive_expanded
    endif
    let expanded = get(b:, 'fugitive_expanded', {'Staged': {}, 'Unstaged': {}})
    let b:fugitive_expanded = {'Staged': {}, 'Unstaged': {}}

    silent keepjumps %delete_

    call s:AddHeader('Head', head)
    call s:AddHeader(pull_type, pull_short)
    if push_ref !=# pull_ref
      call s:AddHeader('Push', push_short)
    endif
    if empty(s:Tree())
      if get(fugitive#ConfigGetAll('core.bare', config), 0, '') !~# '^\%(false\|no|off\|0\|\)$'
        call s:AddHeader('Bare', 'yes')
      else
        call s:AddHeader('Error', s:worktree_error)
      endif
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

    let unique_push_ref = push_ref ==# pull_ref ? '' : push_ref
    let unpushed_push = s:QueryLogRange(unique_push_ref, head)
    if get(props, 'branch.ab') =~# '^+0 '
      let unpushed_pull = {'error': 0, 'overflow': 0, 'entries': []}
    else
      let unpushed_pull = s:QueryLogRange(pull_ref, head)
    endif
    " If the push ref is defined but nowhere to be found at the remote,
    " pretend it's the same as the pull ref
    if unpushed_push.error == 1
      let unpushed_push = unpushed_pull
    endif
    call s:AddLogSection('Unpushed to ' . push_short, unpushed_push)
    call s:AddLogSection('Unpushed to ' . pull_short, unpushed_pull)
    if unpushed_push.error && unpushed_pull.error && empty(rebasing) &&
          \ !empty(push_remote . fetch_remote)
      call s:AddLogSection('Unpushed to *', s:QueryLog([head, '--not', '--remotes'], 256))
    endif
    call s:AddLogSection('Unpulled from ' . push_short, s:QueryLogRange(head, unique_push_ref))
    if len(pull_ref) && get(props, 'branch.ab') !~# ' -0$'
      call s:AddLogSection('Unpulled from ' . pull_short, s:QueryLogRange(head, pull_ref))
    endif

    setlocal nomodified readonly noswapfile
    doautocmd BufReadPost
    setlocal nomodifiable
    if &bufhidden ==# ''
      setlocal bufhidden=delete
    endif
    if !exists('b:dispatch')
      let b:dispatch = ':Git fetch --all'
    endif
    call fugitive#MapJumps()
    call s:Map('n', '-', ":<C-U>execute <SID>Do('Toggle',0)<CR>", '<silent>')
    call s:Map('x', '-', ":<C-U>execute <SID>Do('Toggle',1)<CR>", '<silent>')
    call s:Map('n', 's', ":<C-U>execute <SID>Do('Stage',0)<CR>", '<silent>')
    call s:Map('x', 's', ":<C-U>execute <SID>Do('Stage',1)<CR>", '<silent>')
    call s:Map('n', 'u', ":<C-U>execute <SID>Do('Unstage',0)<CR>", '<silent>')
    call s:Map('x', 'u', ":<C-U>execute <SID>Do('Unstage',1)<CR>", '<silent>')
    call s:Map('n', 'U', ":<C-U>Git reset -q<CR>", '<silent>')
    call s:MapMotion('gu', "exe <SID>StageJump(v:count, 'Untracked', 'Unstaged')")
    call s:MapMotion('gU', "exe <SID>StageJump(v:count, 'Unstaged', 'Untracked')")
    call s:MapMotion('gs', "exe <SID>StageJump(v:count, 'Staged')")
    call s:MapMotion('gp', "exe <SID>StageJump(v:count, 'Unpushed')")
    call s:MapMotion('gP', "exe <SID>StageJump(v:count, 'Unpulled')")
    call s:MapMotion('gr', "exe <SID>StageJump(v:count, 'Rebasing')")
    call s:Map('n', 'C', ":echoerr 'fugitive: C has been removed in favor of cc'<CR>", '<silent><unique>')
    call s:Map('n', 'a', ":<C-U>execute <SID>Do('Toggle',0)<CR>", '<silent>')
    call s:Map('n', 'i', ":<C-U>execute <SID>NextExpandedHunk(v:count1)<CR>", '<silent>')
    call s:Map('n', "=", ":<C-U>execute <SID>StageInline('toggle',line('.'),v:count)<CR>", '<silent>')
    call s:Map('n', "<", ":<C-U>execute <SID>StageInline('hide',  line('.'),v:count)<CR>", '<silent>')
    call s:Map('n', ">", ":<C-U>execute <SID>StageInline('show',  line('.'),v:count)<CR>", '<silent>')
    call s:Map('x', "=", ":<C-U>execute <SID>StageInline('toggle',line(\"'<\"),line(\"'>\")-line(\"'<\")+1)<CR>", '<silent>')
    call s:Map('x', "<", ":<C-U>execute <SID>StageInline('hide',  line(\"'<\"),line(\"'>\")-line(\"'<\")+1)<CR>", '<silent>')
    call s:Map('x', ">", ":<C-U>execute <SID>StageInline('show',  line(\"'<\"),line(\"'>\")-line(\"'<\")+1)<CR>", '<silent>')
    call s:Map('n', 'D', ":echoerr 'fugitive: D has been removed in favor of dd'<CR>", '<silent><unique>')
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
    call s:Map('n', 'gq', ":<C-U>if bufnr('$') == 1<Bar>quit<Bar>else<Bar>bdelete<Bar>endif<CR>", '<silent>')
    call s:Map('n', 'R', ":echohl WarningMsg<Bar>echo 'Reloading is automatic.  Use :e to force'<Bar>echohl NONE<CR>", '<silent>')
    call s:Map('n', 'g<Bar>', ":<C-U>echoerr 'Changed to X'<CR>", '<silent><unique>')
    call s:Map('x', 'g<Bar>', ":<C-U>echoerr 'Changed to X'<CR>", '<silent><unique>')
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
    return s:DoAutocmd('User FugitiveIndex')
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  finally
    let b:fugitive_type = 'index'
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
    let cmd = [dir, 'log', '--pretty=format:%B', '-1', rev, '--']
  elseif rev ==# ':'
    let cmd = [dir, 'status', '--short']
  else
    let cmd = [dir, 'cat-file', '-p', rev, '--']
  endif
  let temp = tempname()
  let [err, exec_error] = s:StdoutToFile(temp, cmd)
  if exec_error
    call delete(temp)
    return 'noautocmd ' . line . 'read ' . s:fnameescape(amatch)
  else
    return 'silent keepalt ' . line . 'read ' . s:fnameescape(temp) . '|call delete(' . string(temp) . ')'
  endif
endfunction

function! fugitive#FileWriteCmd(...) abort
  let temp = tempname()
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
    silent execute "noautocmd keepalt '[,']write ".temp
    let hash = s:TreeChomp([dir, '--literal-pathspecs', 'hash-object', '-w', '--', FugitiveGitPath(temp)])
    let old_mode = matchstr(s:ChompDefault('', ['ls-files', '--stage', '.' . file], dir), '^\d\+')
    if empty(old_mode)
      let old_mode = executable(s:Tree(dir) . file) ? '100755' : '100644'
    endif
    let error = s:UpdateIndex(dir, [old_mode, hash, commit, file[1:-1]])
    if empty(error)
      setlocal nomodified
      if exists('#' . autype . 'WritePost')
        execute s:DoAutocmd(autype . 'WritePost ' . s:fnameescape(amatch))
      endif
      exe s:DoAutocmdChanged(dir)
      return ''
    else
      return 'echoerr '.string('fugitive: '.error)
    endif
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  finally
    call delete(temp)
  endtry
endfunction

function! fugitive#BufReadCmd(...) abort
  let amatch = a:0 ? a:1 : expand('<amatch>')
  try
    let [dir, rev] = s:DirRev(amatch)
    if empty(dir)
      return 'echo "Invalid Fugitive URL"'
    endif
    call s:InitializeBuffer(dir)
    if rev ==# ':'
      return fugitive#BufReadStatus(v:cmdbang)
    elseif rev =~# '^:\d$'
      let b:fugitive_type = 'stage'
    else
      let r = fugitive#Execute([dir, 'cat-file', '-t', rev])
      let b:fugitive_type = get(r.stdout, 0, '')
      if r.exit_status && rev =~# '^:0'
        let r = fugitive#Execute([dir, 'write-tree', '--prefix=' . rev[3:-1]])
        let sha = get(r.stdout, 0, '')
        let b:fugitive_type = 'tree'
      endif
      if r.exit_status
        let error = substitute(join(r.stderr, "\n"), "\n*$", '', '')
        unlet b:fugitive_type
        setlocal noswapfile
        if empty(&bufhidden)
          setlocal bufhidden=delete
        endif
        if rev =~# '^:\d:'
          let &l:readonly = !filewritable(fugitive#Find('.git/index', dir))
          return 'doautocmd BufNewFile'
        else
          setlocal readonly nomodifiable
          return 'doautocmd BufNewFile|echo ' . string(error)
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

    let events = ['User FugitiveObject', 'User Fugitive' . substitute(b:fugitive_type, '^\l', '\u&', '')]

    try
      if b:fugitive_type !=# 'blob'
        setlocal foldmarker=<<<<<<<<,>>>>>>>>
      endif
      exe s:DoAutocmd('BufReadPre')
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
          call s:ReplaceCmd([dir, '-c', 'diff.noprefix=false', '-c', 'log.showRoot=false', 'show', '--no-color', '-m', '--first-parent', '--pretty=format:tree%x20%T%nparent%x20%P%nauthor%x20%an%x20<%ae>%x20%ad%ncommitter%x20%cn%x20<%ce>%x20%cd%nencoding%x20%e%n%n%s%n%n%b', rev])
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
        let blob_or_filters = rev =~# ':' && fugitive#GitVersion(2, 11) ? '--filters' : 'blob'
        call s:ReplaceCmd([dir, 'cat-file', blob_or_filters, rev])
      endif
    finally
      keepjumps call setpos('.',pos)
      setlocal nomodified noswapfile
      let modifiable = rev =~# '^:.:' && b:fugitive_type !=# 'tree'
      if modifiable
        let events = ['User FugitiveStageBlob']
      endif
      let &l:readonly = !modifiable || !filewritable(fugitive#Find('.git/index', dir))
      if empty(&bufhidden)
        setlocal bufhidden=delete
      endif
      let &l:modifiable = modifiable
      call fugitive#MapJumps()
      if b:fugitive_type !=# 'blob'
        call s:Map('n', 'a', ":<C-U>let b:fugitive_display_format += v:count1<Bar>exe fugitive#BufReadCmd(@%)<CR>", '<silent>')
        call s:Map('n', 'i', ":<C-U>let b:fugitive_display_format -= v:count1<Bar>exe fugitive#BufReadCmd(@%)<CR>", '<silent>')
        setlocal filetype=git
      endif
    endtry

    setlocal modifiable

    return s:DoAutocmd('BufReadPost') .
          \ (modifiable ? '' : '|setl nomodifiable') . '|' .
          \ call('s:DoAutocmd', events)
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
  return get(s:temp_files, s:cpath(s:AbsoluteVimPath(a:0 ? a:1 : -1)), {})
endfunction

function! fugitive#Result(...) abort
  if !a:0 && exists('g:fugitive_event')
    return get(g:, 'fugitive_result', {})
  elseif !a:0 || type(a:1) == type('') && a:1 =~# '^-\=$'
    return get(g:, '_fugitive_last_job', {})
  elseif type(a:1) == type(0)
    return s:TempState(a:1)
  elseif type(a:1) == type('')
    return s:TempState(a:1)
  elseif type(a:1) == type({}) && has_key(a:1, 'file')
    return s:TempState(a:1.file)
  else
    return {}
  endif
endfunction

function! s:TempDotMap() abort
  let cfile = s:cfile()
  if empty(cfile)
    if getline('.') =~# '^[*+] \+\f' && col('.') < 2
      return matchstr(getline('.'), '^. \+\zs\f\+')
    else
      return expand('<cfile>')
    endif
  endif
  let name = fugitive#Find(cfile[0])
  let [dir, commit, file] = s:DirCommitFile(name)
  if len(commit) && empty(file)
    return commit
  elseif s:cpath(s:Tree(), getcwd())
    return fugitive#Path(name, "./")
  else
    return fugitive#Real(name)
  endif
endfunction

function! s:TempReadPre(file) abort
  let key = s:cpath(s:AbsoluteVimPath(a:file))
  if has_key(s:temp_files, key)
    let dict = s:temp_files[key]
    setlocal nomodeline
    if empty(&bufhidden)
      setlocal bufhidden=delete
    endif
    setlocal buftype=nowrite
    setlocal nomodifiable
    call s:InitializeBuffer(dict)
    if len(dict.git_dir)
      call extend(b:, {'fugitive_type': 'temp'}, 'keep')
    endif
  endif
  return ''
endfunction

function! s:TempReadPost(file) abort
  let key = s:cpath(s:AbsoluteVimPath(a:file))
  if has_key(s:temp_files, key)
    let dict = s:temp_files[key]
    if !has_key(dict, 'job')
      setlocal nobuflisted
    endif
    if get(dict, 'filetype', '') ==# 'git'
      call fugitive#MapJumps()
      call s:Map('n', '.', ":<C-U> <C-R>=<SID>fnameescape(<SID>TempDotMap())<CR><Home>")
      call s:Map('x', '.', ":<C-U> <C-R>=<SID>fnameescape(<SID>TempDotMap())<CR><Home>")
    endif
    if has_key(dict, 'filetype')
      if dict.filetype ==# 'man' && has('nvim')
        let b:man_sect = matchstr(getline(1), '^\w\+(\zs\d\+\ze)')
      endif
      if !get(g:, 'did_load_ftplugin') && dict.filetype ==# 'fugitiveblame'
        call s:BlameMaps(0)
      endif
      let &l:filetype = dict.filetype
    endif
    setlocal foldmarker=<<<<<<<<,>>>>>>>>
    if !&modifiable
      call s:Map('n', 'gq', ":<C-U>bdelete<CR>", '<silent> <unique>')
    endif
  endif
  return s:DoAutocmd('User FugitivePager')
endfunction

function! s:TempDelete(file) abort
  let key = s:cpath(s:AbsoluteVimPath(a:file))
  if has_key(s:temp_files, key) && !has_key(s:temp_files[key], 'job') && key !=# s:cpath(get(get(g:, '_fugitive_last_job', {}), 'file', ''))
    call delete(a:file)
    call remove(s:temp_files, key)
  endif
  return ''
endfunction

function! s:OriginBufnr(...) abort
  let state = s:TempState(a:0 ? a:1 : bufnr(''))
  return get(state, 'origin_bufnr', -1)
endfunction

augroup fugitive_temp
  autocmd!
  autocmd BufReadPre  * exe s:TempReadPre( +expand('<abuf>'))
  autocmd BufReadPost * exe s:TempReadPost(+expand('<abuf>'))
  autocmd BufWipeout  * exe s:TempDelete(  +expand('<abuf>'))
augroup END

" Section: :Git

function! s:AskPassArgs(dir) abort
  if (len($DISPLAY) || len($TERM_PROGRAM) || has('gui_running')) &&
        \ empty($GIT_ASKPASS) && empty($SSH_ASKPASS) && empty(fugitive#ConfigGetAll('core.askpass', a:dir))
    if s:executable(s:VimExecPath() . '/git-gui--askpass')
      return ['-c', 'core.askPass=' . s:ExecPath()[0] . '/git-gui--askpass']
    elseif s:executable('ssh-askpass')
      return ['-c', 'core.askPass=ssh-askpass']
    endif
  endif
  return []
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
  call fugitive#DidChange(a:state)
endfunction

function! s:RunEdit(state, tmp, job) abort
  if get(a:state, 'request', '') !=# 'edit'
    return 0
  endif
  call remove(a:state, 'request')
  let sentinel = a:state.file . '.edit'
  let file = FugitiveVimPath(readfile(sentinel, '', 1)[0])
  try
    if !&equalalways && a:state.mods !~# '\<tab\>' && 3 > (a:state.mods =~# '\<vert' ? winwidth(0) : winheight(0))
      let noequalalways = 1
      setglobal equalalways
    endif
    let mods = s:Mods(a:state.mods, 'SpanOrigin')
    exe substitute(mods, '\<tab\>', '-tab', 'g') 'keepalt split' s:fnameescape(file)
  finally
    if exists('l:noequalalways')
      setglobal noequalalways
    endif
  endtry
  set bufhidden=wipe
  call s:InitializeBuffer(a:state)
  let bufnr = bufnr('')
  let s:edit_jobs[bufnr] = [a:state, a:tmp, a:job, sentinel]
  call fugitive#DidChange(a:state.git_dir)
  if bufnr == bufnr('') && !exists('g:fugitive_event')
    try
      let g:fugitive_event = a:state.git_dir
      let g:fugitive_result = a:state
      exe s:DoAutocmd('User FugitiveEditor')
    finally
      unlet! g:fugitive_event g:fugitive_result
    endtry
  endif
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
    if !a:state.pager && getwinvar(bufwinid(a:state.capture_bufnr), '&previewwindow')
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
    let running = ch_status(a:job) !~# '^closed$\|^fail$' || job_status(a:job) ==# 'run'
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
    if a:tmp.no_more && &more
      let more = &more
      let &more = 0
    endif
    while get(a:state, 'request', '') !=# 'edit' && s:RunTick(a:job)
      call s:RunEcho(a:tmp)
      if !get(a:tmp, 'closed_in')
        let peek = getchar(1)
        if peek != 0 && !(has('win32') && peek == 128)
          let c = getchar()
          let c = type(c) == type(0) ? nr2char(c) : c
          if c ==# "\<C-D>" || c ==# "\<Esc>"
            let a:tmp.closed_in = 1
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
              exe (&splitbelow ? 'botright' : 'topleft') 'silent pedit ++ff=unix' s:fnameescape(a:state.file)
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
    if exists('l:more')
      let &more = more
    endif
    if !exists('finished')
      try
        if a:state.pty && !get(a:tmp, 'closed_in')
          call s:RunSend(a:job, "\<C-C>")
        elseif type(a:job) == type(0)
          call jobstop(a:job)
        else
          call job_stop(a:job)
        endif
      catch /.*/
      endtry
    elseif finished
      call fugitive#DidChange(a:state)
    endif
  endtry
  return ''
endfunction

if !exists('s:resume_queue')
  let s:resume_queue = []
endif
function! fugitive#Resume() abort
  while len(s:resume_queue)
    let enqueued = remove(s:resume_queue, 0)
    if enqueued[2] isnot# ''
      try
        call call('s:RunWait', enqueued)
      endtry
    endif
  endwhile
endfunction

function! s:RunBufDelete(bufnr) abort
  let state = s:TempState(+a:bufnr)
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
    call feedkeys("\<C-\>\<C-N>:redraw!|call delete(" . string(s:resume_queue[-1][0].file . '.edit') .
          \ ")|call fugitive#Resume()|checktime\r", 'n')
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

function! fugitive#CanPty() abort
  return get(g:, 'fugitive_pty_debug_override',
        \ has('unix') && !has('win32unix') && (has('patch-8.0.0744') || has('nvim')) && fugitive#GitVersion() !~# '\.windows\>')
endfunction

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
function! fugitive#Command(line1, line2, range, bang, mods, arg, ...) abort
  exe s:VersionCheck()
  let dir = call('s:Dir', a:000)
  if len(dir)
    exe s:DirCheck(dir)
  endif
  let config = copy(fugitive#Config(dir))
  let curwin = a:arg =~# '^++curwin\>' || !a:line2
  let [args, after] = s:SplitExpandChain(substitute(a:arg, '^++curwin\>\s*', '', ''), s:Tree(dir))
  let flags = []
  let pager = -1
  let explicit_pathspec_option = 0
  while len(args)
    if args[0] ==# '-c' && len(args) > 1
      call extend(flags, remove(args, 0, 1))
    elseif args[0] =~# '^-p$\|^--paginate$'
      let pager = 2
      call remove(args, 0)
    elseif args[0] =~# '^-P$\|^--no-pager$'
      let pager = 0
      call remove(args, 0)
    elseif args[0] =~# '^--\%([[:lower:]-]\+-pathspecs\)$'
      let explicit_pathspec_option = 1
      call add(flags, remove(args, 0))
    elseif args[0] =~# '^\%(--no-optional-locks\)$'
      call add(flags, remove(args, 0))
    elseif args[0] =~# '^-C$\|^--\%(exec-path=\|git-dir=\|work-tree=\|bare$\)'
      return 'echoerr ' . string('fugitive: ' . args[0] . ' is not supported')
    else
      break
    endif
  endwhile
  if !explicit_pathspec_option
    call insert(flags, '--no-literal-pathspecs')
  endif
  let no_pager = pager is# 0
  if no_pager
    call add(flags, '--no-pager')
  endif
  let env = {}
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
  let options = {'git': s:UserCommandList(), 'git_dir': s:GitDir(dir), 'flags': flags, 'curwin': curwin}
  if empty(args) && pager is# -1
    let cmd = s:StatusCommand(a:line1, a:line2, a:range, curwin ? 0 : a:line2, a:bang, a:mods, '', '', [], options)
    return (empty(cmd) ? 'exe' : cmd) . after
  endif
  let alias = FugitiveConfigGet('alias.' . get(args, 0, ''), config)
  if get(args, 1, '') !=# '--help' && alias !~# '^$\|^!\|[\"'']' && !filereadable(s:VimExecPath() . '/git-' . args[0])
        \ && !(has('win32') && filereadable(s:VimExecPath() . '/git-' . args[0] . '.exe'))
    call remove(args, 0)
    call extend(args, split(alias, '\s\+'), 'keep')
  endif
  let name = substitute(get(args, 0, ''), '\%(^\|-\)\(\l\)', '\u\1', 'g')
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
  call extend(env, get(overrides, 'env', {}))
  call s:PrepareEnv(env, dir)
  if pager is# -1
    let pager = fugitive#PagerFor(args, config)
  endif
  let wants_terminal = type(pager) ==# type('') ||
        \ (s:HasOpt(args, ['add', 'checkout', 'commit', 'reset', 'restore', 'stage', 'stash'], '-p', '--patch') ||
        \ s:HasOpt(args, ['add', 'clean', 'stage'], '-i', '--interactive')) && pager is# 0
  if wants_terminal
    let mods = substitute(s:Mods(a:mods), '\<tab\>', '-tab', 'g')
    let assign = len(dir) ? "|call FugitiveDetect({'git_dir':" . string(options.git_dir) . '})' : ''
    let argv = s:UserCommandList(options) + args
    let term_opts = len(env) ? {'env': env} : {}
    if has('nvim')
      call fugitive#Autowrite()
      return mods . (curwin ? 'enew' : 'new') . '|call termopen(' . string(argv) . ', ' . string(term_opts) . ')' . assign . '|startinsert' . after
    elseif exists('*term_start')
      call fugitive#Autowrite()
      if curwin
        let term_opts.curwin = 1
      endif
      return mods . 'call term_start(' . string(argv) . ', ' . string(term_opts) . ')' . assign . after
    endif
  endif
  let state = {
        \ 'git': options.git,
        \ 'flags': flags,
        \ 'args': args,
        \ 'git_dir': options.git_dir,
        \ 'cwd': s:UserCommandCwd(dir),
        \ 'filetype': 'git',
        \ 'mods': s:Mods(a:mods),
        \ 'file': s:Resolve(tempname())}
  let allow_pty = 1
  let after_edit = ''
  let stream = 0
  if a:bang && pager isnot# 2
    let state.pager = pager
    let pager = 1
    let stream = exists('*setbufline')
    let do_edit = substitute(s:Mods(a:mods, 'Edge'), '\<tab\>', '-tab', 'g') . 'pedit!'
  elseif pager
    let allow_pty = get(args, 0, '') is# 'shortlog'
    if pager is# 2 && a:bang && a:line2 >= 0
      let [do_edit, after_edit] = s:ReadPrepare(a:line1, a:line2, a:range, a:mods)
    elseif pager is# 2 && a:bang
      let do_edit = s:Mods(a:mods, 'SpanOrigin') . 'pedit'
    elseif !curwin
      let do_edit = s:Mods(a:mods, 'SpanOrigin') . 'split'
    else
      let do_edit = s:Mods(a:mods) . 'edit'
      call s:BlurStatus()
    endif
    call extend(env, {'COLUMNS': '' . get(g:, 'fugitive_columns', 80)}, 'keep')
  endif
  if s:run_jobs
    call extend(env, {'COLUMNS': '' . (&columns - 1)}, 'keep')
    let state.pty = allow_pty && fugitive#CanPty()
    if !state.pty
      let args = s:AskPassArgs(dir) + args
    endif
    let tmp = {
          \ 'no_more': no_pager || get(overrides, 'no_more'),
          \ 'line_count': 0,
          \ 'err': '',
          \ 'out': '',
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
          \ 'GIT_PAGER': 'cat',
          \ 'PAGER': 'cat'}, 'keep')
    if len($GPG_TTY) && !has_key(env, 'GPG_TTY')
      let env.GPG_TTY = ''
      let did_override_gpg_tty = 1
    endif
    if stream
      call writefile(['fugitive: aborting edit due to background operation.'], state.file . '.exit')
    elseif pager
      call writefile(['fugitive: aborting edit due to use of pager.'], state.file . '.exit')
      let after = '|' . do_edit . ' ' . s:fnameescape(state.file) . after_edit . after
    else
      let env.GIT_MERGE_AUTOEDIT = '1'
      let tmp.echo = ''
    endif
    let args = s:disable_colors + flags + ['-c', 'advice.waitingForEditor=false'] + args
    let argv = s:UserCommandList({'git': options.git, 'git_dir': options.git_dir}) + args
    let [argv, jobopts] = s:JobOpts(argv, env)
    call fugitive#Autowrite()
    call writefile([], state.file, 'b')
    call s:RunSave(state)
    if has_key(tmp, 'echo')
      echo ""
    endif
    if exists('*ch_close_in')
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
    if pager
      let tmp.closed_in = 1
      call s:RunCloseIn(job)
    endif
    if stream
      exe 'silent' do_edit '++ff=unix' s:fnameescape(state.file)
      let state.capture_bufnr = bufnr(state.file)
      call setbufvar(state.capture_bufnr, '&modified', 1)
      return (after_edit . after)[1:-1]
    endif
    call add(s:resume_queue, [state, tmp, job])
    return 'call fugitive#Resume()|checktime' . after
  elseif pager
    let pre = s:BuildEnvPrefix(env)
    try
      if exists('+guioptions') && &guioptions =~# '!'
        let guioptions = &guioptions
        set guioptions-=!
      endif
      silent! execute '!' . escape(pre . s:shellesc(s:UserCommandList(options) + s:disable_colors + flags + ['--no-pager'] + args), '!#%') .
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
    return do_edit . ' ' . s:fnameescape(state.file) . after_edit .
          \ '|call fugitive#DidChange(fugitive#Result(' . string(state.file) . '))' . after
  elseif has('win32')
    return 'echoerr ' . string('fugitive: Vim 8 with job support required to use :Git on Windows')
  elseif has('gui_running')
    return 'echoerr ' . string('fugitive: Vim 8 with job support required to use :Git in GVim')
  else
    if !explicit_pathspec_option && get(options.flags, 0, '') ==# '--no-literal-pathspecs'
      call remove(options.flags, 0)
    endif
    if exists('l:did_override_gpg_tty')
      call remove(env, 'GPG_TTY')
    endif
    let cmd = s:BuildEnvPrefix(env) . s:shellesc(s:UserCommandList(options) + args)
    let after = '|call fugitive#DidChange(' . string(dir) . ')' . after
    if !wants_terminal && (no_pager || index(['add', 'clean', 'reset', 'restore', 'stage'], get(args, 0, '')) >= 0 || s:HasOpt(args, ['checkout'], '-q', '--quiet', '--no-progress'))
      let output = substitute(s:SystemError(cmd)[0], "\n$", '', '')
      if len(output)
        try
          if &more && no_pager
            let more = 1
            set nomore
          endif
          echo substitute(output, "\n$", "", "")
        finally
          if exists('l:more')
            set more
          endif
        endtry
      endif
      return 'checktime' . after
    else
      return 'exe ' . string('noautocmd !' . escape(cmd, '!#%')) . after
    endif
  endif
endfunction

let s:exec_paths = {}
function! s:ExecPath() abort
  let git = s:GitShellCmd()
  if !has_key(s:exec_paths, git)
    let path = get(s:JobExecute(s:GitCmd() + ['--exec-path'], {}, [], [], {}).stdout, 0, '')
    let s:exec_paths[git] = [path, FugitiveVimPath(path)]
  endif
  return s:exec_paths[git]
endfunction

function! s:VimExecPath() abort
  return s:ExecPath()[1]
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
  let c_exec_path = s:cpath(s:VimExecPath())
  if !has_key(s:path_subcommands, c_exec_path)
    if fugitive#GitVersion(2, 18)
      let [lines, exec_error] = s:LinesError([a:dir, '--list-cmds=list-mainporcelain,nohelpers,list-complete'])
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
  let dir = a:0 == 1 ? a:1 : a:0 >= 3 ? s:Dir(a:3) : s:Dir()
  let root = a:0 >= 4 ? a:4 : s:Tree(s:Dir())
  let pre = a:0 > 1 ? strpart(a:1, 0, a:2) : ''
  let subcmd = matchstr(pre, '\u\w*[! ] *\%(\%(++\S\+\|--\S\+-pathspecs\|-c\s\+\S\+\)\s\+\)*\zs[[:alnum:]][[:alnum:]-]*\ze ')
  if empty(subcmd) && a:lead =~# '^+'
    let results = ['++curwin']
  elseif empty(subcmd) && a:lead =~# '^-'
    let results = ['--literal-pathspecs', '--no-literal-pathspecs', '--glob-pathspecs', '--noglob-pathspecs', '--icase-pathspecs', '--no-optional-locks']
  elseif empty(subcmd)
    let results = s:CompletableSubcommands(dir)
  elseif a:0 ==# 2 && subcmd =~# '^\%(commit\|revert\|push\|fetch\|pull\|merge\|rebase\|bisect\)$'
    let cmdline = substitute(a:1, '\u\w*\([! ] *\)' . subcmd, 'G' . subcmd, '')
    let caps_subcmd = substitute(subcmd, '\%(^\|-\)\l', '\u&', 'g')
    return fugitive#{caps_subcmd}Complete(a:lead, cmdline, a:2 + len(cmdline) - len(a:1), dir, root)
  elseif pre =~# ' -- '
    return fugitive#CompletePath(a:lead, a:1, a:2, dir, root)
  elseif a:lead =~# '^-'
    let results = split(s:ChompDefault('', [dir, subcmd, '--git-completion-helper']), ' ')
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
  return (a:0 && a:1 ? 'lcd ' : 'cd ') . fnameescape(s:VimSlash(path))
endfunction

" Section: :Gstatus

function! s:StatusCommand(line1, line2, range, count, bang, mods, reg, arg, args, ...) abort
  let dir = a:0 ? s:Dir(a:1) : s:Dir()
  exe s:DirCheck(dir)
  try
    let mods = s:Mods(a:mods, 'Edge')
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
  let line = search('^' . escape(info.heading, '^$.*[]~\') . ' (\d\++\=)$', 'wn')
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
    if type(a:dir) == type({}) && has_key(a:dir, 'args') && has_key(a:dir, 'exit_status')
      let g:fugitive_result = a:dir
    endif
    exe s:DoAutocmd('User FugitiveChanged')
  finally
    unlet! g:fugitive_event g:fugitive_result
    " Force statusline reload with the buffer's Git dir
    if dir isnot# FugitiveGitDir()
      let &l:ro = &l:ro
    endif
  endtry
  return ''
endfunction

function! s:ReloadStatusBuffer(...) abort
  if get(b:, 'fugitive_type', '') !=# 'index'
    return ''
  endif
  let original_lnum = a:0 ? a:1 : line('.')
  let info = s:StageInfo(original_lnum)
  exe fugitive#BufReadStatus(0)
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
  if a:bufnr is# -2 || a:bufnr is# 0
    let s:head_cache = {}
    let s:last_time = reltime()
    return ''
  endif
  let head_file = fugitive#Find('.git/HEAD', a:bufnr)
  if !empty(head_file)
    let s:last_times[s:Tree(a:bufnr) . '/'] = reltime()
    if has_key(s:head_cache, head_file)
      call remove(s:head_cache, head_file)
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
        \ reltimestr(reltime(get(s:last_times, s:Tree() . '/', t), t)) =~# '-\|\d\{10\}\.'
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

function! fugitive#DidChange(...) abort
  call s:ExpireStatus(a:0 ? a:1 : -1)
  if a:0 > 1 ? a:2 : (!a:0 || a:1 isnot# 0)
    let t = reltime()
    let t:fugitive_reload_status = t
    for tabnr in range(1, tabpagenr('$'))
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

function! fugitive#ReloadStatus(...) abort
  return call('fugitive#DidChange', a:000)
endfunction

function! fugitive#EfmDir(...) abort
  let dir = matchstr(a:0 ? a:1 : &errorformat, '\c,%\\&\%(git\|fugitive\)_\=dir=\zs\%(\\.\|[^,]\)*')
  let dir = substitute(dir, '%%', '%', 'g')
  let dir = substitute(dir, '\\\ze[\,]', '', 'g')
  return dir
endfunction

augroup fugitive_status
  autocmd!
  autocmd BufWritePost         * call fugitive#DidChange(+expand('<abuf>'), 0)
  autocmd User FileChmodPost,FileUnlinkPost call fugitive#DidChange(+expand('<abuf>'), 0)
  autocmd ShellCmdPost,ShellFilterPost * nested call fugitive#DidChange(0)
  autocmd BufDelete * nested
        \ if getbufvar(+expand('<abuf>'), 'buftype') ==# 'terminal' |
        \   if !empty(FugitiveGitDir(+expand('<abuf>'))) |
        \     call fugitive#DidChange(+expand('<abuf>')) |
        \   else |
        \     call fugitive#DidChange(0) |
        \  endif |
        \ endif
  autocmd QuickFixCmdPost make,lmake,[cl]file,[cl]getfile nested
        \ call fugitive#DidChange(fugitive#EfmDir())
  autocmd FocusGained        *
        \ if get(g:, 'fugitive_focus_gained', !has('win32')) |
        \   call fugitive#DidChange(0) |
        \ endif
  autocmd BufEnter index,index.lock,fugitive://*//
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
    let [lnum, old_lnum, new_lnum] = s:HunkPosition(lnum)
    let offset = sigil ==# '-' ? old_lnum : new_lnum
    while getline(lnum) =~# '^[ @\+-]'
      let lnum -= 1
    endwhile
  endif
  let slnum = lnum + 1
  let heading = ''
  let index = 0
  while len(getline(slnum - 1)) && empty(heading)
    let slnum -= 1
    let heading = matchstr(getline(slnum), '^\u\l\+.\{-\}\ze (\d\++\=)$')
    if empty(heading) && getline(slnum) !~# '^[ @\+-]'
      let index += 1
    endif
  endwhile
  let text = matchstr(getline(lnum), '^[A-Z?] \zs.*')
  let file = get(get(b:fugitive_files, heading, {}), text, {})
  let relative = get(file, 'relative', len(text) ? [text] : [])
  return {'section': matchstr(heading, '^\u\l\+'),
        \ 'heading': heading,
        \ 'sigil': sigil,
        \ 'offset': offset,
        \ 'filename': text,
        \ 'relative': copy(relative),
        \ 'paths': map(copy(relative), 's:Tree() . "/" . v:val'),
        \ 'commit': matchstr(getline(lnum), '^\%(\%(\x\x\x\)\@!\l\+\s\+\)\=\zs[0-9a-f]\{4,\}\ze '),
        \ 'status': matchstr(getline(lnum), '^[A-Z?]\ze \|^\%(\x\x\x\)\@!\l\+\ze [0-9a-f]'),
        \ 'submodule': get(file, 'submodule', ''),
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
  while first <= line('$') && getline(first) =~# '^$\|^[A-Z][a-z]'
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
    let heading = matchstr(getline(slnum), '^\u\l\+.\{-\}\ze (\d\++\=)$')
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
    let heading = matchstr(line, '^\u\l\+\ze.\{-\}\ze (\d\++\=)$')
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
      let text = matchstr(line, '^[A-Z?] \zs.*')
      let file = get(get(b:fugitive_files, template.heading, {}), text, {})
      let relative = get(file, 'relative', len(text) ? [text] : [])
      call add(results, extend(deepcopy(template), {
            \ 'lnum': lnum,
            \ 'filename': text,
            \ 'relative': copy(relative),
            \ 'paths': map(copy(relative), 'root . v:val'),
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
  normal! 0
  for i in range(a:count)
    if &filetype ==# 'fugitive'
      if getline('.') =~# '^@' && getline(line('.') - 1) =~# s:file_pattern
        -
      endif
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
  normal! 0
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
  normal! 0
  for i in range(a:count)
    if !search(s:item_pattern, 'Wb') && getline('.') !~# s:item_pattern
      call search('^commit ', 'Wb')
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
  normal! 0
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
    if info.status ==# 'U'
      let diff_header = 'diff --cc ' . s:Quote(info.relative[0])
    else
      let diff_header = 'diff --git ' . s:Quote(info.relative[-1]) . ' ' . s:Quote(info.relative[0])
    endif
    let stdout = fugitive#Wait(b:fugitive_diff[info.section]).stdout
    let start = index(stdout, diff_header)
    if start == -1
      continue
    endif
    let index = start + 1
    while get(stdout, index, '@@') !~# '^@@\|^diff '
      let index += 1
    endwhile
    while get(stdout, index, '') =~# '^[@ \+-]'
      call add(diff, stdout[index])
      let index += 1
    endwhile
    if len(diff)
      setlocal modifiable noreadonly
      silent call append(lnum, diff)
      let b:fugitive_expanded[info.section][info.filename] = [start]
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
  let diff_lines = fugitive#Wait(b:fugitive_diff[info.section]).stdout
  while get(diff_lines, i, '@') !~# '^@'
    let line = diff_lines[i]
    if line ==# '--- /dev/null'
      call add(head, '--- ' . get(diff_lines, i + 1, '')[4:-1])
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
  let output = s:ChompStderr(cmd)
  if empty(output)
    return 1
  endif
  call s:throw(output)
endfunction

function! s:StageDelete(lnum1, lnum2, count) abort
  let restore = []

  let err = ''
  let did_conflict_err = 0
  let reset_commit = matchstr(getline(a:lnum1), '^Un\w\+ \%(to\| from\) \zs\S\+')
  try
    for info in s:Selection(a:lnum1, a:lnum2)
      if empty(info.paths)
        if len(info.commit)
          let reset_commit = info.commit . '^'
        endif
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
          call delete(info.paths[0])
        else
          call s:TreeChomp('checkout', '--ours', '--', info.paths[0])
        endif
      elseif a:count == 3
        if get(b:fugitive_files['Unstaged'], info.filename, {'status': ''}).status ==# 'D'
          call delete(info.paths[0])
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
        call delete(info.paths[0])
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
    if len(reset_commit) && empty(err)
      call feedkeys(':Git reset ' . reset_commit)
    endif
    return err[1:-1]
  endif
  exe s:ReloadStatus()
  call s:StageReveal()
  return 'checktime|redraw|echomsg ' . string('To restore, ' . join(restore, '|')) . err
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
  exe 'edit' fnameescape(fugitive#Find('.git/'))
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
  if branch ==# '*'
    return
  endif
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
  if branch ==# '*'
    return
  endif
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
      execute 'tab Git reset --patch'
      break
    elseif empty(info.paths) && info.section ==# 'Unstaged'
      execute 'tab Git add --patch'
      break
    elseif empty(info.paths) && info.section ==# 'Untracked'
      execute 'tab Git add --interactive'
      break
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
      execute "tab Git add --patch -- ".join(map(add,'fnameescape(v:val)'))
    endif
    if !empty(reset)
      execute "tab Git reset --patch -- ".join(map(reset,'fnameescape(v:val)'))
    endif
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  return s:ReloadStatus()
endfunction

" Section: :Git commit, :Git revert

function! s:CommitInteractive(line1, line2, range, bang, mods, options, patch) abort
  let status = s:StatusCommand(a:line1, a:line2, a:range, get(a:options, 'curwin') && a:line2 < 0 ? 0 : a:line2, a:bang, a:mods, '', '', [], a:options)
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
  if empty(a:options.subcommand_args) && (
        \ filereadable(fugitive#Find('.git/MERGE_MSG', a:options)) ||
        \ isdirectory(fugitive#Find('.git/rebase-apply', a:options)) ||
        \  !empty(s:TreeChomp([a:options.git_dir, 'diff-files', '--diff-filter=U'])))
    return 'echoerr ":Git merge for loading conflicts has been removed in favor of :Git mergetool"'
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

" Section: :Git bisect

function! s:CompleteBisect(A, L, P, ...) abort
  let bisect_subcmd = matchstr(a:L, '\u\w*[! ] *.\{-\}\s\@<=\zs[^-[:space:]]\S*\ze ')
  if empty(bisect_subcmd)
    let subcmds = ['start', 'bad', 'new', 'good', 'old', 'terms', 'skip', 'next', 'reset', 'replay', 'log', 'run']
    return s:FilterEscape(subcmds, a:A)
  endif
  let dir = a:0 ? a:1 : s:Dir()
  return fugitive#CompleteObject(a:A, dir)
endfunction

function! fugitive#BisectComplete(A, L, P, ...) abort
  return s:CompleteSub('bisect', a:A, a:L, a:P, function('s:CompleteBisect'), a:000)
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
          \ 'filename': diff.filename . s:VimSlash(path),
          \ 'lnum': matchstr(get(a:offsets, i), '\d\+'),
          \ 'text': a:text}
    if len(get(diff, 'module', ''))
      let item.module = diff.module . path
    endif
    call add(items, item)
  endfor
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
  call fugitive#Autowrite()
  let a:state.mode = 'init'
  let a:state.from = ''
  let a:state.to = ''
  let exec = s:UserCommandList({'git': a:options.git, 'git_dir': a:options.git_dir}) + ['-c', 'diff.context=0']
  let exec += a:options.flags + ['--no-pager', 'diff', '--no-ext-diff', '--no-color', '--no-prefix'] + argv
  if prompt
    let title = ':Git ' . s:fnameescape(a:options.flags + [a:options.subcommand] + a:options.subcommand_args)
    return s:QuickfixStream(get(a:options, 'curwin') && a:line2 < 0 ? 0 : a:line2, 'difftool', title, exec, !a:bang, a:mods, s:function('s:ToolParse'), a:state)
  else
    let filename = ''
    let cmd = []
    let tabnr = tabpagenr() + 1
    for line in s:SystemList(exec)[0]
      for item in s:ToolParse(a:state, line)
        if len(get(item, 'filename', '')) && item.filename != filename
          call add(cmd, 'tabedit ' . s:fnameescape(item.filename))
          for i in reverse(range(len(get(item.context, 'diff', []))))
            call add(cmd, (i ? 'rightbelow' : 'leftabove') . ' vertical Gdiffsplit! ' . s:fnameescape(item.context.diff[i].filename))
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
  let dir = a:options.git_dir
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
  let dir = s:Dir(a:options)
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

function! s:GrepParseLine(options, quiet, dir, line) abort
  if !a:quiet
    echo a:line
  endif
  let entry = {'valid': 1}
  let match = matchlist(a:line, '^\(.\{-\}\):\([1-9]\d*\):\([1-9]\d*:\)\=\(.*\)$')
  if a:line =~# '^git: \|^usage: \|^error: \|^fatal: \|^BUG: '
    return {'text': a:line}
  elseif len(match)
    let entry.module = match[1]
    let entry.lnum = +match[2]
    let entry.col = +match[3]
    let entry.text = match[4]
  else
    let entry.module = matchstr(a:line, '\CBinary file \zs.*\ze matches$')
    if len(entry.module)
      let entry.text = 'Binary file'
      let entry.valid = 0
    endif
  endif
  if empty(entry.module) && !a:options.line_number
    let match = matchlist(a:line, '^\(.\{-\}\):\(.*\)$')
    if len(match)
      let entry.module = match[1]
      let entry.pattern = '\M^' . escape(match[2], '\.^$/') . '$'
    endif
  endif
  if empty(entry.module) && a:options.name_count && a:line =~# ':\d\+$'
    let entry.text = matchstr(a:line, '\d\+$')
    let entry.module = strpart(a:line, 0, len(a:line) - len(entry.text) - 1)
  endif
  if empty(entry.module) && a:options.name_only
    let entry.module = a:line
  endif
  if empty(entry.module)
    return {'text': a:line}
  endif
  if entry.module !~# ':'
    let entry.filename = s:PathJoin(a:options.prefix, entry.module)
  else
    let entry.filename = fugitive#Find(entry.module, a:dir)
  endif
  return entry
endfunction

let s:grep_combine_flags = '[aiIrhHEGPFnlLzocpWq]\{-\}'
function! s:GrepOptions(args, dir) abort
  let options = {'name_only': 0, 'name_count': 0, 'line_number': 0}
  let tree = s:Tree(a:dir)
  let prefix = empty(tree) ? fugitive#Find(':0:', a:dir) :
        \ s:cpath(getcwd(), tree) ? '' : s:VimSlash(tree . '/')
  let options.prefix = prefix
  for arg in a:args
    if arg ==# '--'
      break
    endif
    if arg =~# '^\%(-' . s:grep_combine_flags . 'c\|--count\)$'
      let options.name_count = 1
    endif
    if arg =~# '^\%(-' . s:grep_combine_flags . 'n\|--line-number\)$'
      let options.line_number = 1
    elseif arg =~# '^\%(--no-line-number\)$'
      let options.line_number = 0
    endif
    if arg =~# '^\%(-' . s:grep_combine_flags . '[lL]\|--files-with-matches\|--name-only\|--files-without-match\)$'
      let options.name_only = 1
    endif
    if arg ==# '--cached'
      let options.prefix = fugitive#Find(':0:', a:dir)
    elseif arg ==# '--no-cached'
      let options.prefix = prefix
    endif
  endfor
  return options
endfunction

function! s:GrepCfile(result) abort
  let options = s:GrepOptions(a:result.args, a:result)
  let entry = s:GrepParseLine(options, 1, a:result, getline('.'))
  if get(entry, 'col')
    return [entry.filename, entry.lnum, "norm!" . entry.col . "|"]
  elseif has_key(entry, 'lnum')
    return [entry.filename, entry.lnum]
  elseif has_key(entry, 'pattern')
    return [entry.filename, '', 'silent /' . entry.pattern]
  elseif has_key(entry, 'filename')
    return [entry.filename]
  else
    return []
  endif
endfunction

function! s:GrepSubcommand(line1, line2, range, bang, mods, options) abort
  let args = copy(a:options.subcommand_args)
  let handle = -1
  let quiet = 0
  let i = 0
  while i < len(args) && args[i] !=# '--'
    let partition = matchstr(args[i], '^-' . s:grep_combine_flags . '\ze[qzO]')
    if len(partition) > 1
      call insert(args, '-' . strpart(args[i], len(partition)), i+1)
      let args[i] = partition
    elseif args[i] =~# '^\%(-' . s:grep_combine_flags . '[eABC]\|--max-depth\|--context\|--after-context\|--before-context\|--threads\)$'
      let i += 1
    elseif args[i] =~# '^\%(-O\|--open-files-in-pager\)$'
      let handle = 1
      call remove(args, i)
      continue
    elseif args[i] =~# '^\%(-O\|--open-files-in-pager=\)'
      let handle = 0
    elseif args[i] =~# '^-[qz].'
      let args[i] = '-' . args[i][2:-1]
      let quiet = 1
    elseif args[i] =~# '^\%(-[qz]\|--quiet\)$'
      let quiet = 1
      call remove(args, i)
      continue
    elseif args[i] =~# '^--no-quiet$'
      let quiet = 0
    elseif args[i] =~# '^\%(--heading\)$'
      call remove(args, i)
      continue
    endif
    let i += 1
  endwhile
  if handle < 0 ? !quiet : !handle
    return {}
  endif
  call fugitive#Autowrite()
  let listnr = get(a:options, 'curwin') && a:line2 < 0 ? 0 : a:line2
  if s:HasOpt(args, '--no-line-number')
    let lc = []
  else
    let lc = fugitive#GitVersion(2, 19) ? ['-n', '--column'] : ['-n']
  endif
  let cmd = ['grep', '--no-color', '--full-name'] + lc
  let dir = s:Dir(a:options)
  let options = s:GrepOptions(lc + args, dir)
  if listnr > 0
    exe listnr 'wincmd w'
  else
    call s:BlurStatus()
  endif
  let title = (listnr < 0 ? ':Ggrep ' : ':Glgrep ') . s:fnameescape(args)
  call s:QuickfixCreate(listnr, {'title': title})
  let tempfile = tempname()
  let state = {
        \ 'git': a:options.git,
        \ 'flags': a:options.flags,
        \ 'args': cmd + args,
        \ 'git_dir': s:GitDir(a:options),
        \ 'cwd': s:UserCommandCwd(a:options),
        \ 'filetype': 'git',
        \ 'mods': s:Mods(a:mods),
        \ 'file': s:Resolve(tempfile)}
  let event = listnr < 0 ? 'grep-fugitive' : 'lgrep-fugitive'
  exe s:DoAutocmd('QuickFixCmdPre ' . event)
  try
    if !quiet && &more
      let more = 1
      set nomore
    endif
    if !quiet
      echo title
    endif
    let list = s:SystemList(s:UserCommandList(a:options) + cmd + args)[0]
    call writefile(list + [''], tempfile, 'b')
    call s:RunSave(state)
    call map(list, 's:GrepParseLine(options, ' . quiet . ', dir, v:val)')
    call s:QuickfixSet(listnr, list, 'a')
    let press_enter_shortfall = &cmdheight - len(list)
    if press_enter_shortfall > 0 && !quiet
      echo repeat("\n", press_enter_shortfall - 1)
    endif
  finally
    if exists('l:more')
      let &more = more
    endif
  endtry
  call s:RunFinished(state)
  exe s:DoAutocmd('QuickFixCmdPost ' . event)
  if quiet
    let bufnr = bufnr('')
    exe s:QuickfixOpen(listnr, a:mods)
    if bufnr != bufnr('') && !a:bang
      wincmd p
    endif
  end
  if !a:bang && !empty(list)
    return 'silent ' . (listnr < 0 ? 'c' : 'l').'first'
  else
    return ''
  endif
endfunction

function! fugitive#GrepCommand(line1, line2, range, bang, mods, arg) abort
  return fugitive#Command(a:line1, a:line2, a:range, a:bang, a:mods,
        \ "grep -O " . a:arg)
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

function! s:LogParse(state, dir, prefix, line) abort
  if a:state.mode ==# 'hunk' && a:line =~# '^[-+ ]'
    return []
  endif
  let list = matchlist(a:line, '^\%(fugitive \(.\{-\}\)\t\|commit \|From \)\=\(\x\{40,\}\)\%( \(.*\)\)\=$')
  if len(list)
    let queue = s:LogFlushQueue(a:state, a:dir)
    let a:state.mode = 'commit'
    let a:state.base = a:prefix . list[2]
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
          \ 'filename': s:PathJoin(a:state.base, a:state.target),
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
            \ 'filename': s:VimSlash(a:state.base . '/' . a:state.to),
            \ 'module': a:state.base_module . ':' . a:state.to,
            \ 'lnum': offsets[-1],
            \ 'text': a:state.message . matchstr(a:line, ' @@\+ .\+')})
    endif
  elseif a:state.follow &&
        \ a:line =~# '^ \%(mode change \d\|\%(create\|delete\) mode \d\|\%(rename\|copy\|rewrite\) .* (\d\+%)$\)'
    let rename = matchstr(a:line, '^ \%(copy\|rename\) \zs.* => .*\ze (\d\+%)$')
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
  return s:QuickfixStream(listnr, 'log', title, s:UserCommandList(dir) + cmd, !a:bang, a:mods, s:function('s:LogParse'), state, dir, s:DirUrlPrefix(dir)) . after
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
    let arg = matchstr(string, '^\s*\%(\\.\|\S\)\+')
    let string = strpart(string, len(arg))
    let arg = substitute(arg, '^\s\+', '', '')
    call add(args, substitute(arg, '\\\+[|" ]', '\=submatch(0)[len(submatch(0))/2 : -1]', 'g'))
  endwhile
  return args
endfunction

function! s:PlusEscape(string) abort
  return substitute(a:string, '\\*[|" ]', '\=repeat("\\", len(submatch(0))).submatch(0)', 'g')
endfunction

function! s:OpenParse(string, wants_cmd, wants_multiple) abort
  let opts = []
  let cmds = []
  let args = s:ArgSplit(a:string)
  while !empty(args)
    if args[0] =~# '^++'
      call add(opts, ' ' . s:PlusEscape(remove(args, 0)))
    elseif a:wants_cmd && args[0] ==# '+'
      call remove(args, 0)
      call add(cmds, '$')
    elseif a:wants_cmd && args[0] =~# '^+'
      call add(cmds, remove(args, 0)[1:-1])
    else
      break
    endif
  endwhile
  if !a:wants_multiple && empty(args)
    let args = ['>:']
  endif
  let dir = s:Dir()
  let wants_cmd = a:wants_cmd
  let urls = []
  for arg in args
    let [url, lnum] = s:OpenExpand(dir, arg, wants_cmd)
    if lnum
      call insert(cmds, lnum)
    endif
    call add(urls, url)
    let wants_cmd = 0
  endfor

  let pre = join(opts, '')
  if len(cmds) > 1
    let pre .= ' +' . s:PlusEscape(join(map(cmds, '"exe ".string(v:val)'), '|'))
  elseif len(cmds)
    let pre .= ' +' . s:PlusEscape(cmds[0])
  endif
  return [a:wants_multiple ? urls : urls[0], pre]
endfunction

function! s:OpenExpand(dir, file, wants_cmd) abort
  if a:file ==# '-'
    let result = fugitive#Result()
    if has_key(result, 'file')
      let efile = result.file
    else
      throw 'fugitive: no previous command output'
    endif
  else
    let efile = s:Expand(a:file)
  endif
  let url = s:Generate(efile, a:dir)
  if a:wants_cmd && a:file[0] ==# '>' && efile[0] !=# '>' && get(b:, 'fugitive_type', '') isnot# 'tree' && &filetype !=# 'netrw'
    let line = line('.')
    if s:Slash(expand('%:p')) !=# s:Slash(url)
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
      let [res, exec_error] = s:LinesError([a:dir, diffcmd, '-U0'] + args)
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
    return [url, line]
  endif
  return [url, 0]
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
      belowright new +setl\ bufhidden=delete
    endif
    if &diff
      call fugitive#DiffClose()
    endif
  endif
endfunction

let s:bang_edits = {'split': 'Git', 'vsplit': 'vertical Git', 'tabedit': 'tab Git', 'pedit': 'Git!'}
function! fugitive#Open(cmd, bang, mods, arg, ...) abort
  exe s:VersionCheck()
  if a:bang
    return 'echoerr ' . string(':G' . a:cmd . '! for temp buffer output has been replaced by :' . get(s:bang_edits, a:cmd, 'Git') . ' --paginate')
  endif

  try
    let [file, pre] = s:OpenParse(a:arg, 1, 0)
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  let mods = s:Mods(a:mods)
  if a:cmd ==# 'edit'
    call s:BlurStatus()
  endif
  return mods . a:cmd . pre . ' ' . s:fnameescape(file)
endfunction

function! fugitive#DropCommand(line1, count, range, bang, mods, arg, ...) abort
  exe s:VersionCheck()

  let mods = s:Mods(a:mods)
  try
    let [files, pre] = s:OpenParse(a:arg, 1, 1)
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  if empty(files)
    return 'drop'
  endif
  call s:BlurStatus()
  return mods . 'drop' . ' ' . s:fnameescape(files) . substitute(pre, '^ *+', '|', '')
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
  return [pre . 'keepalt ' . mods . after . 'read', '|' . delete . 'diffupdate' . (a:count < 0 ? '|' . line('.') : '')]
endfunction

function! fugitive#ReadCommand(line1, count, range, bang, mods, arg, ...) abort
  exe s:VersionCheck()
  let [read, post] = s:ReadPrepare(a:line1, a:count, a:range, a:mods)
  try
    let [file, pre] = s:OpenParse(a:arg, 0, 0)
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  if file =~# '^fugitive:' && a:count is# 0
    return 'exe ' .string('keepalt ' . s:Mods(a:mods) . fugitive#FileReadCmd(file, 0, pre)) . '|diffupdate'
  endif
  return read . ' ' . pre . ' ' . s:fnameescape(file) . post
endfunction

function! fugitive#EditComplete(A, L, P) abort
  if a:A =~# '^>'
    return map(s:FilterEscape(s:CompleteHeads(s:Dir()), a:A[1:-1]), "'>' . v:val")
  else
    return fugitive#CompleteObject(a:A, a:L, a:P)
  endif
endfunction

function! fugitive#ReadComplete(A, L, P) abort
  return fugitive#EditComplete(a:A, a:L, a:P)
endfunction

" Section: :Gwrite, :Gwq

function! fugitive#WriteCommand(line1, line2, range, bang, mods, arg, ...) abort
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

  let message = s:ChompStderr(['add'] + (a:bang ? ['--force'] : []) + ['--', file])
  if len(message)
    let v:errmsg = 'fugitive: '.message
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
  exe s:DoAutocmd('BufWritePost ' . s:fnameescape(zero))
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
  call fugitive#DidChange()
  return 'checktime' . after
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

function! s:CompletePush(A, L, P, ...) abort
  let dir = a:0 ? a:1 : s:Dir()
  let remote = matchstr(a:L, '\u\w*[! ] *.\{-\}\s\@<=\zs[^-[:space:]]\S*\ze ')
  if empty(remote)
    let matches = s:LinesError([dir, 'remote'])[0]
  elseif a:A =~# ':'
    let lead = matchstr(a:A, '^[^:]*:')
    let matches = s:LinesError([dir, 'ls-remote', remote])[0]
    call filter(matches, 'v:val =~# "\t" && v:val !~# "{"')
    call map(matches, 'lead . s:sub(v:val, "^.*\t", "")')
  else
    let matches = s:CompleteHeads(dir)
    if a:A =~# '^[\''"]\=+'
      call map(matches, '"+" . v:val')
    endif
  endif
  return s:FilterEscape(matches, a:A)
endfunction

function! fugitive#PushComplete(A, L, P, ...) abort
  return s:CompleteSub('push', a:A, a:L, a:P, function('s:CompletePush'), a:000)
endfunction

function! fugitive#FetchComplete(A, L, P, ...) abort
  return s:CompleteSub('fetch', a:A, a:L, a:P, function('s:CompleteRemote'), a:000)
endfunction

function! s:PushSubcommand(...) abort
  return {'no_more': 1}
endfunction

function! s:FetchSubcommand(...) abort
  return {'no_more': 1}
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

function! s:DiffModifier(count, default) abort
  let fdc = matchstr(&diffopt, 'foldcolumn:\zs\d\+')
  if &diffopt =~# 'horizontal' && &diffopt !~# 'vertical'
    return ''
  elseif &diffopt =~# 'vertical'
    return 'vertical '
  elseif !get(g:, 'fugitive_diffsplit_directional_fit', a:default)
    return ''
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

function! s:diffthis() abort
  if !&diff
    let w:fugitive_diff_restore = 1
    diffthis
  endif
endfunction

function! s:diffoff() abort
  unlet! w:fugitive_diff_restore
  diffoff
endfunction

function! s:diffoff_all(dir) abort
  let curwin = winnr()
  for nr in range(1,winnr('$'))
    if getwinvar(nr, '&diff') && !empty(getwinvar(nr, 'fugitive_diff_restore'))
      call setwinvar(nr, 'fugitive_diff_restore', '')
    endif
  endfor
  if curwin != winnr()
    execute curwin.'wincmd w'
  endif
  diffoff!
endfunction

function! s:IsConflicted() abort
  return len(@%) && !empty(s:ChompDefault('', ['ls-files', '--unmerged', '--', expand('%:p')]))
endfunction

function! fugitive#Diffsplit(autodir, keepfocus, mods, arg, ...) abort
  exe s:VersionCheck()
  let args = s:ArgSplit(a:arg)
  let post = ''
  let autodir = a:autodir
  while get(args, 0, '') =~# '^++'
    if args[0] =~? '^++novertical$'
      let autodir = 0
    else
      return 'echoerr ' . string('fugitive: unknown option ' . args[0])
    endif
    call remove(args, 0)
  endwhile
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
  if (empty(args) || args[0] =~# '^>\=:$') && a:keepfocus
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
      let mods = (autodir ? s:DiffModifier(len(parents) + 1, empty(args) || args[0] =~# '^>') : '') . s:Mods(mods, 'leftabove')
      let nr = bufnr('')
      if len(parents) > 1 && !&equalalways
        let equalalways = 0
        set equalalways
      endif
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
        let file = len(commit) ? s:Relative() : s:Relative(s:IsConflicted() ? ':1:' : ':0:')
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
      if a:keepfocus || arg =~# '^>'
        let mods = s:Mods(a:mods, 'leftabove')
      else
        let mods = s:Mods(a:mods)
      endif
    elseif exists('parents')
      let file = get(parents, -1, s:Relative(repeat('0', 40). ':'))
      let mods = s:Mods(a:mods, 'leftabove')
    elseif len(commit)
      let file = s:Relative()
      let mods = s:Mods(a:mods, 'rightbelow')
    elseif s:IsConflicted()
      let file = s:Relative(':1:')
      let mods = s:Mods(a:mods, 'leftabove')
      if get(g:, 'fugitive_legacy_commands', 1)
        let post = 'echohl WarningMsg|echo "Use :Gdiffsplit! for 3 way diff"|echohl NONE|' . post
      endif
    else
      exe s:DirCheck()
      let file = s:Relative(':0:')
      let mods = s:Mods(a:mods, 'leftabove')
    endif
    let spec = s:Generate(file)
    if spec =~# '^fugitive:' && empty(s:DirCommitFile(spec)[2])
      let spec = s:VimSlash(spec . s:Relative('/'))
    endif
    exe pre
    let w:fugitive_diff_restore = 1
    let mods = (autodir ? s:DiffModifier(2, empty(args) || args[0] =~# '^>') : '') . mods
    if &diffopt =~# 'vertical'
      let diffopt = &diffopt
      set diffopt-=vertical
    endif
    execute mods 'diffsplit' s:fnameescape(spec)
    let w:fugitive_diff_restore = 1
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
    if exists('l:equalalways')
      let &g:equalalways = equalalways
    endif
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
  if a:rename
    let default_root = expand('%:p:s?[\/]$??:h') . '/'
  else
    let default_root = s:Tree(dir) . '/'
  endif
  if a:destination =~# '^:/:\='
    let destination = s:Tree(dir) . s:Expand(substitute(a:destination, '^:/:\=', '', ''))
  elseif a:destination =~# '^:(top)'
    let destination = s:Expand(matchstr(a:destination, ')\zs.*'))
    if destination !~# '^/\|^\a\+:'
      let destination = s:Tree(dir) . '/' . destination
    endif
    let destination = s:Tree(dir) .
  elseif a:destination =~# '^:(\%(top,literal\|literal,top\))'
    let destination = s:Tree(dir) . matchstr(a:destination, ')\zs.*')
  elseif a:destination =~# '^:(literal)\.\.\=\%(/\|$\)'
    let destination = simplify(getcwd() . '/' . matchstr(a:destination, ')\zs.*'))
  elseif a:destination =~# '^:(literal)'
    let destination = simplify(default_root . matchstr(a:destination, ')\zs.*'))
  else
    let destination = s:Expand(a:destination)
    if destination =~# '^\.\.\=\%(/\|$\)'
      let destination = simplify(getcwd() . '/' . destination)
    elseif destination !~# '^\a\+:\|^/'
      let destination = default_root . destination
    endif
  endif
  let destination = s:Slash(destination)
  if isdirectory(@%)
    setlocal noswapfile
  endif
  let exec = fugitive#Execute(['mv'] + (a:force ? ['-f'] : []) + ['--', expand('%:p'), destination], dir)
  if exec.exit_status && exec.stderr !=# ['']
    return 'echoerr ' .string('fugitive: '.s:JoinChomp(exec.stderr))
  endif
  if isdirectory(destination)
    let destination = fnamemodify(s:sub(destination,'/$','').'/'.expand('%:t'),':.')
  endif
  let reload = '|call fugitive#DidChange(' . string(exec) . ')'
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

function! fugitive#MoveCommand(line1, line2, range, bang, mods, arg, ...) abort
  return s:Move(a:bang, 0, a:arg)
endfunction

function! fugitive#RenameCommand(line1, line2, range, bang, mods, arg, ...) abort
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
  let message = s:ChompStderr(cmd + ['--', expand('%:p')], dir)
  if len(message)
    let v:errmsg = 'fugitive: '.s:sub(message,'error:.*\zs\n\(.*-f.*',' (add ! to force)')
    return 'echoerr '.string(v:errmsg)
  else
    return a:after . (a:force ? '!' : ''). '|call fugitive#DidChange(' . string(dir) . ')'
  endif
endfunction

function! fugitive#RemoveCommand(line1, line2, range, bang, mods, arg, ...) abort
  return s:Remove('edit', a:bang)
endfunction

function! fugitive#UnlinkCommand(line1, line2, range, bang, mods, arg, ...) abort
  return s:Remove('edit', a:bang)
endfunction

function! fugitive#DeleteCommand(line1, line2, range, bang, mods, arg, ...) abort
  return s:Remove('bdelete', a:bang)
endfunction

" Section: :Git blame

function! s:Keywordprg() abort
  let args = ' --git-dir=' . escape(FugitiveGitPath(s:GitDir()), "\\\"' ")
  if has('gui_running') && !has('win32')
    return s:GitShellCmd() . ' --no-pager' . args . ' log -1'
  else
    return s:GitShellCmd() . args . ' show'
  endif
endfunction

function! s:linechars(pattern) abort
  let chars = strlen(s:gsub(matchstr(getline('.'), a:pattern), '.', '.'))
  if &conceallevel > 1
    for col in range(1, chars)
      let chars -= synconcealed(line('.'), col)[0]
    endfor
  endif
  return chars
endfunction

function! s:BlameBufnr(...) abort
  let state = s:TempState(a:0 ? a:1 : bufnr(''))
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
    let commit = get(s:LinesError([state.git_dir, 'rev-list', '--ancestry-path', '--reverse', commit . '..' . state.blame_reverse_end])[0], 0, '')
  endif
  let lnum = +matchstr(line, ' \zs\d\+\ze \%((\| *\d\+)\)')
  let path = matchstr(line, '^\^\=[?*]*\x* \+\%(\d\+ \+\d\+ \+\)\=\zs.\{-\}\ze\s*\d\+ \%((\| *\d\+)\)')
  if empty(path) && lnum
    let path = get(state, 'blame_file', '')
  endif
  return [commit, path, lnum]
endfunction

function! s:BlameLeave() abort
  let state = s:TempState()
  let bufwinnr = exists('*win_id2win') ? win_id2win(get(state, 'origin_winid')) : 0
  if bufwinnr == 0
    let bufwinnr = bufwinnr(get(state, 'origin_bufnr', -1))
  endif
  if get(state, 'filetype', '') ==# 'fugitiveblame' && bufwinnr > 0
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
  let dir = s:Dir(a:options)
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
        echo s:ChompStderr([dir, 'blame', arg])
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
      if &modified || !empty(s:DirCommitFile(@%)[1])
        let cmd += ['--contents', tempname . '.in']
        silent execute 'noautocmd keepalt %write ' . s:fnameescape(tempname . '.in')
        let delete_in = 1
      elseif &autoread
        exe 'checktime ' . bufnr('')
      endif
    else
      call fugitive#Autowrite()
    endif
    let basecmd = [{'git': a:options.git}, dir, '--literal-pathspecs'] + cmd + ['--'] + (len(files) ? files : [file])
    let [err, exec_error] = s:StdoutToFile(temp, basecmd)
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
            \ 'git_dir': s:GitDir(a:options),
            \ 'cwd': s:UserCommandCwd(a:options),
            \ 'filetype': (raw ? 'git' : 'fugitiveblame'),
            \ 'blame_options': a:options,
            \ 'blame_flags': flags,
            \ 'blame_file': file}
      if s:HasOpt(flags, '--reverse')
        let temp_state.blame_reverse_end = matchstr(get(commits, 0, ''), '\.\.\zs.*')
      endif
      if a:line1 == 0 && a:count == 1
        if get(a:options, 'curwin')
          let edit = 'edit'
        elseif a:bang
          let edit = 'pedit'
        else
          let edit = 'split'
        endif
        return s:BlameCommit(s:Mods(a:mods) . edit, get(readfile(temp), 0, ''), temp_state)
      elseif (a:line1 == 0 || a:range == 1) && a:count > 0
        let edit = s:Mods(a:mods) . get(['edit', 'split', 'pedit', 'vsplit', 'tabedit', 'edit'], a:count - (a:line1 ? a:line1 : 1), 'split')
        return s:BlameCommit(edit, get(readfile(temp), 0, ''), temp_state)
      else
        let temp = s:Resolve(temp)
        let temp_state.file = temp
        call s:RunSave(temp_state)
        if len(ranges + commits + files) || raw
          let reload = '|call fugitive#DidChange(fugitive#Result(' . string(temp_state.file) . '))'
          let mods = s:Mods(a:mods)
          if a:count != 0
            exe 'silent keepalt' mods get(a:options, 'curwin') ? 'edit' : 'split' s:fnameescape(temp)
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
        let temp_state.origin_bufnr = bufnr('')
        if exists('*win_getid')
          let temp_state.origin_winid = win_getid()
        endif
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
          let win_blame_bufnr = s:BlameBufnr(winbufnr(winnr))
          if getwinvar(winnr, '&scrollbind') ? win_blame_bufnr == temp_state.origin_bufnr : win_blame_bufnr > 0
            execute winbufnr(winnr).'bdelete'
          endif
        endfor
        let restore_winnr = get(temp_state, 'origin_winid', 'bufwinnr(' . temp_state.origin_bufnr . ')')
        if !&l:scrollbind
          call add(restore, 'call setwinvar(' . restore_winnr . ',"&scrollbind",0)')
        endif
        if &l:wrap
          call add(restore, 'call setwinvar(' . restore_winnr . ',"&wrap",1)')
        endif
        if &l:foldenable
          call add(restore, 'call setwinvar(' . restore_winnr . ',"&foldenable",1)')
        endif
        setlocal scrollbind nowrap nofoldenable
        let top = line('w0') + &scrolloff
        let current = line('.')
        exe 'silent keepalt' (a:bang ? s:Mods(mods) . 'split' : s:Mods(mods, 'leftabove') . 'vsplit') s:fnameescape(temp)
        let w:fugitive_leave = join(restore, '|')
        execute top
        normal! zt
        execute current
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
  for x in split('01234567890abcdef', '\zs')
    exe 'syn match FugitiveblameHashGroup'.x '"\%(^\^\=[*?]*\)\@<='.x.'\x\{5,\}\>" nextgroup=FugitiveblameAnnotation,FugitiveblameOriginalLineNumber,fugitiveblameOriginalFile skipwhite'
  endfor
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
    let pattern = substitute(orig_hash, '^\(\x\)\x\(\x\)\x\(\x\)\x$', '\1\\x\2\\x\3\\x', '') . '*'
    exe 'syn match FugitiveblameHash'.hash.'       "\%(^\^\=[*?]*\)\@<='.pattern.'" contained containedin=FugitiveblameHashGroup' . orig_hash[0]
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

function! s:BlameMaps(is_ftplugin) abort
  let ft = a:is_ftplugin
  call s:MapGitOps(ft)
  call s:Map('n', '<F1>', ':help :Git_blame<CR>', '<silent>', ft)
  call s:Map('n', 'g?',   ':help :Git_blame<CR>', '<silent>', ft)
  call s:Map('n', 'gq',   ':exe <SID>BlameQuit()<CR>', '<silent>', ft)
  call s:Map('n', '<2-LeftMouse>', ':<C-U>exe <SID>BlameCommit("exe <SID>BlameLeave()<Bar>edit")<CR>', '<silent>', ft)
  call s:Map('n', '<CR>', ':<C-U>exe <SID>BlameCommit("exe <SID>BlameLeave()<Bar>edit")<CR>', '<silent>', ft)
  call s:Map('n', '-',    ':<C-U>exe <SID>BlameJump("")<CR>', '<silent>', ft)
  call s:Map('n', 's',    ':<C-U>exe <SID>BlameJump("")<CR>', '<silent>', ft)
  call s:Map('n', 'u',    ':<C-U>exe <SID>BlameJump("")<CR>', '<silent>', ft)
  call s:Map('n', 'P',    ':<C-U>exe <SID>BlameJump("^".v:count1)<CR>', '<silent>', ft)
  call s:Map('n', '~',    ':<C-U>exe <SID>BlameJump("~".v:count1)<CR>', '<silent>', ft)
  call s:Map('n', 'i',    ':<C-U>exe <SID>BlameCommit("exe <SID>BlameLeave()<Bar>edit")<CR>', '<silent>', ft)
  call s:Map('n', 'o',    ':<C-U>exe <SID>BlameCommit("split")<CR>', '<silent>', ft)
  call s:Map('n', 'O',    ':<C-U>exe <SID>BlameCommit("tabedit")<CR>', '<silent>', ft)
  call s:Map('n', 'p',    ':<C-U>exe <SID>BlameCommit("pedit")<CR>', '<silent>', ft)
  exe s:Map('n', '.',    ":<C-U> <C-R>=substitute(<SID>BlameCommitFileLnum()[0],'^$','@','')<CR><Home>", '', ft)
  exe s:Map('n', '(',    "-", '', ft)
  exe s:Map('n', ')',    "+", '', ft)
  call s:Map('n', 'A',    ":<C-u>exe 'vertical resize '.(<SID>linechars('.\\{-\\}\\ze [0-9:/+-][0-9:/+ -]* \\d\\+)')+1+v:count)<CR>", '<silent>', ft)
  call s:Map('n', 'C',    ":<C-u>exe 'vertical resize '.(<SID>linechars('^\\S\\+')+1+v:count)<CR>", '<silent>', ft)
  call s:Map('n', 'D',    ":<C-u>exe 'vertical resize '.(<SID>linechars('.\\{-\\}\\ze\\d\\ze\\s\\+\\d\\+)')+1-v:count)<CR>", '<silent>', ft)
endfunction

function! fugitive#BlameFileType() abort
  setlocal nomodeline
  setlocal foldmethod=manual
  if len(s:GitDir())
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
  call s:BlameMaps(1)
endfunction

function! s:BlameCursorSync(bufnr, line) abort
  if a:line == line('.')
    return
  endif
  if get(s:TempState(), 'origin_bufnr') == a:bufnr || get(s:TempState(a:bufnr), 'origin_bufnr') == bufnr('')
    if &startofline
      execute a:line
    else
      let pos = getpos('.')
      let pos[1] = a:line
      call setpos('.', pos)
    endif
  endif
endfunction

augroup fugitive_blame
  autocmd!
  autocmd ColorScheme,GUIEnter * call s:BlameRehighlight()
  autocmd BufWinLeave * execute getwinvar(+bufwinnr(+expand('<abuf>')), 'fugitive_leave')
  autocmd WinLeave * let s:cursor_for_blame = [bufnr(''), line('.')]
  autocmd WinEnter * if exists('s:cursor_for_blame') | call call('s:BlameCursorSync', s:cursor_for_blame) | endif
augroup END

" Section: :GBrowse

function! s:BrowserOpen(url, mods, echo_copy) abort
  let [_, main, query, anchor; __] = matchlist(a:url, '^\([^#?]*\)\(?[^#]*\)\=\(#.*\)\=')
  let url = main . tr(query, ' ', '+') . anchor
  let url = substitute(url, '[ <>\|"]', '\="%".printf("%02X",char2nr(submatch(0)))', 'g')
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

function! fugitive#BrowseCommand(line1, count, range, bang, mods, arg, ...) abort
  exe s:VersionCheck()
  let dir = s:Dir()
  try
    let arg = a:arg
    if arg =~# '^++\%([Gg]it\)\=[Rr]emote='
      let remote = matchstr(arg, '^++\%([Gg]it\)\=[Rr]emote=\zs\S\+')
      let arg = matchstr(arg, '\s\zs\S.*')
    endif
    let validremote = '\.\%(git\)\=\|\.\=/.*\|\a[[:alnum:]_-]*\%(://.\{-\}\)\='
    if arg ==# '-'
      let remote = ''
      let rev = ''
      let result = fugitive#Result()
      if filereadable(get(result, 'file', ''))
        let rev = s:fnameescape(result.file)
      else
        return 'echoerr ' . string('fugitive: could not find prior :Git invocation')
      endif
    elseif !exists('l:remote')
      let remote = matchstr(arg, '\\\@<!\%(\\\\\)*[!@]\zs\%('.validremote.'\)$')
      let rev = strpart(arg, 0, len(arg) - len(remote) - (empty(remote) ? 0 : 1))
    else
      let rev = arg
    endif
    let expanded = s:Expand(rev)
    if expanded =~? '^\a\a\+:[\/][\/]' && expanded !~? '^fugitive:'
      return s:BrowserOpen(s:Slash(expanded), a:mods, a:bang)
    endif
    if !exists('l:result')
      let result = s:TempState(empty(expanded) ? bufnr('') : expanded)
    endif
    if !get(result, 'origin_bufnr', 1) && filereadable(get(result, 'file', ''))
      for line in readfile(result.file, '', 4096)
        let rev = s:fnameescape(matchstr(line, '\<https\=://[^[:space:]<>]*[^[:space:]<>.,;:"''!?]'))
        if len(rev)
          return s:BrowserOpen(rev, a:mods, a:bang)
        endif
      endfor
      return 'echoerr ' . string('fugitive: no URL found in output of :Git')
    endif
    if empty(remote) && expanded =~# '^[^-./:^~][^:^~]*$' && !empty(dir)
      let config = fugitive#Config(dir)
      if !empty(FugitiveConfigGet('remote.' . expanded . '.url', config))
        let remote = expanded
        let expanded = ''
      endif
    endif
    if empty(expanded)
      let bufname = &buftype =~# '^\%(nofile\|terminal\)$' ? '' : s:BufName('%')
      let expanded = s:DirRev(bufname)[1]
      if empty(expanded)
        let expanded = fugitive#Path(bufname, ':(top)', dir)
      endif
      if a:count > 0 && has_key(result, 'origin_bufnr') && a:range != 2
        let blame = s:BlameCommitFileLnum(getline(a:count))
        if len(blame[0])
          let expanded = blame[0]
        endif
      endif
    endif
    let full = s:Generate(expanded, dir)
    let commit = ''
    let ref = ''
    let forbid_ref_as_commit = 0
    if full =~# '^fugitive:'
      let [dir, commit, path] = s:DirCommitFile(full)
      if commit =~# '^\d\=$'
        let commit = ''
        let type = path =~# '^/\=$' ? 'tree' : 'blob'
      else
        let ref_match = matchlist(expanded, '^\(@{\@!\|[^:~^@]\+\)\(:\%(//\)\@!\|[~^@]\|$\)')
        let ref = get(ref_match, 1, '')
        let forbid_ref_as_commit = ref =~# '^@\=$' || ref_match[2] !~# '^:\=$'
        if empty(path) && !forbid_ref_as_commit
          let type = 'ref'
        else
          let type = s:ChompDefault(empty(path) ? 'commit': 'blob',
                \ ['cat-file', '-t', commit . substitute(path, '^/', ':', '')], dir)
        endif
      endif
      let path = path[1:-1]
    elseif !empty(s:Tree(dir))
      let relevant_dir = FugitiveExtractGitDir(full)
      if !empty(relevant_dir)
        let dir = relevant_dir
      endif
      let path = fugitive#Path(full, '/', dir)[1:-1]
      if empty(path) || isdirectory(full)
        let type = 'tree'
      else
        let type = 'blob'
      endif
    else
      let path = '.git/' . full[strlen(dir)+1:-1]
      let type = ''
    endif
    exe s:DirCheck(dir)
    if path =~# '^\.git/'
      let ref = matchstr(path, '^.git/\zs\%(refs/[^/]\+/[^/].*\|\w*HEAD\)$')
      let type = empty(ref) ? 'root': 'ref'
      let path = ''
    endif
    if empty(ref) || ref ==# 'HEAD' || ref ==# '@'
      let ref = fugitive#Head(-1, dir)
    endif
    if ref =~# '^\x\{40,\}$'
      let ref = ''
    elseif !empty(ref) && ref !~# '^refs/'
      let ref = FugitiveExecute(['rev-parse', '--symbolic-full-name', ref], dir).stdout[0]
      if ref !~# '^refs/'
        let ref = ''
      endif
    endif

    if !exists('l:config') || s:Dir(config) !=# dir
      let config = fugitive#Config(dir)
    endif
    let merge = ''
    if !empty(remote) && ref =~# '^refs/remotes/[^/]\+/[^/]\|^refs/heads/[^/]'
      let merge = matchstr(ref, '^refs/\%(heads/\|remotes/[^/]\+/\)\zs.\+')
      let ref = 'refs/heads/' . merge
    elseif ref =~# '^refs/remotes/[^/]\+/[^/]'
      let remote = matchstr(ref, '^refs/remotes/\zs[^/]\+')
      let merge = matchstr(ref, '^refs/remotes/[^/]\+/\zs.\+')
      let ref = 'refs/heads/' . merge
    elseif ref =~# '^refs/heads/[^/]'
      let merge = strpart(ref, 11)
      let r = FugitiveConfigGet('branch.' . merge . '.remote', config)
      let m = FugitiveConfigGet('branch.' . merge . '.merge', config)[11:-1]
      if r ==# '.' && !empty(m)
        let r2 = FugitiveConfigGet('branch.'.m.'.remote', config)
        if r2 !~# '^\.\=$'
          let r = r2
          let m = FugitiveConfigGet('branch.'.m.'.merge', config)[11:-1]
        endif
      endif
      if r !~# '^\.\=$'
        let remote = r
      endif
      if !empty(remote)
        let remote_ref = 'refs/remotes/' . remote . '/' . merge
        if FugitiveConfigGet('push.default', config) ==# 'upstream' ||
              \ !filereadable(FugitiveFind('.git/' . remote_ref, dir)) && empty(s:ChompDefault('', ['rev-parse', '--verify', remote_ref, '--'], dir))
          let merge = m
          let ref = 'refs/heads/' . merge
        endif
      endif
    endif

    if empty(remote) || remote ==# '.'
      let remote = s:RemoteDefault(config)
    endif
    if empty(merge) || empty(remote)
      let provider_ref = ref
    else
      let provider_ref = 'refs/remotes/' . remote . '/' . merge
    endif
    if forbid_ref_as_commit || a:count >= 0
      let ref = ''
      if type ==# 'ref'
        let type = 'commit'
      endif
    elseif type ==# 'ref' && ref =~# '^refs/\%(heads\|tags\)/[^/]'
        let commit = matchstr(ref, '^\Crefs/\%(heads\|tags\)/\zs.*')
    endif

    let line1 = a:count > 0 && type ==# 'blob' ? a:line1 : 0
    let line2 = a:count > 0 && type ==# 'blob' ? a:count : 0
    if empty(commit) && type =~# '^\%(tree\|blob\)$'
      if a:count < 0
        let commit = matchstr(ref, '^\Crefs/\%(heads\|tags\)/\zs.*')
      elseif len(provider_ref)
        let owner = s:Owner(@%, dir)
        let commit = s:ChompDefault('', ['merge-base', provider_ref, empty(owner) ? '@' : owner, '--'], dir)
        if line2 > 0 && empty(arg) && commit =~# '^\x\{40,\}$' && type ==# 'blob'
          let blame_list = tempname()
          call writefile([commit, ''], blame_list, 'b')
          let blame_cmd = ['-c', 'blame.coloring=none', 'blame', '-L', line1.','.line2, '-S', blame_list, '-s', '--show-number']
          if !&l:modified || has_key(result, 'origin_bufnr')
            let [blame, exec_error] = s:LinesError(blame_cmd + ['./' . path], dir)
          else
            let blame_in = tempname()
            silent exe 'noautocmd keepalt %write' blame_in
            let [blame, exec_error] = s:LinesError(blame_cmd + ['--contents', blame_in, './' . path], dir)
            call delete(blame_in)
          endif
          call delete(blame_list)
          if !exec_error
            let blame_regex = '^\^\x\+\s\+\zs\d\+\ze\s'
            if get(blame, 0) =~# blame_regex && get(blame, -1) =~# blame_regex
              let line1 = +matchstr(blame[0], blame_regex)
              let line2 = +matchstr(blame[-1], blame_regex)
            else
              throw "fugitive: can't browse to unpushed change"
            endif
          endif
        endif
      endif
      if empty(commit)
        let commit = fugitive#RevParse(empty(ref) ? 'HEAD' : ref, dir)
      endif
    endif

    if remote =~# ':'
      let remote_url = remote
    else
      let remote_url = fugitive#RemoteUrl(remote, config)
    endif
    let raw = empty(remote_url) ? remote : remote_url
    let git_dir = s:GitDir(dir)

    let opts = {
          \ 'git_dir': git_dir,
          \ 'repo': {'git_dir': git_dir},
          \ 'remote': raw,
          \ 'remote_name': remote,
          \ 'commit': s:UrlEncode(commit),
          \ 'path': substitute(s:UrlEncode(path), '%20', ' ', 'g'),
          \ 'type': type,
          \ 'line1': line1,
          \ 'line2': line2}

    if empty(path)
      if type ==# 'ref' && ref =~# '^refs/'
        let opts.path = '.git/' . s:UrlEncode(ref)
        let opts.type = ''
      elseif type ==# 'root'
        let opts.path ='.git/index'
        let opts.type = ''
      endif
    elseif type ==# 'tree' && !empty(path)
      let opts.path = s:sub(opts.path, '/\=$', '/')
    endif

    for l:.Handler in get(g:, 'fugitive_browse_handlers', [])
      let l:.url = call(Handler, [copy(opts)])
      if type(url) == type('') && url =~# '://'
        return s:BrowserOpen(url, a:mods, a:bang)
      endif
    endfor

    if !empty(remote_url)
      return 'echoerr ' . string("fugitive: no GBrowse handler installed for '".remote_url."'")
    else
      return 'echoerr ' . string("fugitive: could not find remote named '".remote."'")
    endif
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
endfunction

" Section: Maps

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
    call s:Map('c', '<C-R><C-F>', '<Plug><cfile>', '<unique>', 1)
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
    let commit = matchstr(getline('.'), '\S\@<!\x\{4,\}\S\@!')
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

function! s:ParseDiffHeader(str) abort
  let list = matchlist(a:str, '\Cdiff --git \("\=\w/.*\|/dev/null\) \("\=\w/.*\|/dev/null\)$')
  if empty(list)
    let list = matchlist(a:str, '\Cdiff --git \("\=[^/].*\|/dev/null\) \("\=[^/].*\|/dev/null\)$')
  endif
  return [fugitive#Unquote(get(list, 1, '')), fugitive#Unquote(get(list, 2, ''))]
endfunction

function! s:HunkPosition(lnum) abort
  let lnum = a:lnum + get({'@': 1, '\': -1}, getline(a:lnum)[0], 0)
  let offsets = {' ': -1, '+': 0, '-': 0}
  let sigil = getline(lnum)[0]
  let line_char = sigil
  while has_key(offsets, line_char)
    let offsets[line_char] += 1
    let lnum -= 1
    let line_char = getline(lnum)[0]
  endwhile
  let starts = matchlist(getline(lnum), '^@@\+[ 0-9,-]* -\(\d\+\)\%(,\d\+\)\= +\(\d\+\)[ ,]')
  if empty(starts)
    return [0, 0, 0]
  endif
  return [lnum,
        \ sigil ==# '+' ? 0 : starts[1] + offsets[' '] + offsets['-'],
        \ sigil ==# '-' ? 0 : starts[2] + offsets[' '] + offsets['+']]
endfunction

function! s:MapMotion(lhs, rhs) abort
  let maps = [
        \ s:Map('n', a:lhs, ":<C-U>" . a:rhs . "<CR>", "<silent>"),
        \ s:Map('o', a:lhs, ":<C-U>" . a:rhs . "<CR>", "<silent>"),
        \ s:Map('x', a:lhs, ":<C-U>exe 'normal! gv'<Bar>" . a:rhs . "<CR>", "<silent>")]
  call filter(maps, '!empty(v:val)')
  return join(maps, '|')
endfunction

function! s:MapGitOps(is_ftplugin) abort
  let ft = a:is_ftplugin
  if &modifiable
    return ''
  endif
  exe s:Map('n', 'c<Space>', ':Git commit<Space>', '', ft)
  exe s:Map('n', 'c<CR>', ':Git commit<CR>', '', ft)
  exe s:Map('n', 'cv<Space>', ':tab Git commit -v<Space>', '', ft)
  exe s:Map('n', 'cv<CR>', ':tab Git commit -v<CR>', '', ft)
  exe s:Map('n', 'ca', ':<C-U>Git commit --amend<CR>', '<silent>', ft)
  exe s:Map('n', 'cc', ':<C-U>Git commit<CR>', '<silent>', ft)
  exe s:Map('n', 'ce', ':<C-U>Git commit --amend --no-edit<CR>', '<silent>', ft)
  exe s:Map('n', 'cw', ':<C-U>Git commit --amend --only<CR>', '<silent>', ft)
  exe s:Map('n', 'cva', ':<C-U>tab Git commit -v --amend<CR>', '<silent>', ft)
  exe s:Map('n', 'cvc', ':<C-U>tab Git commit -v<CR>', '<silent>', ft)
  exe s:Map('n', 'cRa', ':<C-U>Git commit --reset-author --amend<CR>', '<silent>', ft)
  exe s:Map('n', 'cRe', ':<C-U>Git commit --reset-author --amend --no-edit<CR>', '<silent>', ft)
  exe s:Map('n', 'cRw', ':<C-U>Git commit --reset-author --amend --only<CR>', '<silent>', ft)
  exe s:Map('n', 'cf', ':<C-U>Git commit --fixup=<C-R>=<SID>SquashArgument()<CR>', '', ft)
  exe s:Map('n', 'cF', ':<C-U><Bar>Git -c sequence.editor=true rebase --interactive --autosquash<C-R>=<SID>RebaseArgument()<CR><Home>Git commit --fixup=<C-R>=<SID>SquashArgument()<CR>', '', ft)
  exe s:Map('n', 'cs', ':<C-U>Git commit --no-edit --squash=<C-R>=<SID>SquashArgument()<CR>', '', ft)
  exe s:Map('n', 'cS', ':<C-U><Bar>Git -c sequence.editor=true rebase --interactive --autosquash<C-R>=<SID>RebaseArgument()<CR><Home>Git commit --no-edit --squash=<C-R>=<SID>SquashArgument()<CR>', '', ft)
  exe s:Map('n', 'cA', ':<C-U>Git commit --edit --squash=<C-R>=<SID>SquashArgument()<CR>', '', ft)
  exe s:Map('n', 'c?', ':<C-U>help fugitive_c<CR>', '<silent>', ft)

  exe s:Map('n', 'cr<Space>', ':Git revert<Space>', '', ft)
  exe s:Map('n', 'cr<CR>', ':Git revert<CR>', '', ft)
  exe s:Map('n', 'crc', ':<C-U>Git revert <C-R>=<SID>SquashArgument()<CR><CR>', '<silent>', ft)
  exe s:Map('n', 'crn', ':<C-U>Git revert --no-commit <C-R>=<SID>SquashArgument()<CR><CR>', '<silent>', ft)
  exe s:Map('n', 'cr?', ':<C-U>help fugitive_cr<CR>', '<silent>', ft)

  exe s:Map('n', 'cm<Space>', ':Git merge<Space>', '', ft)
  exe s:Map('n', 'cm<CR>', ':Git merge<CR>', '', ft)
  exe s:Map('n', 'cmt', ':Git mergetool', '', ft)
  exe s:Map('n', 'cm?', ':<C-U>help fugitive_cm<CR>', '<silent>', ft)

  exe s:Map('n', 'cz<Space>', ':Git stash<Space>', '', ft)
  exe s:Map('n', 'cz<CR>', ':Git stash<CR>', '', ft)
  exe s:Map('n', 'cza', ':<C-U>Git stash apply --quiet --index stash@{<C-R>=v:count<CR>}<CR>', '', ft)
  exe s:Map('n', 'czA', ':<C-U>Git stash apply --quiet stash@{<C-R>=v:count<CR>}<CR>', '', ft)
  exe s:Map('n', 'czp', ':<C-U>Git stash pop --quiet --index stash@{<C-R>=v:count<CR>}<CR>', '', ft)
  exe s:Map('n', 'czP', ':<C-U>Git stash pop --quiet stash@{<C-R>=v:count<CR>}<CR>', '', ft)
  exe s:Map('n', 'czs', ':<C-U>Git stash push --staged<CR>', '', ft)
  exe s:Map('n', 'czv', ':<C-U>exe "Gedit" fugitive#RevParse("stash@{" . v:count . "}")<CR>', '<silent>', ft)
  exe s:Map('n', 'czw', ':<C-U>Git stash push --keep-index<C-R>=v:count > 1 ? " --all" : v:count ? " --include-untracked" : ""<CR><CR>', '', ft)
  exe s:Map('n', 'czz', ':<C-U>Git stash push <C-R>=v:count > 1 ? " --all" : v:count ? " --include-untracked" : ""<CR><CR>', '', ft)
  exe s:Map('n', 'cz?', ':<C-U>help fugitive_cz<CR>', '<silent>', ft)

  exe s:Map('n', 'co<Space>', ':Git checkout<Space>', '', ft)
  exe s:Map('n', 'co<CR>', ':Git checkout<CR>', '', ft)
  exe s:Map('n', 'coo', ':<C-U>Git checkout <C-R>=substitute(<SID>SquashArgument(),"^$",get(<SID>TempState(),"filetype","") ==# "git" ? expand("<cfile>") : "","")<CR> --<CR>', '', ft)
  exe s:Map('n', 'co?', ':<C-U>help fugitive_co<CR>', '<silent>', ft)

  exe s:Map('n', 'cb<Space>', ':Git branch<Space>', '', ft)
  exe s:Map('n', 'cb<CR>', ':Git branch<CR>', '', ft)
  exe s:Map('n', 'cb?', ':<C-U>help fugitive_cb<CR>', '<silent>', ft)

  exe s:Map('n', 'r<Space>', ':Git rebase<Space>', '', ft)
  exe s:Map('n', 'r<CR>', ':Git rebase<CR>', '', ft)
  exe s:Map('n', 'ri', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><CR>', '<silent>', ft)
  exe s:Map('n', 'rf', ':<C-U>Git -c sequence.editor=true rebase --interactive --autosquash<C-R>=<SID>RebaseArgument()<CR><CR>', '<silent>', ft)
  exe s:Map('n', 'ru', ':<C-U>Git rebase --interactive @{upstream}<CR>', '<silent>', ft)
  exe s:Map('n', 'rp', ':<C-U>Git rebase --interactive @{push}<CR>', '<silent>', ft)
  exe s:Map('n', 'rw', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/reword/e<CR>', '<silent>', ft)
  exe s:Map('n', 'rm', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/edit/e<CR>', '<silent>', ft)
  exe s:Map('n', 'rd', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/drop/e<CR>', '<silent>', ft)
  exe s:Map('n', 'rk', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/drop/e<CR>', '<silent>', ft)
  exe s:Map('n', 'rx', ':<C-U>Git rebase --interactive<C-R>=<SID>RebaseArgument()<CR><Bar>s/^pick/drop/e<CR>', '<silent>', ft)
  exe s:Map('n', 'rr', ':<C-U>Git rebase --continue<CR>', '<silent>', ft)
  exe s:Map('n', 'rs', ':<C-U>Git rebase --skip<CR>', '<silent>', ft)
  exe s:Map('n', 're', ':<C-U>Git rebase --edit-todo<CR>', '<silent>', ft)
  exe s:Map('n', 'ra', ':<C-U>Git rebase --abort<CR>', '<silent>', ft)
  exe s:Map('n', 'r?', ':<C-U>help fugitive_r<CR>', '<silent>', ft)
endfunction

function! fugitive#MapJumps(...) abort
  if !&modifiable
    if get(b:, 'fugitive_type', '') ==# 'blob'
      let blame_tail = '<C-R>=v:count ? " --reverse" : ""<CR><CR>'
      exe s:Map('n', '<2-LeftMouse>', ':<C-U>0,1Git ++curwin blame' . blame_tail, '<silent>')
      exe s:Map('n', '<CR>', ':<C-U>0,1Git ++curwin blame' . blame_tail, '<silent>')
      exe s:Map('n', 'o',    ':<C-U>0,1Git blame' . blame_tail, '<silent>')
      exe s:Map('n', 'p',    ':<C-U>0,1Git blame!' . blame_tail, '<silent>')
      if has('patch-7.4.1898')
        exe s:Map('n', 'gO',   ':<C-U>vertical 0,1Git blame' . blame_tail, '<silent>')
        exe s:Map('n', 'O',    ':<C-U>tab 0,1Git blame' . blame_tail, '<silent>')
      else
        exe s:Map('n', 'gO',   ':<C-U>0,4Git blame' . blame_tail, '<silent>')
        exe s:Map('n', 'O',    ':<C-U>0,5Git blame' . blame_tail, '<silent>')
      endif

      call s:Map('n', 'D', ":echoerr 'fugitive: D has been removed in favor of dd'<CR>", '<silent><unique>')
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
        call s:Map('n', '<C-P>', ':exe <SID>PreviousItem(v:count1)<Bar>echohl WarningMsg<Bar>echo "CTRL-P is deprecated in favor of ("<Bar>echohl NONE<CR>', '<unique>')
        call s:Map('n', '<C-N>', ':exe <SID>NextItem(v:count1)<Bar>echohl WarningMsg<Bar>echo "CTRL-N is deprecated in favor of )"<Bar>echohl NONE<CR>', '<unique>')
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
    call s:Map('n', 'S',    ':<C-U>echoerr "Use gO"<CR>', '<silent><unique>')
    call s:Map('n', 'dq', ":<C-U>call fugitive#DiffClose()<CR>", '<silent>')
    call s:Map('n', '-', ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>NavigateUp(v:count1))<Bar> if getline(1) =~# '^tree \x\{40,\}$' && empty(getline(2))<Bar>call search('^'.escape(expand('#:t'),'.*[]~\').'/\=$','wc')<Bar>endif<CR>", '<silent>')
    call s:Map('n', 'P',     ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit().'^'.v:count1.<SID>Relative(':'))<CR>", '<silent>')
    call s:Map('n', '~',     ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit().'~'.v:count1.<SID>Relative(':'))<CR>", '<silent>')
    call s:Map('n', 'C',     ":<C-U>exe 'Gedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>", '<silent>')
    call s:Map('n', 'cp',    ":<C-U>echoerr 'Use gC'<CR>", '<silent><unique>')
    call s:Map('n', 'gC',    ":<C-U>exe 'Gpedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>", '<silent>')
    call s:Map('n', 'gc',    ":<C-U>exe 'Gpedit ' . <SID>fnameescape(<SID>ContainingCommit())<CR>", '<silent>')
    call s:Map('n', 'gi',    ":<C-U>exe 'Gsplit' (v:count ? '.gitignore' : '.git/info/exclude')<CR>", '<silent>')
    call s:Map('x', 'gi',    ":<C-U>exe 'Gsplit' (v:count ? '.gitignore' : '.git/info/exclude')<CR>", '<silent>')

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
  call s:MapGitOps(0)
endfunction

function! fugitive#GX() abort
  try
    let results = &filetype ==# 'fugitive' ? s:CfilePorcelain() : &filetype ==# 'git' ? s:cfile() : []
    if len(results) && len(results[0])
      return FugitiveReal(s:Generate(results[0]))
    endif
  catch /^fugitive:/
  endtry
  return expand(get(g:, 'netrw_gx', expand('<cfile>')))
endfunction

function! s:CfilePorcelain(...) abort
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

function! fugitive#PorcelainCfile() abort
  let file = fugitive#Find(s:CfilePorcelain()[0])
  return empty(file) ? fugitive#Cfile() : s:fnameescape(file)
endfunction

function! s:StatusCfile(...) abort
  let tree = s:Tree()
  if empty(tree)
    return []
  endif
  let lead = s:cpath(tree, getcwd()) ? './' : tree . '/'
  if getline('.') =~# '^.\=\trenamed:.* -> '
    return [lead . matchstr(getline('.'),' -> \zs.*')]
  elseif getline('.') =~# '^.\=\t\(\k\| \)\+\p\?: *.'
    return [lead . matchstr(getline('.'),': *\zs.\{-\}\ze\%( ([^()[:digit:]]\+)\)\=$')]
  elseif getline('.') =~# '^.\=\t.'
    return [lead . matchstr(getline('.'),'\t\zs.*')]
  elseif getline('.') =~# ': needs merge$'
    return [lead . matchstr(getline('.'),'.*\ze: needs merge$')]
  elseif getline('.') =~# '^\%(. \)\=Not currently on any branch.$'
    return ['HEAD']
  elseif getline('.') =~# '^\%(. \)\=On branch '
    return ['refs/heads/'.getline('.')[12:]]
  elseif getline('.') =~# "^\\%(. \\)\=Your branch .*'"
    return [matchstr(getline('.'),"'\\zs\\S\\+\\ze'")]
  else
    return []
  endif
endfunction

function! fugitive#MessageCfile() abort
  let file = fugitive#Find(get(s:StatusCfile(), 0, ''))
  return empty(file) ? fugitive#Cfile() : s:fnameescape(file)
endfunction

function! s:BranchCfile(result) abort
  return matchstr(getline('.'), '^. \zs\S\+')
endfunction

let s:diff_header_pattern = '^diff --git \%("\=[abciow12]/.*\|/dev/null\) \%("\=[abciow12]/.*\|/dev/null\)$'
function! s:cfile() abort
  let temp_state = s:TempState()
  let name = substitute(get(get(temp_state, 'args', []), 0, ''), '\%(^\|-\)\(\l\)', '\u\1', 'g')
  if exists('*s:' . name . 'Cfile')
    let cfile = s:{name}Cfile(temp_state)
    if !empty(cfile)
      return type(cfile) == type('') ? [cfile] : cfile
    endif
  endif
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
    if empty(myhash) && get(temp_state, 'filetype', '') ==# 'git'
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

      elseif getline('.') =~# '^\%([|/\\_ ]*\*[|/\\_ ]*\)\=commit \x\{40,\}\>'
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

      elseif getline('.') =~# '^[A-Z]\d*\t\S' && len(myhash)
        let files = split(getline('.'), "\t")[1:-1]
        let ref = 'b/' . files[-1]
        if getline('.') =~# '^D'
          let ref = 'a/' . files[0]
        elseif getline('.') !~# '^A'
          let dcmds = ['', 'Gdiffsplit! >' . myhash . '^:' . fnameescape(files[0])]
        endif

      elseif getline('.') =~# '^[+-]'
        let [header_lnum, old_lnum, new_lnum] = s:HunkPosition(line('.'))
        if new_lnum > 0
          let ref = s:ParseDiffHeader(getline(search(s:diff_header_pattern, 'bnW')))[1]
          let dcmds = [new_lnum, 'normal!zv']
        elseif old_lnum > 0
          let ref = s:ParseDiffHeader(getline(search(s:diff_header_pattern, 'bnW')))[0]
          let dcmds = [old_lnum, 'normal!zv']
        else
          let ref = fugitive#Unquote(matchstr(getline('.'), '\C[+-]\{3\} \zs"\=[abciow12]/.*'))
        endif

      elseif getline('.') =~# '^rename from '
        let ref = 'a/'.getline('.')[12:]
      elseif getline('.') =~# '^rename to '
        let ref = 'b/'.getline('.')[10:]

      elseif getline('.') =~# '^@@ -\d\+\%(,\d\+\)\= +\d\+'
        let diff = getline(search(s:diff_header_pattern, 'bcnW'))
        let offset = matchstr(getline('.'), '+\zs\d\+')

        let [dref, ref] = s:ParseDiffHeader(diff)
        let dcmd = 'Gdiffsplit! +'.offset

      elseif getline('.') =~# s:diff_header_pattern
        let [dref, ref] = s:ParseDiffHeader(getline('.'))
        let dcmd = 'Gdiffsplit!'

      elseif getline('.') =~# '^index ' && getline(line('.')-1) =~# s:diff_header_pattern
        let [dref, ref] = s:ParseDiffHeader(getline(line('.') - '.'))
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
        return [ref, dcmd . ' >' . s:fnameescape(dref)] + dcmds
      elseif ref != ""
        return [ref] + dcmds
      endif

    endif
    return []
  endtry
endfunction

function! s:GF(mode) abort
  try
    let results = &filetype ==# 'fugitive' ? s:CfilePorcelain() : &filetype ==# 'gitcommit' ? s:StatusCfile() : s:cfile()
  catch /^fugitive:/
    return 'echoerr ' . string(v:exception)
  endtry
  if len(results) > 1
    let cmd = 'G' . a:mode .
          \ (empty(results[1]) ? '' : ' +' . s:PlusEscape(results[1])) . ' ' .
          \ fnameescape(results[0])
    let tail = join(map(results[2:-1], '"|" . v:val'), '')
    if a:mode ==# 'pedit' && len(tail)
      return cmd . '|wincmd P|exe ' . string(tail[1:-1]) . '|wincmd p'
    else
      return cmd . tail
    endif
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
    if !empty(s:TempState())
      let cfile = s:TempDotMap()
      if !empty(cfile)
        return fnameescape(s:Generate(cfile))
      endif
    endif
    let cfile = expand('<cfile>')
    if &includeexpr =~# '\<v:fname\>'
      sandbox let cfile = eval(substitute(&includeexpr, '\C\<v:fname\>', '\=string(cfile)', 'g'))
    endif
    return cfile
  elseif len(results) > 1
    let pre = '+' . join(map(results[1:-1], 'escape(v:val, " ")'), '\|') . ' '
  endif
  return pre . fnameescape(s:Generate(results[0]))
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
  let status .= '('.fugitive#Head(7, dir).')'
  return '[Git'.status.']'
endfunction

function! fugitive#statusline(...) abort
  return fugitive#Statusline()
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
      if filename ==# '' && line =~# '^[+-]\{3\} "\=[abciow12]/'
        let filename = fugitive#Unquote(line[4:-1])[2:-1]
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
      let filename = fugitive#Unquote(matchstr(line_foldstart, '^diff .\{-\} \zs"\=[abciow12]/\zs.*\ze "\=[abciow12]/'))[2:-1]
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
  elseif &filetype ==# 'fugitive' && line_foldstart =~# '^[A-Z][a-z].* (\d\+)$'
    let c = +matchstr(line_foldstart, '(\zs\d\+\ze)$')
    return '+-' . v:folddashes . printf('%3d item', c) . (c == 1 ? ':  ' : 's: ') . matchstr(line_foldstart, '.*\ze (\d\+)$')
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

" Section: End
