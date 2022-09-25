" fugitive.vim - A Git wrapper so awesome, it should be illegal
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      3.7
" GetLatestVimScripts: 2975 1 :AutoInstall: fugitive.vim

if exists('g:loaded_fugitive')
  finish
endif
let g:loaded_fugitive = 1

let s:bad_git_dir = '/$\|^fugitive:'

" FugitiveGitDir() returns the detected Git dir for the given buffer number,
" or the current buffer if no argument is passed.  This will be an empty
" string if no Git dir was found.  Use !empty(FugitiveGitDir()) to check if
" Fugitive is active in the current buffer.  Do not rely on this for direct
" filesystem access; use FugitiveFind('.git/whatever') instead.
function! FugitiveGitDir(...) abort
  if v:version < 704
    return ''
  elseif !a:0 || type(a:1) == type(0) && a:1 < 0 || a:1 is# get(v:, 'true', -1)
    if exists('g:fugitive_event')
      return g:fugitive_event
    endif
    let dir = get(b:, 'git_dir', '')
    if empty(dir) && (empty(bufname('')) || &buftype =~# '^\%(nofile\|acwrite\|quickfix\|terminal\|prompt\)$')
      return FugitiveExtractGitDir(getcwd())
    elseif (!exists('b:git_dir') || b:git_dir =~# s:bad_git_dir) && &buftype =~# '^\%(nowrite\)\=$'
      let b:git_dir = FugitiveExtractGitDir(bufnr(''))
      return b:git_dir
    endif
    return dir =~# s:bad_git_dir ? '' : dir
  elseif type(a:1) == type(0) && a:1 isnot# 0
    if a:1 == bufnr('') && (!exists('b:git_dir') || b:git_dir =~# s:bad_git_dir) && &buftype =~# '^\%(nowrite\)\=$'
      let b:git_dir = FugitiveExtractGitDir(a:1)
    endif
    let dir = getbufvar(a:1, 'git_dir')
    return dir =~# s:bad_git_dir ? '' : dir
  elseif type(a:1) == type('')
    return substitute(s:Slash(a:1), '/$', '', '')
  elseif type(a:1) == type({})
    return get(a:1, 'fugitive_dir', get(a:1, 'git_dir', ''))
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
  if type(file) ==# type({})
    let dir = FugitiveGitDir(file)
    let tree = s:Tree(dir)
    return s:VimSlash(empty(tree) ? dir : tree)
  elseif file =~# '^\a\a\+:' || a:0 > 1
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
  if a:0 && (type(a:1) ==# type({}) || type(a:1) ==# type(0))
    return call('fugitive#Find', a:000[1:-1] + [FugitiveGitDir(a:1)])
  else
    return fugitive#Find(a:0 ? a:1 : bufnr(''), FugitiveGitDir(a:0 > 1 ? a:2 : -1))
  endif
endfunction

" FugitiveParse() takes a fugitive:// URL and returns a 2 element list
" containing an object name ("commit:file") and the Git dir.  It's effectively
" the inverse of FugitiveFind().
function! FugitiveParse(...) abort
  let path = s:Slash(a:0 ? a:1 : @%)
  if path !~# '^fugitive://'
    return ['', '']
  endif
  let [rev, dir] = fugitive#Parse(path)
  if !empty(dir)
    return [rev, dir]
  endif
  throw 'fugitive: invalid Fugitive URL ' . path
endfunction

" FugitiveGitVersion() queries the version of Git in use.  Pass up to 3
" arguments to return a Boolean of whether a certain minimum version is
" available (FugitiveGitVersion(2,3,4) checks for 2.3.4 or higher) or no
" arguments to get a raw string.
function! FugitiveGitVersion(...) abort
  return call('fugitive#GitVersion', a:000)
endfunction

" FugitiveResult() returns an object encapsulating the result of the most
" recent :Git command.  Will be empty if no result is available.  During a
" User FugitiveChanged event, this is guaranteed to correspond to the :Git
" command that triggered the event, or be empty if :Git was not the trigger.
" Pass in the name of a temp buffer to get the result object for that command
" instead.  Contains the following keys:
"
" * "args": List of command arguments, starting with the subcommand.  Will be
"   empty for usages like :Git --help.
" * "git_dir": Git dir of the relevant repository.
" * "exit_status": The integer exit code of the process.
" * "flags": Flags passed directly to Git, like -c and --help.
" * "file": Path to file containing command output.  Not guaranteed to exist,
"   so verify with filereadable() before trying to access it.
function! FugitiveResult(...) abort
  return call('fugitive#Result', a:000)
endfunction

" FugitiveExecute() runs Git with a list of arguments and returns a dictionary
" with the following keys:
"
" * "exit_status": The integer exit code of the process.
" * "stdout": The stdout produced by the process, as a list of lines.
" * "stderr": The stdout produced by the process, as a list of lines.
"
" An optional second argument provides the Git dir, or the buffer number of a
" buffer with a Git dir.  The default is the current buffer.
"
" An optional final argument is a callback Funcref, for asynchronous
" execution.
function! FugitiveExecute(args, ...) abort
  return call('fugitive#Execute', [a:args] + a:000)
endfunction

" FugitiveShellCommand() turns an array of arugments into a Git command string
" which can be executed with functions like system() and commands like :!.
" Integer arguments will be treated as buffer numbers, and the appropriate
" relative path inserted in their place.
"
" An optional second argument provides the Git dir, or the buffer number of a
" buffer with a Git dir.  The default is the current buffer.
function! FugitiveShellCommand(...) abort
  return call('fugitive#ShellCommand', a:000)
endfunction

" FugitiveConfig() get returns an opaque structure that can be passed to other
" FugitiveConfig functions in lieu of a Git directory.  This can be faster
" when performing multiple config queries.  Do not rely on the internal
" structure of the return value as it is not guaranteed.  If you want a full
" dictionary of every config value, use FugitiveConfigGetRegexp('.*').
"
" An optional argument provides the Git dir, or the buffer number of a
" buffer with a Git dir.  The default is the current buffer.  Pass a blank
" string to limit to the global config.
function! FugitiveConfig(...) abort
  return call('fugitive#Config', a:000)
endfunction

" FugitiveConfigGet() retrieves a Git configuration value.  An optional second
" argument can be either the object returned by FugitiveConfig(), or a Git
" dir or buffer number to be passed along to FugitiveConfig().
function! FugitiveConfigGet(name, ...) abort
  return get(call('FugitiveConfigGetAll', [a:name] + (a:0 ? [a:1] : [])), 0, get(a:, 2, ''))
endfunction

" FugitiveConfigGetAll() is like FugitiveConfigGet() but returns a list of
" all values.
function! FugitiveConfigGetAll(name, ...) abort
  return call('fugitive#ConfigGetAll', [a:name] + a:000)
endfunction

" FugitiveConfigGetRegexp() retrieves a dictionary of all configuration values
" with a key matching the given pattern.  Like git config --get-regexp, but
" using a Vim regexp.  Second argument has same semantics as
" FugitiveConfigGet().
function! FugitiveConfigGetRegexp(pattern, ...) abort
  return call('fugitive#ConfigGetRegexp', [a:pattern] + a:000)
endfunction

" FugitiveRemoteUrl() retrieves the remote URL for the given remote name,
" defaulting to the current branch's remote or "origin" if no argument is
" given.  Similar to `git remote get-url`, but also attempts to resolve HTTP
" redirects and SSH host aliases.
"
" An optional second argument provides the Git dir, or the buffer number of a
" buffer with a Git dir.  The default is the current buffer.
function! FugitiveRemoteUrl(...) abort
  return call('fugitive#RemoteUrl', a:000)
endfunction

" FugitiveRemote() returns a data structure parsed from the remote URL.
" For example, for remote URL "https://me@example.com:1234/repo.git", the
" returned dictionary will contain the following:
"
" * "scheme": "https"
" * "authority": "user@example.com:1234"
" * "path": "/repo.git" (for SSH URLs this may be a relative path)
" * "pathname": "/repo.git" (always coerced to absolute path)
" * "host": "example.com:1234"
" * "hostname": "example.com"
" * "port": "1234"
" * "user": "me"
" * "path": "/repo.git"
" * "url": "https://me@example.com:1234/repo.git"
function! FugitiveRemote(...) abort
  return call('fugitive#Remote', a:000)
endfunction

" FugitiveDidChange() triggers a FugitiveChanged event and reloads the summary
" buffer for the current or given buffer number's repository.  You can also
" give the result of a FugitiveExecute() and that context will be made
" available inside the FugitiveChanged() event.
"
" Passing the special argument 0 (the number zero) softly expires summary
" buffers for all repositories.  This can be used after a call to system()
" with unclear implications.
function! FugitiveDidChange(...) abort
  return call('fugitive#DidChange', a:000)
endfunction

" FugitiveHead() retrieves the name of the current branch. If the current HEAD
" is detached, FugitiveHead() will return the empty string, unless the
" optional argument is given, in which case the hash of the current commit
" will be truncated to the given number of characters.
"
" An optional second argument provides the Git dir, or the buffer number of a
" buffer with a Git dir.  The default is the current buffer.
function! FugitiveHead(...) abort
  if a:0 && (type(a:1) ==# type({}) || type(a:1) ==# type('') && a:1 !~# '^\d\+$')
    let dir = FugitiveGitDir(a:1)
    let arg = get(a:, 2, 0)
  elseif a:0 > 1
    let dir = FugitiveGitDir(a:2)
    let arg = a:1
  else
    let dir = FugitiveGitDir()
    let arg = get(a:, 1, 0)
  endif
  if empty(dir)
    return ''
  endif
  return fugitive#Head(arg, dir)
endfunction

function! FugitivePath(...) abort
  if a:0 > 2 && type(a:1) ==# type({})
    return fugitive#Path(a:2, a:3, FugitiveGitDir(a:1))
  elseif a:0 && type(a:1) ==# type({})
    return FugitiveReal(a:0 > 1 ? a:2 : @%)
  elseif a:0 > 1
    return fugitive#Path(a:1, a:2, FugitiveGitDir(a:0 > 2 ? a:3 : -1))
  else
    return FugitiveReal(a:0 ? a:1 : @%)
  endif
endfunction

function! FugitiveStatusline(...) abort
  if empty(FugitiveGitDir(bufnr('')))
    return ''
  endif
  return fugitive#Statusline()
endfunction

let s:resolved_git_dirs = {}
function! FugitiveActualDir(...) abort
  let dir = call('FugitiveGitDir', a:000)
  if empty(dir)
    return ''
  endif
  if !has_key(s:resolved_git_dirs, dir)
    let s:resolved_git_dirs[dir] = s:ResolveGitDir(dir)
  endif
  return empty(s:resolved_git_dirs[dir]) ? dir : s:resolved_git_dirs[dir]
endfunction

let s:commondirs = {}
function! FugitiveCommonDir(...) abort
  let dir = call('FugitiveActualDir', a:000)
  if empty(dir)
    return ''
  endif
  if has_key(s:commondirs, dir)
    return s:commondirs[dir]
  endif
  if getfsize(dir . '/HEAD') >= 10
    let cdir = get(s:ReadFile(dir . '/commondir', 1), 0, '')
    if cdir =~# '^/\|^\a:/'
      let s:commondirs[dir] = s:Slash(FugitiveVimPath(cdir))
    elseif len(cdir)
      let s:commondirs[dir] = simplify(dir . '/' . cdir)
    else
      let s:commondirs[dir] = dir
    endif
  else
    let s:commondirs[dir] = dir
  endif
  return s:commondirs[dir]
endfunction

function! FugitiveWorkTree(...) abort
  let tree = s:Tree(FugitiveGitDir(a:0 ? a:1 : -1))
  if tree isnot# 0 || a:0 > 1
    return tree
  else
    return ''
  endif
endfunction

function! FugitiveIsGitDir(...) abort
  if !a:0 || type(a:1) !=# type('')
    return !empty(call('FugitiveGitDir', a:000))
  endif
  let path = substitute(a:1, '[\/]$', '', '') . '/'
  return len(path) && getfsize(path.'HEAD') > 10 && (
        \ isdirectory(path.'objects') && isdirectory(path.'refs') ||
        \ getftype(path.'commondir') ==# 'file')
endfunction

function! s:ReadFile(path, line_count) abort
  if v:version < 800 && !filereadable(a:path)
    return []
  endif
  try
    return readfile(a:path, 'b', a:line_count)
  catch
    return []
  endtry
endfunction

let s:worktree_for_dir = {}
let s:dir_for_worktree = {}
function! s:Tree(path) abort
  if a:path =~# '/\.git$'
    return len(a:path) ==# 5 ? '/' : a:path[0:-6]
  elseif a:path ==# ''
    return ''
  endif
  let dir = FugitiveActualDir(a:path)
  if !has_key(s:worktree_for_dir, dir)
    let s:worktree_for_dir[dir] = ''
    let ext_wtc_pat = 'v:val =~# "^\\s*worktreeConfig *= *\\%(true\\|yes\\|on\\|1\\) *$"'
    let config = s:ReadFile(dir . '/config', 50)
    if len(config)
      let ext_wtc_config = filter(copy(config), ext_wtc_pat)
      if len(ext_wtc_config) == 1 && filereadable(dir . '/config.worktree')
         let config += s:ReadFile(dir . '/config.worktree', 50)
      endif
    else
      let worktree = fnamemodify(FugitiveVimPath(get(s:ReadFile(dir . '/gitdir', 1), '0', '')), ':h')
      if worktree ==# '.'
        unlet! worktree
      endif
      if len(filter(s:ReadFile(FugitiveCommonDir(dir) . '/config', 50), ext_wtc_pat))
        let config = s:ReadFile(dir . '/config.worktree', 50)
      endif
    endif
    if len(config)
      let wt_config = filter(copy(config), 'v:val =~# "^\\s*worktree *="')
      if len(wt_config)
        let worktree = FugitiveVimPath(matchstr(wt_config[0], '= *\zs.*'))
      elseif !exists('worktree')
        call filter(config,'v:val =~# "^\\s*bare *= *true *$"')
        if empty(config)
          let s:worktree_for_dir[dir] = 0
        endif
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
        call add(s:ceiling_directories, s:Slash(resolve(dir)))
      else
        call add(s:ceiling_directories, s:Slash(dir))
      endif
    endfor
  endif
  return s:ceiling_directories + get(g:, 'ceiling_directories', [s:Slash(fnamemodify(expand('~'), ':h'))])
endfunction

function! s:ResolveGitDir(git_dir) abort
  let type = getftype(a:git_dir)
  if type ==# 'dir' && FugitiveIsGitDir(a:git_dir)
    return a:git_dir
  elseif type ==# 'link' && FugitiveIsGitDir(a:git_dir)
    return resolve(a:git_dir)
  elseif type !=# ''
    let line = get(s:ReadFile(a:git_dir, 1), 0, '')
    let file_dir = s:Slash(FugitiveVimPath(matchstr(line, '^gitdir: \zs.*')))
    if file_dir !~# '^/\|^\a:\|^$' && a:git_dir =~# '/\.git$' && FugitiveIsGitDir(a:git_dir[0:-5] . file_dir)
      return simplify(a:git_dir[0:-5] . file_dir)
    elseif file_dir =~# '^/\|^\a:' && FugitiveIsGitDir(file_dir)
      return file_dir
    endif
  endif
  return ''
endfunction

function! FugitiveExtractGitDir(path) abort
  if type(a:path) ==# type({})
    return get(a:path, 'fugitive_dir', get(a:path, 'git_dir', ''))
  elseif type(a:path) == type(0)
    let path = s:Slash(a:path > 0 ? bufname(a:path) : bufname(''))
  else
    let path = s:Slash(a:path)
  endif
  if path =~# '^fugitive://'
    return fugitive#Parse(path)[1]
  elseif empty(path)
    return ''
  endif
  let pre = substitute(matchstr(path, '^\a\a\+\ze:'), '^.', '\u&', '')
  if len(pre) && exists('*' . pre . 'Real')
    let path = {pre}Real(path)
  endif
  let root = s:Slash(fnamemodify(path, ':p:h'))
  let previous = ""
  let env_git_dir = len($GIT_DIR) ? s:Slash(simplify(fnamemodify(FugitiveVimPath($GIT_DIR), ':p:s?[\/]$??'))) : ''
  call s:Tree(env_git_dir)
  let ceiling_directories = s:CeilingDirectories()
  while root !=# previous && root !~# '^$\|^//[^/]*$'
    if index(ceiling_directories, root) >= 0
      break
    endif
    if root ==# $GIT_WORK_TREE && FugitiveIsGitDir(env_git_dir)
      return env_git_dir
    elseif has_key(s:dir_for_worktree, root)
      return s:dir_for_worktree[root]
    endif
    let dir = substitute(root, '[\/]$', '', '') . '/.git'
    let resolved = s:ResolveGitDir(dir)
    if !empty(resolved)
      let s:resolved_git_dirs[dir] = resolved
      return dir is# resolved || s:Tree(resolved) is# 0 ? dir : resolved
    elseif FugitiveIsGitDir(root)
      let s:resolved_git_dirs[root] = root
      return root
    endif
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile
  return ''
endfunction

function! FugitiveDetect(...) abort
  if v:version < 704
    return ''
  endif
  if exists('b:git_dir') && b:git_dir =~# '^$\|' . s:bad_git_dir
    unlet b:git_dir
  endif
  if !exists('b:git_dir')
    let b:git_dir = FugitiveExtractGitDir(a:0 ? a:1 : bufnr(''))
  endif
  return ''
endfunction

function! FugitiveGitPath(path) abort
  return s:Slash(a:path)
endfunction

if exists('+shellslash')

  function! s:Slash(path) abort
    return tr(a:path, '\', '/')
  endfunction

  function! s:VimSlash(path) abort
    return tr(a:path, '\/', &shellslash ? '//' : '\\')
  endfunction

  function FugitiveVimPath(path) abort
    return tr(a:path, '\/', &shellslash ? '//' : '\\')
  endfunction

else

  function! s:Slash(path) abort
    return a:path
  endfunction

  function! s:VimSlash(path) abort
    return a:path
  endfunction

  if has('win32unix') && filereadable('/git-bash.exe')
    function! FugitiveVimPath(path) abort
      return substitute(a:path, '^\(\a\):', '/\l\1', '')
    endfunction
  else
    function! FugitiveVimPath(path) abort
      return a:path
    endfunction
  endif

endif

function! s:ProjectionistDetect() abort
  let file = s:Slash(get(g:, 'projectionist_file', ''))
  let dir = FugitiveExtractGitDir(file)
  let base = matchstr(file, '^fugitive://.\{-\}//\x\+')
  if empty(base)
    let base = s:Tree(dir)
  endif
  if !empty(base)
    if exists('+shellslash') && !&shellslash
      let base = tr(base, '/', '\')
    endif
    let file = FugitiveFind('.git/info/projections.json', dir)
    if filereadable(file)
      call projectionist#append(base, file)
    endif
  endif
endfunction

let s:addr_other = has('patch-8.1.560') || has('nvim-0.5.0') ? '-addr=other' : ''
let s:addr_tabs  = has('patch-7.4.542') ? '-addr=tabs' : ''
let s:addr_wins  = has('patch-7.4.542') ? '-addr=windows' : ''

if exists(':G') != 2
  command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#Complete G   exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)
endif
command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#Complete Git exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)

if exists(':Gstatus') != 2 && get(g:, 'fugitive_legacy_commands', 0)
  exe 'command! -bang -bar     -range=-1' s:addr_other 'Gstatus exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
        \ '|echohl WarningMSG|echomsg ":Gstatus is deprecated in favor of :Git (with no arguments)"|echohl NONE'
endif

for s:cmd in ['Commit', 'Revert', 'Merge', 'Rebase', 'Pull', 'Push', 'Fetch', 'Blame']
  if exists(':G' . tolower(s:cmd)) != 2 && get(g:, 'fugitive_legacy_commands', 0)
    exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#' . s:cmd . 'Complete G' . tolower(s:cmd)
          \ 'echohl WarningMSG|echomsg ":G' . tolower(s:cmd) . ' is deprecated in favor of :Git ' . tolower(s:cmd) . '"|echohl NONE|'
          \ 'exe fugitive#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", "' . tolower(s:cmd) . ' " . <q-args>)'
  endif
endfor
unlet s:cmd

exe "command! -bar -bang -nargs=? -complete=customlist,fugitive#CdComplete Gcd  exe fugitive#Cd(<q-args>, 0)"
exe "command! -bar -bang -nargs=? -complete=customlist,fugitive#CdComplete Glcd exe fugitive#Cd(<q-args>, 1)"

exe 'command! -bang -nargs=? -range=-1' s:addr_wins '-complete=customlist,fugitive#GrepComplete Ggrep  exe fugitive#GrepCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bang -nargs=? -range=-1' s:addr_wins '-complete=customlist,fugitive#GrepComplete Glgrep exe fugitive#GrepCommand(0, <count> > 0 ? <count> : 0, +"<range>", <bang>0, "<mods>", <q-args>)'

exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete Gclog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "c")'
exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete GcLog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "c")'
exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete Gllog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "l")'
exe 'command! -bang -nargs=? -range=-1 -complete=customlist,fugitive#LogComplete GlLog :exe fugitive#LogCommand(<line1>,<count>,+"<range>",<bang>0,"<mods>",<q-args>, "l")'

exe 'command! -bar -bang -nargs=*                          -complete=customlist,fugitive#EditComplete   Ge       exe fugitive#Open("edit<bang>", 0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=*                          -complete=customlist,fugitive#EditComplete   Gedit    exe fugitive#Open("edit<bang>", 0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=*                          -complete=customlist,fugitive#EditComplete   Gpedit   exe fugitive#Open("pedit", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=* -range=-1' s:addr_other '-complete=customlist,fugitive#EditComplete   Gsplit   exe fugitive#Open((<count> > 0 ? <count> : "").(<count> ? "split" : "edit"), <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=* -range=-1' s:addr_other '-complete=customlist,fugitive#EditComplete   Gvsplit  exe fugitive#Open((<count> > 0 ? <count> : "").(<count> ? "vsplit" : "edit!"), <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=* -range=-1' s:addr_tabs  '-complete=customlist,fugitive#EditComplete   Gtabedit exe fugitive#Open((<count> >= 0 ? <count> : "")."tabedit", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=*                          -complete=customlist,fugitive#EditComplete   Gdrop    exe fugitive#DropCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'

if exists(':Gr') != 2
  exe 'command! -bar -bang -nargs=* -range=-1                -complete=customlist,fugitive#ReadComplete   Gr     exe fugitive#ReadCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
endif
exe 'command! -bar -bang -nargs=* -range=-1                -complete=customlist,fugitive#ReadComplete   Gread    exe fugitive#ReadCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'

exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gdiffsplit  exe fugitive#Diffsplit(1, <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Ghdiffsplit exe fugitive#Diffsplit(0, <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gvdiffsplit exe fugitive#Diffsplit(0, <bang>0, "vertical <mods>", <q-args>)'

exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gw     exe fugitive#WriteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gwrite exe fugitive#WriteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=* -complete=customlist,fugitive#EditComplete Gwq    exe fugitive#WqCommand(   <line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'

exe 'command! -bar -bang -nargs=0 GRemove exe fugitive#RemoveCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=0 GUnlink exe fugitive#UnlinkCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=0 GDelete exe fugitive#DeleteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#CompleteObject GMove   exe fugitive#MoveCommand(  <line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#RenameComplete GRename exe fugitive#RenameCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
if exists(':Gremove') != 2 && get(g:, 'fugitive_legacy_commands', 0)
  exe 'command! -bar -bang -nargs=0 Gremove exe fugitive#RemoveCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
        \ '|echohl WarningMSG|echomsg ":Gremove is deprecated in favor of :GRemove"|echohl NONE'
elseif exists(':Gremove') != 2 && !exists('g:fugitive_legacy_commands')
  exe 'command! -bar -bang -nargs=0 Gremove echoerr ":Gremove has been removed in favor of :GRemove"'
endif
if exists(':Gdelete') != 2 && get(g:, 'fugitive_legacy_commands', 0)
  exe 'command! -bar -bang -nargs=0 Gdelete exe fugitive#DeleteCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
        \ '|echohl WarningMSG|echomsg ":Gdelete is deprecated in favor of :GDelete"|echohl NONE'
elseif exists(':Gdelete') != 2 && !exists('g:fugitive_legacy_commands')
  exe 'command! -bar -bang -nargs=0 Gdelete echoerr ":Gremove has been removed in favor of :GRemove"'
endif
if exists(':Gmove') != 2 && get(g:, 'fugitive_legacy_commands', 0)
  exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#CompleteObject Gmove   exe fugitive#MoveCommand(  <line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
        \ '|echohl WarningMSG|echomsg ":Gmove is deprecated in favor of :GMove"|echohl NONE'
elseif exists(':Gmove') != 2 && !exists('g:fugitive_legacy_commands')
  exe 'command! -bar -bang -nargs=? -complete=customlist,fugitive#CompleteObject Gmove'
        \ 'echoerr ":Gmove has been removed in favor of :GMove"'
endif
if exists(':Grename') != 2 && get(g:, 'fugitive_legacy_commands', 0)
  exe 'command! -bar -bang -nargs=1 -complete=customlist,fugitive#RenameComplete Grename exe fugitive#RenameCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
        \ '|echohl WarningMSG|echomsg ":Grename is deprecated in favor of :GRename"|echohl NONE'
elseif exists(':Grename') != 2 && !exists('g:fugitive_legacy_commands')
  exe 'command! -bar -bang -nargs=? -complete=customlist,fugitive#RenameComplete Grename'
        \ 'echoerr ":Grename has been removed in favor of :GRename"'
endif

exe 'command! -bar -bang -range=-1 -nargs=* -complete=customlist,fugitive#CompleteObject GBrowse exe fugitive#BrowseCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
if exists(':Gbrowse') != 2 && get(g:, 'fugitive_legacy_commands', 0)
  exe 'command! -bar -bang -range=-1 -nargs=* -complete=customlist,fugitive#CompleteObject Gbrowse exe fugitive#BrowseCommand(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)'
        \ '|if <bang>1|redraw!|endif|echohl WarningMSG|echomsg ":Gbrowse is deprecated in favor of :GBrowse"|echohl NONE'
elseif exists(':Gbrowse') != 2 && !exists('g:fugitive_legacy_commands')
  exe 'command! -bar -bang -range=-1 -nargs=* -complete=customlist,fugitive#CompleteObject Gbrowse'
        \ 'echoerr ":Gbrowse has been removed in favor of :GBrowse"'
endif

if v:version < 704
  finish
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

  autocmd BufNewFile,BufReadPost *
        \ if exists('b:git_dir') && b:git_dir =~# '^$\|' . s:bad_git_dir |
        \   unlet b:git_dir |
        \ endif
  autocmd FileType           netrw
        \ if exists('b:git_dir') && b:git_dir =~# '^$\|' . s:bad_git_dir |
        \   unlet b:git_dir |
        \ endif
  autocmd BufFilePost            *  unlet! b:git_dir

  autocmd FileType git
        \ call fugitive#MapCfile()
  autocmd FileType gitcommit
        \ call fugitive#MapCfile('fugitive#MessageCfile()')
  autocmd FileType git,gitcommit
        \ if &foldtext ==# 'foldtext()' |
        \    setlocal foldtext=fugitive#Foldtext() |
        \ endif
  autocmd FileType fugitive
        \ call fugitive#MapCfile('fugitive#PorcelainCfile()')
  autocmd FileType gitrebase
        \ let &l:include = '^\%(pick\|squash\|edit\|reword\|fixup\|drop\|[pserfd]\)\>' |
        \ if &l:includeexpr !~# 'Fugitive' |
        \   let &l:includeexpr = 'v:fname =~# ''^\x\{4,\}$'' && len(FugitiveGitDir()) ? FugitiveFind(v:fname) : ' .
        \     (len(&l:includeexpr) ? &l:includeexpr : 'v:fname') |
        \ endif |
        \ let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') . '|setl inex= inc='

  autocmd BufReadCmd index{,.lock} nested
        \ if FugitiveIsGitDir(expand('<amatch>:p:h')) |
        \   let b:git_dir = s:Slash(expand('<amatch>:p:h')) |
        \   exe fugitive#BufReadStatus(v:cmdbang) |
        \   echohl WarningMSG |
        \   echo "fugitive: Direct editing of .git/" . expand('%:t') . " is deprecated" |
        \   echohl NONE |
        \ elseif filereadable(expand('<amatch>')) |
        \   silent doautocmd BufReadPre |
        \   keepalt noautocmd read <amatch> |
        \   silent 1delete_ |
        \   silent doautocmd BufReadPost |
        \ else |
        \   silent doautocmd BufNewFile |
        \ endif

  autocmd BufReadCmd   fugitive://*          nested exe fugitive#BufReadCmd() |
        \ if &path =~# '^\.\%(,\|$\)' |
        \   let &l:path = substitute(&path, '^\.,\=', '', '') |
        \ endif
  autocmd BufWriteCmd  fugitive://*          nested exe fugitive#BufWriteCmd()
  autocmd FileReadCmd  fugitive://*          nested exe fugitive#FileReadCmd()
  autocmd FileWriteCmd fugitive://*          nested exe fugitive#FileWriteCmd()
  if exists('##SourceCmd')
    autocmd SourceCmd     fugitive://*       nested exe fugitive#SourceCmd()
  endif

  autocmd User Flags call Hoist('buffer', function('FugitiveStatusline'))

  autocmd User ProjectionistDetect call s:ProjectionistDetect()
augroup END

nmap <script><silent> <Plug>fugitive:y<C-G> :<C-U>call setreg(v:register, fugitive#Object(@%))<CR>
nmap <script> <Plug>fugitive: <Nop>

if get(g:, 'fugitive_no_maps')
  finish
endif

function! s:Map(mode, lhs, rhs, flags) abort
  let flags = a:flags . (a:rhs =~# '<Plug>' ? '' : '<script>') . '<nowait>'
  let head = a:lhs
  let tail = ''
  let keys = get(g:, a:mode.'remap', {})
  if len(keys) && type(keys) == type({})
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
  endif
  if empty(mapcheck(head.tail, a:mode))
    exe a:mode.'map' flags head.tail a:rhs
  endif
endfunction

call s:Map('c', '<C-R><C-G>', 'fnameescape(fugitive#Object(@%))', '<expr>')
call s:Map('n', 'y<C-G>', ':<C-U>call setreg(v:register, fugitive#Object(@%))<CR>', '<silent>')
