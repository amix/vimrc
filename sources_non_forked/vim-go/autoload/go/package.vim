" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" This file provides a utility function that performs auto-completion of
" package names, for use by other commands.

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:goos = $GOOS
let s:goarch = $GOARCH

if len(s:goos) == 0
  if exists('g:golang_goos')
    let s:goos = g:golang_goos
  elseif has('win32') || has('win64')
    let s:goos = 'windows'
  elseif has('macunix')
    let s:goos = 'darwin'
  else
    let s:goos = '*'
  endif
endif

if len(s:goarch) == 0
  if exists('g:golang_goarch')
    let s:goarch = g:golang_goarch
  else
    let s:goarch = '*'
  endif
endif

function! s:paths() abort
  let dirs = []

  if !exists("s:goroot")
    if executable('go')
      let s:goroot = go#util#env("goroot")
      if go#util#ShellError() != 0
        echomsg '''go env GOROOT'' failed'
      endif
    else
      let s:goroot = $GOROOT
    endif
  endif

  if len(s:goroot) != 0 && isdirectory(s:goroot)
    let dirs += [s:goroot]
  endif

  let workspaces = split(go#path#Default(), go#util#PathListSep())
  if workspaces != []
    let dirs += workspaces
  endif

  return dirs
endfunction

function! s:module() abort
  let [l:out, l:err] = go#util#ExecInDir(['go', 'list', '-m', '-f', '{{.Dir}}'])
  if l:err != 0
    return {}
  endif
  let l:dir = split(l:out, '\n')[0]

  let [l:out, l:err] = go#util#ExecInDir(['go', 'list', '-m', '-f', '{{.Path}}'])
  if l:err != 0
    return {}
  endif
  let l:path = split(l:out, '\n')[0]

  return {'dir': l:dir, 'path': l:path}
endfunction

function! s:vendordirs() abort
  let l:vendorsuffix = go#util#PathSep() . 'vendor'
  let l:module = s:module()
  if empty(l:module)
    let [l:root, l:err] = go#util#ExecInDir(['go', 'list', '-f', '{{.Root}}'])
    if l:err != 0
      return []
    endif
    let l:root = split(l:root, '\n')[0] . go#util#PathSep() . 'src'

    let [l:dir, l:err] = go#util#ExecInDir(['go', 'list', '-f', '{{.Dir}}'])
    if l:err != 0
      return []
    endif
    let l:dir = split(l:dir, '\n')[0]

    let l:vendordirs = []
    while l:dir != l:root
      let l:vendordir = l:dir . l:vendorsuffix
      if isdirectory(l:vendordir)
        let l:vendordirs = add(l:vendordirs, l:vendordir)
      endif

      let l:dir = fnamemodify(l:dir, ':h')
    endwhile

    return l:vendordirs
  endif

  let l:vendordir = l:module.dir . l:vendorsuffix
  if !isdirectory(l:vendordir)
    return []
  endif
  return [l:vendordir]
endfunction

let s:import_paths = {}
" ImportPath returns the import path of the package for current buffer.
function! go#package#ImportPath() abort
  let dir = expand("%:p:h")
  if has_key(s:import_paths, dir)
    return s:import_paths[dir]
  endif

  let [l:out, l:err] = go#util#ExecInDir(['go', 'list'])
  if l:err != 0
    return -1
  endif

  let l:importpath = split(out, '\n')[0]

  " go list returns '_CURRENTDIRECTORY' if the directory is not inside GOPATH.
  " Check it and retun an error if that is the case
  if l:importpath[0] ==# '_'
    return -1
  endif

  let s:import_paths[dir] = l:importpath

  return l:importpath
endfunction


" go#package#FromPath returns the import path of arg. -1 is returned when arg
" does not specify a package. -2 is returned when arg is a relative path
" outside of GOPATH and not in a module.
function! go#package#FromPath(arg) abort
  let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let l:dir = getcwd()

  let l:path = a:arg
  if !isdirectory(l:path)
    let l:path = fnamemodify(l:path, ':h')
  endif

  execute l:cd fnameescape(l:path)
  let [l:out, l:err] = go#util#Exec(['go', 'list'])
  execute l:cd fnameescape(l:dir)
  if l:err != 0
    return -1
  endif

  let l:importpath = split(l:out, '\n')[0]

  " go list returns '_CURRENTDIRECTORY' if the directory is neither in GOPATH
  " nor in a module. Check it and retun an error if that is the case
  if l:importpath[0] ==# '_'
    return -2
  endif

  return l:importpath
endfunction

function! go#package#CompleteMembers(package, member) abort
  let [l:content, l:err] = go#util#Exec(['go', 'doc', a:package])
  if l:err || !len(content)
    return []
  endif

  let lines = filter(split(content, "\n"),"v:val !~ '^\\s\\+$'")
  try
    let mx1 = '^\s\+\(\S+\)\s\+=\s\+.*'
    let mx2 = '^\%(const\|var\|type\|func\) \([A-Z][^ (]\+\).*'
    let candidates = map(filter(copy(lines), 'v:val =~ mx1'),
          \ 'substitute(v:val, mx1, "\\1", "")')
          \ + map(filter(copy(lines), 'v:val =~ mx2'),
          \ 'substitute(v:val, mx2, "\\1", "")')
    return filter(candidates, '!stridx(v:val, a:member)')
  catch
    return []
  endtry
endfunction

function! go#package#Complete(ArgLead, CmdLine, CursorPos) abort
  let words = split(a:CmdLine, '\s\+', 1)

  " do not complete package members for these commands
  let neglect_commands = ["GoImportAs", "GoGuruScope"]

  if len(words) > 2 && index(neglect_commands, words[0]) == -1
    " Complete package members
    return go#package#CompleteMembers(words[1], words[2])
  endif

  let dirs = s:paths()
  let module = s:module()

  if len(dirs) == 0 && empty(module)
    " should not happen
    return []
  endif

  let vendordirs = s:vendordirs()

  let ret = {}
  for dir in dirs
    " this may expand to multiple lines
    let root = split(expand(dir . '/pkg/' . s:goos . '_' . s:goarch), "\n")
    let root = add(root, expand(dir . '/src'), )
    let root = extend(root, vendordirs)
    let root = add(root, module)
    for item in root
      " item may be a dictionary when operating in a module.
      if type(item) == type({})
        if empty(item)
          continue
        endif
        let dir = item.dir
        let path = item.path
      else
        let dir = item
        let path = item
      endif

      if !empty(module) && dir ==# module.dir
        if stridx(a:ArgLead, module.path) == 0
          if len(a:ArgLead) != len(module.path)
            let glob = globpath(module.dir, substitute(a:ArgLead, module.path . '/\?', '', '').'*')
          else
            let glob = module.dir
          endif
        elseif stridx(module.path, a:ArgLead) == 0 && stridx(module.path, '/', len(a:ArgLead)) < 0
          " use the module directory when a:ArgLead is contained in
          " module.path and module.path does not have any path segments after
          " a:ArgLead.
          let glob = module.dir
        else
          continue
        endif
      else
        let glob = globpath(dir, a:ArgLead.'*')
      endif
      for candidate in split(glob)
        if isdirectory(candidate)
          " TODO(bc): use wildignore instead of filtering out vendor
          " directories manually?
          if fnamemodify(candidate, ':t') == 'vendor'
            continue
          endif
          let candidate .= '/'
        elseif candidate !~ '\.a$'
          continue
        endif

        if dir !=# path
          let candidate = substitute(candidate, '^' . dir, path, 'g')
        else
          let candidate = candidate[len(dir)+1:]
        endif
        " replace a backslash with a forward slash and drop .a suffixes
        let candidate = substitute(substitute(candidate, '[\\]', '/', 'g'),
                          \ '\.a$', '', 'g')

        " without this the result can have duplicates in form of
        " 'encoding/json' and '/encoding/json/'
        let candidate = go#util#StripPathSep(candidate)

        let ret[candidate] = candidate
      endfor
    endfor
  endfor
  return sort(keys(ret))
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
