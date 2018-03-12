" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" This file provides a utility function that performs auto-completion of
" package names, for use by other commands.

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

function! go#package#Paths() abort
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

" ImportPath returns the import path in the current directory it was executed
function! go#package#ImportPath() abort
  let out = go#tool#ExecuteInDir("go list")
  if go#util#ShellError() != 0
    return -1
  endif

  let import_path = split(out, '\n')[0]

  " go list returns '_CURRENTDIRECTORY' if the directory is not inside GOPATH.
  " Check it and retun an error if that is the case
  if import_path[0] ==# '_'
    return -1
  endif

  return import_path
endfunction


function! go#package#FromPath(arg) abort
  let path = fnamemodify(resolve(a:arg), ':p')
  let dirs = go#package#Paths()

  for dir in dirs
    if len(dir) && match(path, dir) == 0
      let workspace = dir
      break
    endif
  endfor

  if !exists('workspace')
    return -1
  endif

  let path = substitute(path, '/*$', '', '')
  let workspace = substitute(workspace . '/src/', '/+', '', '')
  if isdirectory(path)
    return substitute(path, workspace, '', '')
  else
    return substitute(substitute(path, workspace, '', ''),
          \ '/' . fnamemodify(path, ':t'), '', '')
  endif
endfunction

function! go#package#CompleteMembers(package, member) abort
  silent! let content = go#util#System('godoc ' . a:package)
  if go#util#ShellError() || !len(content)
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

    let dirs = go#package#Paths()

    if len(dirs) == 0
        " should not happen
        return []
    endif

    let ret = {}
    for dir in dirs
        " this may expand to multiple lines
        let root = split(expand(dir . '/pkg/' . s:goos . '_' . s:goarch), "\n")
        call add(root, expand(dir . '/src'))
        for r in root
            for i in split(globpath(r, a:ArgLead.'*'), "\n")
                if isdirectory(i)
                    let i .= '/'
                elseif i !~ '\.a$'
                    continue
                endif
                let i = substitute(substitute(i[len(r)+1:], '[\\]', '/', 'g'),
                                  \ '\.a$', '', 'g')

                " without this the result can have duplicates in form of
                " 'encoding/json' and '/encoding/json/'
                let i = go#util#StripPathSep(i)

                let ret[i] = i
            endfor
        endfor
    endfor
    return sort(keys(ret))
endfunction

" vim: sw=2 ts=2 et
