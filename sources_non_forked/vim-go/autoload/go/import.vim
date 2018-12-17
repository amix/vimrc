" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" Check out the docs for more information at /doc/vim-go.txt
"

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#import#SwitchImport(enabled, localname, path, bang) abort
  let view = winsaveview()
  let path = substitute(a:path, '^\s*\(.\{-}\)\s*$', '\1', '')

  " Quotes are not necessary, so remove them if provided.
  if path[0] == '"'
    let path = strpart(path, 1)
  endif
  if path[len(path)-1] == '"'
    let path = strpart(path, 0, len(path) - 1)
  endif

  " if given a trailing slash, eg. `github.com/user/pkg/`, remove it
  if path[len(path)-1] == '/'
    let path = strpart(path, 0, len(path) - 1)
  endif

  if path == ''
    call s:Error('Import path not provided')
    return
  endif

  if a:bang == "!"
    let [l:out, l:err] = go#util#Exec(['go', 'get', '-u', '-v', path])
    if err != 0
      call s:Error("Can't find import: " . path . ":" . out)
    endif
  endif
  let exists = go#tool#Exists(path)
  if exists == -1
    call s:Error("Can't find import: " . path)
    return
  endif

  " Extract any site prefix (e.g. github.com/).
  " If other imports with the same prefix are grouped separately,
  " we will add this new import with them.
  " Only up to and including the first slash is used.
  let siteprefix = matchstr(path, "^[^/]*/")

  let qpath = '"' . path . '"'
  if a:localname != ''
    let qlocalpath = a:localname . ' ' . qpath
  else
    let qlocalpath = qpath
  endif
  let indentstr = 0
  let packageline = -1 " Position of package name statement
  let appendline = -1  " Position to introduce new import
  let deleteline = -1  " Position of line with existing import
  let linesdelta = 0   " Lines added/removed

  " Find proper place to add/remove import.
  let line = 0
  while line <= line('$')
    let linestr = getline(line)

    if linestr =~# '^package\s'
      let packageline = line
      let appendline = line

    elseif linestr =~# '^import\s\+(\+)'
      let appendline = line
      let appendstr = qlocalpath
    elseif linestr =~# '^import\s\+('
      let appendstr = qlocalpath
      let indentstr = 1
      let appendline = line
      let firstblank = -1
      let lastprefix = ""
      while line <= line("$")
        let line = line + 1
        let linestr = getline(line)
        let m = matchlist(getline(line), '^\()\|\(\s\+\)\(\S*\s*\)"\(.\+\)"\)')
        if empty(m)
          if siteprefix == "" && a:enabled
            " must be in the first group
            break
          endif
          " record this position, but keep looking
          if firstblank < 0
            let firstblank = line
          endif
          continue
        endif
        if m[1] == ')'
          " if there's no match, add it to the first group
          if appendline < 0 && firstblank >= 0
            let appendline = firstblank
          endif
          break
        endif
        let lastprefix = matchstr(m[4], "^[^/]*/")
        if a:localname != '' && m[3] != ''
          let qlocalpath = printf('%-' . (len(m[3])-1) . 's %s', a:localname, qpath)
        endif
        let appendstr = m[2] . qlocalpath
        let indentstr = 0
        if m[4] == path
          let appendline = -1
          let deleteline = line
          break
        elseif m[4] < path
          " don't set candidate position if we have a site prefix,
          " we've passed a blank line, and this doesn't share the same
          " site prefix.
          if siteprefix == "" || firstblank < 0 || match(m[4], "^" . siteprefix) >= 0
            let appendline = line
          endif
        elseif siteprefix != "" && match(m[4], "^" . siteprefix) >= 0
          " first entry of site group
          let appendline = line - 1
          break
        endif
      endwhile
      break

    elseif linestr =~# '^import '
      if appendline == packageline
        let appendstr = 'import ' . qlocalpath
        let appendline = line - 1
      endif
      let m = matchlist(linestr, '^import\(\s\+\)\(\S*\s*\)"\(.\+\)"')
      if !empty(m)
        if m[3] == path
          let appendline = -1
          let deleteline = line
          break
        endif
        if m[3] < path
          let appendline = line
        endif
        if a:localname != '' && m[2] != ''
          let qlocalpath = printf("%s %" . len(m[2])-1 . "s", a:localname, qpath)
        endif
        let appendstr = 'import' . m[1] . qlocalpath
      endif

    elseif linestr =~# '^\(var\|const\|type\|func\)\>'
      break

    endif
    let line = line + 1
  endwhile

  " Append or remove the package import, as requested.
  if a:enabled
    if deleteline != -1
      call s:Error(qpath . ' already being imported')
    elseif appendline == -1
      call s:Error('No package line found')
    else
      if appendline == packageline
        call append(appendline + 0, '')
        call append(appendline + 1, 'import (')
        call append(appendline + 2, ')')
        let appendline += 2
        let linesdelta += 3
        let appendstr = qlocalpath
        let indentstr = 1
        call append(appendline, appendstr)
      elseif getline(appendline) =~# '^import\s\+(\+)'
        call setline(appendline, 'import (')
        call append(appendline + 0, appendstr)
        call append(appendline + 1, ')')
        let linesdelta -= 1
        let indentstr = 1
      else
        call append(appendline, appendstr)
      endif
      execute appendline + 1
      if indentstr
        execute 'normal! >>'
      endif
      let linesdelta += 1
    endif
  else
    if deleteline == -1
      call s:Error(qpath . ' not being imported')
    else
      execute deleteline . 'd'
      let linesdelta -= 1

      if getline(deleteline-1) =~# '^import\s\+(' && getline(deleteline) =~# '^)'
        " Delete empty import block
        let deleteline -= 1
        execute deleteline . "d"
        execute deleteline . "d"
        let linesdelta -= 2
      endif

      if getline(deleteline) == '' && getline(deleteline - 1) == ''
        " Delete spacing for removed line too.
        execute deleteline . "d"
        let linesdelta -= 1
      endif
    endif
  endif

  " Adjust view for any changes.
  let view.lnum += linesdelta
  let view.topline += linesdelta
  if view.topline < 0
    let view.topline = 0
  endif

  " Put buffer back where it was.
  call winrestview(view)

endfunction


function! s:Error(s) abort
  echohl Error | echo a:s | echohl None
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
