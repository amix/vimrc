" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" From "go list -h".
function! go#tool#ValidFiles(...)
  let l:list = ["GoFiles", "CgoFiles", "IgnoredGoFiles", "CFiles", "CXXFiles",
    \ "MFiles", "HFiles", "FFiles", "SFiles", "SwigFiles", "SwigCXXFiles",
    \ "SysoFiles", "TestGoFiles", "XTestGoFiles"]

  " Used as completion
  if len(a:000) > 0
    let l:list = filter(l:list, 'strpart(v:val, 0, len(a:1)) == a:1')
  endif

  return l:list
endfunction

function! go#tool#Files(...) abort
  if len(a:000) > 0
    let source_files = a:000
  else
    let source_files = ['GoFiles']
  endif

  let combined = ''
  for sf in source_files
    " Strip dot in case people used ":GoFiles .GoFiles".
    let sf = substitute(sf, '^\.', '', '')

    " Make sure the passed options are valid.
    if index(go#tool#ValidFiles(), sf) == -1
      echoerr "unknown source file variable: " . sf
    endif

    if go#util#IsWin()
      let combined .= '{{range $f := .' . sf . '}}{{$.Dir}}\{{$f}}{{printf \"\n\"}}{{end}}{{range $f := .CgoFiles}}{{$.Dir}}\{{$f}}{{printf \"\n\"}}{{end}}'
    else
      let combined .= "{{range $f := ." . sf . "}}{{$.Dir}}/{{$f}}{{printf \"\\n\"}}{{end}}{{range $f := .CgoFiles}}{{$.Dir}}/{{$f}}{{printf \"\\n\"}}{{end}}"
    endif
  endfor

  let [l:out, l:err] = go#util#ExecInDir(['go', 'list', '-tags', go#config#BuildTags(), '-f', l:combined])
  return split(l:out, '\n')
endfunction

function! go#tool#Deps() abort
  if go#util#IsWin()
    let format = '{{range $f := .Deps}}{{$f}}{{printf \"\n\"}}{{end}}'
  else
    let format = "{{range $f := .Deps}}{{$f}}\n{{end}}"
  endif
  let [l:out, l:err] = go#util#ExecInDir(['go', 'list', '-tags', go#config#BuildTags(), '-f', l:format])
  return split(l:out, '\n')
endfunction

function! go#tool#Imports() abort
  let imports = {}
  if go#util#IsWin()
    let format = '{{range $f := .Imports}}{{$f}}{{printf \"\n\"}}{{end}}'
  else
    let format = "{{range $f := .Imports}}{{$f}}{{printf \"\\n\"}}{{end}}"
  endif
  let [l:out, l:err] = go#util#ExecInDir(['go', 'list', '-tags', go#config#BuildTags(), '-f', l:format])
  if l:err != 0
    echo out
    return imports
  endif

  for package_path in split(out, '\n')
    let [l:out, l:err] = go#util#ExecInDir(['go', 'list', '-tags', go#config#BuildTags(), '-f', '{{.Name}}', l:package_path])
    if l:err != 0
      echo out
      return imports
    endif
    let package_name = substitute(l:out, '\n$', '', '')
    let imports[package_name] = package_path
  endfor

  return imports
endfunction

function! go#tool#Info(showstatus) abort
  let l:mode = go#config#InfoMode()
  if l:mode == 'gocode'
    call go#complete#Info(a:showstatus)
  elseif l:mode == 'guru'
    call go#guru#DescribeInfo(a:showstatus)
  elseif l:mode == 'gopls'
    call go#lsp#Info(a:showstatus)
  else
    call go#util#EchoError('go_info_mode value: '. l:mode .' is not valid. Valid values are: [gocode, guru, gopls]')
  endif
endfunction

function! go#tool#PackageName() abort
  let [l:out, l:err] = go#util#ExecInDir(['go', 'list', '-tags', go#config#BuildTags(), '-f', '{{.Name}}'])
  if l:err != 0
      return -1
  endif

  return split(out, '\n')[0]
endfunction

" Exists checks whether the given importpath exists or not. It returns 0 if
" the importpath exists under GOPATH.
function! go#tool#Exists(importpath) abort
    let [l:out, l:err] = go#util#ExecInDir(['go', 'list', a:importpath])
    if l:err != 0
        return -1
    endif

    return 0
endfunction

function! go#tool#DescribeBalloon()
  let l:fname = fnamemodify(bufname(v:beval_bufnr), ':p')

  let l:winid = win_getid()

  call win_gotoid(bufwinid(v:beval_bufnr))

  let [l:line, l:col] = go#lsp#lsp#Position(v:beval_lnum, v:beval_col)
  call go#lsp#Hover(l:fname, l:line, l:col, funcref('s:balloon', []))

  call win_gotoid(l:winid)
  return ''
endfunction

function! s:balloon(msg)
  let l:msg = a:msg
  if has('balloon_eval')
    if has('balloon_multiline')
      let l:msg = join(a:msg, "\n")
    else
      let l:msg = substitute(join(map(deepcopy(a:msg), 'substitute(v:val, "\t", "", "")'), '; '), '{;', '{', '')
    endif
  endif

  call balloon_show(l:msg)
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
