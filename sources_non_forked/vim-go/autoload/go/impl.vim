function! go#impl#Impl(...) abort
  let recv = ""
  let iface = ""
  let interactive = 0

  let pos = getpos('.')

  if a:0 is 0
    " Interactive mode if user didn't pass any arguments.
    let recv = s:getReceiver()
    let iface = input("vim-go: generating method stubs for interface: ")
    redraw!
    if empty(iface)
      call go#util#EchoError('usage: interface type is not provided')
      return
    endif
  elseif a:0 is 1
    " we assume the user only passed the interface type,
    " i.e: ':GoImpl io.Writer'
    let recv = s:getReceiver()
    let iface = a:1
  elseif a:0 > 2
    " user passed receiver and interface type both,
    " i.e: 'GoImpl f *Foo io.Writer'
    let recv = join(a:000[:-2], ' ')
    let iface = a:000[-1]
  else
    call go#util#EchoError('usage: GoImpl {receiver} {interface}')
    return
  endif

  " Make sure we put the generated code *after* the struct.
  if getline(".") =~ "struct "
    normal! $%
  endif

  try
    let dirname = fnameescape(expand('%:p:h'))
    let [result, err] = go#util#Exec(['impl', '-dir', dirname, recv, iface])
    let result = substitute(result, "\n*$", "", "")
    if err
      call go#util#EchoError(result)
      return
    endif

    if result is# ''
      return
    end

    put =''
    silent put =result
  finally
    call setpos('.', pos)
  endtry
endfunction

function! s:getReceiver()
  let receiveType = expand("<cword>")
  if receiveType == "type"
    normal! w
    let receiveType = expand("<cword>")
  elseif receiveType == "struct"
    normal! ge
    let receiveType = expand("<cword>")
  endif
  return printf("%s *%s", tolower(receiveType)[0], receiveType)
endfunction

if exists('*uniq')
  function! s:uniq(list)
    return uniq(a:list)
  endfunction
else
  " Note: Believe that the list is sorted
  function! s:uniq(list)
    let i = len(a:list) - 1
    while 0 < i
      if a:list[i-1] ==# a:list[i]
        call remove(a:list, i)
        let i -= 2
      else
        let i -= 1
      endif
    endwhile
    return a:list
  endfunction
endif

function! s:root_dirs() abort
  let dirs = []
  let root = go#util#env("goroot")
  if root !=# '' && isdirectory(root)
    call add(dirs, root)
  endif

  let paths = map(split(go#util#env("gopath"), go#util#PathListSep()), "substitute(v:val, '\\\\', '/', 'g')")
  if !empty(filter(paths, 'isdirectory(v:val)'))
    call extend(dirs, paths)
  endif

  return dirs
endfunction

function! s:go_packages(dirs) abort
  let pkgs = []
  for d in a:dirs
    let pkg_root = expand(d . '/pkg/' . go#util#osarch())
    call extend(pkgs, split(globpath(pkg_root, '**/*.a', 1), "\n"))
  endfor
  return map(pkgs, "fnamemodify(v:val, ':t:r')")
endfunction

function! s:interface_list(pkg) abort
  let [contents, err] = go#util#Exec(['go', 'doc', a:pkg])
  if err
    return []
  endif

  let contents = split(contents, "\n")
  call filter(contents, 'v:val =~# ''^type\s\+\h\w*\s\+interface''')
  return map(contents, 'a:pkg . "." . matchstr(v:val, ''^type\s\+\zs\h\w*\ze\s\+interface'')')
endfunction

" Complete package and interface for {interface}
function! go#impl#Complete(arglead, cmdline, cursorpos) abort
  let words = split(a:cmdline, '\s\+', 1)
  if words[-1] ==# ''
    return s:uniq(sort(s:go_packages(s:root_dirs())))
  elseif words[-1] =~# '^\h\w*$'
    return s:uniq(sort(filter(s:go_packages(s:root_dirs()), 'stridx(v:val, words[-1]) == 0')))
  elseif words[-1] =~# '^\h\w*\.\%(\h\w*\)\=$'
    let [pkg, interface] = split(words[-1], '\.', 1)
    echomsg pkg
    return s:uniq(sort(filter(s:interface_list(pkg), 'v:val =~? words[-1]')))
  else
    return []
  endif
endfunction

" vim: sw=2 ts=2 et
