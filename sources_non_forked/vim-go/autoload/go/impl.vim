function! go#impl#Impl(...)
  let binpath = go#path#CheckBinPath('impl')
  if empty(binpath)
    return
  endif

  let recv = ""
  let iface = ""

  if a:0 == 0
    " user didn't passed anything,  just called ':GoImpl'
    let receiveType = expand("<cword>")
    let recv = printf("%s *%s", tolower(receiveType)[0], receiveType)
    let iface = input("vim-go: generating method stubs for interface: ")
    redraw!
    if empty(iface)
      call go#util#EchoError('usage: interface type is not provided')
      return
    endif
  elseif a:0 == 1
    " we assume the user only passed the interface type, 
    " i.e: ':GoImpl io.Writer'
    let receiveType = expand("<cword>")
    let recv = printf("%s *%s", tolower(receiveType)[0], receiveType)
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

  let result = go#util#System(printf("%s '%s' '%s'", binpath, recv, iface))
  if go#util#ShellError() != 0
    call go#util#EchoError(result)
    return
  endif

  if result ==# ''
    return
  end

  let pos = getpos('.')
  put ='' 
  put =result
  call setpos('.', pos)
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

function! s:root_dirs()
  let dirs = []
  let root = go#util#goroot()
  if root !=# '' && isdirectory(root)
    call add(dirs, root)
  endif

  let paths = map(split(go#util#gopath(), go#util#PathListSep()), "substitute(v:val, '\\\\', '/', 'g')")
  if go#util#ShellError()
    return []
  endif

  if !empty(filter(paths, 'isdirectory(v:val)'))
    call extend(dirs, paths)
  endif

  return dirs
endfunction

function! s:go_packages(dirs)
  let pkgs = []
  for d in a:dirs
    let pkg_root = expand(d . '/pkg/' . go#util#osarch())
    call extend(pkgs, split(globpath(pkg_root, '**/*.a', 1), "\n"))
  endfor
  return map(pkgs, "fnamemodify(v:val, ':t:r')")
endfunction

function! s:interface_list(pkg)
  let contents = split(go#util#System('go doc ' . a:pkg), "\n")
  if go#util#ShellError()
    return []
  endif

  call filter(contents, 'v:val =~# ''^type\s\+\h\w*\s\+interface''')
  return map(contents, 'a:pkg . "." . matchstr(v:val, ''^type\s\+\zs\h\w*\ze\s\+interface'')')
endfunction

" Complete package and interface for {interface}
function! go#impl#Complete(arglead, cmdline, cursorpos)
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
