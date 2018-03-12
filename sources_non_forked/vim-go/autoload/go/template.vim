let s:current_file = expand("<sfile>")

function! go#template#create() abort
  let l:go_template_use_pkg = get(g:, 'go_template_use_pkg', 0)
  let l:root_dir = fnamemodify(s:current_file, ':h:h:h')

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  let l:package_name = -1

  if isdirectory(expand('%:p:h'))
    execute cd . fnameescape(expand('%:p:h'))
    let l:package_name = go#tool#PackageName()
  endif

  " if we can't figure out any package name(no Go files or non Go package
  " files) from the directory create the template or use the cwd
  " as the name
  if l:package_name == -1 && l:go_template_use_pkg != 1
    let l:filename = fnamemodify(expand("%"), ':t')
    if l:filename =~ "_test.go$"
      let l:template_file = get(g:, 'go_template_test_file', "hello_world_test.go")
    else
      let l:template_file = get(g:, 'go_template_file', "hello_world.go")
    endif
    let l:template_path = go#util#Join(l:root_dir, "templates", l:template_file)
    silent exe '0r ' . fnameescape(l:template_path)
  elseif l:package_name == -1 && l:go_template_use_pkg == 1
    " cwd is now the dir of the package
    let l:path = fnamemodify(getcwd(), ':t')
    let l:content = printf("package %s", l:path)
    call append(0, l:content)
  else
    let l:content = printf("package %s", l:package_name)
    call append(0, l:content)
  endif
  $delete _

  execute cd . fnameescape(dir)
endfunction

function! go#template#ToggleAutoCreate() abort
  if get(g:, "go_template_autocreate", 1)
    let g:go_template_autocreate = 0
    call go#util#EchoProgress("auto template create disabled")
    return
  end

  let g:go_template_autocreate = 1
  call go#util#EchoProgress("auto template create enabled")
endfunction

" vim: sw=2 ts=2 et
