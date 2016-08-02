let s:current_file = expand("<sfile>")

function! go#template#create()
  let l:root_dir = fnamemodify(s:current_file, ':h:h:h')

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  execute cd . fnameescape(expand("%:p:h"))

  let l:package_name = go#tool#PackageName()

  " if we can't figure out any package name(no Go files or non Go package
  " files) from the directory create the template
  if l:package_name == -1
    let l:template_file = get(g:, 'go_template_file', "hello_world.go")
    let l:template_path = go#util#Join(l:root_dir, "templates", l:template_file)
    exe '0r ' . fnameescape(l:template_path)
    $delete _
  else
    let l:content = printf("package %s", l:package_name)
    call append(0, l:content)
    $delete _
  endif

  " Remove the '... [New File]' message line from the command line
  echon

  execute cd . fnameescape(dir)
endfunction

function! go#template#ToggleAutoCreate()
  if get(g:, "go_template_autocreate", 1)
    let g:go_template_autocreate = 0
    call go#util#EchoProgress("auto template create disabled")
    return
  end

  let g:go_template_autocreate = 1
  call go#util#EchoProgress("auto template create enabled")
endfunction

" vim: sw=2 ts=2 et
