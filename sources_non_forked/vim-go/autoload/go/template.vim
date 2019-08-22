" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:current_file = expand("<sfile>")

function! go#template#create() abort
  let l:go_template_use_pkg = go#config#TemplateUsePkg()
  let l:root_dir = fnamemodify(s:current_file, ':h:h:h')

  let l:package_name = go#tool#PackageName()

  " if we can't figure out any package name (i.e. no Go files in the directory)
  " from the directory create the template or use the directory as the name.
  if l:package_name == -1
    if l:go_template_use_pkg == 1
      let l:path = fnamemodify(expand('%:p:h'), ':t')
      let l:content = printf("package %s", l:path)
      call append(0, l:content)
    else
      let l:filename = expand('%:t')
      if l:filename =~ "_test.go$"
        let l:template_file = go#config#TemplateTestFile()
      else
        let l:template_file = go#config#TemplateFile()
      endif
      " If template_file is an absolute path, use it as-is. This is to support
      " overrides pointing to templates outside of the vim-go plugin dir
      if fnamemodify(l:template_file, ':p') != l:template_file
        let l:template_file = go#util#Join(l:root_dir, "templates", l:template_file)
      endif

      silent exe 'keepalt 0r ' . fnameescape(l:template_file)
    endif
  else
    let l:content = printf("package %s", l:package_name)
    call append(0, l:content)
  endif
  " checking that the last line is empty shouldn't be necessary, but for some
  " reason the last line isn't the expected empty line when run via tests.
  if getline('$') is ''
    $delete _
  endif
endfunction

function! go#template#ToggleAutoCreate() abort
  if go#config#TemplateAutocreate()
    call go#config#SetTemplateAutocreate(0)
    call go#util#EchoProgress("auto template create disabled")
    return
  end

  call go#config#SetTemplateAutocreate(1)
  call go#util#EchoProgress("auto template create enabled")
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
