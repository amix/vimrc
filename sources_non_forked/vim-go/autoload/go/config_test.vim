" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

func! Test_SetBuildTags() abort
  if !go#util#has_job()
    return
  endif

  try
    let g:go_def_mode = 'gopls'
    let l:dir = 'test-fixtures/config/buildtags'
    let l:jumpstart = [0, 4, 2, 0]

    execute 'e ' . printf('%s/buildtags.go', l:dir)
    let l:jumpstartbuf = bufnr('')

    call setpos('.', [l:jumpstartbuf, l:jumpstart[1], l:jumpstart[2], 0])

    let l:expectedfilename = printf('%s/foo.go', l:dir)

    let l:expected = [0, 5, 1, 0]
    call assert_notequal(l:expected, l:jumpstart)

    call go#def#Jump('', 0)

    let l:start = reltime()
    while getpos('.') != l:expected && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile

    call assert_equal(l:expectedfilename, bufname("%"))
    call assert_equal(l:expected, getpos('.'))

    execute 'e ' . printf('%s/buildtags.go', l:dir)

    " prepare to wait for the workspace/configuration request
    let g:go_debug=['lsp']

    " set the build constraint
    call go#config#SetBuildTags('constrained')

    " wait for the workspace/configuration request
    let l:lsplog = getbufline('__GOLSP_LOG__', 1, '$')
    let l:start = reltime()
    while match(l:lsplog, 'workspace/configuration') == -1 && reltimefloat(reltime(l:start)) < 10
      sleep 50m
      let l:lsplog = getbufline('__GOLSP_LOG__', 1, '$')
    endwhile
    unlet g:go_debug
    " close the __GOLSP_LOG__ window
    only

    " verify the cursor position within buildtags.go
    call setpos('.', [l:jumpstartbuf, l:jumpstart[1], l:jumpstart[2], 0])
    call assert_equal(l:jumpstart, getpos('.'))

    let l:expectedfilename = printf('%s/constrainedfoo.go', l:dir)
    let l:expected = [0, 6, 1, 0]
    call assert_notequal(l:expected, l:jumpstart)

    call go#def#Jump('', 0)

    let l:start = reltime()
    while getpos('.') != l:expected && reltimefloat(reltime(l:start)) < 10
      sleep 100m
    endwhile

    call assert_equal(l:expectedfilename, bufname("%"))
    call assert_equal(l:expected, getpos('.'))

    let l:lsplog = getbufline('__GOLSP_LOG__', 1, '$')

  finally
    call go#config#SetBuildTags('')
    unlet g:go_def_mode
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
