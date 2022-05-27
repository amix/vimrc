" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#rename#Rename(bang, ...) abort
  let to_identifier = ""
  if a:0 == 0
    let ask = printf("vim-go: rename '%s' to: ", expand("<cword>"))
    let prefill = go#config#GorenamePrefill()
    if prefill != ''
      let to_identifier = input(ask, eval(prefill))
    else
      let to_identifier = input(ask)
    endif
    redraw!
    if empty(to_identifier)
      return
    endif
  else
    let to_identifier = a:1
  endif

  let l:bin = go#config#RenameCommand()

  " return with a warning if the bin doesn't exist
  let bin_path = go#path#CheckBinPath(l:bin)
  if empty(bin_path)
    return
  endif

  let fname = expand('%:p')
  let pos = go#util#OffsetCursor()
  let offset = printf('%s:#%d', fname, pos)

  if l:bin == 'gopls'
    call go#lsp#Rename(to_identifier)
    return
  endif

  let args = []
  if l:bin == 'gorename'
    let l:args = extend(l:args, ['-tags', go#config#BuildTags(), '-offset', offset, '-to', to_identifier])
  else
    call go#util#EchoWarning('unexpected rename command')
  endif

  let l:cmd = extend([l:bin_path], l:args)

  if go#util#has_job()
    call s:rename_job({
          \ 'cmd': cmd,
          \ 'bang': a:bang,
          \})
    return
  endif

  let l:wd = go#util#ModuleRoot()
  if l:wd == -1
    let l:wd = expand("%:p:h")
  endif

  let [l:out, l:err] = go#util#ExecInWorkDir(l:cmd, l:wd)
  call s:parse_errors(l:err, a:bang, split(l:out, '\n'))
endfunction

function s:rename_job(args)
  let l:job_opts = {
        \ 'bang': a:args.bang,
        \ 'for': 'GoRename',
        \ 'statustype': 'gorename',
        \ }

  " autowrite is not enabled for jobs
  call go#cmd#autowrite()
  let l:cbs = go#job#Options(l:job_opts)

  let l:wd = go#util#ModuleRoot()
  if l:wd != -1
    let l:cbs.cwd = l:wd
  endif

  " wrap l:cbs.exit_cb in s:exit_cb.
  let l:cbs.exit_cb = funcref('s:exit_cb', [l:cbs.exit_cb])

  call go#job#Start(a:args.cmd, l:cbs)
endfunction

function! s:reload_changed() abort
  " reload all files to reflect the new changes. We explicitly call
  " checktime to trigger a reload of all files. See
  " http://www.mail-archive.com/vim@vim.org/msg05900.html for more info
  " about the autoread bug
  let current_autoread = &autoread
  set autoread
  silent! checktime
  let &autoread = current_autoread
endfunction

" s:exit_cb reloads any changed buffers and then calls next.
function! s:exit_cb(next, job, exitval) abort
  call s:reload_changed()
  call call(a:next, [a:job, a:exitval])
endfunction

function s:parse_errors(exit_val, bang, out)
  " reload all files to reflect the new changes. We explicitly call
  " checktime to trigger a reload of all files. See
  " http://www.mail-archive.com/vim@vim.org/msg05900.html for more info
  " about the autoread bug
  let current_autoread = &autoread
  set autoread
  silent! checktime
  let &autoread = current_autoread

  let l:listtype = go#list#Type("GoRename")
  if a:exit_val != 0
    let errors = go#util#ParseErrors(a:out)
    call go#list#Populate(l:listtype, errors, 'Rename')
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    elseif empty(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError(a:out)
    endif

    return
  endif

  " strip out newline on the end that gorename puts. If we don't remove, it
  " will trigger the 'Hit ENTER to continue' prompt
  call go#list#Clean(l:listtype)
  call go#util#EchoSuccess(a:out[0])

  " refresh the buffer so we can see the new content
  silent execute ":e"
endfunction

" Commandline completion: original, unexported camelCase, and exported
" CamelCase.
function! go#rename#Complete(lead, cmdline, cursor)
  let l:word = expand('<cword>')
  return filter(uniq(sort(
        \ [l:word, go#util#camelcase(l:word), go#util#pascalcase(l:word)])),
        \ 'strpart(v:val, 0, len(a:lead)) == a:lead')
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
