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

  " return with a warning if the bin doesn't exist
  let bin_path = go#path#CheckBinPath(go#config#GorenameBin())
  if empty(bin_path)
    return
  endif

  let fname = expand('%:p')
  let pos = go#util#OffsetCursor()
  let offset = printf('%s:#%d', fname, pos)
  let cmd = [bin_path, "-offset", offset, "-to", to_identifier, '-tags', go#config#BuildTags()]

  if go#util#has_job()
    call go#util#EchoProgress(printf("renaming to '%s' ...", to_identifier))
    call s:rename_job({
          \ 'cmd': cmd,
          \ 'bang': a:bang,
          \})
    return
  endif

  let [l:out, l:err] = go#tool#ExecuteInDir(l:cmd)
  call s:parse_errors(l:err, a:bang, split(l:out, '\n'))
endfunction

function s:rename_job(args)
  let state = {
        \ 'exited': 0,
        \ 'closed': 0,
        \ 'exitval': 0,
        \ 'messages': [],
        \ 'status_dir': expand('%:p:h'),
        \ 'bang': a:args.bang
      \ }

  function! s:callback(chan, msg) dict
    call add(self.messages, a:msg)
  endfunction

  function! s:exit_cb(job, exitval) dict
    let self.exited = 1
    let self.exitval = a:exitval

    let status = {
          \ 'desc': 'last status',
          \ 'type': "gorename",
          \ 'state': "finished",
          \ }

    if a:exitval
      let status.state = "failed"
    endif

    call go#statusline#Update(self.status_dir, status)

    if self.closed
      call s:parse_errors(self.exitval, self.bang, self.messages)
    endif
  endfunction

  function! s:close_cb(ch) dict
    let self.closed = 1

    if self.exited
      call s:parse_errors(self.exitval, self.bang, self.messages)
    endif
  endfunction

  " explicitly bind the callbacks to state so that self within them always
  " refers to state. See :help Partial for more information.
  let start_options = {
        \ 'callback': funcref("s:callback", [], state),
        \ 'exit_cb': funcref("s:exit_cb", [], state),
        \ 'close_cb': funcref("s:close_cb", [], state),
        \ }

  call go#statusline#Update(state.status_dir, {
        \ 'desc': "current status",
        \ 'type': "gorename",
        \ 'state': "started",
        \})

  call job_start(a:args.cmd, start_options)
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
    call go#util#EchoError("FAILED")
    let errors = go#tool#ParseErrors(a:out)
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
  " TODO(arslan): also find all other buffers and refresh them too. For this
  " we need a way to get the list of changes from gorename upon an success
  " change.
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

" vim: sw=2 ts=2 et
