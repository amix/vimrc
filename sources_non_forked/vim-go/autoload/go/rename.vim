if !exists("g:go_gorename_bin")
  let g:go_gorename_bin = "gorename"
endif

" Set the default value. A value of "1" is a shortcut for this, for
" compatibility reasons.
function! s:default() abort
  if !exists("g:go_gorename_prefill") || g:go_gorename_prefill == 1
    let g:go_gorename_prefill = 'expand("<cword>") =~# "^[A-Z]"' .
          \ '? go#util#pascalcase(expand("<cword>"))' .
          \ ': go#util#camelcase(expand("<cword>"))'
  endif
endfunction
call s:default()

function! go#rename#Rename(bang, ...) abort
  call s:default()

  let to_identifier = ""
  if a:0 == 0
    let ask = printf("vim-go: rename '%s' to: ", expand("<cword>"))
    if g:go_gorename_prefill != ''
      let to_identifier = input(ask, eval(g:go_gorename_prefill))
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
  let bin_path = go#path#CheckBinPath(g:go_gorename_bin)
  if empty(bin_path)
    return
  endif

  let fname = expand('%:p')
  let pos = go#util#OffsetCursor()
  let offset = printf('%s:#%d', fname, pos)

  " no need to escape for job call
  let bin_path = go#util#has_job() ? bin_path : shellescape(bin_path)
  let offset = go#util#has_job() ? offset : shellescape(offset)
  let to_identifier = go#util#has_job() ? to_identifier : shellescape(to_identifier)

  let cmd = [bin_path, "-offset", offset, "-to", to_identifier]

  " check for any tags
  if exists('g:go_build_tags')
    let tags = get(g:, 'go_build_tags')
    call extend(cmd, ["-tags", tags])
  endif

  if go#util#has_job()
    call go#util#EchoProgress(printf("renaming to '%s' ...", to_identifier))
    call s:rename_job({
          \ 'cmd': cmd,
          \ 'bang': a:bang,
          \})
    return
  endif

  let command = join(cmd, " ")
  let out = go#tool#ExecuteInDir(command)

  let splitted = split(out, '\n')
  call s:parse_errors(go#util#ShellError(), a:bang, splitted)
endfunction

function s:rename_job(args)
  let exited = 0
  let closed = 0
  let exitval = 0
  let messages = []

  function! s:callback(chan, msg) closure
    call add(messages, a:msg)
  endfunction

  let status_dir =  expand('%:p:h')

  function! s:exit_cb(job, exitval) closure
    let exited = 1
    let exitval = a:exitval

    let status = {
          \ 'desc': 'last status',
          \ 'type': "gorename",
          \ 'state': "finished",
          \ }

    if a:exitval
      let status.state = "failed"
    endif

    call go#statusline#Update(status_dir, status)

    if closed
      call s:parse_errors(a:exitval, a:args.bang, messages)
    endif
  endfunction

  function! s:close_cb(ch) closure
    let closed = 1

    if exited
      call s:parse_errors(exitval, a:args.bang, messages)
    endif
  endfunction

  let start_options = {
        \ 'callback': funcref("s:callback"),
        \ 'exit_cb': funcref("s:exit_cb"),
        \ 'close_cb': funcref("s:close_cb"),
        \ }

  call go#statusline#Update(status_dir, {
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
  call go#list#Window(l:listtype)
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
