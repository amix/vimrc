if !exists("g:go_gorename_bin")
  let g:go_gorename_bin = "gorename"
endif

if !exists("g:go_gorename_prefill")
  let g:go_gorename_prefill = 1
endif

function! go#rename#Rename(bang, ...)
  let to = ""
  if a:0 == 0
    let from = expand("<cword>")
    let ask = printf("vim-go: rename '%s' to: ", from)
    if g:go_gorename_prefill
      let to = input(ask, from)
    else
      let to = input(ask)
    endif
    redraw!
    if empty(to)
      return
    endif
  else
    let to = a:1
  endif

  "return with a warning if the bin doesn't exist
  let bin_path = go#path#CheckBinPath(g:go_gorename_bin)
  if empty(bin_path)
    return
  endif

  let fname = expand('%:p')
  let pos = go#util#OffsetCursor()
  let cmd = printf('%s -offset %s -to %s', shellescape(bin_path), shellescape(printf('%s:#%d', fname, pos)), shellescape(to))

  let out = go#tool#ExecuteInDir(cmd)

  " reload all files to reflect the new changes. We explicitly call
  " checktime to trigger a reload of all files. See
  " http://www.mail-archive.com/vim@vim.org/msg05900.html for more info
  " about the autoread bug
  let current_autoread = &autoread
  set autoread
  silent! checktime
  let &autoread = current_autoread

  " strip out newline on the end that gorename puts. If we don't remove, it
  " will trigger the 'Hit ENTER to continue' prompt
  let clean = split(out, '\n')

  let l:listtype = "quickfix"
  if go#util#ShellError() != 0
    let errors = go#tool#ParseErrors(split(out, '\n'))
    call go#list#Populate(l:listtype, errors)
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    elseif empty(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError(out)
    endif
    return
  else
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)
    redraw | echon "vim-go: " | echohl Function | echon clean[0] | echohl None
  endif

  " refresh the buffer so we can see the new content
  " TODO(arslan): also find all other buffers and refresh them too. For this
  " we need a way to get the list of changes from gorename upon an success
  " change.
  silent execute ":e"
endfunction

" vim: sw=2 ts=2 et
