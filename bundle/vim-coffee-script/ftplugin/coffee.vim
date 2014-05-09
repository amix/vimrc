" Language:    CoffeeScript
" Maintainer:  Mick Koch <kchmck@gmail.com>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL

if exists('b:did_ftplugin')
  finish
endif

let b:did_ftplugin = 1
call coffee#CoffeeSetUpVariables()

setlocal formatoptions-=t formatoptions+=croql
setlocal comments=:# commentstring=#\ %s
setlocal omnifunc=javascriptcomplete#CompleteJS

" Create custom augroups.
augroup CoffeeBufUpdate | augroup END
augroup CoffeeBufNew | augroup END

" Enable coffee compiler if a compiler isn't set already.
if !len(&l:makeprg)
  compiler coffee
endif

" Switch to the window for buf.
function! s:SwitchWindow(buf)
  exec bufwinnr(a:buf) 'wincmd w'
endfunction

" Create a new scratch buffer and return the bufnr of it. After the function
" returns, vim remains in the scratch buffer so more set up can be done.
function! s:ScratchBufBuild(src, vert, size)
  if a:size <= 0
    if a:vert
      let size = winwidth(bufwinnr(a:src)) / 2
    else
      let size = winheight(bufwinnr(a:src)) / 2
    endif
  endif

  if a:vert
    vertical belowright new
    exec 'vertical resize' size
  else
    belowright new
    exec 'resize' size
  endif

  setlocal bufhidden=wipe buftype=nofile nobuflisted noswapfile nomodifiable
  nnoremap <buffer> <silent> q :hide<CR>

  return bufnr('%')
endfunction

" Replace buffer contents with text and delete the last empty line.
function! s:ScratchBufUpdate(buf, text)
  " Move to the scratch buffer.
  call s:SwitchWindow(a:buf)

  " Double check we're in the scratch buffer before overwriting.
  if bufnr('%') != a:buf
    throw 'unable to change to scratch buffer'
  endif

  setlocal modifiable
    silent exec '% delete _'
    silent put! =a:text
    silent exec '$ delete _'
  setlocal nomodifiable
endfunction

" Parse the output of coffee into a qflist entry for src buffer.
function! s:ParseCoffeeError(output, src, startline)
  " Coffee error is always on first line?
  let match = matchlist(a:output,
  \                     '^\(\f\+\|\[stdin\]\):\(\d\):\(\d\): error: \(.\{-}\)' . "\n")

  if !len(match)
    return
  endif

  " Consider the line number from coffee as relative and add it to the beginning
  " line number of the range the command was called on, then subtract one for
  " zero-based relativity.
  call setqflist([{'bufnr': a:src, 'lnum': a:startline + str2nr(match[2]) - 1,
  \                'type': 'E', 'col': str2nr(match[3]), 'text': match[4]}], 'r')
endfunction

" Reset source buffer variables.
function! s:CoffeeCompileResetVars()
  " Variables defined in source buffer:
  "   b:coffee_compile_buf: bufnr of output buffer
  " Variables defined in output buffer:
  "   b:coffee_src_buf: bufnr of source buffer
  "   b:coffee_compile_pos: previous cursor position in output buffer

  let b:coffee_compile_buf = -1
endfunction

function! s:CoffeeWatchResetVars()
  " Variables defined in source buffer:
  "   b:coffee_watch_buf: bufnr of output buffer
  " Variables defined in output buffer:
  "   b:coffee_src_buf: bufnr of source buffer
  "   b:coffee_watch_pos: previous cursor position in output buffer

  let b:coffee_watch_buf = -1
endfunction

function! s:CoffeeRunResetVars()
  " Variables defined in CoffeeRun source buffer:
  "   b:coffee_run_buf: bufnr of output buffer
  " Variables defined in CoffeeRun output buffer:
  "   b:coffee_src_buf: bufnr of source buffer
  "   b:coffee_run_pos: previous cursor position in output buffer

  let b:coffee_run_buf = -1
endfunction

" Clean things up in the source buffers.
function! s:CoffeeCompileClose()
  " Switch to the source buffer if not already in it.
  silent! call s:SwitchWindow(b:coffee_src_buf)
  call s:CoffeeCompileResetVars()
endfunction

function! s:CoffeeWatchClose()
  silent! call s:SwitchWindow(b:coffee_src_buf)
  silent! autocmd! CoffeeAuWatch * <buffer>
  call s:CoffeeWatchResetVars()
endfunction

function! s:CoffeeRunClose()
  silent! call s:SwitchWindow(b:coffee_src_buf)
  call s:CoffeeRunResetVars()
endfunction

" Compile the lines between startline and endline and put the result into buf.
function! s:CoffeeCompileToBuf(buf, startline, endline)
  let src = bufnr('%')
  let input = join(getline(a:startline, a:endline), "\n")

  " Coffee doesn't like empty input.
  if !len(input)
    " Function should still return within output buffer.
    call s:SwitchWindow(a:buf)
    return
  endif

  " Pipe lines into coffee.
  let output = system(g:coffee_compiler .
  \                   ' -scb' .
  \                   ' ' . b:coffee_litcoffee .
  \                   ' 2>&1', input)

  " Paste output into output buffer.
  call s:ScratchBufUpdate(a:buf, output)

  " Highlight as JavaScript if there were no compile errors.
  if v:shell_error
    call s:ParseCoffeeError(output, src, a:startline)
    setlocal filetype=
  else
    " Clear the quickfix list.
    call setqflist([], 'r')
    setlocal filetype=javascript
  endif
endfunction

" Peek at compiled CoffeeScript in a scratch buffer. We handle ranges like this
" to prevent the cursor from being moved (and its position saved) before the
" function is called.
function! s:CoffeeCompile(startline, endline, args)
  if a:args =~ '\<watch\>'
    echoerr 'CoffeeCompile watch is deprecated! Please use CoffeeWatch instead'
    sleep 5
    call s:CoffeeWatch(a:args)
    return
  endif

  " Switch to the source buffer if not already in it.
  silent! call s:SwitchWindow(b:coffee_src_buf)

  " Bail if not in source buffer.
  if !exists('b:coffee_compile_buf')
    return
  endif

  " Build the output buffer if it doesn't exist.
  if bufwinnr(b:coffee_compile_buf) == -1
    let src = bufnr('%')

    let vert = exists('g:coffee_compile_vert') || a:args =~ '\<vert\%[ical]\>'
    let size = str2nr(matchstr(a:args, '\<\d\+\>'))

    " Build the output buffer and save the source bufnr.
    let buf = s:ScratchBufBuild(src, vert, size)
    let b:coffee_src_buf = src

    " Set the buffer name.
    exec 'silent! file [CoffeeCompile ' . src . ']'

    " Clean up the source buffer when the output buffer is closed.
    autocmd BufWipeout <buffer> call s:CoffeeCompileClose()
    " Save the cursor when leaving the output buffer.
    autocmd BufLeave <buffer> let b:coffee_compile_pos = getpos('.')

    " Run user-defined commands on new buffer.
    silent doautocmd CoffeeBufNew User CoffeeCompile

    " Switch back to the source buffer and save the output bufnr. This also
    " triggers BufLeave above.
    call s:SwitchWindow(src)
    let b:coffee_compile_buf = buf
  endif

  " Fill the scratch buffer.
  call s:CoffeeCompileToBuf(b:coffee_compile_buf, a:startline, a:endline)
  " Reset cursor to previous position.
  call setpos('.', b:coffee_compile_pos)

  " Run any user-defined commands on the scratch buffer.
  silent doautocmd CoffeeBufUpdate User CoffeeCompile
endfunction

" Update the scratch buffer and switch back to the source buffer.
function! s:CoffeeWatchUpdate()
  call s:CoffeeCompileToBuf(b:coffee_watch_buf, 1, '$')
  call setpos('.', b:coffee_watch_pos)
  silent doautocmd CoffeeBufUpdate User CoffeeWatch
  call s:SwitchWindow(b:coffee_src_buf)
endfunction

" Continually compile a source buffer.
function! s:CoffeeWatch(args)
  silent! call s:SwitchWindow(b:coffee_src_buf)

  if !exists('b:coffee_watch_buf')
    return
  endif

  if bufwinnr(b:coffee_watch_buf) == -1
    let src = bufnr('%')

    let vert = exists('g:coffee_watch_vert') || a:args =~ '\<vert\%[ical]\>'
    let size = str2nr(matchstr(a:args, '\<\d\+\>'))

    let buf = s:ScratchBufBuild(src, vert, size)
    let b:coffee_src_buf = src

    exec 'silent! file [CoffeeWatch ' . src . ']'

    autocmd BufWipeout <buffer> call s:CoffeeWatchClose()
    autocmd BufLeave <buffer> let b:coffee_watch_pos = getpos('.')

    silent doautocmd CoffeeBufNew User CoffeeWatch

    call s:SwitchWindow(src)
    let b:coffee_watch_buf = buf
  endif

  " Make sure only one watch autocmd is defined on this buffer.
  silent! autocmd! CoffeeAuWatch * <buffer>

  augroup CoffeeAuWatch
    autocmd InsertLeave <buffer> call s:CoffeeWatchUpdate()
    autocmd BufWritePost <buffer> call s:CoffeeWatchUpdate()
  augroup END

  call s:CoffeeWatchUpdate()
endfunction

" Run a snippet of CoffeeScript between startline and endline.
function! s:CoffeeRun(startline, endline, args)
  silent! call s:SwitchWindow(b:coffee_src_buf)

  if !exists('b:coffee_run_buf')
    return
  endif

  if bufwinnr(b:coffee_run_buf) == -1
    let src = bufnr('%')

    let buf = s:ScratchBufBuild(src, exists('g:coffee_run_vert'), 0)
    let b:coffee_src_buf = src

    exec 'silent! file [CoffeeRun ' . src . ']'

    autocmd BufWipeout <buffer> call s:CoffeeRunClose()
    autocmd BufLeave <buffer> let b:coffee_run_pos = getpos('.')

    silent doautocmd CoffeeBufNew User CoffeeRun

    call s:SwitchWindow(src)
    let b:coffee_run_buf = buf
  endif

  if a:startline == 1 && a:endline == line('$')
    let output = system(g:coffee_compiler .
    \                   ' ' . b:coffee_litcoffee .
    \                   ' ' . fnameescape(expand('%')) .
    \                   ' ' . a:args)
  else
    let input = join(getline(a:startline, a:endline), "\n")

    if !len(input)
      return
    endif

    let output = system(g:coffee_compiler .
    \                   ' -s' .
    \                   ' ' . b:coffee_litcoffee .
    \                   ' ' . a:args, input)
  endif

  call s:ScratchBufUpdate(b:coffee_run_buf, output)
  call setpos('.', b:coffee_run_pos)

  silent doautocmd CoffeeBufUpdate User CoffeeRun
endfunction

" Run coffeelint on a file, and add any errors between startline and endline
" to the quickfix list.
function! s:CoffeeLint(startline, endline, bang, args)
  let input = join(getline(a:startline, a:endline), "\n")

  if !len(input)
    return
  endif

  let output = system(g:coffee_linter .
  \                   ' -s --csv' .
  \                   ' ' . b:coffee_litcoffee .
  \                   ' ' . g:coffee_lint_options .
  \                   ' ' . a:args .
  \                   ' 2>&1', input)

  " Convert output into an array and strip off the csv header.
  let lines = split(output, "\n")[1:]
  let buf = bufnr('%')
  let qflist = []

  for line in lines
    let match = matchlist(line, '^stdin,\(\d\+\),\d*,\(error\|warn\),\(.\+\)$')

    " Ignore unmatched lines.
    if !len(match)
      continue
    endif

    " The 'type' will result in either 'E' or 'W'.
    call add(qflist, {'bufnr': buf, 'lnum': a:startline + str2nr(match[1]) - 1,
    \                 'type': toupper(match[2][0]), 'text': match[3]})
  endfor

  " Replace the quicklist with our items.
  call setqflist(qflist, 'r')

  " If not given a bang, jump to first error.
  if !len(a:bang)
    silent! cc 1
  endif
endfunction

" Complete arguments for Coffee* commands.
function! s:CoffeeComplete(cmd, cmdline, cursor)
  let args = ['vertical']

  " If no partial command, return all possibilities.
  if !len(a:cmd)
    return args
  endif

  let pat = '^' . a:cmd

  for arg in args
    if arg =~ pat
      return [arg]
    endif
  endfor
endfunction

" Set initial state variables if they don't exist
if !exists('b:coffee_compile_buf')
  call s:CoffeeCompileResetVars()
endif

if !exists('b:coffee_watch_buf')
  call s:CoffeeWatchResetVars()
endif

if !exists('b:coffee_run_buf')
  call s:CoffeeRunResetVars()
endif

command! -range=% -bar -nargs=* -complete=customlist,s:CoffeeComplete
\        CoffeeCompile call s:CoffeeCompile(<line1>, <line2>, <q-args>)
command! -bar -nargs=* -complete=customlist,s:CoffeeComplete
\        CoffeeWatch call s:CoffeeWatch(<q-args>)
command! -range=% -bar -nargs=* CoffeeRun
\        call s:CoffeeRun(<line1>, <line2>, <q-args>)
command! -range=% -bang -bar -nargs=* CoffeeLint
\        call s:CoffeeLint(<line1>, <line2>, <q-bang>, <q-args>)
