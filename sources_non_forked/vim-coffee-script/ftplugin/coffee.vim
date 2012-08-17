" Language:    CoffeeScript
" Maintainer:  Mick Koch <kchmck@gmail.com>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1

setlocal formatoptions-=t formatoptions+=croql
setlocal comments=:#
setlocal commentstring=#\ %s
setlocal omnifunc=javascriptcomplete#CompleteJS

" Enable CoffeeMake if it won't overwrite any settings.
if !len(&l:makeprg)
  compiler coffee
endif

" Check here too in case the compiler above isn't loaded.
if !exists('coffee_compiler')
  let coffee_compiler = 'coffee'
endif

" Path to coffeelint executable
if !exists('coffee_linter')
  let coffee_linter = 'coffeelint'
endif

" Options passed to CoffeeLint
if !exists('coffee_lint_options')
  let coffee_lint_options = ''
endif

" Reset the CoffeeCompile variables for the current buffer.
function! s:CoffeeCompileResetVars()
  " Compiled output buffer
  let b:coffee_compile_buf = -1
  let b:coffee_compile_pos = []

  " If CoffeeCompile is watching a buffer
  let b:coffee_compile_watch = 0
endfunction

" Clean things up in the source buffer.
function! s:CoffeeCompileClose()
  exec bufwinnr(b:coffee_compile_src_buf) 'wincmd w'
  silent! autocmd! CoffeeCompileAuWatch * <buffer>
  call s:CoffeeCompileResetVars()
endfunction

" Update the CoffeeCompile buffer given some input lines.
function! s:CoffeeCompileUpdate(startline, endline)
  let input = join(getline(a:startline, a:endline), "\n")

  " Move to the CoffeeCompile buffer.
  exec bufwinnr(b:coffee_compile_buf) 'wincmd w'

  " Coffee doesn't like empty input.
  if !len(input)
    return
  endif

  " Compile input.
  let output = system(g:coffee_compiler . ' -scb 2>&1', input)

  " Be sure we're in the CoffeeCompile buffer before overwriting.
  if exists('b:coffee_compile_buf')
    echoerr 'CoffeeCompile buffers are messed up'
    return
  endif

  " Replace buffer contents with new output and delete the last empty line.
  setlocal modifiable
    exec '% delete _'
    put! =output
    exec '$ delete _'
  setlocal nomodifiable

  " Highlight as JavaScript if there is no compile error.
  if v:shell_error
    setlocal filetype=
  else
    setlocal filetype=javascript
  endif

  call setpos('.', b:coffee_compile_pos)
endfunction

" Update the CoffeeCompile buffer with the whole source buffer.
function! s:CoffeeCompileWatchUpdate()
  call s:CoffeeCompileUpdate(1, '$')
  exec bufwinnr(b:coffee_compile_src_buf) 'wincmd w'
endfunction

" Peek at compiled CoffeeScript in a scratch buffer. We handle ranges like this
" to prevent the cursor from being moved (and its position saved) before the
" function is called.
function! s:CoffeeCompile(startline, endline, args)
  if !executable(g:coffee_compiler)
    echoerr "Can't find CoffeeScript compiler `" . g:coffee_compiler . "`"
    return
  endif

  " If in the CoffeeCompile buffer, switch back to the source buffer and
  " continue.
  if !exists('b:coffee_compile_buf')
    exec bufwinnr(b:coffee_compile_src_buf) 'wincmd w'
  endif

  " Parse arguments.
  let watch = a:args =~ '\<watch\>'
  let unwatch = a:args =~ '\<unwatch\>'
  let size = str2nr(matchstr(a:args, '\<\d\+\>'))

  " Determine default split direction.
  if exists('g:coffee_compile_vert')
    let vert = 1
  else
    let vert = a:args =~ '\<vert\%[ical]\>'
  endif

  " Remove any watch listeners.
  silent! autocmd! CoffeeCompileAuWatch * <buffer>

  " If just unwatching, don't compile.
  if unwatch
    let b:coffee_compile_watch = 0
    return
  endif

  if watch
    let b:coffee_compile_watch = 1
  endif

  " Build the CoffeeCompile buffer if it doesn't exist.
  if bufwinnr(b:coffee_compile_buf) == -1
    let src_buf = bufnr('%')
    let src_win = bufwinnr(src_buf)

    " Create the new window and resize it.
    if vert
      let width = size ? size : winwidth(src_win) / 2

      belowright vertical new
      exec 'vertical resize' width
    else
      " Try to guess the compiled output's height.
      let height = size ? size : min([winheight(src_win) / 2,
      \                               a:endline - a:startline + 2])

      belowright new
      exec 'resize' height
    endif

    " We're now in the scratch buffer, so set it up.
    setlocal bufhidden=wipe buftype=nofile
    setlocal nobuflisted nomodifiable noswapfile nowrap

    autocmd BufWipeout <buffer> call s:CoffeeCompileClose()
    " Save the cursor when leaving the CoffeeCompile buffer.
    autocmd BufLeave <buffer> let b:coffee_compile_pos = getpos('.')

    nnoremap <buffer> <silent> q :hide<CR>

    let b:coffee_compile_src_buf = src_buf
    let buf = bufnr('%')

    " Go back to the source buffer and set it up.
    exec bufwinnr(b:coffee_compile_src_buf) 'wincmd w'
    let b:coffee_compile_buf = buf
  endif

  if b:coffee_compile_watch
    call s:CoffeeCompileWatchUpdate()

    augroup CoffeeCompileAuWatch
      autocmd InsertLeave <buffer> call s:CoffeeCompileWatchUpdate()
    augroup END
  else
    call s:CoffeeCompileUpdate(a:startline, a:endline)
  endif
endfunction

" Complete arguments for the CoffeeCompile command.
function! s:CoffeeCompileComplete(arg, cmdline, cursor)
  let args = ['unwatch', 'vertical', 'watch']

  if !len(a:arg)
    return args
  endif

  let match = '^' . a:arg

  for arg in args
    if arg =~ match
      return [arg]
    endif
  endfor
endfunction

" Run coffeelint on a file, and add any errors between @startline and @endline
" to the quickfix list.
function! s:CoffeeLint(startline, endline, bang, args)
  if !executable(g:coffee_linter)
    echoerr "Can't find CoffeeScript linter `" . g:coffee_linter . "`"
    return
  endif

  let filename = expand('%')

  if !len(filename)
    echoerr 'CoffeeLint must be ran on a saved file'
    return
  endif

  let lines = split(system(g:coffee_linter . ' --csv ' . g:coffee_lint_options .
  \                        ' ' . a:args . ' ' . filename . ' 2>&1'), '\n')
  let qflist = []

  for line in lines
    let match = matchlist(line, '\f\+,\(\d\+\),error,\(.\+\)')

    " Ignore invalid lines.
    if !len(match)
      continue
    endif

    let lnum = str2nr(match[1])

    " Don't add the error if it's not in the range.
    if lnum < a:startline || lnum > a:endline
      continue
    endif

    let text = match[2]

    call add(qflist, {'bufnr': bufnr('%'), 'lnum': lnum, 'text': text})
  endfor

  call setqflist(qflist, 'r')

  " Don't jump if there's a bang.
  if !len(a:bang)
    silent! cc 1
  endif
endfunction

" Don't overwrite the CoffeeCompile variables.
if !exists('b:coffee_compile_buf')
  call s:CoffeeCompileResetVars()
endif

" Peek at compiled CoffeeScript.
command! -range=% -bar -nargs=* -complete=customlist,s:CoffeeCompileComplete
\        CoffeeCompile call s:CoffeeCompile(<line1>, <line2>, <q-args>)
" Run some CoffeeScript.
command! -range=% -bar CoffeeRun <line1>,<line2>:w !coffee -s
" Run coffeelint on the file.
command! -range=% -bang -bar -nargs=* CoffeeLint
\        call s:CoffeeLint(<line1>, <line2>, '<bang>', <q-args>)
