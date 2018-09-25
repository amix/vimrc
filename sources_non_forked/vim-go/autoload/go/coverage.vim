let s:toggle = 0

" Buffer creates a new cover profile with 'go test -coverprofile' and changes
" the current buffers highlighting to show covered and uncovered sections of
" the code. If run again it clears the annotation.
function! go#coverage#BufferToggle(bang, ...) abort
  if s:toggle
    call go#coverage#Clear()
    return
  endif

  if a:0 == 0
    return call(function('go#coverage#Buffer'), [a:bang])
  endif

  return call(function('go#coverage#Buffer'), [a:bang] + a:000)
endfunction

" Buffer creates a new cover profile with 'go test -coverprofile' and changes
" the current buffers highlighting to show covered and uncovered sections of
" the code. Calling it again reruns the tests and shows the last updated
" coverage.
function! go#coverage#Buffer(bang, ...) abort
  " we use matchaddpos() which was introduce with 7.4.330, be sure we have
  " it: http://ftp.vim.org/vim/patches/7.4/7.4.330
  if !exists("*matchaddpos")
    call go#util#EchoError("GoCoverage is supported with Vim version 7.4-330 or later")
    return -1
  endif

  " check if there is any test file, if not we just return
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  try
    execute cd . fnameescape(expand("%:p:h"))
    if empty(glob("*_test.go"))
      call go#util#EchoError("no test files available")
      return
    endif
  finally
    execute cd . fnameescape(dir)
  endtry

  let s:toggle = 1
  let l:tmpname = tempname()

  if go#util#has_job()
    call s:coverage_job({
          \ 'cmd': ['go', 'test', '-tags', go#config#BuildTags(), '-coverprofile', l:tmpname] + a:000,
          \ 'complete': function('s:coverage_callback', [l:tmpname]),
          \ 'bang': a:bang,
          \ 'for': 'GoTest',
          \ 'statustype': 'coverage',
          \ })
    return
  endif

  if go#config#EchoCommandInfo()
    call go#util#EchoProgress("testing...")
  endif

  let args = [a:bang, 0, "-coverprofile", l:tmpname]
  if a:0
    call extend(args, a:000)
  endif

  let id = call('go#test#Test', args)

  if go#util#ShellError() == 0
    call go#coverage#overlay(l:tmpname)
  endif

  call delete(l:tmpname)
endfunction

" Clear clears and resets the buffer annotation matches
function! go#coverage#Clear() abort
  call clearmatches()

  if exists("s:toggle") | let s:toggle = 0 | endif

  " remove the autocmd we defined
  augroup vim-go-coverage
    autocmd!
  augroup end
endfunction

" Browser creates a new cover profile with 'go test -coverprofile' and opens
" a new HTML coverage page from that profile in a new browser
function! go#coverage#Browser(bang, ...) abort
  let l:tmpname = tempname()
  if go#util#has_job()
    call s:coverage_job({
          \ 'cmd': ['go', 'test', '-tags', go#config#BuildTags(), '-coverprofile', l:tmpname],
          \ 'complete': function('s:coverage_browser_callback', [l:tmpname]),
          \ 'bang': a:bang,
          \ 'for': 'GoTest',
          \ 'statustype': 'coverage',
          \ })
    return
  endif

  let args = [a:bang, 0, "-coverprofile", l:tmpname]
  if a:0
    call extend(args, a:000)
  endif

  let id = call('go#test#Test', args)

  if go#util#ShellError() == 0
    call go#tool#ExecuteInDir(['go', 'tool', 'cover', '-html=' . l:tmpname])
  endif

  call delete(l:tmpname)
endfunction

" Parses a single line from the cover file generated via go test -coverprofile
" and returns a single coverage profile block.
function! go#coverage#parsegocoverline(line) abort
  " file:startline.col,endline.col numstmt count
  let mx = '\([^:]\+\):\(\d\+\)\.\(\d\+\),\(\d\+\)\.\(\d\+\)\s\(\d\+\)\s\(\d\+\)'
  let tokens = matchlist(a:line, mx)
  let ret = {}
  let ret.file = tokens[1]
  let ret.startline  = str2nr(tokens[2])
  let ret.startcol = str2nr(tokens[3])
  let ret.endline = str2nr(tokens[4])
  let ret.endcol = str2nr(tokens[5])
  let ret.numstmt = tokens[6]
  let ret.cnt = tokens[7]
  return ret
endfunction

" Generates matches to be added to matchaddpos for the given coverage profile
" block
function! go#coverage#genmatch(cov) abort
  let color = 'goCoverageCovered'
  if a:cov.cnt == 0
    let color = 'goCoverageUncover'
  endif

  let matches = []

  " if start and end are the same, also specify the byte length
  " example: foo.go:92.2,92.65 1 0
  if a:cov.startline == a:cov.endline
    call add(matches, {
          \ 'group': color,
          \ 'pos': [[a:cov.startline, a:cov.startcol, a:cov.endcol - a:cov.startcol]],
          \ 'priority': 2,
          \ })
    return matches
  endif

  " add start columns. Because we don't know the length of the of
  " the line, we assume it is at maximum 200 bytes. I know this is hacky,
  " but that's only way of fixing the issue
  call add(matches, {
        \ 'group': color,
        \ 'pos': [[a:cov.startline, a:cov.startcol, 200]],
        \ 'priority': 2,
        \ })

  " and then the remaining lines
  let start_line = a:cov.startline
  while start_line < a:cov.endline
    let start_line += 1
    call add(matches, {
          \ 'group': color,
          \ 'pos': [[start_line]],
          \ 'priority': 2,
          \ })
  endwhile

  " finally end columns
  call add(matches, {
        \ 'group': color,
        \ 'pos': [[a:cov.endline, a:cov.endcol-1]],
        \ 'priority': 2,
        \ })

  return matches
endfunction

" Reads the given coverprofile file and annotates the current buffer
function! go#coverage#overlay(file) abort
  if !filereadable(a:file)
    return
  endif
  let lines = readfile(a:file)

  " cover mode, by default it's 'set'. Just here for debugging purposes
  let mode = lines[0]

  " contains matches for matchaddpos()
  let matches = []

  " first mark all lines as goCoverageNormalText. We use a custom group to not
  " interfere with other buffers highlightings. Because the priority is
  " lower than the cover and uncover matches, it'll be overridden.
  let cnt = 1
  while cnt <= line('$')
    call add(matches, {'group': 'goCoverageNormalText', 'pos': [cnt], 'priority': 1})
    let cnt += 1
  endwhile

  let fname = expand('%')

  " when called for a _test.go file, run the coverage for the actuall file
  " file
  if fname =~# '^\f\+_test\.go$'
    let l:root = split(fname, '_test.go$')[0]
    let fname = l:root . ".go"

    if !filereadable(fname)
      call go#util#EchoError("couldn't find ".fname)
      return
    endif

    " open the alternate file to show the coverage
    exe ":edit ". fnamemodify(fname, ":p")
  endif

  " cov.file includes only the filename itself, without full path
  let fname = fnamemodify(fname, ":t")

  for line in lines[1:]
    let cov = go#coverage#parsegocoverline(line)

    " TODO(arslan): for now only include the coverage for the current
    " buffer
    if fname != fnamemodify(cov.file, ':t')
      continue
    endif

    call extend(matches, go#coverage#genmatch(cov))
  endfor

  " clear the matches if we leave the buffer
  augroup vim-go-coverage
    autocmd!
    autocmd BufWinLeave <buffer> call go#coverage#Clear()
  augroup end

  for m in matches
    call matchaddpos(m.group, m.pos)
  endfor
endfunction


" ---------------------
" | Vim job callbacks |
" ---------------------
"
function s:coverage_job(args)
  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  let disabled_term = 0
  if go#config#TermEnabled()
    let disabled_term = 1
    call go#config#SetTermEnabled(0)
  endif

  call go#job#Spawn(a:args.cmd, a:args)

  if disabled_term
    call go#config#SetTermEnabled(1)
  endif
endfunction

" coverage_callback is called when the coverage execution is finished
function! s:coverage_callback(coverfile, job, exit_status, data)
  if a:exit_status == 0
    call go#coverage#overlay(a:coverfile)
  endif

  call delete(a:coverfile)
endfunction

function! s:coverage_browser_callback(coverfile, job, exit_status, data)
  if a:exit_status == 0
    call go#tool#ExecuteInDir(['go', 'tool', 'cover', '-html=' . a:coverfile])
  endif

  call delete(a:coverfile)
endfunction

" vim: sw=2 ts=2 et
