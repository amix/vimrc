"  guru.vim -- Vim integration for the Go guru.

" guru_cmd returns a dict that contains the command to execute guru. option
" is dict with following options:
"  mode        : guru mode, such as 'implements'
"  format      : output format, either 'plain' or 'json'
"  needs_scope : if 1, adds the current package to the scope
"  selected    : if 1, means it's a range of selection, otherwise it picks up the
"                offset under the cursor
" example output:
"  {'cmd' : ['guru', '-json', 'implements', 'demo/demo.go:#66']}
function! s:guru_cmd(args) range abort
  let mode = a:args.mode
  let format = a:args.format
  let needs_scope = a:args.needs_scope
  let selected = a:args.selected

  let result = {}
  let dirname = expand('%:p:h')
  let pkg = go#package#ImportPath(dirname)

  " this is important, check it!
  if pkg == -1 && needs_scope
    return {'err': "current directory is not inside of a valid GOPATH"}
  endif

  "return with a warning if the binary doesn't exist
  let bin_path = go#path#CheckBinPath("guru") 
  if empty(bin_path)
    return {'err': "bin path not found"}
  endif

  " start constructing the command
  let cmd = [bin_path]

  let filename = fnamemodify(expand("%"), ':p:gs?\\?/?')
  let stdin_content = ""
  if &modified
    let sep = go#util#LineEnding()
    let content  = join(getline(1, '$'), sep )
    let result.stdin_content = filename . "\n" . strlen(content) . "\n" . content
    call add(cmd, "-modified")
  endif

  " enable outputting in json format
  if format == "json" 
    call add(cmd, "-json")
  endif

  " check for any tags
  if exists('g:go_guru_tags')
    let tags = get(g:, 'go_guru_tags')
    call extend(cmd, ["-tags", tags])
    let result.tags = tags
  endif

  " some modes require scope to be defined (such as callers). For these we
  " choose a sensible setting, which is using the current file's package
  let scopes = []
  if needs_scope
    let scopes = [pkg]
  endif

  " check for any user defined scope setting. users can define the scope,
  " in package pattern form. examples:
  "  golang.org/x/tools/cmd/guru # a single package
  "  golang.org/x/tools/...      # all packages beneath dir
  "  ...                         # the entire workspace.
  if exists('g:go_guru_scope')
    " check that the setting is of type list
    if type(get(g:, 'go_guru_scope')) != type([])
      return {'err' : "go_guru_scope should of type list"}
    endif

    let scopes = get(g:, 'go_guru_scope')
  endif

  " now add the scope to our command if there is any
  if !empty(scopes)
    " strip trailing slashes for each path in scoped. bug:
    " https://github.com/golang/go/issues/14584
    let scopes = go#util#StripTrailingSlash(scopes)

    " create shell-safe entries of the list
    if !go#util#has_job() | let scopes = go#util#Shelllist(scopes) | endif

    " guru expect a comma-separated list of patterns, construct it
    let l:scope = join(scopes, ",")
    let result.scope = l:scope
    call extend(cmd, ["-scope", l:scope])
  endif

  let pos = printf("#%s", go#util#OffsetCursor())
  if selected != -1
    " means we have a range, get it
    let pos1 = go#util#Offset(line("'<"), col("'<"))
    let pos2 = go#util#Offset(line("'>"), col("'>"))
    let pos = printf("#%s,#%s", pos1, pos2)
  endif

  let filename .= ':'.pos
  call extend(cmd, [mode, filename])

  let result.cmd = cmd
  return result
endfunction

" sync_guru runs guru in sync mode with the given arguments
function! s:sync_guru(args) abort
  let result = s:guru_cmd(a:args)
  if has_key(result, 'err')
    call go#util#EchoError(result.err)
    return -1
  endif

  if !has_key(a:args, 'disable_progress')
    if a:args.needs_scope
      call go#util#EchoProgress("analysing with scope ". result.scope . " ...")
    elseif a:args.mode !=# 'what'
      " the query might take time, let us give some feedback
      call go#util#EchoProgress("analysing ...")
    endif
  endif

  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()

  " run, forrest run!!!
  let command = join(result.cmd, " ")
  if &modified
    let out = go#util#System(command, result.stdin_content)
  else
    let out = go#util#System(command)
  endif

  let $GOPATH = old_gopath

  if has_key(a:args, 'custom_parse')
    call a:args.custom_parse(go#util#ShellError(), out)
  else
    call s:parse_guru_output(go#util#ShellError(), out, a:args.mode)
  endif

  return out
endfunc

" async_guru runs guru in async mode with the given arguments
function! s:async_guru(args) abort
  let result = s:guru_cmd(a:args)
  if has_key(result, 'err')
    call go#util#EchoError(result.err)
    return
  endif

  let status_dir =  expand('%:p:h')
  let statusline_type = printf("%s", a:args.mode)

  if !has_key(a:args, 'disable_progress')
    if a:args.needs_scope
      call go#util#EchoProgress("analysing with scope ". result.scope . " ...")
    endif
  endif

  function! s:close_cb(chan) closure
    let messages = []
    while ch_status(a:chan, {'part': 'out'}) == 'buffered'
      let msg = ch_read(a:chan, {'part': 'out'})
      call add(messages, msg)
    endwhile

    while ch_status(a:chan, {'part': 'err'}) == 'buffered'
      let msg = ch_read(a:chan, {'part': 'err'})
      call add(messages, msg)
    endwhile

    let l:job = ch_getjob(a:chan)
    let l:info = job_info(l:job)

    let out = join(messages, "\n")

    let status = {
          \ 'desc': 'last status',
          \ 'type': statusline_type,
          \ 'state': "finished",
          \ }

    if l:info.exitval
      let status.state = "failed"
    endif

    call go#statusline#Update(status_dir, status)

    if has_key(a:args, 'custom_parse')
      call a:args.custom_parse(l:info.exitval, out)
    else
      call s:parse_guru_output(l:info.exitval, out, a:args.mode)
    endif
  endfunction

  let start_options = {
        \ 'close_cb': function("s:close_cb"),
        \ }

  if &modified
    let l:tmpname = tempname()
    call writefile(split(result.stdin_content, "\n"), l:tmpname, "b")
    let l:start_options.in_io = "file"
    let l:start_options.in_name = l:tmpname
  endif

  call go#statusline#Update(status_dir, {
        \ 'desc': "current status",
        \ 'type': statusline_type,
        \ 'state': "analysing",
        \})

  return job_start(result.cmd, start_options)
endfunc

" run_guru runs the given guru argument
function! s:run_guru(args) abort
  if go#util#has_job()
    return s:async_guru(a:args)
  endif

  return s:sync_guru(a:args)
endfunction

" Show 'implements' relation for selected package
function! go#guru#Implements(selected) abort
  let args = {
        \ 'mode': 'implements',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }

  call s:run_guru(args)
endfunction

" Report the possible constants, global variables, and concrete types that may
" appear in a value of type error
function! go#guru#Whicherrs(selected) abort
  let args = {
        \ 'mode': 'whicherrs',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }


  " TODO(arslan): handle empty case for both sync/async
  " if empty(out.out)
  "   call go#util#EchoSuccess("no error variables found. Try to change the scope with :GoGuruScope")
  "   return
  " endif
  call s:run_guru(args)
endfunction

" Describe selected syntax: definition, methods, etc
function! go#guru#Describe(selected) abort
  let args = {
        \ 'mode': 'describe',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }

  call s:run_guru(args)
endfunction

function! go#guru#DescribeInfo() abort
  " json_encode() and friends are introduced with this patch (7.4.1304)
  " vim: https://groups.google.com/d/msg/vim_dev/vLupTNhQhZ8/cDGIk0JEDgAJ
  " nvim: https://github.com/neovim/neovim/pull/4131        
  if !exists("*json_decode")
    call go#util#EchoError("requires 'json_decode'. Update your Vim/Neovim version.")
    return
  endif

  function! s:info(exit_val, output)
    if a:exit_val != 0
      return
    endif

    if a:output[0] !=# '{'
      return
    endif

    if empty(a:output) || type(a:output) != type("")
      return
    endif

    let result = json_decode(a:output)
    if type(result) != type({})
      call go#util#EchoError(printf("malformed output from guru: %s", a:output))
      return
    endif

    if !has_key(result, 'detail')
      " if there is no detail check if there is a description and print it
      if has_key(result, "desc")
        call go#util#EchoInfo(result["desc"])
        return
      endif

      call go#util#EchoError("detail key is missing. Please open a bug report on vim-go repo.")
      return
    endif

    let detail = result['detail']
    let info = ""

    " guru gives different information based on the detail mode. Let try to
    " extract the most useful information

    if detail == "value"
      if !has_key(result, 'value')
        call go#util#EchoError("value key is missing. Please open a bug report on vim-go repo.")
        return
      endif

      let val = result["value"]
      if !has_key(val, 'type')
        call go#util#EchoError("type key is missing (value.type). Please open a bug report on vim-go repo.")
        return
      endif

      let info  = val["type"]
    elseif detail == "type"
      if !has_key(result, 'type')
        call go#util#EchoError("type key is missing. Please open a bug report on vim-go repo.")
        return
      endif

      let type = result["type"]
      if !has_key(type, 'type')
        call go#util#EchoError("type key is missing (type.type). Please open a bug report on vim-go repo.")
        return
      endif

      let info  = type["type"]
    elseif detail == "package"
      if !has_key(result, 'package')
        call go#util#EchoError("package key is missing. Please open a bug report on vim-go repo.")
        return
      endif

      let package = result["package"]
      if !has_key(package, 'path')
        call go#util#EchoError("path key is missing (package.path). Please open a bug report on vim-go repo.")
        return
      endif

      let info = printf("package %s", package["path"])
    elseif detail == "unknown"
      let info = result["desc"]
    else
      call go#util#EchoError(printf("unknown detail mode found '%s'. Please open a bug report on vim-go repo", detail))
      return
    endif

    call go#util#EchoInfo(info)
  endfunction

  let args = {
        \ 'mode': 'describe',
        \ 'format': 'json',
        \ 'selected': -1,
        \ 'needs_scope': 1,
        \ 'custom_parse': function('s:info'),
        \ 'disable_progress': 1,
        \ }

  call s:run_guru(args)
endfunction

" Show possible targets of selected function call
function! go#guru#Callees(selected) abort
  let args = {
        \ 'mode': 'callees',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }

  call s:run_guru(args)
endfunction

" Show possible callers of selected function
function! go#guru#Callers(selected) abort
  let args = {
        \ 'mode': 'callers',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }

  call s:run_guru(args)
endfunction

" Show path from callgraph root to selected function
function! go#guru#Callstack(selected) abort
  let args = {
        \ 'mode': 'callstack',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }

  call s:run_guru(args)
endfunction

" Show free variables of selection
function! go#guru#Freevars(selected) abort
  " Freevars requires a selection
  if a:selected == -1
    call go#util#EchoError("GoFreevars requires a selection (range) of code")
    return
  endif

  let args = {
        \ 'mode': 'freevars',
        \ 'format': 'plain',
        \ 'selected': 1,
        \ 'needs_scope': 0,
        \ }

  call s:run_guru(args)
endfunction

" Show send/receive corresponding to selected channel op
function! go#guru#ChannelPeers(selected) abort
  let args = {
        \ 'mode': 'peers',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }

  call s:run_guru(args)
endfunction

" Show all refs to entity denoted by selected identifier
function! go#guru#Referrers(selected) abort
  let args = {
        \ 'mode': 'referrers',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 0,
        \ }

  call s:run_guru(args)
endfunction

function! go#guru#SameIdsTimer() abort
  call timer_start(200, function('go#guru#SameIds'), {'repeat': -1})
endfunction

function! go#guru#SameIds() abort
  " we use matchaddpos() which was introduce with 7.4.330, be sure we have
  " it: http://ftp.vim.org/vim/patches/7.4/7.4.330
  if !exists("*matchaddpos")
    call go#util#EchoError("GoSameIds requires 'matchaddpos'. Update your Vim/Neovim version.")
    return
  endif

  " json_encode() and friends are introduced with this patch (7.4.1304)
  " vim: https://groups.google.com/d/msg/vim_dev/vLupTNhQhZ8/cDGIk0JEDgAJ
  " nvim: https://github.com/neovim/neovim/pull/4131        
  if !exists("*json_decode")
    call go#util#EchoError("GoSameIds requires 'json_decode'. Update your Vim/Neovim version.")
    return
  endif

  let args = {
        \ 'mode': 'what',
        \ 'format': 'json',
        \ 'selected': -1,
        \ 'needs_scope': 0,
        \ 'custom_parse': function('s:same_ids_highlight'),
        \ }

  call s:run_guru(args)
endfunction

function! s:same_ids_highlight(exit_val, output) abort
  call go#guru#ClearSameIds() " run after calling guru to reduce flicker.

  if a:output[0] !=# '{'
    if !get(g:, 'go_auto_sameids', 0)
      call go#util#EchoError(a:output)
    endif
    return
  endif

  let result = json_decode(a:output)
  if type(result) != type({}) && !get(g:, 'go_auto_sameids', 0)
    call go#util#EchoError("malformed output from guru")
    return
  endif

  if !has_key(result, 'sameids')
    if !get(g:, 'go_auto_sameids', 0)
      call go#util#EchoError("no same_ids founds for the given identifier")
    endif
    return
  endif

  let poslen = 0
  for enclosing in result['enclosing']
    if enclosing['desc'] == 'identifier'
      let poslen = enclosing['end'] - enclosing['start']
      break
    endif
  endfor

  " return when there's no identifier to highlight.
  if poslen == 0
    return
  endif

  let same_ids = result['sameids']
  " highlight the lines
  for item in same_ids
    let pos = split(item, ':')
    call matchaddpos('goSameId', [[str2nr(pos[-2]), str2nr(pos[-1]), str2nr(poslen)]])
  endfor

  if get(g:, "go_auto_sameids", 0)
    " re-apply SameIds at the current cursor position at the time the buffer
    " is redisplayed: e.g. :edit, :GoRename, etc.
    autocmd BufWinEnter <buffer> nested call go#guru#SameIds()
  endif
endfunction

function! go#guru#ClearSameIds() abort
  let m = getmatches()
  for item in m
    if item['group'] == 'goSameId'
      call matchdelete(item['id'])
    endif
  endfor

  " remove the autocmds we defined
  if exists("#BufWinEnter<buffer>")
    autocmd! BufWinEnter <buffer>
  endif
endfunction

function! go#guru#ToggleSameIds() abort
  if len(getmatches()) != 0 
    call go#guru#ClearSameIds()
  else
    call go#guru#SameIds()
  endif
endfunction

function! go#guru#AutoToogleSameIds() abort
  if get(g:, "go_auto_sameids", 0)
    call go#util#EchoProgress("sameids auto highlighting disabled")
    call go#guru#ClearSameIds()
    let g:go_auto_sameids = 0
    return
  endif

  call go#util#EchoSuccess("sameids auto highlighting enabled")
  let g:go_auto_sameids = 1
endfunction


""""""""""""""""""""""""""""""""""""""""
"" HELPER FUNCTIONS
""""""""""""""""""""""""""""""""""""""""

" This uses Vim's errorformat to parse the output from Guru's 'plain output
" and put it into location list. I believe using errorformat is much more
" easier to use. If we need more power we can always switch back to parse it
" via regex. Match two possible styles of errorformats:
"
"   'file:line.col-line2.col2: message'
"   'file:line:col: message'
"
" We discard line2 and col2 for the first errorformat, because it's not
" useful and location only has the ability to show one line and column
" number
function! s:parse_guru_output(exit_val, output, title) abort
  if a:exit_val
    call go#util#EchoError(a:output)
    return
  endif

  let old_errorformat = &errorformat
  let errformat = "%f:%l.%c-%[%^:]%#:\ %m,%f:%l:%c:\ %m"
  call go#list#ParseFormat("locationlist", errformat, a:output, a:title)
  let &errorformat = old_errorformat

  let errors = go#list#Get("locationlist")
  call go#list#Window("locationlist", len(errors))
endfun

function! go#guru#Scope(...) abort
  if a:0
    if a:0 == 1 && a:1 == '""'
      unlet g:go_guru_scope
      call go#util#EchoSuccess("guru scope is cleared")
    else
      let g:go_guru_scope = a:000
      call go#util#EchoSuccess("guru scope changed to: ". join(a:000, ","))
    endif

    return
  endif

  if !exists('g:go_guru_scope')
    call go#util#EchoError("guru scope is not set")
  else
    call go#util#EchoSuccess("current guru scope: ". join(g:go_guru_scope, ","))
  endif
endfunction

function! go#guru#Tags(...) abort
  if a:0
    if a:0 == 1 && a:1 == '""'
      unlet g:go_guru_tags
      call go#util#EchoSuccess("guru tags is cleared")
    else
      let g:go_guru_tags = a:1
      call go#util#EchoSuccess("guru tags changed to: ". a:1)
    endif

    return
  endif

  if !exists('g:go_guru_tags')
    call go#util#EchoSuccess("guru tags is not set")
  else
    call go#util#EchoSuccess("current guru tags: ". a:1)
  endif
endfunction

" vim: sw=2 ts=2 et
