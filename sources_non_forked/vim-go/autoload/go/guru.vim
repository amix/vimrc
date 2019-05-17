"  guru.vim -- Vim integration for the Go guru.

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" guru_cmd returns a dict that contains the command to execute guru. args
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
  let postype = get(a:args, 'postype', 'cursor')

  let result = {}

  "return with a warning if the binary doesn't exist
  let bin_path = go#path#CheckBinPath("guru")
  if empty(bin_path)
    return {'err': "bin path not found"}
  endif

  " start constructing the command
  let cmd = [bin_path, '-tags', go#config#BuildTags()]

  if &modified
    let result.stdin_content = go#util#archive()
    call add(cmd, "-modified")
  endif

  " enable outputting in json format
  if format == "json"
    call add(cmd, "-json")
  endif

  let scopes = go#config#GuruScope()
  if empty(scopes)
    " some modes require scope to be defined (such as callers). For these we
    " choose a sensible setting, which is using the current file's package
    if needs_scope
      let pkg = go#package#ImportPath()
      if pkg == -1
        return {'err': "current directory is not inside of a valid GOPATH"}
      endif
      let scopes = [pkg]
    endif
  endif

  " Add the scope.
  if !empty(scopes)
    " guru expect a comma-separated list of patterns.
    let l:scope = join(scopes, ",")
    let result.scope = l:scope
    call extend(cmd, ["-scope", l:scope])
  endif

  if postype == 'balloon'
    let pos = printf("#%s", go#util#Offset(v:beval_lnum, v:beval_col))
  else
    let pos = printf("#%s", go#util#OffsetCursor())
    if selected != -1
      " means we have a range, get it
      let pos1 = go#util#Offset(line("'<"), col("'<"))
      let pos2 = go#util#Offset(line("'>"), col("'>"))
      let pos = printf("#%s,#%s", pos1, pos2)
    endif
  endif

  let l:filename = fnamemodify(expand("%"), ':p:gs?\\?/?') . ':' . pos
  call extend(cmd, [mode, l:filename])

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
      call go#util#EchoProgress("analysing with scope ". result.scope .
            \ " (see ':help go-guru-scope' if this doesn't work)...")
    elseif a:args.mode !=# 'what'
      " the query might take time, let us give some feedback
      call go#util#EchoProgress("analysing ...")
    endif
  endif

  " run, forrest run!!!
  if has_key(l:result, 'stdin_content')
    let [l:out, l:err] = go#util#Exec(l:result.cmd, l:result.stdin_content)
  else
    let [l:out, l:err] = go#util#Exec(l:result.cmd)
  endif

  if has_key(a:args, 'custom_parse')
    call a:args.custom_parse(l:err, l:out, a:args.mode)
  else
    call s:parse_guru_output(l:err, l:out, a:args.mode)
  endif

  return l:out
endfunc

" async_guru runs guru in async mode with the given arguments
function! s:async_guru(args) abort
  let result = s:guru_cmd(a:args)
  if has_key(result, 'err')
    call go#util#EchoError(result.err)
    return
  endif

  let state = {
        \ 'mode': a:args.mode,
        \ 'parse' : get(a:args, 'custom_parse', funcref("s:parse_guru_output"))
      \ }

  " explicitly bind complete to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let state.complete = function('s:complete', [], state)

  let opts = {
        \ 'statustype': get(a:args, 'statustype', a:args.mode),
        \ 'for': '_',
        \ 'errorformat': "%f:%l.%c-%[%^:]%#:\ %m,%f:%l:%c:\ %m",
        \ 'complete': state.complete,
        \ }

  if has_key(a:args, 'disable_progress')
    let opts.statustype = ''
  endif

  let opts = go#job#Options(l:opts)

  if has_key(result, 'stdin_content')
    let l:tmpname = tempname()
    call writefile(split(result.stdin_content, "\n"), l:tmpname, "b")
    let l:opts.in_io = "file"
    let l:opts.in_name = l:tmpname
  endif

  call go#job#Start(result.cmd, opts)

  if a:args.needs_scope && go#config#EchoCommandInfo() && !has_key(a:args, 'disable_progress')
    call go#util#EchoProgress("analysing with scope " . result.scope .
          \ " (see ':help go-guru-scope' if this doesn't work)...")
  endif
endfunc

function! s:complete(job, exit_status, messages) dict abort
  let output = join(a:messages, "\n")
  call self.parse(a:exit_status, output, self.mode)
endfunction

" run_guru runs the given guru argument
function! s:run_guru(args) abort
  if go#util#has_job()
    let res = s:async_guru(a:args)
  else
    let res = s:sync_guru(a:args)
  endif

  return res
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

" Shows the set of possible objects to which a pointer may point.
function! go#guru#PointsTo(selected) abort
  let l:args = {
        \ 'mode': 'pointsto',
        \ 'format': 'plain',
        \ 'selected': a:selected,
        \ 'needs_scope': 1,
        \ }

  call s:run_guru(l:args)
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

function! go#guru#DescribeInfo(showstatus) abort
  " json_encode() and friends are introduced with this patch (7.4.1304)
  " vim: https://groups.google.com/d/msg/vim_dev/vLupTNhQhZ8/cDGIk0JEDgAJ
  " nvim: https://github.com/neovim/neovim/pull/4131
  if !exists("*json_decode")
    call go#util#EchoError("requires 'json_decode'. Update your Vim/Neovim version.")
    return
  endif

  let args = {
        \ 'mode': 'describe',
        \ 'format': 'json',
        \ 'selected': -1,
        \ 'needs_scope': 0,
        \ 'custom_parse': function('s:info'),
        \ 'disable_progress': a:showstatus == 0,
        \ }

  call s:run_guru(args)
endfunction

function! s:info(exit_val, output, mode)
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

    let info = val["type"]
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

    let info = type["type"]
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

  call go#util#ShowInfo(info)
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

function! go#guru#SameIds(showstatus) abort
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
  if !a:showstatus
    let args.disable_progress = 1
  endif

  call s:run_guru(args)
endfunction

function! s:same_ids_highlight(exit_val, output, mode) abort
  call go#guru#ClearSameIds() " run after calling guru to reduce flicker.

  if a:output[0] !=# '{'
    if !go#config#AutoSameids()
      call go#util#EchoError(a:output)
    endif
    return
  endif

  let result = json_decode(a:output)
  if type(result) != type({}) && !go#config#AutoSameids()
    call go#util#EchoError("malformed output from guru")
    return
  endif

  if !has_key(result, 'sameids')
    if !go#config#AutoSameids()
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

  if go#config#AutoSameids()
    " re-apply SameIds at the current cursor position at the time the buffer
    " is redisplayed: e.g. :edit, :GoRename, etc.
    augroup vim-go-sameids
      autocmd! * <buffer>
      autocmd BufWinEnter <buffer> nested call go#guru#SameIds(0)
    augroup end
  endif
endfunction

" ClearSameIds returns 0 when it removes goSameId groups and non-zero if no
" goSameId groups are found.
function! go#guru#ClearSameIds() abort
  let l:cleared = 0

  let m = getmatches()
  for item in m
    if item['group'] == 'goSameId'
      call matchdelete(item['id'])
      let l:cleared = 1
    endif
  endfor

  if !l:cleared
    return 1
  endif

  " remove the autocmds we defined
  augroup vim-go-sameids
    autocmd! * <buffer>
  augroup end

  return 0
endfunction

function! go#guru#ToggleSameIds() abort
  if go#guru#ClearSameIds() != 0
    call go#guru#SameIds(1)
  endif
endfunction

function! go#guru#AutoToggleSameIds() abort
  if go#config#AutoSameids()
    call go#util#EchoProgress("sameids auto highlighting disabled")
    call go#guru#ClearSameIds()
    call go#config#SetAutoSameids(0)
    return
  endif

  call go#util#EchoSuccess("sameids auto highlighting enabled")
  call go#config#SetAutoSameids(1)
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

  let errformat = "%f:%l.%c-%[%^:]%#:\ %m,%f:%l:%c:\ %m"
  let l:listtype = go#list#Type("_guru")
  call go#list#ParseFormat(l:listtype, errformat, a:output, a:title)

  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
endfun

function! go#guru#Scope(...) abort
  if a:0
    let scope = a:000
    if a:0 == 1 && a:1 == '""'
      let scope = []
    endif

    call go#config#SetGuruScope(scope)
    if empty(scope)
      call go#util#EchoSuccess("guru scope is cleared")
    else
      call go#util#EchoSuccess("guru scope changed to: ". join(a:000, ","))
    endif

    return
  endif

  let scope = go#config#GuruScope()
  if empty(scope)
    call go#util#EchoError("guru scope is not set")
  else
    call go#util#EchoSuccess("current guru scope: ". join(scope, ","))
  endif
endfunction

function! go#guru#DescribeBalloon() abort
  " don't even try if async isn't available.
  if !go#util#has_job()
    return
  endif

  " json_encode() and friends are introduced with this patch (7.4.1304)
  " vim: https://groups.google.com/d/msg/vim_dev/vLupTNhQhZ8/cDGIk0JEDgAJ
  " nvim: https://github.com/neovim/neovim/pull/4131
  if !exists("*json_decode")
    call go#util#EchoError("requires 'json_decode'. Update your Vim/Neovim version.")
    return
  endif

  " change the active window to the window where the cursor is.
  let l:winid = win_getid(winnr())
  call win_gotoid(v:beval_winid)

  let l:args = {
        \ 'mode': 'describe',
        \ 'format': 'json',
        \ 'selected': -1,
        \ 'needs_scope': 0,
        \ 'custom_parse': function('s:describe_balloon'),
        \ 'disable_progress': 1,
        \ 'postype': 'balloon',
        \ }

  call s:async_guru(args)

  " make the starting window active again
  call win_gotoid(l:winid)

  return ''
endfunction

function! s:describe_balloon(exit_val, output, mode)
  if a:exit_val != 0
    return
  endif

  if a:output[0] !=# '{'
    return
  endif

  if empty(a:output) || type(a:output) != type("")
    return
  endif

  let l:result = json_decode(a:output)
  if type(l:result) != type({})
    call go#util#EchoError(printf('malformed output from guru: %s', a:output))
    return
  endif

  let l:info = []
  if has_key(l:result, 'desc')
    if l:result['desc'] != 'identifier'
      let l:info = add(l:info, l:result['desc'])
    endif
  endif

  if has_key(l:result, 'detail')
    let l:detail = l:result['detail']

    " guru gives different information based on the detail mode. Let try to
    " extract the most useful information

    if l:detail == 'value'
      if !has_key(l:result, 'value')
        call go#util#EchoError('value key is missing. Please open a bug report on vim-go repo.')
        return
      endif

      let l:val = l:result['value']
      if !has_key(l:val, 'type')
        call go#util#EchoError('type key is missing (value.type). Please open a bug report on vim-go repo.')
        return
      endif

      let l:info = add(l:info, printf('type: %s', l:val['type']))
      if has_key(l:val, 'value')
        let l:info = add(l:info, printf('value: %s', l:val['value']))
      endif
    elseif l:detail == 'type'
      if !has_key(l:result, 'type')
        call go#util#EchoError('type key is missing. Please open a bug report on vim-go repo.')
        return
      endif

      let l:type = l:result['type']
      if !has_key(l:type, 'type')
        call go#util#EchoError('type key is missing (type.type). Please open a bug report on vim-go repo.')
        return
      endif

      let l:info = add(l:info, printf('type: %s', l:type['type']))

      if has_key(l:type, 'methods')
        let l:info = add(l:info, 'methods:')
        for l:m in l:type.methods
          let l:info = add(l:info, printf("\t%s", l:m['name']))
        endfor
      endif
    elseif l:detail == 'package'
      if !has_key(l:result, 'package')
        call go#util#EchoError('package key is missing. Please open a bug report on vim-go repo.')
        return
      endif

      let l:package = result['package']
      if !has_key(l:package, 'path')
        call go#util#EchoError('path key is missing (package.path). Please open a bug report on vim-go repo.')
        return
      endif

      let l:info = add(l:info, printf('package: %s', l:package["path"]))
    elseif l:detail == 'unknown'
      " the description is already included in l:info, and there's no other
      " information on unknowns.
    else
      call go#util#EchoError(printf('unknown detail mode (%s) found. Please open a bug report on vim-go repo', l:detail))
      return
    endif
  endif

  if has('balloon_eval')
    call balloon_show(join(l:info, "\n"))
    return
  endif

  call balloon_show(l:info)
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
