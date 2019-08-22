" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

if !exists('s:state')
  let s:state = {
      \ 'rpcid': 1,
      \ 'running': 0,
      \ 'currentThread': {},
      \ 'localVars': {},
      \ 'functionArgs': {},
      \ 'message': [],
      \}

  if go#util#HasDebug('debugger-state')
     call go#config#SetDebugDiag(s:state)
  endif
endif

if !exists('s:start_args')
  let s:start_args = []
endif

function! s:groutineID() abort
  return s:state['currentThread'].goroutineID
endfunction

function! s:complete(job, exit_status, data) abort
  let l:gotready = get(s:state, 'ready', 0)
  " copy messages to a:data _only_ when dlv exited non-zero and it was never
  " detected as ready (e.g. there was a compiler error).
  if a:exit_status > 0 && !l:gotready
      " copy messages to data so that vim-go's usual handling of errors from
      " async jobs will occur.
      call extend(a:data, s:state['message'])
  endif

  " return early instead of clearing any variables when the current job is not
  " a:job
  if has_key(s:state, 'job') && s:state['job'] != a:job
    return
  endif

  if has_key(s:state, 'job')
    call remove(s:state, 'job')
  endif

  if has_key(s:state, 'ready')
    call remove(s:state, 'ready')
  endif

  if has_key(s:state, 'ch')
    call remove(s:state, 'ch')
  endif

  call s:clearState()
endfunction

function! s:logger(prefix, ch, msg) abort
  let l:cur_win = bufwinnr('')
  let l:log_win = bufwinnr(bufnr('__GODEBUG_OUTPUT__'))
  if l:log_win == -1
    return
  endif
  exe l:log_win 'wincmd w'

  try
    setlocal modifiable
    if getline(1) == ''
      call setline('$', a:prefix . a:msg)
    else
      call append('$', a:prefix . a:msg)
    endif
    normal! G
    setlocal nomodifiable
  finally
    exe l:cur_win 'wincmd w'
  endtry
endfunction

function! s:call_jsonrpc(method, ...) abort
  if go#util#HasDebug('debugger-commands')
    echom 'sending to dlv ' . a:method
  endif

  let l:args = a:000
  let s:state['rpcid'] += 1
  let l:req_json = json_encode({
      \  'id': s:state['rpcid'],
      \  'method': a:method,
      \  'params': l:args,
      \})

  try
    let l:ch = s:state['ch']
    if has('nvim')
      call chansend(l:ch, l:req_json)
      while len(s:state.data) == 0
        sleep 50m
        if get(s:state, 'ready', 0) == 0
          return
        endif
      endwhile
      let resp_json = s:state.data[0]
      let s:state.data = s:state.data[1:]
    else
      call ch_sendraw(l:ch, req_json)
      let l:resp_raw = ch_readraw(l:ch)
      let resp_json = json_decode(l:resp_raw)
    endif

    if go#util#HasDebug('debugger-commands')
      let g:go_debug_commands = add(go#config#DebugCommands(), {
            \ 'request':  l:req_json,
            \ 'response': l:resp_json,
      \ })
    endif

    if type(l:resp_json) == v:t_dict && has_key(l:resp_json, 'error') && !empty(l:resp_json.error)
      throw l:resp_json.error
    endif
    return l:resp_json
  catch
    throw substitute(v:exception, '^Vim', '', '')
  endtry
endfunction

" Update the location of the current breakpoint or line we're halted on based on
" response from dlv.
function! s:update_breakpoint(res) abort
  if type(a:res) ==# type(v:null)
    return
  endif

  let state = a:res.result.State
  if !has_key(state, 'currentThread')
    return
  endif

  let s:state['currentThread'] = state.currentThread
  let bufs = filter(map(range(1, winnr('$')), '[v:val,bufname(winbufnr(v:val))]'), 'v:val[1]=~"\.go$"')
  if len(bufs) == 0
    return
  endif

  exe bufs[0][0] 'wincmd w'
  let filename = state.currentThread.file
  let linenr = state.currentThread.line
  let oldfile = fnamemodify(expand('%'), ':p:gs!\\!/!')
  if oldfile != filename
    silent! exe 'edit' filename
  endif
  silent! exe 'norm!' linenr.'G'
  silent! normal! zvzz
  silent! sign unplace 9999
  silent! exe 'sign place 9999 line=' . linenr . ' name=godebugcurline file=' . filename
endfunction

" Populate the stacktrace window.
function! s:show_stacktrace(res) abort
  if !has_key(a:res, 'result')
    return
  endif

  let l:stack_win = bufwinnr(bufnr('__GODEBUG_STACKTRACE__'))
  if l:stack_win == -1
    return
  endif

  let l:cur_win = bufwinnr('')
  exe l:stack_win 'wincmd w'

  try
    setlocal modifiable
    silent %delete _
    for i in range(len(a:res.result.Locations))
      let loc = a:res.result.Locations[i]
      call setline(i+1, printf('%s - %s:%d', loc.function.name, fnamemodify(loc.file, ':p'), loc.line))
    endfor
  finally
    setlocal nomodifiable
    exe l:cur_win 'wincmd w'
  endtry
endfunction

" Populate the variable window.
function! s:show_variables() abort
  let l:var_win = bufwinnr(bufnr('__GODEBUG_VARIABLES__'))
  if l:var_win == -1
    return
  endif

  let l:cur_win = bufwinnr('')
  exe l:var_win 'wincmd w'

  try
    setlocal modifiable
    silent %delete _

    let v = []
    let v += ['# Local Variables']
    if type(get(s:state, 'localVars', [])) is type([])
      for c in s:state['localVars']
        let v += split(s:eval_tree(c, 0), "\n")
      endfor
    endif

    let v += ['']
    let v += ['# Function Arguments']
    if type(get(s:state, 'functionArgs', [])) is type([])
      for c in s:state['functionArgs']
        let v += split(s:eval_tree(c, 0), "\n")
      endfor
    endif

    call setline(1, v)
  finally
    setlocal nomodifiable
    exe l:cur_win 'wincmd w'
  endtry
endfunction

function! s:clearState() abort
  let s:state['currentThread'] = {}
  let s:state['localVars'] = {}
  let s:state['functionArgs'] = {}
  let s:state['message'] = []

  silent! sign unplace 9999
endfunction

function! s:stop() abort
  let l:res = s:call_jsonrpc('RPCServer.Detach', {'kill': v:true})

  if has_key(s:state, 'job')
    call go#job#Wait(s:state['job'])

    " while waiting, the s:complete may have already removed job from s:state.
    if has_key(s:state, 'job')
      call remove(s:state, 'job')
    endif
  endif

  if has_key(s:state, 'ready')
    call remove(s:state, 'ready')
  endif

  if has_key(s:state, 'ch')
    call remove(s:state, 'ch')
  endif

  call s:clearState()
endfunction

function! go#debug#Stop() abort
  " Remove all commands and add back the default commands.
  for k in map(split(execute('command GoDebug'), "\n")[1:], 'matchstr(v:val, "^\\s*\\zs\\S\\+")')
    exe 'delcommand' k
  endfor
  command! -nargs=* -complete=customlist,go#package#Complete GoDebugStart call go#debug#Start(0, <f-args>)
  command! -nargs=* -complete=customlist,go#package#Complete GoDebugTest  call go#debug#Start(1, <f-args>)
  command! -nargs=? GoDebugBreakpoint call go#debug#Breakpoint(<f-args>)

  " Remove all mappings.
  for k in map(split(execute('map <Plug>(go-debug-'), "\n")[1:], 'matchstr(v:val, "^n\\s\\+\\zs\\S\\+")')
    exe 'unmap' k
  endfor

  call s:stop()

  let bufs = filter(map(range(1, winnr('$')), '[v:val,bufname(winbufnr(v:val))]'), 'v:val[1]=~"\.go$"')
  if len(bufs) > 0
    exe bufs[0][0] 'wincmd w'
  else
    wincmd p
  endif
  silent! exe bufwinnr(bufnr('__GODEBUG_STACKTRACE__')) 'wincmd c'
  silent! exe bufwinnr(bufnr('__GODEBUG_VARIABLES__')) 'wincmd c'
  silent! exe bufwinnr(bufnr('__GODEBUG_OUTPUT__')) 'wincmd c'

  if has('balloon_eval')
    let &ballooneval=s:ballooneval
    let &balloonexpr=s:balloonexpr
  endif

  augroup vim-go-debug
    autocmd!
  augroup END
  augroup! vim-go-debug
endfunction

function! s:goto_file() abort
  let m = matchlist(getline('.'), ' - \(.*\):\([0-9]\+\)$')
  if m[1] == ''
    return
  endif
  let bufs = filter(map(range(1, winnr('$')), '[v:val,bufname(winbufnr(v:val))]'), 'v:val[1]=~"\.go$"')
  if len(bufs) == 0
    return
  endif
  exe bufs[0][0] 'wincmd w'
  let filename = m[1]
  let linenr = m[2]
  let oldfile = fnamemodify(expand('%'), ':p:gs!\\!/!')
  if oldfile != filename
    silent! exe 'edit' filename
  endif
  silent! exe 'norm!' linenr.'G'
  silent! normal! zvzz
endfunction

function! s:delete_expands()
  let nr = line('.')
  while 1
    let l = getline(nr+1)
    if empty(l) || l =~ '^\S'
      return
    endif
    silent! exe (nr+1) . 'd _'
  endwhile
  silent! exe 'norm!' nr.'G'
endfunction

function! s:expand_var() abort
  " Get name from struct line.
  let name = matchstr(getline('.'), '^[^:]\+\ze: [a-zA-Z0-9\.·]\+{\.\.\.}$')
  " Anonymous struct
  if name == ''
    let name = matchstr(getline('.'), '^[^:]\+\ze: struct {.\{-}}$')
  endif

  if name != ''
    setlocal modifiable
    let not_open = getline(line('.')+1) !~ '^ '
    let l = line('.')
    call s:delete_expands()

    if not_open
      call append(l, split(s:eval(name), "\n")[1:])
    endif
    silent! exe 'norm!' l.'G'
    setlocal nomodifiable
    return
  endif

  " Expand maps
  let m = matchlist(getline('.'), '^[^:]\+\ze: map.\{-}\[\(\d\+\)\]$')
  if len(m) > 0 && m[1] != ''
    setlocal modifiable
    let not_open = getline(line('.')+1) !~ '^ '
    let l = line('.')
    call s:delete_expands()
    if not_open
      " TODO: Not sure how to do this yet... Need to get keys of the map.
      " let vs = ''
      " for i in range(0, min([10, m[1]-1]))
      "   let vs .= ' ' . s:eval(printf("%s[%s]", m[0], ))
      " endfor
      " call append(l, split(vs, "\n"))
    endif

    silent! exe 'norm!' l.'G'
    setlocal nomodifiable
    return
  endif

  " Expand string.
  let m = matchlist(getline('.'), '^\([^:]\+\)\ze: \(string\)\[\([0-9]\+\)\]\(: .\{-}\)\?$')
  if len(m) > 0 && m[1] != ''
    setlocal modifiable
    let not_open = getline(line('.')+1) !~ '^ '
    let l = line('.')
    call s:delete_expands()

    if not_open
      let vs = ''
      for i in range(0, min([10, m[3]-1]))
        let vs .= ' ' . s:eval(m[1] . '[' . i . ']')
      endfor
      call append(l, split(vs, "\n"))
    endif

    silent! exe 'norm!' l.'G'
    setlocal nomodifiable
    return
  endif

  " Expand slice.
  let m = matchlist(getline('.'), '^\([^:]\+\)\ze: \(\[\]\w\{-}\)\[\([0-9]\+\)\]$')
  if len(m) > 0 && m[1] != ''
    setlocal modifiable
    let not_open = getline(line('.')+1) !~ '^ '
    let l = line('.')
    call s:delete_expands()

    if not_open
      let vs = ''
      for i in range(0, min([10, m[3]-1]))
        let vs .= ' ' . s:eval(m[1] . '[' . i . ']')
      endfor
      call append(l, split(vs, "\n"))
    endif
    silent! exe 'norm!' l.'G'
    setlocal nomodifiable
    return
  endif
endfunction

function! s:start_cb() abort
  let l:winid = win_getid()
  silent! only!

  let winnum = bufwinnr(bufnr('__GODEBUG_STACKTRACE__'))
  if winnum != -1
    return
  endif

  let debugwindows = go#config#DebugWindows()
  if has_key(debugwindows, "stack") && debugwindows['stack'] != ''
    exe 'silent ' . debugwindows['stack']
    silent file `='__GODEBUG_STACKTRACE__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=godebugstacktrace
    nmap <buffer> <cr> :<c-u>call <SID>goto_file()<cr>
    nmap <buffer> q <Plug>(go-debug-stop)
  endif

  if has_key(debugwindows, "out") && debugwindows['out'] != ''
    exe 'silent ' . debugwindows['out']
    silent file `='__GODEBUG_OUTPUT__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=godebugoutput
    nmap <buffer> q <Plug>(go-debug-stop)
  endif

  if has_key(debugwindows, "vars") && debugwindows['vars'] != ''
    exe 'silent ' . debugwindows['vars']
    silent file `='__GODEBUG_VARIABLES__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=godebugvariables
    call append(0, ["# Local Variables", "", "# Function Arguments"])
    nmap <buffer> <silent> <cr> :<c-u>call <SID>expand_var()<cr>
    nmap <buffer> q <Plug>(go-debug-stop)
  endif

  silent! delcommand GoDebugStart
  silent! delcommand GoDebugTest
  command! -nargs=0 GoDebugContinue   call go#debug#Stack('continue')
  command! -nargs=0 GoDebugNext       call go#debug#Stack('next')
  command! -nargs=0 GoDebugStep       call go#debug#Stack('step')
  command! -nargs=0 GoDebugStepOut    call go#debug#Stack('stepOut')
  command! -nargs=0 GoDebugRestart    call go#debug#Restart()
  command! -nargs=0 GoDebugStop       call go#debug#Stop()
  command! -nargs=* GoDebugSet        call go#debug#Set(<f-args>)
  command! -nargs=1 GoDebugPrint      call go#debug#Print(<q-args>)

  nnoremap <silent> <Plug>(go-debug-breakpoint) :<C-u>call go#debug#Breakpoint()<CR>
  nnoremap <silent> <Plug>(go-debug-next)       :<C-u>call go#debug#Stack('next')<CR>
  nnoremap <silent> <Plug>(go-debug-step)       :<C-u>call go#debug#Stack('step')<CR>
  nnoremap <silent> <Plug>(go-debug-stepout)    :<C-u>call go#debug#Stack('stepout')<CR>
  nnoremap <silent> <Plug>(go-debug-continue)   :<C-u>call go#debug#Stack('continue')<CR>
  nnoremap <silent> <Plug>(go-debug-stop)       :<C-u>call go#debug#Stop()<CR>
  nnoremap <silent> <Plug>(go-debug-print)      :<C-u>call go#debug#Print(expand('<cword>'))<CR>

  if has('balloon_eval')
    let s:balloonexpr=&balloonexpr
    let s:ballooneval=&ballooneval

    set balloonexpr=go#debug#BalloonExpr()
    set ballooneval
  endif

  call win_gotoid(l:winid)

  augroup vim-go-debug
    autocmd! * <buffer>
    autocmd FileType go nmap <buffer> <F5>   <Plug>(go-debug-continue)
    autocmd FileType go nmap <buffer> <F6>   <Plug>(go-debug-print)
    autocmd FileType go nmap <buffer> <F9>   <Plug>(go-debug-breakpoint)
    autocmd FileType go nmap <buffer> <F10>  <Plug>(go-debug-next)
    autocmd FileType go nmap <buffer> <F11>  <Plug>(go-debug-step)
  augroup END
  doautocmd vim-go-debug FileType go
endfunction

function! s:err_cb(ch, msg) abort
  if get(s:state, 'ready', 0) != 0
    call call('s:logger', ['ERR: ', a:ch, a:msg])
    return
  endif

  let s:state['message'] += [a:msg]
endfunction

function! s:out_cb(ch, msg) abort
  if get(s:state, 'ready', 0) != 0
    call call('s:logger', ['OUT: ', a:ch, a:msg])
    return
  endif

  let s:state['message'] += [a:msg]

  if stridx(a:msg, go#config#DebugAddress()) != -1
    if has('nvim')
      let s:state['data'] = []
      let l:state = {'databuf': ''}

      " explicitly bind callback to state so that within it, self will
      " always refer to state. See :help Partial for more information.
      let l:state.on_data = function('s:on_data', [], l:state)
      let l:ch = sockconnect('tcp', go#config#DebugAddress(), {'on_data': l:state.on_data, 'state': l:state})
      if l:ch == 0
        call go#util#EchoError("could not connect to debugger")
        call go#job#Stop(s:state['job'])
        return
      endif
    else
      let l:ch = ch_open(go#config#DebugAddress(), {'mode': 'raw', 'timeout': 20000})
      if ch_status(l:ch) !=# 'open'
        call go#util#EchoError("could not connect to debugger")
        call go#job#Stop(s:state['job'])
        return
      endif
    endif

    let s:state['ch'] = l:ch

    " After this block executes, Delve will be running with all the
    " breakpoints setup, so this callback doesn't have to run again; just log
    " future messages.
    let s:state['ready'] = 1

    " replace all the breakpoints set before delve started so that the ids won't overlap.
    let l:breakpoints = s:list_breakpoints()
    for l:bt in s:list_breakpoints()
      exe 'sign unplace '. l:bt.id
      call go#debug#Breakpoint(l:bt.line, l:bt.file)
    endfor

    call s:start_cb()
  endif
endfunction

function! s:on_data(ch, data, event) dict abort
  let l:data = self.databuf
  for l:msg in a:data
    let l:data .= l:msg
  endfor

  try
    let l:res = json_decode(l:data)
    let s:state['data'] = add(s:state['data'], l:res)
    let self.databuf = ''
  catch
    " there isn't a complete message in databuf: buffer l:data and try
    " again when more data comes in.
    let self.databuf = l:data
  finally
  endtry
endfunction

" Start the debug mode. The first argument is the package name to compile and
" debug, anything else will be passed to the running program.
function! go#debug#Start(is_test, ...) abort
  call go#cmd#autowrite()

  if !go#util#has_job()
    call go#util#EchoError('This feature requires either Vim 8.0.0087 or newer with +job or Neovim.')
    return
  endif

  " It's already running.
  if has_key(s:state, 'job')
    return s:state['job']
  endif

  let s:start_args = [a:is_test] + a:000

  if go#util#HasDebug('debugger-state')
    call go#config#SetDebugDiag(s:state)
  endif

  let dlv = go#path#CheckBinPath("dlv")
  if empty(dlv)
    return
  endif

  try
    let l:cmd = [
          \ dlv,
          \ (a:is_test ? 'test' : 'debug'),
     \]

    " append the package when it's given.
    if len(a:000) > 0
      let l:pkgname = a:1
      if l:pkgname[0] == '.'
        let l:pkgabspath = fnamemodify(l:pkgname, ':p')

        let l:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
        let l:dir = getcwd()
        execute l:cd fnameescape(expand('%:p:h'))

        try
          let l:pkgname = go#package#FromPath(l:pkgabspath)
          if type(l:pkgname) == type(0)
            call go#util#EchoError('could not determine package name')
            return
          endif
        finally
          execute l:cd fnameescape(l:dir)
        endtry
      endif

      let l:cmd += [l:pkgname]
    endif

    let l:cmd += [
          \ '--output', tempname(),
          \ '--headless',
          \ '--api-version', '2',
          \ '--listen', go#config#DebugAddress(),
    \]
    let l:debugLogOutput = go#config#DebugLogOutput()
    if l:debugLogOutput != ''
      let cmd += ['--log', '--log-output', l:debugLogOutput]
    endif

    let l:buildtags = go#config#BuildTags()
    if buildtags isnot ''
      let l:cmd += ['--build-flags', '--tags=' . buildtags]
    endif

    if len(a:000) > 1
      let l:cmd += ['--'] + a:000[1:]
    endif

    let s:state['message'] = []
    let l:opts = {
          \ 'for': 'GoDebug',
          \ 'statustype': 'debug',
          \ 'complete': function('s:complete'),
          \ }
    let l:opts = go#job#Options(l:opts)
    let l:opts.out_cb = function('s:out_cb')
    let l:opts.err_cb = function('s:err_cb')
    let l:opts.stoponexit = 'kill'

    let s:state['job'] = go#job#Start(l:cmd, l:opts)
  catch
    call go#util#EchoError(v:exception)
  endtry

  return s:state['job']
endfunction

" Translate a reflect kind constant to a human string.
function! s:reflect_kind(k)
  " Kind constants from Go's reflect package.
  return [
        \ 'Invalid Kind',
        \ 'Bool',
        \ 'Int',
        \ 'Int8',
        \ 'Int16',
        \ 'Int32',
        \ 'Int64',
        \ 'Uint',
        \ 'Uint8',
        \ 'Uint16',
        \ 'Uint32',
        \ 'Uint64',
        \ 'Uintptr',
        \ 'Float32',
        \ 'Float64',
        \ 'Complex64',
        \ 'Complex128',
        \ 'Array',
        \ 'Chan',
        \ 'Func',
        \ 'Interface',
        \ 'Map',
        \ 'Ptr',
        \ 'Slice',
        \ 'String',
        \ 'Struct',
        \ 'UnsafePointer',
  \ ][a:k]
endfunction

function! s:eval_tree(var, nest) abort
  if a:var.name =~ '^\~'
    return ''
  endif
  let nest = a:nest
  let v = ''
  let kind = s:reflect_kind(a:var.kind)
  if !empty(a:var.name)
    let v .= repeat(' ', nest) . a:var.name . ': '

    if kind == 'Bool'
      let v .= printf("%s\n", a:var.value)

    elseif kind == 'Struct'
      " Anonymous struct
      if a:var.type[:8] == 'struct { '
        let v .= printf("%s\n", a:var.type)
      else
        let v .= printf("%s{...}\n", a:var.type)
      endif

    elseif kind == 'String'
      let v .= printf("%s[%d]%s\n", a:var.type, a:var.len,
            \ len(a:var.value) > 0 ? ': ' . a:var.value : '')

    elseif kind == 'Slice' || kind == 'String' || kind == 'Map' || kind == 'Array'
      let v .= printf("%s[%d]\n", a:var.type, a:var.len)

    elseif kind == 'Chan' || kind == 'Func' || kind == 'Interface'
      let v .= printf("%s\n", a:var.type)

    elseif kind == 'Ptr'
      " TODO: We can do something more useful here.
      let v .= printf("%s\n", a:var.type)

    elseif kind == 'Complex64' || kind == 'Complex128'
      let v .= printf("%s%s\n", a:var.type, a:var.value)

    " Int, Float
    else
      let v .= printf("%s(%s)\n", a:var.type, a:var.value)
    endif
  else
    let nest -= 1
  endif

  if index(['Chan', 'Complex64', 'Complex128'], kind) == -1 && a:var.type != 'error'
    for c in a:var.children
      let v .= s:eval_tree(c, nest+1)
    endfor
  endif
  return v
endfunction

function! s:eval(arg) abort
  try
    let l:res = s:call_jsonrpc('RPCServer.State')
    let l:res = s:call_jsonrpc('RPCServer.Eval', {
          \ 'expr':  a:arg,
          \ 'scope': {'GoroutineID': l:res.result.State.currentThread.goroutineID}
      \ })
    return s:eval_tree(l:res.result.Variable, 0)
  catch
    call go#util#EchoError(v:exception)
    return ''
  endtry
endfunction

function! go#debug#BalloonExpr() abort
  silent! let l:v = s:eval(v:beval_text)
  return l:v
endfunction

function! go#debug#Print(arg) abort
  try
    echo substitute(s:eval(a:arg), "\n$", "", 0)
  catch
    call go#util#EchoError(v:exception)
  endtry
endfunction

function! s:update_variables() abort
  " FollowPointers requests pointers to be automatically dereferenced.
  " MaxVariableRecurse is how far to recurse when evaluating nested types.
  " MaxStringLen is the maximum number of bytes read from a string
  " MaxArrayValues is the maximum number of elements read from an array, a slice or a map.
  " MaxStructFields is the maximum number of fields read from a struct, -1 will read all fields.
  let l:cfg = {
        \ 'scope': {'GoroutineID': s:groutineID()},
        \ 'cfg':   {'MaxStringLen': 20, 'MaxArrayValues': 20}
        \ }

  try
    let res = s:call_jsonrpc('RPCServer.ListLocalVars', l:cfg)
    let s:state['localVars'] = res.result['Variables']
  catch
    call go#util#EchoError(v:exception)
  endtry

  try
    let res = s:call_jsonrpc('RPCServer.ListFunctionArgs', l:cfg)
    let s:state['functionArgs'] = res.result['Args']
  catch
    call go#util#EchoError(v:exception)
  endtry

  call s:show_variables()
endfunction

function! go#debug#Set(symbol, value) abort
  try
    let l:res = s:call_jsonrpc('RPCServer.State')
    call s:call_jsonrpc('RPCServer.Set', {
          \ 'symbol': a:symbol,
          \ 'value':  a:value,
          \ 'scope':  {'GoroutineID': l:res.result.State.currentThread.goroutineID}
    \ })
  catch
    call go#util#EchoError(v:exception)
  endtry

  call s:update_variables()
endfunction

function! s:update_stacktrace() abort
  try
    let l:res = s:call_jsonrpc('RPCServer.Stacktrace', {'id': s:groutineID(), 'depth': 5})
    call s:show_stacktrace(l:res)
  catch
    call go#util#EchoError(v:exception)
  endtry
endfunction

function! s:stack_cb(res) abort
  let s:stack_name = ''

  if empty(a:res) || !has_key(a:res, 'result')
    return
  endif
  call s:update_breakpoint(a:res)
  call s:update_stacktrace()
  call s:update_variables()
endfunction

" Send a command to change the cursor location to Delve.
"
" a:name must be one of continue, next, step, or stepOut.
function! go#debug#Stack(name) abort
  let l:name = a:name

  " Run continue if the program hasn't started yet.
  if s:state.running is 0
    let s:state.running = 1
    let l:name = 'continue'
  endif

  " Add a breakpoint to the main.Main if the user didn't define any.
  if len(s:list_breakpoints()) is 0
    if go#debug#Breakpoint() isnot 0
      let s:state.running = 0
      return
    endif
  endif

  try
    " TODO: document why this is needed.
    if l:name is# 'next' && get(s:, 'stack_name', '') is# 'next'
      call s:call_jsonrpc('RPCServer.CancelNext')
    endif
    let s:stack_name = l:name
    try
      let res =  s:call_jsonrpc('RPCServer.Command', {'name': l:name})
      call s:stack_cb(res)
    catch
      call go#util#EchoError(v:exception)
      call s:clearState()
      call go#debug#Restart()
    endtry
  catch
    call go#util#EchoError(v:exception)
  endtry
endfunction

function! go#debug#Restart() abort
  call go#cmd#autowrite()

  try
    call s:stop()

    let s:state = {
        \ 'rpcid': 1,
        \ 'running': 0,
        \ 'currentThread': {},
        \ 'localVars': {},
        \ 'functionArgs': {},
        \ 'message': [],
        \}

    call call('go#debug#Start', s:start_args)
  catch
    call go#util#EchoError(v:exception)
  endtry
endfunction

" Report if debugger mode is active.
function! s:isActive()
  return len(s:state['message']) > 0
endfunction

" Toggle breakpoint. Returns 0 on success and 1 on failure.
function! go#debug#Breakpoint(...) abort
  let l:filename = fnamemodify(expand('%'), ':p:gs!\\!/!')
  let l:linenr = line('.')

  " Get line number from argument.
  if len(a:000) > 0
    let l:linenr = str2nr(a:1)
    if l:linenr is 0
      call go#util#EchoError('not a number: ' . a:1)
      return 0
    endif
    if len(a:000) > 1
      let l:filename = a:2
    endif
  endif

  try
    " Check if we already have a breakpoint for this line.
    let l:found = {}
    for l:bt in s:list_breakpoints()
      if l:bt.file is# l:filename && l:bt.line is# l:linenr
        let l:found = l:bt
        break
      endif
    endfor

    " Remove breakpoint.
    if type(l:found) == v:t_dict && !empty(l:found)
      exe 'sign unplace '. l:found.id .' file=' . l:found.file
      if s:isActive()
        let res = s:call_jsonrpc('RPCServer.ClearBreakpoint', {'id': l:found.id})
      endif
    " Add breakpoint.
    else
      if s:isActive()
        let l:res = s:call_jsonrpc('RPCServer.CreateBreakpoint', {'Breakpoint': {'file': l:filename, 'line': l:linenr}})
        let l:bt = res.result.Breakpoint
        exe 'sign place '. l:bt.id .' line=' . l:bt.line . ' name=godebugbreakpoint file=' . l:bt.file
      else
        let l:id = len(s:list_breakpoints()) + 1
        exe 'sign place ' . l:id . ' line=' . l:linenr . ' name=godebugbreakpoint file=' . l:filename
      endif
    endif
  catch
    call go#util#EchoError(v:exception)
    return 1
  endtry

  return 0
endfunction

function! s:list_breakpoints()
  " :sign place
  " --- Signs ---
  " Signs for a.go:
  "     line=15  id=2  name=godebugbreakpoint
  "     line=16  id=1  name=godebugbreakpoint
  " Signs for a_test.go:
  "     line=6  id=3  name=godebugbreakpoint

  let l:signs = []
  let l:file = ''
  for l:line in split(execute('sign place'), '\n')[1:]
    if l:line =~# '^Signs for '
      let l:file = l:line[10:-2]
      continue
    endif

    if l:line !~# 'name=godebugbreakpoint'
      continue
    endif

    let l:sign = matchlist(l:line, '\vline\=(\d+) +id\=(\d+)')
    call add(l:signs, {
          \ 'id': l:sign[2],
          \ 'file': fnamemodify(l:file, ':p'),
          \ 'line': str2nr(l:sign[1]),
    \ })
  endfor

  return l:signs
endfunction

sign define godebugbreakpoint text=> texthl=GoDebugBreakpoint
sign define godebugcurline    text== texthl=GoDebugCurrent    linehl=GoDebugCurrent

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
