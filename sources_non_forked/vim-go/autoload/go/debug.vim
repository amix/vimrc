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
        \ 'registers': {},
        \ 'message': [],
        \ 'resultHandlers': {},
        \ 'kill_on_detach': v:true,
      \ }

  if go#util#HasDebug('debugger-state')
     call go#config#SetDebugDiag(s:state)
  endif
endif

if !exists('s:start_args')
  let s:start_args = []
endif

if !exists('s:mapargs')
  let s:mapargs = {}
endif

function! s:goroutineID() abort
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

" s:call_jsonrpc will call method, passing all of s:call_jsonrpc's optional
" arguments in the rpc request's params field.

" The first argument to s:call_jsonrpc should be a function that takes two
" arguments. The first argument will be a function that takes no arguments and will
" throw an exception if the response to the request is an error response. The
" second argument is the response itself.
function! s:call_jsonrpc(handle_result, method, ...) abort
  if go#util#HasDebug('debugger-commands')
    call go#util#EchoInfo('sending to dlv ' . a:method)
  endif

  let l:args = a:000
  let s:state['rpcid'] += 1
  let l:reqid = s:state['rpcid']
  let l:req_json = json_encode({
      \  'id': l:reqid,
      \  'method': a:method,
      \  'params': l:args,
      \})

  try
    let l:ch = s:state['ch']
    if has('nvim')
      call chansend(l:ch, l:req_json)
    else
      call ch_sendraw(l:ch, req_json)
    endif

    let s:state.resultHandlers[l:reqid] = a:handle_result

    if go#util#HasDebug('debugger-commands')
      let g:go_debug_commands = add(go#config#DebugCommands(), {
            \ 'request':  l:req_json,
      \ })
    endif

    redraw
  catch
    throw substitute(v:exception, '^Vim', '', '')
  endtry
endfunction

function! s:exited(res) abort
  if type(a:res) ==# type(v:null)
    return 0
  endif

  let state = a:res.result.State
  return state.exited == v:true
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
  let filename = s:substituteRemotePath(state.currentThread.file)
  let linenr = state.currentThread.line
  let oldfile = fnamemodify(expand('%'), ':p:gs!\\!/!')
  if oldfile != filename
    silent! exe 'edit' filename
  endif
  silent! exe 'norm!' linenr.'G'
  silent! normal! zvzz
  " TODO(bc): convert to use s:sign_unplace()
  silent! sign unplace 9999
  " TODO(bc): convert to use s:sign_place()
  silent! exe 'sign place 9999 line=' . linenr . ' name=godebugcurline file=' . filename
  call s:warn_when_stale(fnamemodify(l:filename, ':p'))
endfunction

" Populate the stacktrace window.
function! s:show_stacktrace(check_errors, res) abort
  try
    call a:check_errors()
  catch
    call go#util#EchoError(printf('could not update stack: %s', v:exception))
    return
  endtry

  if type(a:res) isnot type({}) || !has_key(a:res, 'result') || empty(a:res.result)
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
      if loc.file is# '?' || !has_key(loc, 'function')
        continue
      endif
      call setline(i+1, printf('%s - %s:%d', loc.function.name, s:substituteRemotePath(fnamemodify(loc.file, ':p')), loc.line))
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
        let v += split(s:eval_tree(c, 0, 0), "\n")
      endfor
    endif

    let v += ['']
    let v += ['# Function Arguments']
    if type(get(s:state, 'functionArgs', [])) is type([])
      for c in s:state['functionArgs']
        let v += split(s:eval_tree(c, 0, 0), "\n")
      endfor
    endif

    let v += ['']
    let v += ['# Registers']
    if type(get(s:state, 'registers', [])) is type([])
      for c in s:state['registers']
        let v += [printf("%s = %s", c.Name, c.Value)]
      endfor
    endif

    call setline(1, v)
  finally
    setlocal nomodifiable
    exe l:cur_win 'wincmd w'
  endtry
endfunction

function! s:clearState() abort
  let s:state['running'] = 0
  let s:state['currentThread'] = {}
  let s:state['localVars'] = {}
  let s:state['functionArgs'] = {}
  let s:state['registers'] = {}
  let s:state['message'] = []

  silent! sign unplace 9999
endfunction

function! s:stop() abort
  call s:call_jsonrpc(function('s:noop'), 'RPCServer.Detach', {'kill': s:state['kill_on_detach']})

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
  command! -nargs=* -complete=customlist,go#package#Complete GoDebugStart call go#debug#Start('debug', <f-args>)
  command! -nargs=* -complete=customlist,go#package#Complete GoDebugTest  call go#debug#Start('test', <f-args>)
  command! -nargs=? GoDebugConnect call go#debug#Start('connect', <f-args>)
  command! -nargs=* GoDebugTestFunc  call go#debug#TestFunc(<f-args>)
  command! -nargs=1 GoDebugAttach call go#debug#Start('attach', <f-args>)
  command! -nargs=? GoDebugBreakpoint call go#debug#Breakpoint(<f-args>)

  " Restore mappings configured prior to debugging.
  call s:restoreMappings()

  " remove plug mappings
  for k in map(split(execute('nmap <Plug>(go-debug-'), "\n"), 'matchstr(v:val, "^n\\s\\+\\zs\\S\\+")')
    execute(printf('nunmap %s', k))
  endfor

  call s:stop()

  let bufs = filter(map(range(1, winnr('$')), '[v:val,bufname(winbufnr(v:val))]'), 'v:val[1]=~"\.go$"')
  if len(bufs) > 0
    exe bufs[0][0] 'wincmd w'
  else
    wincmd p
  endif

  let stackbufnr = bufnr('__GODEBUG_STACKTRACE__')
  if stackbufnr != -1
    silent! exe bufwinnr(stackbufnr) 'wincmd c'
  endif

  let varbufnr = bufnr('__GODEBUG_VARIABLES__')
  if varbufnr != -1
    silent! exe bufwinnr(varbufnr) 'wincmd c'
  endif

  let outbufnr = bufnr('__GODEBUG_OUTPUT__')
  if outbufnr != -1
    silent! exe bufwinnr(outbufnr) 'wincmd c'
  endif

  let gorobufnr = bufnr('__GODEBUG_GOROUTINES__')
  if gorobufnr != -1
    silent! exe bufwinnr(gorobufnr) 'wincmd c'
  endif

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
  let filename = s:substituteLocalPath(m[1])
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
  let name = matchstr(getline('.'), '^[^:]\+\ze: \*\?[a-zA-Z0-9-_/\.]\+\({\.\.\.}\)\?$')
  " Anonymous struct
  if name == ''
    let name = matchstr(getline('.'), '^[^:]\+\ze: \*\?struct {.\{-}}$')
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
  let l:debugwindows = go#config#DebugWindows()
  let l:debugpreservelayout = go#config#DebugPreserveLayout()

  if !(empty(l:debugwindows) || l:debugpreservelayout)
    silent! only!
  endif

  let winnum = bufwinnr(bufnr('__GODEBUG_STACKTRACE__'))
  if winnum != -1
    return
  endif

  if has_key(l:debugwindows, "vars") && l:debugwindows['vars'] != ''
    exe 'silent ' . l:debugwindows['vars']
    silent file `='__GODEBUG_VARIABLES__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=godebugvariables
    call append(0, ["# Local Variables", "", "# Function Arguments", "", "# Registers"])
    nmap <buffer> <silent> <cr> :<c-u>call <SID>expand_var()<cr>
    nmap <buffer> q <Plug>(go-debug-stop)
  endif

  if has_key(l:debugwindows, "stack") && l:debugwindows['stack'] != ''
    exe 'silent ' . l:debugwindows['stack']
    silent file `='__GODEBUG_STACKTRACE__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=godebugstacktrace
    nmap <buffer> <cr> :<c-u>call <SID>goto_file()<cr>
    nmap <buffer> q <Plug>(go-debug-stop)
  endif

  if has_key(l:debugwindows, "goroutines") && l:debugwindows['goroutines'] != ''
    exe 'silent ' . l:debugwindows['goroutines']
    silent file `='__GODEBUG_GOROUTINES__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=godebugvariables
    call append(0, ["# Goroutines"])
    nmap <buffer> <silent> <cr> :<c-u>call go#debug#Goroutine()<cr>
  endif

  if has_key(l:debugwindows, "out") && l:debugwindows['out'] != ''
    exe 'silent ' . l:debugwindows['out']
    silent file `='__GODEBUG_OUTPUT__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=godebugoutput
    nmap <buffer> q <Plug>(go-debug-stop)
  endif
  call win_gotoid(l:winid)

  silent! delcommand GoDebugStart
  silent! delcommand GoDebugTest
  silent! delcommand GoDebugTestFunc
  silent! delcommand GoDebugAttach
  silent! delcommand GoDebugConnect

  command! -nargs=0 GoDebugContinue   call go#debug#Stack('continue')
  command! -nargs=0 GoDebugStop       call go#debug#Stop()

  nnoremap <silent> <Plug>(go-debug-breakpoint) :<C-u>call go#debug#Breakpoint()<CR>
  nnoremap <silent> <Plug>(go-debug-continue)   :<C-u>call go#debug#Stack('continue')<CR>
  nnoremap <silent> <Plug>(go-debug-stop)       :<C-u>call go#debug#Stop()<CR>

  augroup vim-go-debug
    autocmd! *
    call s:configureMappings('(go-debug-breakpoint)', '(go-debug-continue)')
  augroup END
  doautocmd vim-go-debug BufWinEnter *.go
endfunction

function! s:continue()
  command! -nargs=0 GoDebugNext       call go#debug#Stack('next')
  command! -nargs=0 GoDebugStep       call go#debug#Stack('step')
  command! -nargs=0 GoDebugStepOut    call go#debug#Stack('stepOut')
  command! -nargs=0 GoDebugRestart    call go#debug#Restart()
  command! -nargs=* GoDebugSet        call go#debug#Set(<f-args>)
  command! -nargs=1 GoDebugPrint      call go#debug#Print(<q-args>)
  command! -nargs=0 GoDebugHalt       call go#debug#Stack('halt')

  nnoremap <silent> <Plug>(go-debug-next)       :<C-u>call go#debug#Stack('next')<CR>
  nnoremap <silent> <Plug>(go-debug-step)       :<C-u>call go#debug#Stack('step')<CR>
  nnoremap <silent> <Plug>(go-debug-stepout)    :<C-u>call go#debug#Stack('stepOut')<CR>
  nnoremap <silent> <Plug>(go-debug-print)      :<C-u>call go#debug#Print(expand('<cword>'))<CR>
  nnoremap <silent> <Plug>(go-debug-halt)       :<C-u>call go#debug#Stack('halt')<CR>

  if has('balloon_eval')
    let s:balloonexpr=&balloonexpr
    let s:ballooneval=&ballooneval

    set balloonexpr=go#debug#BalloonExpr()
    set ballooneval
  endif

  " Some debug mappings were already added. Restore any mappings the user had
  " before the complete mappings are configured so that the mappings are
  " returned to the user's original state after the debugger is stopped.
  call s:restoreMappings()
  augroup vim-go-debug
    autocmd! *
    call s:configureMappings('(go-debug-breakpoint)', '(go-debug-continue)', '(go-debug-halt)', '(go-debug-next)', '(go-debug-print)', '(go-debug-step)', '(go-debug-stepout)')
  augroup END
  doautocmd vim-go-debug BufWinEnter *.go
endfunction

function! s:err_cb(ch, msg) abort
  if get(s:state, 'ready', 0) != 0
    call s:logger('ERR: ', a:ch, a:msg)
    return
  endif

  let s:state['message'] += [a:msg]
endfunction

function! s:out_cb(ch, msg) abort
  if get(s:state, 'ready', 0) != 0
    call s:logger('OUT: ', a:ch, a:msg)
    return
  endif

  let s:state['message'] += [a:msg]

  if stridx(a:msg, go#config#DebugAddress()) != -1
    call s:connect(go#config#DebugAddress())
  endif
endfunction

function! s:connect(addr) abort
  let s:state['data'] = []
  let l:state = {'databuf': ''}

  " explicitly bind callback to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let l:state.on_data = function('s:on_data', [], l:state)

  if has('nvim')
    let l:ch = sockconnect('tcp', a:addr, {'on_data': l:state.on_data, 'state': l:state})
    if l:ch == 0
      call go#util#EchoError("could not connect to debugger")
      if has_key(s:state, 'job')
        call go#job#Stop(s:state['job'])
      endif
      return
    endif
  else
    let l:ch = ch_open(a:addr, {'mode': 'raw', 'waittime': 5000, 'timeout': 20000, 'callback': l:state.on_data})
    if ch_status(l:ch) !=# 'open'
      call go#util#EchoError("could not connect to debugger")
      if has_key(s:state, 'job')
        call go#job#Stop(s:state['job'])
      endif
      return
    endif
  endif

  let s:state['ch'] = l:ch

  " After this block executes, Delve will be running with all the
  " breakpoints setup, so this callback doesn't have to run again; just log
  " future messages.
  let s:state['ready'] = 1

  " replace all the breakpoints set before delve started so that the ids won't overlap.
  for l:bt in s:list_breakpoints()
    call s:sign_unplace(l:bt.id, l:bt.file)
    call go#debug#Breakpoint(l:bt.line, l:bt.file)
  endfor

  call s:start_cb()
endfunction

" s:on_data's third optional argument is provided, but not used, so that the
" same function can be used for Vim's 'callback' and Neovim's 'data'.
function! s:on_data(ch, data, ...) dict abort
  let l:data = s:message(self.databuf, a:data)

  let l:messages = split(l:data, "\n")
  for l:msg in l:messages
    let l:data = l:messages[0]
    try
      let l:res = json_decode(l:data)
      " remove the decoded message
      call remove(l:messages, 0)
    catch
      return
    finally
      " Rejoin messages and assign to databuf so that any messages that come
      " in if s:handleRPCResult sleeps will be appended correctly.
      "
      " Because the current message is removed in the try immediately after
      " decoding,  that l:messages contains all the messages that have not
      " yet been decoded including the current message if decoding it
      " failed.
      let self.databuf = join(l:messages, "\n")
    endtry

    if go#util#HasDebug('debugger-commands')
      let g:go_debug_commands = add(go#config#DebugCommands(), {
            \ 'response': l:data,
      \ })
    endif
    call s:handleRPCResult(l:res)
  endfor
endfunction

function! s:message(buf, data) abort
  if has('nvim')
    " dealing with the channel lines of Neovim is awful. The docs (:help
    " channel-lines) say:
    "     stream event handlers may receive partial (incomplete) lines. For a
    "     given invocation of on_stdout etc, `a:data` is not guaranteed to end
    "     with a newline.
    "       - `abcdefg` may arrive as `['abc']`, `['defg']`.
    "       - `abc\nefg` may arrive as `['abc', '']`, `['efg']` or `['abc']`,
    "         `['','efg']`, or even `['ab']`, `['c','efg']`.
    "
    " Thankfully, though, this is explained a bit better in an issue:
    " https://github.com/neovim/neovim/issues/3555. Specifically in these two
    " comments:
    "     * https://github.com/neovim/neovim/issues/3555#issuecomment-152290804
    "     * https://github.com/neovim/neovim/issues/3555#issuecomment-152588749
    "
    " The key is
    "     Every item in the list passed to job control callbacks represents a
    "     string after a newline(Except the first, of course). If the program
    "     outputs: "hello\nworld" the corresponding list is ["hello", "world"].
    "     If the program outputs "hello\nworld\n", the corresponding list is
    "     ["hello", "world", ""]. In other words, you can always determine if
    "     the last line received is complete or not.
    " and
    "     for every list you receive in a callback, all items except the first
    "     represent newlines.

    let l:data = printf('%s%s', a:buf, a:data[0])
    for l:msg in a:data[1:]
      let l:data = printf("%s\n%s", l:data, l:msg)
    endfor

    return l:data
  endif

  return printf('%s%s', a:buf, a:data)
endfunction

" s:check_errors will be curried and injected into rpc result handlers so that
" those result handlers can consistently check for errors in the response by
" catching exceptions and handling the error appropriately.
function! s:check_errors(resp_json) abort
  if type(a:resp_json) == v:t_dict && has_key(a:resp_json, 'error') && !empty(a:resp_json.error)
    throw a:resp_json.error
  endif
endfunction

function! s:handleRPCResult(resp) abort
  try
    let l:id = a:resp.id
    " call the result handler with its first argument set to a curried
    " s:check_errors value so that the result handler can call s:check_errors
    " without passing any arguments to check whether the response is an error
    " response.
    call call(s:state.resultHandlers[l:id], [function('s:check_errors', [a:resp]), a:resp])
  catch
    throw v:exception
  finally
    if has_key(s:state.resultHandlers, l:id)
      call remove(s:state.resultHandlers, l:id)
    endif
  endtry
endfunction

function! go#debug#TestFunc(...) abort
  let l:test = go#util#TestName()
  if l:test is ''
    call go#util#Warn("vim-go: [debug] no test found immediate to cursor")
    return
  endif
  call call('go#debug#Start', extend(['test', '.', '-test.run', printf('%s$', l:test)], a:000))
endfunction

" Start the debug mode. The first variadic argument is the package name to
" compile and debug, anything else will be passed to the running program.
function! go#debug#Start(mode, ...) abort
  call go#cmd#autowrite()

  if !go#util#has_job()
    call go#util#EchoError('This feature requires either Vim 8.0.0087 or newer with +job or Neovim.')
    return
  endif

  " It's already running.
  if has_key(s:state, 'job')
    return s:state['job']
  endif

  let s:start_args = [a:mode] + a:000

  if go#util#HasDebug('debugger-state')
    call go#config#SetDebugDiag(s:state)
  endif

  let dlv = go#path#CheckBinPath("dlv")
  if empty(dlv)
    return
  endif

  try
    if a:mode is 'connect'
      let l:addr = go#config#DebugAddress()
      if a:0 > 0
        let l:addr = a:1
      endif
      let s:state['kill_on_detach'] = v:false

      call s:connect(l:addr)
    else
      let l:cmd = [dlv, a:mode]

      let s:state['kill_on_detach'] = v:true
      if a:mode is 'debug' || a:mode is 'test'
        let l:cmd = extend(l:cmd, s:package(a:000))
        let l:cmd = extend(l:cmd, ['--output', tempname()])
      elseif a:mode is 'attach'
        let l:cmd = add(l:cmd, a:1)
        let s:state['kill_on_detach'] = v:false
      else
        call go#util#EchoError('Unknown dlv command')
      endif

      let l:cmd += [
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
      return s:state['job']
    endif
  catch
    call go#util#EchoError(printf('could not start debugger: %s', v:exception))
  endtry
endfunction

" s:package returns the import path of package name of a :GoDebug(Start|Test)
" call as a list so that the package can be appended to a command list using
" extend(). args is expected to be a (potentially empty) list. The first
" element in args (if there are any) is expected to be a package path. An
" empty list is returned when either args is an empty list or the import path
" cannot be determined.
function! s:package(args)
  if len(a:args) == 0
    return []
  endif

  " append the package when it's given.
  let l:pkgname = a:args[0]
  if l:pkgname[0] == '.'
    let l:pkgabspath = fnamemodify(l:pkgname, ':p')

    let l:dir = go#util#Chdir(expand('%:p:h'))
    try
      let l:pkgname = go#package#FromPath(l:pkgabspath)
      if type(l:pkgname) == type(0)
        call go#util#EchoError('could not determine package name')
        return []
      endif
    finally
      call go#util#Chdir(l:dir)
    endtry
  endif

  return [l:pkgname]
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

function! s:eval_tree(var, nest, isMapOrSliceChild) abort
  if a:var.name =~ '^\~'
    return ''
  endif
  let nest = a:nest
  let v = ''
  let kind = s:reflect_kind(a:var.kind)

  if !empty(a:var.name) || a:isMapOrSliceChild is 1
    if a:isMapOrSliceChild == 0
      let v .= repeat(' ', nest) . a:var.name . ': '
    endif

    if kind == 'Bool'
      let v .= printf("%s", a:var.value)

    elseif kind == 'Struct'
      " Anonymous struct
      if a:var.type[:8] == 'struct { '
        let v .= printf("%s", a:var.type)
      else
        let v .= printf("%s{...}", a:var.type)
      endif

    elseif kind == 'String'
      let v .= printf("%s[%d]%s", a:var.type, a:var.len,
            \ len(a:var.value) > 0 ? ': ' . a:var.value : '')

    elseif kind == 'Slice' || kind == 'String' || kind == 'Map' || kind == 'Array'
      let v .= printf("%s[%d]", a:var.type, a:var.len)

    elseif kind == 'Chan' || kind == 'Func' || kind == 'Interface'
      let v .= printf("%s", a:var.type)

    elseif kind == 'Ptr'
      " TODO: We can do something more useful here.
      let v .= printf("%s", a:var.type)

    elseif kind == 'Complex64' || kind == 'Complex128'
      let v .= printf("%s%s", a:var.type, a:var.value)

    " Int, Float
    else
      let v .= printf("%s(%s)", a:var.type, a:var.value)
    endif
    if a:isMapOrSliceChild == 0
      let v = printf("%s\n", v)
    endif
  else
    let nest -= 1
  endif

  if index(['Chan', 'Complex64', 'Complex128'], kind) == -1 && a:var.type != 'error'
    let l:idx = 0
    for c in a:var.children
      if kind == 'Map'
        " Maps alternate children between keys and values. Keys will be even
        " number indexes.
        let l:isMapKey = (l:idx % 2) is 0
        if l:isMapKey == 1
          let v .= printf("%s%s:\n", repeat(' ', nest + 1), s:eval_tree(c, 0, 1))
        else
          let v .= printf("%s%s\n", repeat(' ', nest + 2), s:eval_tree(c, 0, 1))
        endif
      elseif kind == 'Slice'
        let v .= printf("%d: %s\n", l:idx, s:eval_tree(c, nest + 1, 1))
      else
        let v .= s:eval_tree(c, nest + 1, 0)
      endif
      let l:idx += 1
    endfor
  endif
  return v
endfunction

function! s:eval(arg) abort
  try
    let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
    call s:call_jsonrpc(l:promise.wrapper, 'RPCServer.State')
    let l:res = l:promise.await()

    let l:cmd = 'RPCServer.Eval'
    let l:args = {
          \ 'expr':  a:arg,
          \ 'scope': {'GoroutineID': l:res.result.State.currentThread.goroutineID}
      \ }

    let l:ResultFn = funcref('s:evalResult', [])
    if a:arg =~ '^call '
      let l:cmd = 'RPCServer.Command'
      let l:args = {
            \ 'name': 'call',
            \ 'Expr': a:arg[5:],
            \ 'ReturnInfoLoadConfig': {
              \ 'FollowPointers': v:false,
              \ 'MaxVariableRecurse': 10,
              \ 'MaxStringLen': 80,
              \ 'MaxArrayValues': 10,
              \ 'MaxStructFields': 10,
            \ },
          \ }

      let l:ResultFn = funcref('s:callResult', [])
    endif

    let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
    call s:call_jsonrpc(l:promise.wrapper, l:cmd, l:args)

    let l:res = l:promise.await()

    let l:result = call(l:ResultFn, [l:res.result])

    " l:result will be a list when evaluating a call expression.
    if type(l:result) is type([])
      let l:result = map(l:result, funcref('s:renameEvalReturnValue'))
      if len(l:result) isnot 1
        return map(l:result, 's:eval_tree(v:val, 0, 0)')
      endif
      let l:result = l:result[0]
    endif
    return s:eval_tree(l:result, 0, 0)
  catch
    call go#util#EchoError(printf('evaluation failed: %s', v:exception))
    return ''
  endtry
endfunction

function! s:callResult(res) abort
  return a:res.State.currentThread.ReturnValues
endfunction

function! s:evalResult(res) abort
  return a:res.Variable
endfunction

function! s:renameEvalReturnValue(key, val) abort
  let a:val.name = printf('[%s]', string(a:key))
  return a:val
endfunction

function! go#debug#BalloonExpr() abort
  silent! let l:v = s:eval(v:beval_text)
  return l:v
endfunction

function! go#debug#Print(arg) abort
  try
    let l:result = s:eval(a:arg)
    if type(l:result) is type([])
      echo join(map(l:result, 'substitute(v:val, "\n$", "", '''')'), "\n")
      return
    elseif type(l:result) isnot type('')
      throw 'unexpected result'
    endif
    echo substitute(l:result, "\n$", "", '')
  catch
    call go#util#EchoError(printf('could not print: %s', v:exception))
  endtry
endfunction

function! s:update_goroutines() abort
  call s:call_jsonrpc(function('s:update_goroutines_state_handler'), 'RPCServer.State')
endfunction

function! s:update_goroutines_state_handler(check_errors, res) abort
  try
    call a:check_errors()

    let l:currentGoroutineID = 0
    try
      if type(a:res) is type({}) && has_key(a:res, 'result') && !empty(a:res['result'])
        let l:currentGoroutineID = a:res["result"]["State"]["currentGoroutine"]["id"]
      endif
    catch
      call go#util#EchoWarning("current goroutine not found...")
    endtry

    call s:call_jsonrpc(function('s:list_goroutines_handler', [l:currentGoroutineID]), 'RPCServer.ListGoroutines')
  catch
    call go#util#EchoError(printf('could not list goroutines: %s', v:exception))
  endtry
endfunction

function s:list_goroutines_handler(currentGoroutineID, check_errors, res) abort
  try
    call a:check_errors()
    call s:show_goroutines(a:currentGoroutineID, a:res)
  catch
    call go#util#EchoError(printf('could not show goroutines: %s', v:exception))
  endtry
endfunction

function! s:show_goroutines(currentGoroutineID, res) abort
  let l:goroutines_winid = bufwinid('__GODEBUG_GOROUTINES__')
  if l:goroutines_winid == -1
    return
  endif

  let l:winid = win_getid()
  call win_gotoid(l:goroutines_winid)

  try
    setlocal modifiable
    silent %delete _

    let v = []

    if type(a:res) isnot type({}) || !has_key(a:res, 'result') || empty(a:res['result'])
      call setline(1, v)
      return
    endif

    let l:goroutines = a:res["result"]["Goroutines"]
    if len(l:goroutines) == 0
      call go#util#EchoWarning("No Goroutines Running Now...")
      call setline(1, v)
      return
    endif

    for l:idx in range(len(l:goroutines))
      let l:goroutine = l:goroutines[l:idx]
      let l:goroutineType = ""
      let l:loc = 0
      if l:goroutine.startLoc.file != ""
          let l:loc = l:goroutine.startLoc
          let l:goroutineType = "Start"
      endif
      if l:goroutine.goStatementLoc.file != ""
          let l:loc = l:goroutine.goStatementLoc
          let l:goroutineType = "Go"
      endif
      if l:goroutine.currentLoc.file != ""
          let l:loc = l:goroutine.currentLoc
          let l:goroutineType = "Runtime"
      endif
      if l:goroutine.userCurrentLoc.file != ""
          let l:loc=l:goroutine.userCurrentLoc
          let l:goroutineType = "User"
      endif

      " The current goroutine can be changed by pressing enter on one of the
      " lines listing a non-active goroutine. If the format of either of these
      " lines is modified, then make sure that go#debug#Goroutine is also
      " changed if needed.
      if l:goroutine.id == a:currentGoroutineID
        let l:g = printf("* Goroutine %s - %s: %s:%s %s (thread: %s)", l:goroutine.id, l:goroutineType, s:substituteRemotePath(l:loc.file), l:loc.line, l:loc.function.name, l:goroutine.threadID)
        let l:currentGoroutine = [l:g]
        continue
      else
        let l:g = printf("  Goroutine %s - %s: %s:%s %s (thread: %s)", l:goroutine.id, l:goroutineType, s:substituteRemotePath(l:loc.file), l:loc.line, l:loc.function.name, l:goroutine.threadID)
      endif
      let v += [l:g]
    endfor

    let v = ['# Goroutines'] + l:currentGoroutine + v

    call setline(1, v)
  finally
    setlocal nomodifiable
    call win_gotoid(l:winid)
  endtry
endfunction

function! s:update_variables() abort
  " FollowPointers requests pointers to be automatically dereferenced.
  " MaxVariableRecurse is how far to recurse when evaluating nested types.
  " MaxStringLen is the maximum number of bytes read from a string
  " MaxArrayValues is the maximum number of elements read from an array, a slice or a map.
  " MaxStructFields is the maximum number of fields read from a struct, -1 will read all fields.
  let l:cfg = {
        \ 'scope': {'GoroutineID': s:goroutineID()},
        \ 'cfg':   {'MaxStringLen': 20, 'MaxArrayValues': 20, 'MaxVariableRecurse': 10}
        \ }

  try
    call s:call_jsonrpc(function('s:handle_list_local_vars'), 'RPCServer.ListLocalVars', l:cfg)
  catch
    call go#util#EchoError(printf('could not list variables: %s', v:exception))
  endtry

  try
    call s:call_jsonrpc(function('s:handle_list_function_args'), 'RPCServer.ListFunctionArgs', l:cfg)
  catch
    call go#util#EchoError(printf('could not list function arguments: %s', v:exception))
  endtry

  try
    call s:call_jsonrpc(function('s:handle_list_registers'), 'RPCServer.ListRegisters', l:cfg)
  catch
    call go#util#EchoError(printf('could not list registers: %s', v:exception))
  endtry

endfunction

function! s:handle_list_local_vars(check_errors, res) abort
  let s:state['localVars'] = {}
  try
    call a:check_errors()
    if type(a:res) is type({}) && has_key(a:res, 'result') && !empty(a:res.result)
      let s:state['localVars'] = a:res.result['Variables']
    endif
  catch
    call go#util#EchoWarning(printf('could not list variables: %s', v:exception))
  endtry

  call s:show_variables()
endfunction

function! s:handle_list_function_args(check_errors, res) abort
  let s:state['functionArgs'] = {}
  try
    call a:check_errors()
    if type(a:res) is type({}) && has_key(a:res, 'result') && !empty(a:res.result)
      let s:state['functionArgs'] = a:res.result['Args']
    endif
  catch
    call go#util#EchoWarning(printf('could not list function arguments: %s', v:exception))
  endtry

  call s:show_variables()
endfunction

function! s:handle_list_registers(check_errors, res) abort
  let s:state['registers'] = {}
  try
    call a:check_errors()
    if type(a:res) is type({}) && has_key(a:res, 'result') && !empty(a:res.result)
      let s:state['registers'] = a:res.result['Regs']
    endif
  catch
    call go#util#EchoWarning(printf('could not list registers: %s', v:exception))
  endtry

  call s:show_variables()
endfunction

function! go#debug#Set(symbol, value) abort
  try
    let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
    call s:call_jsonrpc(l:promise.wrapper, 'RPCServer.State')
    let l:res = l:promise.await()

    call s:call_jsonrpc(function('s:handle_set'), 'RPCServer.Set', {
          \ 'symbol': a:symbol,
          \ 'value':  a:value,
          \ 'scope':  {'GoroutineID': l:res.result.State.currentThread.goroutineID}
    \ })
  catch
    call go#util#EchoError(printf('could not set symbol value: %s', v:exception))
  endtry

  call s:update_variables()
endfunction

function! s:handle_set(check_errors, res) abort
  try
    call a:check_errors()
  catch
    call go#util#EchoError(printf('could not set symbol value: %s', v:exception))
  endtry

  call s:update_variables()
endfunction

function! s:update_stacktrace() abort
  try
    call s:call_jsonrpc(function('s:show_stacktrace'), 'RPCServer.Stacktrace', {'id': s:goroutineID(), 'depth': 5})
  catch
    call go#util#EchoError(printf('could not update stack: %s', v:exception))
  endtry
endfunction

function! s:stack_cb(res) abort
  let s:stack_name = ''

  if type(a:res) isnot type({}) || !has_key(a:res, 'result') || empty(a:res.result)
    return
  endif

  if s:exited(a:res)
    call go#debug#Stop()
    return
  endif
  call s:update_breakpoint(a:res)
  call s:update_goroutines()
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
    call s:continue()
  endif

  " Add a breakpoint to the main.Main if the user didn't define any.
  " TODO(bc): actually set the breakpoint in main.Main
  if len(s:list_breakpoints()) is 0
    if go#debug#Breakpoint() isnot 0
      let s:state.running = 0
      return
    endif
  endif

  try
    " s:stack_name is reset in s:stack_cb(). While its value is 'next', the
    " current operation being performed by delve is a next operation and it
    " must be cancelled before another next operation can start. See
    " https://github.com/go-delve/delve/blob/ab5713d3ec5d12754f4b2edf85e4b36a08b67c48/Documentation/api/ClientHowto.md#special-continue-commands-and-asynchronous-breakpoints
    " for more information.
    if l:name is# 'next' && get(s:, 'stack_name', '') is# 'next'
      " use s:rpc_response so that the any errors will be checked instead of
      " completely discarding the result with s:noop.
      let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
      call s:call_jsonrpc(l:promise.wrapper, 'RPCServer.CancelNext')
      call l:promise.await()
    endif
    let s:stack_name = l:name
    try
      silent! sign unplace 9999
      call s:call_jsonrpc(function('s:handle_stack_response', [l:name]), 'RPCServer.Command', {'name': l:name})
    catch
      call go#util#EchoError(printf('rpc failure: %s', v:exception))
      call s:clearState()
      call go#util#EchoInfo('restarting debugger')
      call go#debug#Restart()
    endtry
  catch
    call go#util#EchoError(printf('CancelNext RPC call failed: %s', v:exception))
  endtry
endfunction

function! s:handle_stack_response(command, check_errors, res) abort
  try
    call a:check_errors()

    if a:command is# 'next'
      call s:handleNextInProgress(a:res)
    endif

    call s:stack_cb(a:res)
  catch
    call go#util#EchoError(printf('rpc failure: %s', v:exception))
    call s:clearState()
    call go#util#EchoInfo('restarting debugger')
    call go#debug#Restart()
  endtry
endfunction

function! s:handleNextInProgress(res)
  try
    let l:res = a:res
    let l:w = 0
    while l:w < 1
      if l:res.result.State.NextInProgress == v:true
        " TODO(bc): message the user that a breakpoint was hit in a different
        " goroutine while trying to resume.
        let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
        call s:call_jsonrpc(l:promise.wrapper, 'RPCServer.Command', {'name': 'continue'})
        let l:res = l:promise.await()
      else
        return
      endif
    endwhile
  catch
    throw v:exception
  endtry
endfunction

function! go#debug#Restart() abort
  call go#cmd#autowrite()

  try
    call s:restoreMappings()
    call s:stop()

    let s:state = {
          \ 'rpcid': 1,
          \ 'running': 0,
          \ 'currentThread': {},
          \ 'localVars': {},
          \ 'functionArgs': {},
          \ 'registers': {},
          \ 'message': [],
          \ 'resultHandlers': {},
          \ 'kill_on_detach': s:state['kill_on_detach'],
        \ }

    call call('go#debug#Start', s:start_args)
  catch
    call go#util#EchoError(printf('restart failed: %s', v:exception))
  endtry
endfunction

" Report if debugger mode is ready.
function! s:isReady()
  return get(s:state, 'ready', 0) != 0
endfunction

" Change Goroutine
function! go#debug#Goroutine() abort
  let l:goroutineID = str2nr(substitute(getline('.'), '^  Goroutine \(.\{-1,\}\) - .*', '\1', 'g'))

  if l:goroutineID <= 0
    return
  endif

  try
    let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
    call s:call_jsonrpc(l:promise.wrapper, 'RPCServer.Command', {'Name': 'switchGoroutine', 'GoroutineID': l:goroutineID})
    let l:res = l:promise.await()
    call s:stack_cb(l:res)
    call go#util#EchoInfo("Switched goroutine to: " . l:goroutineID)
  catch
    call go#util#EchoError(printf('could not switch goroutine: %s', v:exception))
  endtry
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
      call s:sign_unplace(l:found.id, l:found.file)
      if s:isReady()
        let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
        call s:call_jsonrpc(l:promise.wrapper, 'RPCServer.ClearBreakpoint', {'id': l:found.id})
        let res = l:promise.await()
      endif
    else " Add breakpoint
      if s:isReady()
        let l:promise = go#promise#New(function('s:rpc_response'), 20000, {})
        call s:call_jsonrpc(l:promise.wrapper, 'RPCServer.CreateBreakpoint', {'Breakpoint': {'file': s:substituteLocalPath(l:filename), 'line': l:linenr}})
        let l:res = l:promise.await()
        let l:bt = l:res.result.Breakpoint
        call s:sign_place(l:bt.id, s:substituteRemotePath(l:bt.file), l:bt.line)
      else
        let l:id = len(s:list_breakpoints()) + 1
        call s:sign_place(l:id, l:filename, l:linenr)
      endif
    endif
  catch
    call go#util#EchoError(printf('could not toggle breakpoint: %s', v:exception))
    return 1
  endtry

  return 0
endfunction

function! s:sign_unplace(id, file) abort
  if !exists('*sign_unplace')
    exe 'sign unplace ' . a:id .' file=' . a:file
    return
  endif

  call sign_unplace('vim-go-debug', {'buffer': a:file, 'id': a:id})
endfunction

function! s:sign_place(id, expr, lnum) abort
  if !exists('*sign_place')
    exe 'sign place ' . a:id . ' line=' . a:lnum . ' name=godebugbreakpoint file=' . a:expr
    return
  endif

  call sign_place(a:id, 'vim-go-debug', 'godebugbreakpoint', a:expr, {'lnum': a:lnum})
endfunction

function! s:list_breakpoints()
  let l:breakpoints = []
  let l:signs = s:sign_getplaced()
  for l:item in l:signs
    let l:file = fnamemodify(bufname(l:item.bufnr), ':p')
    for l:sign in l:item.signs
      call add(l:breakpoints, {
            \ 'id': l:sign.id,
            \ 'file': l:file,
            \ 'line': l:sign.lnum,
      \ })
    endfor
  endfor

  return l:breakpoints
endfunction

function! s:sign_getplaced() abort
  if !exists('*sign_getplaced') " sign_getplaced was introduced in Vim 8.1.0614
    " :sign place
    " --- Signs ---
    " Signs for a.go:
    "     line=15  id=2  name=godebugbreakpoint
    "     line=16  id=1  name=godebugbreakpoint
    " Signs for a_test.go:
    "     line=6  id=3  name=godebugbreakpoint

    " l:signs should be the same sam form as the return  value for
    " sign_getplaced(), a list with the following entries:
    "   * bufnr - number of the buffer with the sign
    "   * signs = list of signs placed in bufnr
    let l:signs = []
    let l:file = ''
    for l:line in split(execute('sign place'), '\n')[1:]
      if l:line =~# '^Signs for '
        let l:file = l:line[10:-2]
        continue
      else
        " sign place's output may end with Signs instead of starting with Signs.
        " See
        " https://github.com/fatih/vim-go/issues/2920#issuecomment-644885774.
        let l:idx = match(l:line, '\.go .* Signs:$')
        if l:idx >= 0
          let l:file = l:line[0:l:idx+2]
          continue
        endif
      endif

      if l:line !~# 'name=godebugbreakpoint'
        continue
      endif

      let l:sign = matchlist(l:line, '\vline\=(\d+) +id\=(\d+)')
      call add(l:signs, {
                          \ 'bufnr': bufnr(l:file),
                          \ 'signs': [{
                            \ 'id': str2nr(l:sign[2]),
                            \ 'lnum': str2nr(l:sign[1]),
                          \ }],
                      \ })
    endfor

    return l:signs
  endif

  " it would be nice to use lambda's here, but vim-vimparser currently fails
  " to parse lamdas as map() arguments.
  " TODO(bc): return flatten(map(filter(copy(getbufinfo()), { _, val -> val.listed }), { _, val -> sign_getplaced(val.bufnr, {'group': 'vim-go-debug', 'name': 'godebugbreakpoint'})}))
  let l:bufinfo = getbufinfo()
  let l:listed = []
  for l:info in l:bufinfo
    if l:info.listed
      let l:listed = add(l:listed, l:info)
    endif
  endfor

  let l:signs = []
  for l:buf in l:listed
    let l:signs = add(l:signs, sign_getplaced(l:buf.bufnr, {'group': 'vim-go-debug', 'name': 'godebugbreakpoint'})[0])
  endfor
  return l:signs
endfunction

exe 'sign define godebugbreakpoint text='.go#config#DebugBreakpointSignText().' texthl=GoDebugBreakpoint'
sign define godebugcurline    text== texthl=GoDebugCurrent    linehl=GoDebugCurrent

" s:rpc_response is a convenience function to check for errors and return
" a:res when a:res is not an error response.
function! s:rpc_response(check_errors, res) abort
  call a:check_errors()
  return a:res
endfunction

" s:noop is a noop function. It takes any number of arguments and does
" nothing.
function s:noop(...) abort
endfunction

function! s:warn_when_stale(filename) abort
  let l:bufinfo = getbufinfo(a:filename)
  if len(l:bufinfo) == 0
    return
  endif

  if l:bufinfo[0].changed
    call s:warn_stale()
    return
  endif

  call s:call_jsonrpc(function('s:handle_staleness_check_response', [fnamemodify(a:filename, ':p')]), 'RPCServer.LastModified')
endfunction

function! s:handle_staleness_check_response(filename, check_errors, res) abort
  try
    call a:check_errors()
  catch
    " swallow any errors
    return
  endtry

  let l:ftime = strftime('%Y-%m-%dT%H:%M:%S', getftime(a:filename))
  if l:ftime < a:res.result.Time[0:(len(l:ftime) - 1)]
    return
  endif
  call s:warn_stale(a:filename)
endfunction

function! s:warn_stale(filename) abort
  call go#util#EchoWarning(printf('file locations may be incorrect, because %s has changed since debugging started', a:filename))
endfunction


function! s:configureMappings(...) abort
  if a:0 == 0
    return
  endif

  let l:debug_mappings = go#config#DebugMappings()

  for l:arg in a:000
    if !has_key(l:debug_mappings, l:arg)
      continue
    endif

    let l:config = l:debug_mappings[l:arg]

    " do not attempt to apply the mapping when the key is empty or missing.
    if get(l:config, 'key', '') == ''
      continue
    endif

    let l:lhs = l:config.key
    try
      call execute(printf('autocmd BufWinEnter *.go call s:save_maparg_for(expand(''%%''), ''%s'')', l:lhs))
      call execute('autocmd BufWinLeave  *.go call s:restoreMappings()')

      let l:mapping = 'autocmd BufWinEnter *.go nmap <buffer>'
      if has_key(l:config, 'arguments')
        let l:mapping = printf('%s %s', l:mapping, l:config.arguments)
      endif
      let l:mapping = printf('%s %s <Plug>%s', l:mapping, l:lhs, l:arg)
      call execute(l:mapping)
    catch
      call go#util#EchoError(printf('could not configure mapping for %s: %s', l:lhs, v:exception))
    endtry
  endfor
endfunction

function! s:save_maparg_for(bufname, lhs) abort
  " make sure bufname is the active buffer.
  if fnamemodify(a:bufname, ':p') isnot expand('%:p')
    call go#util#EchoWarning('buffer must be active to save its mappings')
    return
  endif

  " only normal-mode buffer-local mappings are needed, because all
  " vim-go-debug mappings are normal-mode buffer-local mappings. Therefore,
  " we only need to retrieve normal mode mappings that need to be saved.
  let l:maparg = maparg(a:lhs, 'n', 0, 1)
  if empty(l:maparg)
    return
  endif

  if l:maparg.buffer
    let l:bufmapargs = get(s:mapargs, a:bufname, [])
    let l:bufmapargs = add(l:bufmapargs, l:maparg)
    let s:mapargs[a:bufname] = l:bufmapargs
  endif
endfunction

function! s:restoreMappings() abort
  " Remove all debugging mappings.
  for l:mapping in values(go#config#DebugMappings())
    let l:lhs = get(l:mapping, 'key', '')
    if l:lhs == ''
      continue
    endif
    let l:maparg = maparg(l:lhs, 'n', 0, 1)
    if empty(l:maparg)
      continue
    endif
    if l:maparg.buffer
      call execute(printf('nunmap <buffer> %s', l:lhs))
    endif
  endfor

  call s:restoremappingfor(bufname(''))
endfunction

function! s:restoremappingfor(bufname) abort
  if !has_key(s:mapargs, a:bufname)
    return
  endif

  for l:maparg in s:mapargs[a:bufname]
    call s:restore_mapping(l:maparg)
  endfor
  call remove(s:mapargs, a:bufname)
endfunction

function! s:restore_mapping(maparg)
  if empty(a:maparg)
    return
  endif
  if !exists('*mapset')
    " see :h :map-arguments
    let l:silent_attr = get(a:maparg, 'silent',  0) ? '<silent>' : ''
    let l:nowait_attr = get(a:maparg, 'no_wait', 0) ? '<nowait>' : ''
    let l:buffer_attr = get(a:maparg, 'buffer',  0) ? '<buffer>' : ''
    let l:expr_attr   = get(a:maparg, 'expr',    0) ? '<expr>'   : ''
    let l:unique_attr = get(a:maparg, 'unique',  0) ? '<unique>' : ''
    let l:script_attr = get(a:maparg, 'script',  0) ? '<script>' : ''

    let l:command     = [a:maparg['mode'], (get(a:maparg, 'noremap', 0) ? 'nore' : ''), 'map']
    let l:command     = join(filter(l:command, '!empty(v:val)'), '')
    let l:rhs         = a:maparg['rhs']
    let l:lhs         = a:maparg['lhs']

    " NOTE: most likely <buffer> should be first
    let l:mapping = join(filter([l:command, l:buffer_attr, l:silent_attr, l:nowait_attr, l:expr_attr, l:unique_attr, l:script_attr, l:lhs, l:rhs], '!empty(v:val)'))
    call execute(l:mapping)
    return
  endif

  call mapset('n', 0, a:maparg)
  return
endfunction

function! s:substituteRemotePath(path) abort
  return s:substitutePath(a:path, go#config#DebugSubstitutePaths())
endfunction

function! s:substituteLocalPath(path) abort
  return s:substitutePath(a:path, map(deepcopy(go#config#DebugSubstitutePaths()), '[v:val[1], v:val[0]]'))
endfunction

function! s:substitutePath(path, substitutions) abort
  for [l:from, l:to] in a:substitutions
    if len(a:path) < len(l:from)
      continue
    endif
    if a:path[0:len(l:from)-1] != l:from
      continue
    endif

    return printf('%s%s', l:to, a:path[len(l:from):-1])
  endfor

  return a:path
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
