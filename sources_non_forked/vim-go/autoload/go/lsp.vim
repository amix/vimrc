" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

let s:lspfactory = {}

function! s:lspfactory.get() dict abort
  if empty(get(self, 'current', {})) || empty(get(self.current, 'job', {}))
    let self.current = s:newlsp()
  endif

  return self.current
endfunction

function! s:lspfactory.reset() dict abort
  if has_key(self, 'current')
    call remove(self, 'current')
  endif
endfunction

function! s:newlsp() abort
  if !go#util#has_job()
    let l:oldshortmess=&shortmess
    if has('nvim')
      set shortmess-=F
    endif
    call go#util#EchoWarning('Features that rely on gopls will not work without either Vim 8.0.0087 or newer with +job or Neovim')
    " Sleep one second to make sure people see the message. Otherwise it is
    " often immediately overwritten by an async message.
    sleep 1
    let &shortmess=l:oldshortmess
    return {'sendMessage': funcref('s:noop')}
  endif

  " job is the job used to talk to the backing instance of gopls.
  " ready is 0 until the initialize response has been received. 1 afterwards.
  " queue is messages to send after initialization
  " last_request_id is id of the most recently sent request.
  " buf is unprocessed/incomplete responses
  " handlers is a mapping of request ids to dictionaries of functions.
  "   request id -> {start, requestComplete, handleResult, error}
  "   * start is a function that takes no arguments
  "   * requestComplete is a function that takes 1 argument. The parameter will be 1
  "     if the call was succesful.
  "   * handleResult takes a single argument, the result message received from gopls
  "   * error takes a single argument, the error message received from gopls.
  "     The error method is optional.
  let l:lsp = {
        \ 'job':  '',
        \ 'ready': 0,
        \ 'queue': [],
        \ 'last_request_id': 0,
        \ 'buf': '',
        \ 'handlers': {},
        \ 'workspaceDirectories': [],
        \ 'wd' : '',
        \ }

  function! l:lsp.readMessage(data) dict abort
    let l:responses = []
    let l:rest = a:data

    while 1
      " Look for the end of the HTTP headers
      let l:body_start_idx = matchend(l:rest, "\r\n\r\n")

      if l:body_start_idx < 0
        " incomplete header
        break
      endif

      " Parse the Content-Length header.
      let l:header = l:rest[:l:body_start_idx - 4]
      let l:length_match = matchlist(
      \   l:header,
      \   '\vContent-Length: *(\d+)'
      \)

      if empty(l:length_match)
        " TODO(bc): shutdown gopls?
        throw "invalid JSON-RPC header:\n" . l:header
      endif

      " get the start of the rest
      let l:next_start_idx = l:body_start_idx + str2nr(l:length_match[1])

      if len(l:rest) < l:next_start_idx
        " incomplete response body
        break
      endif

      call s:debug('received', l:rest[:l:next_start_idx - 1])

      let l:body = l:rest[l:body_start_idx : l:next_start_idx - 1]
      let l:rest = l:rest[l:next_start_idx :]

      try
        " add the json body to the list.
        call add(l:responses, json_decode(l:body))
      catch
        " TODO(bc): log the message and/or show an error message.
      finally
        " intentionally left blank.
      endtry
    endwhile

    return [l:rest, l:responses]
  endfunction

  function! l:lsp.handleMessage(ch, data) dict abort
      let self.buf .= a:data

      let [self.buf, l:messages] = self.readMessage(self.buf)

      for l:message in l:messages
        if has_key(l:message, 'method')
          if has_key(l:message, 'id')
            call self.handleRequest(l:message)
          else
            call self.handleNotification(l:message)
          endif
        elseif has_key(l:message, 'result') || has_key(l:message, 'error')
          call self.handleResponse(l:message)
        endif
      endfor
  endfunction

  function! l:lsp.handleRequest(req) dict abort
    if a:req.method == 'workspace/workspaceFolders'
      let l:resp = go#lsp#message#WorkspaceFoldersResult(self.workspaceDirectories)
    elseif a:req.method == 'workspace/configuration' && has_key(a:req, 'params') && has_key(a:req.params, 'items')
      let l:resp = go#lsp#message#ConfigurationResult(a:req.params.items)
    elseif a:req.method == 'client/registerCapability' && has_key(a:req, 'params') && has_key(a:req.params, 'registrations')
      let l:resp = v:null
    else
      return
    endif

    if get(self, 'exited', 0)
      return
    endif

    let l:msg = self.newResponse(a:req.id, l:resp)
    call self.write(l:msg)
  endfunction

  function! l:lsp.handleNotification(req) dict abort
      " TODO(bc): handle notifications (e.g. window/showMessage).
  endfunction

  function! l:lsp.handleResponse(resp) dict abort
    if has_key(a:resp, 'id') && has_key(self.handlers, a:resp.id)
      try
        let l:handler = self.handlers[a:resp.id]

        let l:winid = win_getid(winnr())
        " Always set the active window to the window that was active when
        " the request was sent. Among other things, this makes sure that
        " the correct window's location list will be populated when the
        " list type is 'location' and the user has moved windows since
        " sending the request.
        call win_gotoid(l:handler.winid)

        if has_key(a:resp, 'error')
          call l:handler.requestComplete(0)
          if has_key(l:handler, 'error')
            call call(l:handler.error, [a:resp.error.message])
          else
            call go#util#EchoError(a:resp.error.message)
          endif
          call win_gotoid(l:winid)
          return
        endif
        call l:handler.requestComplete(1)

        let l:winidBeforeHandler = l:handler.winid
        call call(l:handler.handleResult, [a:resp.result])

        " change the window back to the window that was active when
        " starting to handle the message _only_ if the handler didn't
        " update the winid, so that handlers can set the winid if needed
        " (e.g. :GoDef).
        if l:handler.winid == l:winidBeforeHandler
          call win_gotoid(l:winid)
        endif
      finally
        call remove(self.handlers, a:resp.id)
      endtry
    endif
  endfunction

  function! l:lsp.handleInitializeResult(result) dict abort
    if go#config#EchoCommandInfo()
      call go#util#EchoProgress("initialized gopls")
    endif
    let status = {
          \ 'desc': '',
          \ 'type': 'gopls',
          \ 'state': 'initialized',
        \ }
    call go#statusline#Update(self.wd, status)

    let self.ready = 1
    let  l:msg = self.newMessage(go#lsp#message#Initialized())
    call self.write(l:msg)

    " send messages queued while waiting for ready.
    for l:item in self.queue
      call self.sendMessage(l:item.data, l:item.handler)
    endfor

    " reset the queue
    let self.queue = []
  endfunction

  function! l:lsp.sendMessage(data, handler) dict abort
    if !self.last_request_id
      let l:wd = go#util#ModuleRoot()
      if l:wd == -1
        call go#util#EchoError('could not determine appropriate working directory for gopls')
        return -1
      endif

      if l:wd == ''
        let l:wd = getcwd()
      endif
      let self.wd = l:wd

      if go#config#EchoCommandInfo()
        call go#util#EchoProgress("initializing gopls")
      endif

      let l:status = {
            \ 'desc': '',
            \ 'type': 'gopls',
            \ 'state': 'initializing',
          \ }
      call go#statusline#Update(l:wd, l:status)

      let self.workspaceDirectories = add(self.workspaceDirectories, l:wd)
      let l:msg = self.newMessage(go#lsp#message#Initialize(l:wd))

      let l:state = s:newHandlerState('')
      let l:state.handleResult = funcref('self.handleInitializeResult', [], l:self)

      let self.handlers[l:msg.id] = l:state

      call l:state.start()
      call self.write(l:msg)
    endif

    if !self.ready
      call add(self.queue, {'data': a:data, 'handler': a:handler})
      return
    endif

    let l:msg = self.newMessage(a:data)
    if has_key(l:msg, 'id')
      let self.handlers[l:msg.id] = a:handler
    endif

    call a:handler.start()
    call self.write(l:msg)
  endfunction

  " newMessage returns a message constructed from data. data should be a dict
  " with 2 or 3 keys: notification, method, and optionally params.
  function! l:lsp.newMessage(data) dict abort
    let l:msg = {
          \ 'method': a:data.method,
          \ 'jsonrpc': '2.0',
        \ }

    if !a:data.notification
      let self.last_request_id += 1
      let l:msg.id = self.last_request_id
    endif

    if has_key(a:data, 'params')
      let l:msg.params = a:data.params
    endif

    return l:msg
  endfunction

  function l:lsp.newResponse(id, result) dict abort
    let l:msg = {
          \ 'jsonrpc': '2.0',
          \ 'id': a:id,
          \ 'result': a:result,
        \ }

    return l:msg
  endfunction

  function! l:lsp.write(msg) dict abort
    let l:body = json_encode(a:msg)
    let l:data = 'Content-Length: ' . strlen(l:body) . "\r\n\r\n" . l:body

    call s:debug('sent', l:data)

    if has('nvim')
      call chansend(self.job, l:data)
      return
    endif

    call ch_sendraw(self.job, l:data)
  endfunction

  function! l:lsp.exit_cb(job, exit_status) dict
    let self.exited = 1
    if !get(self, 'restarting', 0)
      return
    endif

    let l:queue = self.queue

    let l:workspaces = self.workspaceDirectories

    call s:lspfactory.reset()
    let l:lsp = s:lspfactory.get()

    " restore workspaces
    call call('go#lsp#AddWorkspaceDirectory', l:workspaces)
    " * send DidOpen messages for all buffers that have b:did_lsp_open set
    " TODO(bc): check modifiable and filetype, too?
    bufdo if get(b:, 'go_lsp_did_open', 0) | if &modified | call go#lsp#DidOpen(expand('%:p')) | else | call go#lsp#DidChange(expand('%:p')) | endif | endif
    let l:lsp.queue = extend(l:lsp.queue, l:queue)
    return
  endfunction

  function! l:lsp.close_cb(ch) dict abort
    " TODO(bc): remove the buffer variables that indicate that gopls has been
    " informed that the file is open
  endfunction

  function! l:lsp.err_cb(ch, msg) dict abort
    if a:msg =~ '^\tPort = \d\+$' && !get(self, 'debugport', 0)
      let self.debugport = substitute(a:msg, '^\tPort = \(\d\+\).*$', '\1', '')
    endif

    call s:debug('stderr', a:msg)
  endfunction

  " explicitly bind callbacks to l:lsp so that within it, self will always refer
  " to l:lsp instead of l:opts. See :help Partial for more information.
  let l:opts = {
        \ 'in_mode': 'raw',
        \ 'out_mode': 'raw',
        \ 'err_mode': 'nl',
        \ 'noblock': 1,
        \ 'err_cb': funcref('l:lsp.err_cb', [], l:lsp),
        \ 'out_cb': funcref('l:lsp.handleMessage', [], l:lsp),
        \ 'close_cb': funcref('l:lsp.close_cb', [], l:lsp),
        \ 'exit_cb': funcref('l:lsp.exit_cb', [], l:lsp),
        \ 'cwd': getcwd(),
        \}

  let l:bin_path = go#path#CheckBinPath("gopls")
  if empty(l:bin_path)
    return
  endif

  let l:cmd = [l:bin_path]
  if go#util#HasDebug('lsp')
    let l:cmd = extend(l:cmd, ['-debug', 'localhost:0'])
  endif

  let l:lsp.job = go#job#Start(l:cmd, l:opts)

  return l:lsp
endfunction

function! s:noop(...) abort
endfunction

function! s:newHandlerState(statustype) abort
  let l:state = {
        \ 'winid': win_getid(winnr()),
        \ 'statustype': a:statustype,
        \ 'jobdir': getcwd(),
      \ }

  " explicitly bind requestComplete to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let l:state.requestComplete = funcref('s:requestComplete', [], l:state)

  " explicitly bind start to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let l:state.start = funcref('s:start', [], l:state)

  return l:state
endfunction

function! s:requestComplete(ok) abort dict
  if self.statustype == ''
    return
  endif

  if go#config#EchoCommandInfo()
    let prefix = '[' . self.statustype . '] '
    if a:ok
      call go#util#EchoSuccess(prefix . "SUCCESS")
    else
      call go#util#EchoError(prefix . "FAIL")
    endif
  endif

  let status = {
        \ 'desc': 'last status',
        \ 'type': self.statustype,
        \ 'state': "success",
        \ }

  if !a:ok
    let status.state = "failed"
  endif

  if has_key(self, 'started_at')
    let elapsed_time = reltimestr(reltime(self.started_at))
    " strip whitespace
    let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')
    let status.state .= printf(" (%ss)", elapsed_time)
  endif

  call go#statusline#Update(self.jobdir, status)
endfunction

function! s:start() abort dict
  let self.started_at = reltime()
  if self.statustype == ''
    return
  endif
  let status = {
        \ 'desc': 'current status',
        \ 'type': self.statustype,
        \ 'state': "started",
        \ }

  call go#statusline#Update(self.jobdir, status)
endfunction

" go#lsp#Definition calls gopls to get the definition of the identifier at
" line and col in fname. handler should be a dictionary function that takes a
" list of strings in the form 'file:line:col: message'. handler will be
" attached to a dictionary that manages state (statuslines, sets the winid,
" etc.)
function! go#lsp#Definition(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:state = s:newHandlerState('definition')
  let l:state.handleResult = funcref('s:definitionHandler', [function(a:handler, [], l:state)], l:state)
  let l:msg = go#lsp#message#Definition(fnamemodify(a:fname, ':p'), a:line, a:col)
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:definitionHandler(next, msg) abort dict
  " gopls returns a []Location; just take the first one.
  let l:msg = a:msg[0]
  let l:args = [[printf('%s:%d:%d: %s', go#path#FromURI(l:msg.uri), l:msg.range.start.line+1, go#lsp#lsp#PositionOf(getline(l:msg.range.start.line+1), l:msg.range.start.character), 'lsp does not supply a description')]]
  call call(a:next, l:args)
endfunction

" go#lsp#Type calls gopls to get the type definition of the identifier at
" line and col in fname. handler should be a dictionary function that takes a
" list of strings in the form 'file:line:col: message'. handler will be
" attached to a dictionary that manages state (statuslines, sets the winid,
" etc.)
function! go#lsp#TypeDef(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:state = s:newHandlerState('type definition')
  let l:msg = go#lsp#message#TypeDefinition(fnamemodify(a:fname, ':p'), a:line, a:col)
  let l:state.handleResult = funcref('s:typeDefinitionHandler', [function(a:handler, [], l:state)], l:state)
  return  l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:typeDefinitionHandler(next, msg) abort dict
  " gopls returns a []Location; just take the first one.
  let l:msg = a:msg[0]
  let l:args = [[printf('%s:%d:%d: %s', go#path#FromURI(l:msg.uri), l:msg.range.start.line+1, go#lsp#lsp#PositionOf(getline(l:msg.range.start.line+1), l:msg.range.start.character), 'lsp does not supply a description')]]
  call call(a:next, l:args)
endfunction

function! go#lsp#DidOpen(fname) abort
  if get(b:, 'go_lsp_did_open', 0)
    return
  endif

  if !filereadable(a:fname)
    return
  endif

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#DidOpen(fnamemodify(a:fname, ':p'), join(go#util#GetLines(), "\n") . "\n")
  let l:state = s:newHandlerState('')
  let l:state.handleResult = funcref('s:noop')

  " TODO(bc): setting a buffer level variable here assumes that a:fname is the
  " current buffer. Change to a:fname first before setting it and then change
  " back to active buffer.
  let b:go_lsp_did_open = 1

  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! go#lsp#DidChange(fname) abort
  " DidChange is called even when fname isn't open in a buffer (e.g. via
  " go#lsp#Info); don't report the file as open or as having changed when it's
  " not actually a buffer.
  if bufnr(a:fname) == -1
    return
  endif

  if !filereadable(a:fname)
    return
  endif

  call go#lsp#DidOpen(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#DidChange(fnamemodify(a:fname, ':p'), join(go#util#GetLines(), "\n") . "\n")
  let l:state = s:newHandlerState('')
  let l:state.handleResult = funcref('s:noop')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! go#lsp#DidClose(fname) abort
  if !filereadable(a:fname)
    return
  endif

  if !get(b:, 'go_lsp_did_open', 0)
    return
  endif

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#DidClose(fnamemodify(a:fname, ':p'))
  let l:state = s:newHandlerState('')
  let l:state.handleResult = funcref('s:noop')
  " TODO(bc): setting a buffer level variable here assumes that a:fname is the
  " current buffer. Change to a:fname first before setting it and then change
  " back to active buffer.
  let b:go_lsp_did_open = 0

  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! go#lsp#Completion(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Completion(a:fname, a:line, a:col)
  let l:state = s:newHandlerState('completion')
  let l:state.handleResult = funcref('s:completionHandler', [function(a:handler, [], l:state)], l:state)
  let l:state.error = funcref('s:completionErrorHandler', [function(a:handler, [], l:state)], l:state)
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:completionHandler(next, msg) abort dict
  " gopls returns a CompletionList.
  let l:matches = []
  let l:start = -1

  for l:item in a:msg.items
    let l:start = l:item.textEdit.range.start.character

    let l:match = {'abbr': l:item.label, 'word': l:item.textEdit.newText, 'info': '', 'kind': go#lsp#completionitemkind#Vim(l:item.kind)}
    if has_key(l:item, 'detail')
        let l:match.menu = l:item.detail
        if go#lsp#completionitemkind#IsFunction(l:item.kind) || go#lsp#completionitemkind#IsMethod(l:item.kind)
          let l:match.info = printf('%s %s', l:item.label, l:item.detail)

          " The detail provided by gopls hasn't always provided the the full
          " signature including the return value. The label used to be the
          " function signature and the detail was the return value. Handle
          " that case for backward compatibility. This can be removed in the
          " future once it's likely that the majority of users are on a recent
          " version of gopls.
          if l:item.detail !~ '^func'
            let l:match.info = printf('func %s %s', l:item.label, l:item.detail)
          endif
        endif
    endif

    if has_key(l:item, 'documentation')
      let l:match.info .= "\n\n" . l:item.documentation
    endif

    let l:matches = add(l:matches, l:match)
  endfor
  let l:args = [l:start, l:matches]
  call call(a:next, l:args)
endfunction

function! s:completionErrorHandler(next, error) abort dict
  call call(a:next, [-1, []])
endfunction

function! go#lsp#Hover(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Hover(a:fname, a:line, a:col)
  let l:state = s:newHandlerState('')
  let l:state.handleResult = funcref('s:hoverHandler', [function(a:handler, [], l:state)], l:state)
  let l:state.error = funcref('s:noop')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:hoverHandler(next, msg) abort dict
  let l:content = split(a:msg.contents.value, '; ')
  if len(l:content) > 1
    let l:curly = stridx(l:content[0], '{')
    let l:content = extend([l:content[0][0:l:curly]], map(extend([l:content[0][l:curly+1:]], l:content[1:]), '"\t" . v:val'))
    let l:content[len(l:content)-1] = '}'
  endif

  let l:args = [l:content]
  call call(a:next, l:args)
endfunction

function! go#lsp#Info(showstatus)
  let l:fname = expand('%:p')
  let [l:line, l:col] = go#lsp#lsp#Position()

  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()

  if a:showstatus
    let l:state = s:newHandlerState('info')
  else
    let l:state = s:newHandlerState('')
  endif

  let l:state.handleResult = funcref('s:infoDefinitionHandler', [function('s:info', [1], l:state), a:showstatus], l:state)
  let l:state.error = funcref('s:noop')
  let l:msg = go#lsp#message#Definition(l:fname, l:line, l:col)
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! go#lsp#GetInfo()
  let l:fname = expand('%:p')
  let [l:line, l:col] = go#lsp#lsp#Position()

  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()

  let l:state = s:newHandlerState('')

  let l:info = go#promise#New(function('s:info', [0], l:state), 10000, '')

  let l:state.handleResult = funcref('s:infoDefinitionHandler', [l:info.wrapper, 0], l:state)
  let l:state.error = funcref('s:noop')
  let l:msg = go#lsp#message#Definition(l:fname, l:line, l:col)
  call l:lsp.sendMessage(l:msg, l:state)
  return l:info.await()
endfunction

function! s:infoDefinitionHandler(next, showstatus, msg) abort dict
  " gopls returns a []Location; just take the first one.
  let l:msg = a:msg[0]

  let l:fname = go#path#FromURI(l:msg.uri)
  let l:line = l:msg.range.start.line
  let l:col = l:msg.range.start.character

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Hover(l:fname, l:line, l:col)

  if a:showstatus
    let l:state = s:newHandlerState('info')
  else
    let l:state = s:newHandlerState('')
  endif

  let l:state.handleResult = funcref('s:hoverHandler', [a:next], l:state)
  let l:state.error = funcref('s:noop')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:info(show, content) abort dict
  let l:content = s:infoFromHoverContent(a:content)

  if a:show
    call go#util#ShowInfo(l:content)
  endif

  return l:content
endfunction

function! s:infoFromHoverContent(content) abort
  if len(a:content) < 1
    return ''
  endif

  let l:content = a:content[0]

  " strip off the method set and fields of structs and interfaces.
  if l:content =~# '^\(type \)\?[^ ]\+ \(struct\|interface\)'
    let l:content = substitute(l:content, '{.*', '', '')
  endif

  return l:content
endfunction

function! go#lsp#AddWorkspaceDirectory(...) abort
  if a:0 == 0
    return
  endif

  call go#lsp#CleanWorkspaces()

  let l:workspaces = []
  for l:dir in a:000
    let l:dir = fnamemodify(l:dir, ':p')
    if !isdirectory(l:dir)
      continue
    endif

    let l:workspaces = add(l:workspaces, l:dir)
  endfor

  let l:lsp = s:lspfactory.get()
  let l:state = s:newHandlerState('')
  let l:state.handleResult = funcref('s:noop')
  let l:lsp.workspaceDirectories = extend(l:lsp.workspaceDirectories, l:workspaces)
  let l:msg = go#lsp#message#ChangeWorkspaceFolders(l:workspaces, [])
  call l:lsp.sendMessage(l:msg, l:state)

  return 0
endfunction

function! go#lsp#CleanWorkspaces() abort
  let l:workspaces = []

  let l:lsp = s:lspfactory.get()

  let l:i = 0
  let l:missing = []
  for l:dir in l:lsp.workspaceDirectories
    if !isdirectory(l:dir)
      let l:dir = add(l:missing, l:dir)
      call remove(l:lsp.workspaceDirectories, l:i)
      continue
    endif
    let l:i += 1
  endfor

  let l:state = s:newHandlerState('')
  let l:state.handleResult = funcref('s:noop')
  let l:msg = go#lsp#message#ChangeWorkspaceFolders([], l:missing)
  call l:lsp.sendMessage(l:msg, l:state)

  return 0
endfunction

" go#lsp#ResetWorkspaceDiretories removes and then re-adds all workspace
" folders to cause gopls to send configuration requests for all of them again.
" This is useful, for instance, when build tags have been added and gopls
" needs to use them.
function! go#lsp#ResetWorkspaceDirectories() abort
  call go#lsp#CleanWorkspaces()

  let l:lsp = s:lspfactory.get()

  let l:state = s:newHandlerState('')
  let l:state.handleResult = funcref('s:noop')
  let l:msg = go#lsp#message#ChangeWorkspaceFolders(l:lsp.workspaceDirectories, l:lsp.workspaceDirectories)
  call l:lsp.sendMessage(l:msg, l:state)

  return 0
endfunction

function! go#lsp#DebugBrowser() abort
  let l:lsp = s:lspfactory.get()
  let l:port = get(l:lsp, 'debugport', 0)
  if !l:port
    call go#util#EchoError("gopls was not started with debugging enabled. See :help g:go_debug.")
    return
  endif

  call go#util#OpenBrowser(printf('http://localhost:%d', l:port))
endfunction

function! go#lsp#Restart() abort
  if !go#util#has_job() || len(s:lspfactory) == 0 || !has_key(s:lspfactory, 'current')
    return
  endif

  let l:lsp = s:lspfactory.get()

  let l:lsp.restarting = 1

  let l:state = s:newHandlerState('exit')

  let l:msg = go#lsp#message#Shutdown()
  let l:state.handleResult = funcref('s:noop')
  let l:retval = l:lsp.sendMessage(l:msg, l:state)

  let l:msg = go#lsp#message#Exit()
  let l:retval = l:lsp.sendMessage(l:msg, l:state)

  return l:retval
endfunction

function! s:debug(event, data) abort
  if !go#util#HasDebug('lsp')
    return
  endif

  let l:winid = win_getid()

  let l:name = '__GOLSP_LOG__'
  let l:log_winid = bufwinid(l:name)
  if l:log_winid == -1
    silent keepalt botright 10new
    silent file `='__GOLSP_LOG__'`
    setlocal buftype=nofile bufhidden=wipe nomodified nobuflisted noswapfile nowrap nonumber nocursorline
    setlocal filetype=golsplog
  else
    call win_gotoid(l:log_winid)
  endif

  try
    setlocal modifiable
    if getline(1) == ''
      call setline('$', printf('%s: %s', a:event, a:data))
    else
      call append('$', printf('%s: %s', a:event, a:data))
    endif
    normal! G
    setlocal nomodifiable
  finally
    call win_gotoid(l:winid)
  endtry
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
