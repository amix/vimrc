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
  " workspaceDirectories is an array of named workspaces.
  " wd is the working directory for gopls
  " diagnostics is a dictionary whose keys are filenames and each value is a
  "   list of diagnostic messages for the file.
  " diagnosticsQueue is a queue of diagnostics notifications that have been
  "   received, but not yet processed.
  " fileVersions is a dictionary of filenames to versions.
  " notificationQueue is a dictionary of filenames to functions. For a given
  "   filename, each notification will call the first function in the list of
  "   function values and remove it from the list. The functions should accept
  "   two arguments: an absolute path and a list of diagnotics messages for
  "   the file.
  let l:lsp = {
        \ 'job':  '',
        \ 'ready': 0,
        \ 'queue': [],
        \ 'last_request_id': 0,
        \ 'buf': '',
        \ 'handlers': {},
        \ 'workspaceDirectories': [],
        \ 'wd' : '',
        \ 'diagnosticsQueue': [],
        \ 'diagnostics': {},
        \ 'fileVersions': {},
        \ 'notificationQueue': {},
        \ }

  if !go#config#GoplsEnabled()
    let l:lsp.sendMessage = funcref('s:noop')
    return l:lsp
  endif

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
    return l:lsp
  endif

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
    elseif a:req.method == 'workspace/applyEdit'
      try
        let l:ok = v:true
        for l:change in a:req.params.edit.documentChanges
          call s:applyDocumentChanges(a:req.params.edit.documentChanges)
        endfor
      catch
        call go#util#EchoError(printf('could not apply edit: %s', v:exception))
        let l:ok = v:false
      endtry
      let l:resp = go#lsp#message#ApplyWorkspaceEditResponse(l:ok)
    else
      return
    endif

    if get(self, 'exited', 0)
      return
    endif

    let l:msg = self.newResponse(a:req.id, l:resp)
    call self.write(l:msg)
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

  function! l:lsp.handleNotification(req) dict abort
      " TODO(bc): handle more notifications (e.g. window/showMessage).
      if a:req.method == 'textDocument/publishDiagnostics'
        call self.handleDiagnostics(a:req.params)
      elseif a:req.method == 'window/showMessage'
        call self.showMessage(a:req.params)
      endif
  endfunction

  function! l:lsp.handleDiagnostics(data) dict abort
    let self.diagnosticsQueue = add(self.diagnosticsQueue, a:data)
    call self.updateDiagnostics()
  endfunction

  function! l:lsp.showMessage(data) dict abort
    let l:msg = a:data.message
    if a:data.type == 1
      call go#util#EchoError(l:msg)
    elseif a:data.type == 2
      call go#util#EchoWarning(l:msg)
    elseif a:data.type == 3
      call go#util#EchoInfo(l:msg)
    elseif a:data.type == 4
      " do nothing for Log messages
    endif
  endfunction

  " TODO(bc): process the queue asynchronously
  function! l:lsp.updateDiagnostics() dict abort
    let l:level = go#config#DiagnosticsLevel()

    for l:data in self.diagnosticsQueue
      call remove(self.diagnosticsQueue, 0)

      try
        let l:diagnostics = []
        let l:errorMatches = []
        let l:warningMatches = []
        let l:fname = go#path#FromURI(l:data.uri)

        " get the buffer name relative to the current directory, because
        " Vim says that a buffer name can't be an absolute path.
        let l:bufname = fnamemodify(l:fname, ':.')

        if len(l:data.diagnostics) > 0 && (l:level > 0 || bufnr(l:bufname) == bufnr(''))
          " make sure the buffer is listed and loaded before calling getbufline() on it
          if !bufexists(l:bufname)
            call bufadd(l:bufname)
          endif

          if !bufloaded(l:bufname)
            call bufload(l:bufname)
          endif

          for l:diag in l:data.diagnostics
            if l:level < l:diag.severity
              continue
            endif
            let [l:diagnostic, l:matchpos] = s:processDiagnostic(l:diag, l:bufname, l:fname)
            let l:diagnostics = add(l:diagnostics, l:diagnostic)

            if empty(l:matchpos)
              continue
            endif

            if l:diag.severity == 1
              let l:errorMatches = add(l:errorMatches, l:matchpos)
            elseif l:diag.severity == 2
              let l:warningMatches = add(l:warningMatches, l:matchpos)
            endif
          endfor
        endif

        if bufnr(l:bufname) == bufnr('')
          " only apply highlighting when the diagnostics are for the current
          " version.
          let l:lsp = s:lspfactory.get()
          let l:version = get(l:lsp.fileVersions, l:fname, 0)
          " it's tempting to only highlight matches when they are for the
          " current version of the buffer, but that causes problems when the
          " version number has been updated and the content has not. In such a
          " case, the diagnostics may not be sent for later versions.
          call s:highlightMatches(l:errorMatches, l:warningMatches)
        endif

        let self.diagnostics[l:fname] = l:diagnostics
        if has_key(self.notificationQueue, l:fname) && len(self.notificationQueue[l:fname]) > 0
          call call(self.notificationQueue[l:fname][0], copy(l:diagnostics))
          call remove(self.notificationQueue[l:fname], 0)
        endif
      catch
        "call go#util#EchoError(printf('%s: %s', v:throwpoint, v:exception))
      endtry
    endfor
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
    if empty(get(self, 'job', {}))
      return
    endif

    let l:body = json_encode(a:msg)
    let l:data = 'Content-Length: ' . strlen(l:body) . "\r\n\r\n" . l:body

    call s:debug('sent', l:data)

    if has('nvim')
      call chansend(self.job, l:data)
      return
    endif

    try
      call ch_sendraw(self.job, l:data)
    catch
      call go#util#EchoError(printf('could not send message: %s', v:exception))
    endtry
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
    if a:msg =~ '^\d\{4}/\d\d/\d\d\ \d\d:\d\d:\d\d debug server listening on port \d\+$' && !get(self, 'debugport', 0)
      let self.debugport = substitute(a:msg, '\d\{4}/\d\d/\d\d\ \d\d:\d\d:\d\d debug server listening on port \(\d\+\).*$', '\1', '')
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
    let l:lsp.sendMessage = funcref('s:noop')
    return l:lsp
  endif

  let l:cmd = [l:bin_path]
  let l:cmdopts = go#config#GoplsOptions()

  if go#util#HasDebug('lsp')
    " debugging can be enabled either with g:go_debug or with
    " g:go_gopls_options; use g:go_gopls_options if it's given in case users
    " are running the gopls debug server on a known port.
    let l:needsDebug = 1

    for l:item in l:cmdopts
      let l:idx = stridx(l:item, '-debug')
      if l:idx == 0 || l:idx == 1
        let l:needsDebug = 0
      endif
    endfor
    if l:needsDebug
      let l:cmd = extend(l:cmd, ['-debug', 'localhost:0'])
    endif
  endif

  let l:lsp.job = go#job#Start(l:cmd+l:cmdopts, l:opts)

  return l:lsp
endfunction

function! s:noop(...) abort
endfunction

function! s:newHandlerState(statustype) abort
  let l:state = {
        \ 'winid': win_getid(winnr()),
        \ 'statustype': a:statustype,
        \ 'jobdir': getcwd(),
        \ 'handleResult': funcref('s:noop'),
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
    " redraw to avoid messages piling up
    redraw
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
  if a:msg is v:null || len(a:msg) == 0
    return
  endif

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
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:typeDefinitionHandler(next, msg) abort dict
  if a:msg is v:null || len(a:msg) == 0
    return
  endif

  " gopls returns a []Location; just take the first one.
  let l:msg = a:msg[0]
  let l:args = [[printf('%s:%d:%d: %s', go#path#FromURI(l:msg.uri), l:msg.range.start.line+1, go#lsp#lsp#PositionOf(getline(l:msg.range.start.line+1), l:msg.range.start.character), 'lsp does not supply a description')]]
  call call(a:next, l:args)
endfunction

" go#lsp#Callers calls gopls to get callers of the identifier at
" line and col in fname. handler should be a dictionary function that takes a
" list of strings in the form 'file:line:col: message'. handler will be
" attached to a dictionary that manages state (statuslines, sets the winid,
" etc.)
function! go#lsp#Callers(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:state = s:newHandlerState('callers')
  let l:msg = go#lsp#message#PrepareCallHierarchy(fnamemodify(a:fname, ':p'), a:line, a:col)
  let l:state.handleResult = funcref('s:prepareCallHierarchyHandler', [function(a:handler, [], l:state)], l:state)
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:prepareCallHierarchyHandler(next, msg) abort dict
  if a:msg is v:null || len(a:msg) == 0
    return
  endif

  let l:lsp = s:lspfactory.get()
  let l:state = s:newHandlerState('callers')
  let l:msg = go#lsp#message#IncomingCalls(a:msg[0])
  let l:state.handleResult = funcref('s:incomingCallsHandler', [function(a:next, [], l:state)], l:state)
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:incomingCallsHandler(next, msg) abort dict
  if a:msg is v:null || len(a:msg) == 0
    return
  endif

  let l:locations = []
  for l:item in a:msg
    try
      let l:fname = go#path#FromURI(l:item.from.uri)

      for l:fromRange in l:item.fromRanges
        let l:line = l:fromRange.start.line+1
        let l:content = s:lineinfile(l:fname, l:line)
        if l:content is -1
          continue
        endif
        let l:locations = add(l:locations, printf('%s:%s:%s: %s', l:fname, l:line, go#lsp#lsp#PositionOf(content, l:fromRange.start.character), l:item.from.name))
      endfor
    catch
    endtry
  endfor

  call call(a:next, [l:locations])
  return
endfunction

function! go#lsp#DidOpen(fname) abort
  if get(b:, 'go_lsp_did_open', 0)
    return
  endif

  let l:fname = fnamemodify(a:fname, ':p')
  if !isdirectory(fnamemodify(l:fname, ':h'))
    return
  endif

  let l:lsp = s:lspfactory.get()

  if !has_key(l:lsp.notificationQueue, l:fname)
    let l:lsp.notificationQueue[l:fname] = []
  endif

  call s:ensureWorkspace(fnamemodify(l:fname, ':h'))

  let l:lsp.fileVersions[l:fname] = getbufvar(l:fname, 'changedtick')

  let l:msg = go#lsp#message#DidOpen(l:fname, join(go#util#GetLines(), "\n") . "\n", l:lsp.fileVersions[l:fname])
  let l:state = s:newHandlerState('')

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

  let l:fname = fnamemodify(a:fname, ':p')
  if !isdirectory(fnamemodify(l:fname, ':h'))
    return
  endif

  call go#lsp#DidOpen(a:fname)

  let l:lsp = s:lspfactory.get()

  let l:version = getbufvar(l:fname, 'changedtick')
  if has_key(l:lsp.fileVersions, l:fname) && l:lsp.fileVersions[l:fname] == l:version
    return
  endif
  let l:lsp.fileVersions[l:fname] = l:version

  let l:msg = go#lsp#message#DidChange(l:fname, join(go#util#GetLines(), "\n") . "\n", l:lsp.fileVersions[l:fname])
  let l:state = s:newHandlerState('')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! go#lsp#DidClose(fname) abort
  let l:fname = fnamemodify(a:fname, ':p')
  if !isdirectory(fnamemodify(l:fname, ':h'))
    return
  endif

  if !get(b:, 'go_lsp_did_open', 0)
    return
  endif

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#DidClose(l:fname)
  let l:state = s:newHandlerState('')
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

    let l:match = {'abbr': l:item.label, 'word': l:item.textEdit.newText, 'info': '', 'kind': go#lsp#completionitemkind#Vim(l:item.kind), 'user_data': '', 'icase': go#config#CodeCompletionIcase()}
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

    let l:match.user_data = l:match.info
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

" go#lsp#SameIDs calls gopls to get the references to the identifier at line
" and col in fname. handler should be a dictionary function that takes a list
" of strings in the form 'file:line:col: message'. handler will be attached to
" a dictionary that manages state (statuslines, sets the winid, etc.). handler
" should take three arguments: an exit_code, a JSON object encoded to a string
" that mimics guru's ouput for `what`, and third mode parameter that only
" exists for compatibility with the guru implementation of SameIDs.
" TODO(bc): refactor to not need the guru adapter.
function! go#lsp#SameIDs(showstatus, fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#References(a:fname, a:line, a:col)

  if a:showstatus
    let l:state = s:newHandlerState('same ids')
  else
    let l:state = s:newHandlerState('')
  endif

  let l:state.handleResult = funcref('s:sameIDsHandler', [function(a:handler, [], l:state)], l:state)
  let l:state.error = funcref('s:noop')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:sameIDsHandler(next, msg) abort dict
  let l:furi = go#path#ToURI(expand('%:p'))

  let l:result = {
        \ 'sameids': [],
        \ 'enclosing': [],
      \ }

  let l:msg = a:msg
  if a:msg is v:null
    let l:msg = []
  endif

  for l:loc in l:msg
    if l:loc.uri !=# l:furi
      continue
    endif

    if len(l:result.enclosing) == 0
      let l:result.enclosing = [{
            \ 'desc': 'identifier',
            \ 'start': l:loc.range.start.character+1,
            \ 'end': l:loc.range.end.character+1,
          \ }]
    endif

    let l:result.sameids = add(l:result.sameids, printf('%s:%s:%s', go#path#FromURI(l:loc.uri), l:loc.range.start.line+1, l:loc.range.start.character+1))
  endfor

  call call(a:next, [0, json_encode(l:result), ''])
endfunction

" go#lsp#Referrers calls gopls to get the references to the identifier at line
" and col in fname. handler should be a dictionary function that takes a list
" of strings in the form 'file:line:col: message'. handler will be attached to
" a dictionary that manages state (statuslines, sets the winid, etc.). handler
" should take three arguments: an exit_code, a JSON object encoded to a string
" that mimics guru's ouput for `what`, and third mode parameter that only
" exists for compatibility with the guru implementation of SameIDs.
" TODO(bc): refactor to not need the guru adapter.
function! go#lsp#Referrers(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#References(a:fname, a:line, a:col)

  let l:state = s:newHandlerState('referrers')

  let l:state.handleResult = funcref('s:handleReferences', [function(a:handler, [], l:state)], l:state)
  let l:state.error = funcref('s:noop')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:handleReferences(next, msg) abort dict
  call s:handleLocations(a:next, a:msg)
endfunction

function! s:handleLocations(next, msg) abort
  let l:result = []

  let l:msg = a:msg

  if l:msg is v:null
    let l:msg = []
  endif

  call sort(l:msg, funcref('s:compareLocations'))

  for l:loc in l:msg
    let l:fname = go#path#FromURI(l:loc.uri)
    let l:line = l:loc.range.start.line+1
    let l:content = s:lineinfile(l:fname, l:line)
    if l:content is -1
      continue
    endif

    let l:item = printf('%s:%s:%s: %s', l:fname, l:line, go#lsp#lsp#PositionOf(l:content, l:loc.range.start.character), l:content)

    let l:result = add(l:result, l:item)
  endfor

  call call(a:next, [0, l:result, ''])
endfunction

" go#lsp#Implementations calls gopls to get the implementations to the
" identifier at line and col in fname. handler should be a dictionary function
" that takes a list of strings in the form 'file:line:col: message'. handler
" will be attached to a dictionary that manages state (statuslines, sets the
" winid, etc.). handler should take three arguments: an exit_code, a JSON
" object encoded to a string that mimics guru's ouput for guru implements, and
" a third parameter that only exists for compatibility with guru implements.
function! go#lsp#Implements(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Implementation(a:fname, a:line, a:col)

  let l:state = s:newHandlerState('implements')

  let l:state.handleResult = funcref('s:handleImplements', [function(a:handler, [], l:state)], l:state)
  let l:state.error = funcref('s:handleImplementsError', [function(a:handler, [], l:state)], l:state)
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:handleImplements(next, msg) abort dict
  call s:handleLocations(a:next, a:msg)
endfunction

function! s:handleImplementsError(next, error) abort dict
  call call(a:next, [1, [a:error], ''])
endfunction

function! go#lsp#Hover(fname, line, col, handler) abort
  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Hover(a:fname, a:line, a:col)
  let l:state = s:newHandlerState('')
  let l:diagnosticMsg = ''

  if has_key(l:lsp.diagnostics, a:fname)
    for l:diagnostic in l:lsp.diagnostics[a:fname]
      if !s:within(l:diagnostic.range, a:line, a:col)
        continue
      endif

      let l:diagnosticMsg = l:diagnostic.message
      break
    endfor
  endif
  let l:state.handleResult = funcref('s:hoverHandler', [function(a:handler, [], l:state), l:diagnosticMsg], l:state)
  let l:state.error = funcref('s:hoverError', [function(a:handler, [], l:state), l:diagnosticMsg], l:state)
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:hoverHandler(next, diagnostic, msg) abort dict
  if a:msg is v:null || !has_key(a:msg, 'contents')
    if len(a:diagnostic) > 0
      call call(a:next, [a:diagnostic])
    endif
    return
  endif

  try
    let l:value = json_decode(a:msg.contents.value)

    let l:msg = []
    if len(a:diagnostic) > 0
      let l:msg = split(a:diagnostic, "\n")
      let l:msg = add(l:msg, '')
    endif
    let l:signature = split(l:value.signature, "\n")
    let l:msg = extend(l:msg, l:signature)
    if go#config#DocBalloon()
      " use synopsis instead of fullDocumentation to keep the hover window
      " small.
      let l:doc = l:value.synopsis
      if len(l:doc) isnot 0
        let l:msg = extend(l:msg, ['', l:doc])
      endif
    endif

    call call(a:next, [l:msg])
  catch
    " TODO(bc): log the message and/or show an error message.
  endtry
endfunction

function! s:hoverError(next, diagnostic, msg) abort dict
  try
    if len(a:diagnostic) > 0
      let l:msg = split(a:diagnostic, "\n")
      call call(a:next, [l:msg])
    endif
  catch
  endtry

  return
endfunction

function! go#lsp#Doc() abort
  let l:fname = expand('%:p')
  let [l:line, l:col] = go#lsp#lsp#Position()

  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Hover(l:fname, l:line, l:col)
  let l:state = s:newHandlerState('doc')
  let l:resultHandler = go#promise#New(function('s:docFromHoverResult', [], l:state), 10000, '')
  let l:state.handleResult = l:resultHandler.wrapper
  let l:state.error = l:resultHandler.wrapper
  call l:lsp.sendMessage(l:msg, l:state)
  return l:resultHandler.await()
endfunction

function! s:docFromHoverResult(msg) abort dict
  if type(a:msg) is type('')
    return [a:msg, 1]
  endif

  if a:msg is v:null || !has_key(a:msg, 'contents')
    return ['Undocumented', 0]
  endif

  let l:value = json_decode(a:msg.contents.value)
  let l:doc = l:value.fullDocumentation
  if len(l:doc) is 0
    let l:doc = 'Undocumented'
  endif
  let l:content = printf("%s\n\n%s", l:value.signature, l:doc)
  return [l:content, 0]
endfunction

function! go#lsp#DocLink() abort
  let l:fname = expand('%:p')
  let [l:line, l:col] = go#lsp#lsp#Position()

  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Hover(l:fname, l:line, l:col)
  let l:state = s:newHandlerState('doc url')
  let l:resultHandler = go#promise#New(function('s:docLinkFromHoverResult', [], l:state), 10000, '')
  let l:state.handleResult = l:resultHandler.wrapper
  let l:state.error = l:resultHandler.wrapper
  call l:lsp.sendMessage(l:msg, l:state)
  return l:resultHandler.await()
endfunction

function! s:docLinkFromHoverResult(msg) abort dict
  if type(a:msg) is type('')
    return [a:msg, 1]
  endif

  if a:msg is v:null || !has_key(a:msg, 'contents')
    return ['', 0]
  endif
  let l:doc = json_decode(a:msg.contents.value)

  " for backward compatibility with older gopls
  if has_key(l:doc, 'link')
    let l:link = l:doc.link
    return [l:doc.link, 0]
  endif

  if !has_key(l:doc, 'linkPath') || empty(l:doc.linkPath)
    return ['', 0]
  endif
  let l:link = l:doc.linkPath . "#" . l:doc.linkAnchor
  return [l:link, 0]
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
  if a:msg is v:null || len(a:msg) == 0
    return
  endif

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

  let l:state.handleResult = a:next
  let l:state.error = funcref('s:noop')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:info(show, msg) abort dict
  if a:msg is v:null || !has_key(a:msg, 'contents')
    return
  endif

  let l:value = json_decode(a:msg.contents.value)
  let l:content = [l:value.singleLine]
  let l:content = s:infoFromHoverContent(l:content)

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
    if len(l:dir) > 1 && l:dir[-1:] == '/'
      let l:dir = l:dir[:-2]
    endif
    if !isdirectory(l:dir)
      continue
    endif

    let l:workspaces = add(l:workspaces, l:dir)
  endfor

  let l:lsp = s:lspfactory.get()
  let l:state = s:newHandlerState('')
  let l:lsp.workspaceDirectories = s:dedup(extend(l:lsp.workspaceDirectories, l:workspaces))
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
      let l:missing = add(l:missing, l:dir)
      call remove(l:lsp.workspaceDirectories, l:i)
      continue
    endif
    let l:i += 1
  endfor

  if len(l:missing) == 0
    return 0
  endif

  let l:state = s:newHandlerState('')
  let l:msg = go#lsp#message#ChangeWorkspaceFolders([], s:dedup(l:missing))
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
  let l:msg = go#lsp#message#ChangeWorkspaceFolders(s:dedup(l:lsp.workspaceDirectories), s:dedup(l:lsp.workspaceDirectories))
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

function! go#lsp#Exit() abort
  call s:exit(0)
endfunction

function! go#lsp#Restart() abort
  call s:exit(1)
endfunction

function! s:exit(restart) abort
  if !go#util#has_job() || len(s:lspfactory) == 0 || !has_key(s:lspfactory, 'current')
    return
  endif

  let l:lsp = s:lspfactory.get()

  " reset the factory so that future requests don't use the same instance of
  " gopls.
  call s:lspfactory.reset()

  let l:lsp.restarting = a:restart

  let l:state = s:newHandlerState('exit')

  let l:msg = go#lsp#message#Shutdown()
  let l:retval = l:lsp.sendMessage(l:msg, l:state)

  let l:msg = go#lsp#message#Exit()
  let l:retval = l:lsp.sendMessage(l:msg, l:state)

  return l:retval
endfunction

let s:log = []
function! s:debugasync(timer) abort
  if !go#util#HasDebug('lsp')
    let s:log = []
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
    for [l:event, l:data] in s:log
      call remove(s:log, 0)
      if getline(1) == ''
        call setline('$', printf('===== %s =====', l:event))
      else
        call append('$', printf('===== %s =====', l:event))
      endif
      call append('$', split(l:data, "\r\n"))
    endfor
    normal! G
    setlocal nomodifiable
  finally
    call win_gotoid(l:winid)
  endtry
endfunction

function! s:debug(event, data) abort
  let l:shouldStart = len(s:log) == 0
  let s:log = add(s:log, [a:event, a:data])

  if l:shouldStart
    call timer_start(10, function('s:debugasync', []))
  endif
endfunction

function! s:compareLocations(left, right) abort
  if a:left.uri < a:right.uri
    return -1
  endif

  if a:left.uri == a:right.uri && a:left.range.start.line < a:right.range.start.line
    return -1
  endif

  if a:left.uri == a:right.uri && a:left.range.start.line == a:right.range.start.line && a:left.range.start.character < a:right.range.start.character
    return -1
  endif

  if a:left.uri == a:right.uri && a:left.range.start.line == a:right.range.start.line && a:left.range.start.character == a:right.range.start.character
    return 0
  endif

  return 1
endfunction

function! go#lsp#Diagnostics(...) abort
  if a:0 == 0
    return []
  endif

  let l:dirsToPackages = {}

  let l:lsp = s:lspfactory.get()

  let l:diagnostics = []
  for [l:key, l:val] in items(l:lsp.diagnostics)
    let l:dir = fnamemodify(l:key, ':h')

    if !has_key(l:dirsToPackages, l:dir)
      let l:pkg = go#package#FromPath(l:dir)
      let l:dirsToPackages[l:dir] = l:pkg
    else
      let l:pkg = l:dirsToPackages[l:dir]
    endif

    if type(l:pkg) == type(0)
      continue
    endif

    for l:arg in a:000
      if l:arg == l:pkg || l:arg == 'all'
        let l:diagnostics = extend(l:diagnostics, map(l:val, 'v:val["error"]'))
      endif
    endfor
  endfor

  return sort(l:diagnostics)
endfunction

function! go#lsp#AnalyzeFile(fname) abort
  let l:fname = fnamemodify(a:fname, ':p')
  if !isdirectory(fnamemodify(l:fname, ':h'))
    return []
  endif

  let l:lsp = s:lspfactory.get()

  let l:lastdiagnostics = get(l:lsp.diagnostics, l:fname, [])

  let l:version = get(l:lsp.fileVersions, a:fname, 0)
  if l:version == getbufvar(a:fname, 'changedtick')
    return map(l:lastdiagnostics, 'v:val["error"]')
  endif

  call go#lsp#DidChange(a:fname)

  let l:diagnostics = go#promise#New(function('s:setDiagnostics', []), 10000, l:lastdiagnostics)
  let l:lsp.notificationQueue[l:fname] = add(get(l:lsp.notificationQueue, l:fname, []), l:diagnostics.wrapper)
  return l:diagnostics.await()
endfunction

function! s:setDiagnostics(...) abort
  return map(a:000, 'v:val["error"]')
endfunction

" s:processDiagnostic converts a diagnostic from LSP into useful values for
" Vim. It returns the a value with the original message, the diagnostic range
" as expressed by LSP, an error string, and the Vim match position described
" in the diagnostic. The match position will be an empty list when bufname is
" not a valid name for the current buffer.
function! s:processDiagnostic(diagnostic, bufname, fname) abort
  let l:range = a:diagnostic.range

  let l:diagnostic = {
        \ "message": a:diagnostic.message,
        \ "range": {
          \ "start": {
            \ "line": l:range.start.line,
            \ "character": l:range.start.character,
          \ },
          \ "end": {
            \ "line": l:range.end.line,
            \ "character": l:range.end.character,
          \ },
        \ },
      \ }

  let l:line = l:range.start.line + 1
  let l:endline = l:range.end.line + 1

  let l:buflines = getbufline(a:bufname, l:line)
  let l:col = ''
  if len(l:buflines) > 0
    let l:col = go#lsp#lsp#PositionOf(l:buflines[0], l:range.start.character)
  endif

  let l:severity = go#lsp#lsp#SeverityToErrorType(a:diagnostic.severity)
  let l:diagnostic.error = printf('%s:%s:%s:%s: %s', a:fname, l:line, l:col, l:severity, l:diagnostic.message)

  if !(a:diagnostic.severity == 1 || a:diagnostic.severity == 2)
    return [l:diagnostic, []]
  endif

  " return when the diagnostic is not for the current buffer.
  if bufnr(a:bufname) != bufnr('')
    return [l:diagnostic, []]
  end

  " don't bother trying to highlight errors or warnings that span
  " the whole file (e.g when there's missing package documentation).
  if l:line == 1 && (l:endline) == line('$')
    return [l:diagnostic, []]
  endif

  if len(l:buflines) == 0
    return [l:diagnostic, []]
  endif

  let l:buflines = getbufline(a:bufname, l:endline)
  if len(l:buflines) > 0
    let l:endcol = go#lsp#lsp#PositionOf(l:buflines[0], l:range.end.character)
  else
    return [l:diagnostic, []]
  endif

  " the length of the match is the number of bytes between the start of
  " the match and the end of the match.
  let l:matchLength = line2byte(l:endline) + l:endcol - (line2byte(l:line) + l:col)
  let l:pos = [l:line, l:col, l:matchLength]

  return [l:diagnostic, l:pos]
endfunction

function! s:highlightMatches(errorMatches, warningMatches) abort
  " set buffer variables for errors and warnings to zero values
  let b:go_diagnostic_matches = {'errors': [], 'warnings': []}

  if hlexists('goDiagnosticError')
    " clear the old matches just before adding the new ones to keep flicker
    " to a minimum and clear before checking the level so that if the user
    " changed the level since the last highlighting, the highlighting will be
    " be properly cleared.
    call go#util#ClearHighlights('goDiagnosticError')
    if go#config#DiagnosticsLevel() >= 2
      let b:go_diagnostic_matches.errors = copy(a:errorMatches)
      if go#config#HighlightDiagnosticErrors()
        call go#util#HighlightPositions('goDiagnosticError', a:errorMatches)
      endif
    endif
  endif

  if hlexists('goDiagnosticWarning')
    " clear the old matches just before adding the new ones to keep flicker
    " to a minimum and clear before checking the level so that if the user
    " changed the level since the last highlighting, the highlighting will be
    " be properly cleared.
    call go#util#ClearHighlights('goDiagnosticWarning')
    if go#config#DiagnosticsLevel() >= 2
      let b:go_diagnostic_matches.warnings = copy(a:warningMatches)
      if go#config#HighlightDiagnosticWarnings()
        call go#util#HighlightPositions('goDiagnosticWarning', a:warningMatches)
      endif
    endif
  endif

  " re-apply matches at the time the buffer is displayed in a new window or
  " redisplayed in an existing window: e.g. :edit,
  augroup vim-go-diagnostics
    autocmd! * <buffer>
    autocmd BufDelete <buffer> autocmd! vim-go-diagnostics * <buffer=abuf>
    if has('textprop')
      autocmd BufReadPost <buffer> nested call s:highlightMatches(b:go_diagnostic_matches.errors, b:go_diagnostic_matches.warnings)
    else
      autocmd BufWinEnter <buffer> nested call s:highlightMatches(b:go_diagnostic_matches.errors, b:go_diagnostic_matches.warnings)
    endif
  augroup end
endfunction

" ClearDiagnosticHighlights removes all goDiagnosticError and
" goDiagnosticWarning matches.
function! go#lsp#ClearDiagnosticHighlights() abort
  call go#util#ClearHighlights('goDiagnosticError')
  call go#util#ClearHighlights('goDiagnosticWarning')
endfunction

" Format formats the current buffer.
function! go#lsp#Format() abort
  let l:fname = expand('%:p')
  " send the current file so that TextEdits will be relative to the current
  " state of the buffer.
  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()

  let l:state = s:newHandlerState('')
  let l:handleFormat = go#promise#New(function('s:handleFormat', [], l:state), 10000, '')
  let l:state.handleResult = l:handleFormat.wrapper
  let l:state.error = l:handleFormat.wrapper
  let l:state.handleError = function('s:handleFormatError', [l:fname], l:state)
  let l:msg = go#lsp#message#Format(l:fname)
  call l:lsp.sendMessage(l:msg, l:state)

  call go#fmt#CleanErrors()

  " await the result to avoid any race conditions among autocmds (e.g.
  " BufWritePre and BufWritePost)
  call l:handleFormat.await()
endfunction

" Imports executes the source.organizeImports code action for the current
" buffer.
function! go#lsp#Imports() abort
  let l:fname = expand('%:p')
  " send the current file so that TextEdits will be relative to the current
  " state of the buffer.
  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()

  let l:state = s:newHandlerState('')
  let l:handler = go#promise#New(function('s:handleCodeAction', ['source.organizeImports', ''], l:state), 10000, '')
  let l:state.handleResult = l:handler.wrapper
  let l:state.error = l:handler.wrapper
  let l:state.handleError = function('s:handleCodeActionError', [l:fname], l:state)
  let l:msg = go#lsp#message#CodeActionImports(l:fname)
  call l:lsp.sendMessage(l:msg, l:state)

  " await the result to avoid any race conditions among autocmds (e.g.
  " BufWritePre and BufWritePost)
  call l:handler.await()
endfunction

" FillStruct executes the refactor.rewrite code action for the current buffer
" and configures the handler to only apply the fillstruct command for the
" current location.
function! go#lsp#FillStruct() abort
  let l:fname = expand('%:p')
  " send the current file so that TextEdits will be relative to the current
  " state of the buffer.
  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()

  let l:state = s:newHandlerState('')
  let l:handler = go#promise#New(function('s:handleCodeAction', ['refactor.rewrite', 'apply_fix'], l:state), 10000, '')
  let l:state.handleResult = l:handler.wrapper
  let l:state.error = l:handler.wrapper
  let l:state.handleError = function('s:handleCodeActionError', [l:fname], l:state)

  let [l:line, l:col] = go#lsp#lsp#Position()
  let l:msg = go#lsp#message#CodeActionFillStruct(l:fname, l:line, l:col)
  call l:lsp.sendMessage(l:msg, l:state)

  " await the result to avoid any race conditions among autocmds (e.g.
  " BufWritePre and BufWritePost)
  call l:handler.await()
endfunction

function! go#lsp#Rename(newName) abort
  let l:fname = expand('%:p')
  let [l:line, l:col] = go#lsp#lsp#Position()

  call go#lsp#DidChange(l:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#PrepareRename(l:fname, l:line, l:col)
  let l:state = s:newHandlerState('rename')
  let l:resultHandler = go#promise#New(function('s:rename', [l:fname, l:line, l:col, a:newName], l:state), 10000, '')

  let l:state.handleResult = l:resultHandler.wrapper
  let l:state.error = l:resultHandler.wrapper
  let l:state.handleError = function('s:handleRenameError', [], l:state)
  call l:lsp.sendMessage(l:msg, l:state)

  return l:resultHandler.await()
endfunction

function! go#lsp#ModReload(...) abort
  let l:gomod = 'go.mod'
  if a:0 is 0
    let l:modroot = go#util#ModuleRoot()
    if l:modroot is -1
      call go#util#EchoError('go module not found')
      return
    endif
    let l:gomod = printf('%s/%s', l:modroot, 'go.mod')
  else
    let l:gomod = a:1
  endif

  if l:gomod !~ 'go.mod$'
    let l:gomod = printf('%s/%s', l:gomod, 'go.mod')
  endif
  let l:gomod = fnamemodify(l:gomod, ':p')

  if !filereadable(l:gomod)
    call go#util#EchoError('go.mod does not exist')
    return
  endif

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#DidChangeWatchedFile(l:gomod, 'Changed')
  let l:state = s:newHandlerState('')
  return l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:rename(fname, line, col, newName, msg) abort dict
  if type(a:msg) is type('')
    call self.handleError(a:msg)
    return
  endif

  if a:msg is v:null
    call go#util#EchoWarning('cannot rename the identifier at the requested position')
    return
  endif

  call go#lsp#DidChange(a:fname)

  let l:lsp = s:lspfactory.get()
  let l:msg = go#lsp#message#Rename(a:fname, a:line, a:col, a:newName)
  let l:state = s:newHandlerState('rename')
  let l:resultHandler = go#promise#New(function('s:handleRename', [], l:state), 10000, '')

  let l:state.handleResult = l:resultHandler.wrapper
  let l:state.error = l:resultHandler.wrapper
  let l:state.handleError = function('s:handleRenameError', [], l:state)
  call l:lsp.sendMessage(l:msg, l:state)

  return l:resultHandler.await()
endfunction

function! s:handleRename(msg) abort dict
  if type(a:msg) is type('')
    call self.handleError(a:msg)
    return
  endif

  if a:msg is v:null
    return
  endif
  call s:applyDocumentChanges(a:msg.documentChanges)
endfunction

function! s:executeCommand(cmd, args) abort
  let l:lsp = s:lspfactory.get()

  let l:state = s:newHandlerState('')

  let l:msg = go#lsp#message#ExecuteCommand(a:cmd, a:args)
  call l:lsp.sendMessage(l:msg, l:state)
endfunction

function! s:handleFormat(msg) abort dict
  call go#fmt#CleanErrors()

  if type(a:msg) is type('')
    call self.handleError(a:msg)
    return
  endif
  call s:applyTextEdits(bufnr(''), a:msg)
endfunction

function! s:handleCodeAction(kind, cmd, msg) abort dict
  if type(a:msg) is type('')
    call self.handleError(a:msg)
    return
  endif

  if a:msg is v:null
    return
  endif

  for l:item in a:msg
    if get(l:item, 'kind', '') is a:kind
      if !has_key(l:item, 'edit')
        continue
      endif

      if has_key(l:item, 'disabled') && get(l:item.disabled, 'reason', '') isnot ''
        call go#util#EchoWarning(printf('code action is disabled: %s', l:item.disabled.reason))
        continue
      endif

      if has_key(l:item, 'command')
        if has_key(l:item.command, 'command') && (l:item.command.command is a:cmd || l:item.command.command is printf('gopls.%s', a:cmd))
          call s:executeCommand(l:item.command.command, l:item.command.arguments)
          continue
        endif
      endif

      if !has_key(l:item.edit, 'documentChanges')
        continue
      endif
      call s:applyDocumentChanges(l:item.edit.documentChanges)
    endif
  endfor
endfunction

function s:applyDocumentChanges(changes)
  let l:bufnr = bufnr('')

  for l:change in a:changes
    if !has_key(l:change, 'edits')
      continue
    endif
    let l:fname = go#path#FromURI(l:change.textDocument.uri)

    " get the buffer name relative to the current directory, because
    " Vim says that a buffer name can't be an absolute path.
    let l:bufname = fnamemodify(l:fname, ':.')

    let l:bufadded = 0
    let l:bufloaded = 0

    let l:editbufnr = bufnr(l:bufname)
    if l:editbufnr != bufnr('')
      " make sure the buffer is listed and loaded before applying text edits
      if !bufexists(l:bufname)
        call bufadd(l:bufname)
        let l:bufadded = 1
      endif

      if !bufloaded(l:bufname)
        call bufload(l:bufname)
        let l:bufloaded = 1
      endif

      let l:editbufnr = bufnr(l:bufname)
      if l:editbufnr == -1
        call go#util#EchoWarn(printf('could not apply changes to %s', l:fname))
        continue
      endif

       " TODO(bc): do not edit the buffer when vim-go drops support for Vim
       " 8.0. Instead, use the functions to modify a buffer (e.g.
       " appendbufline, getbufline, deletebufline).
       execute printf('keepalt keepjumps buffer! %d', l:editbufnr)
    endif
    call s:applyTextEdits(l:editbufnr, l:change.edits)

    " TODO(bc): save the buffer?
    " TODO(bc): unload and/or delete a buffer that was loaded or added,
    " respectively?
  endfor
  if bufnr('') != l:bufnr
    execute printf('keepalt keepjumps buffer! %d', l:bufnr)
  endif
endfunction

" s:applyTextEdit applies the list of WorkspaceEdit values in msg.
function s:applyTextEdits(bufnr, msg) abort
  if a:msg is v:null
    return
  endif

  " TODO(bc): start using the functions to modify a buffer (e.g. appendbufline,
  " deletebufline, etc) instead of operating on the current buffer when vim-go
  " drops support from Vim 8.0.

  " process the TextEdit list in reverse order, because the positions are
  " based on the current line numbers; processing in forward order would
  " require keeping track of how the proper position of each TextEdit would be
  " affected by all the TextEdits that came before.
  call reverse(sort(a:msg, function('s:textEditLess')))
  for l:msg in a:msg
    let l:startline = l:msg.range.start.line+1
    let l:endline = l:msg.range.end.line+1
    let l:text = l:msg.newText

    " handle the deletion of whole lines
    if len(l:text) == 0 && l:msg.range.start.character == 0 && l:msg.range.end.character == 0 && l:startline < l:endline
      call s:deleteline(l:startline, l:endline-1)
      continue
    endif

    " Assume that l:startcontent will be an empty string. When the replacement
    " is not at the beginning of the line, then l:startcontent must be what
    " comes before the start position on the start line.
    let l:startcontent = ''
    if l:msg.range.start.character > 0
      let l:startcontent = getline(l:startline)
      let l:preSliceEnd = go#lsp#lsp#PositionOf(l:startcontent, l:msg.range.start.character-1) - 1
      let l:startcontent = l:startcontent[:l:preSliceEnd]
    endif

    let l:endcontent = getline(l:endline)
    let l:postSliceStart = 0
    if l:msg.range.end.character > 0
      let l:postSliceStart = go#lsp#lsp#PositionOf(l:endcontent, l:msg.range.end.character-1)
      let l:endcontent = l:endcontent[(l:postSliceStart):]
    endif

    " There isn't an easy way to replace the text in a byte or character
    " range, so append to l:text any text on l:endline starting from
    " l:postSliceStart and prepend to l:text any text on l:startline prior to
    " l:preSliceEnd, and finally replace the lines with a delete followed by
    " and append.
    let l:text = printf('%s%s%s', l:startcontent, l:text, l:endcontent)

    " TODO(bc): deal with the undo file
    " TODO(bc): deal with folds

    " TODO(bc): can we use appendbufline instead of deleting and appending?
    call s:deleteline(l:startline, l:endline)
    for l:line in split(l:text, "\n", 1)
      call append(l:startline-1, l:line)
      let l:startline += 1
    endfor
  endfor

  call go#lsp#DidChange(expand('%:p'))
  return
endfunction

function! s:handleFormatError(filename, msg) abort dict
  if go#config#FmtFailSilently()
    return
  endif

  let l:errors = split(a:msg, '\n')
  let l:errors = map(l:errors, printf('substitute(v:val, ''^'', ''%s:'', '''')', a:filename))
  let l:errors = join(l:errors, "\n")
  call go#fmt#ShowErrors(l:errors)
endfunction

function! s:handleCodeActionError(filename, msg) abort dict
  " TODO(bc): handle the error?
endfunction

function! s:handleRenameError(msg) abort dict
  call go#util#EchoError(a:msg)
endfunction

function! s:textEditLess(left, right) abort
  " TextEdits in a TextEdit[] never overlap and Vim's sort() is stable.
  if a:left.range.start.line < a:right.range.start.line
    return -1
  endif

  if a:left.range.start.line > a:right.range.start.line
    return 1
  endif

  if a:left.range.start.line == a:right.range.start.line
    if a:left.range.start.character < a:right.range.start.character
      return -1
    endif

    if a:left.range.start.character > a:right.range.start.character
      return 1
    endif
  endif

  " return 0, because a:left and a:right refer to the same position.
  return 0
endfunction

function! s:deleteline(start, end) abort
  if exists('*deletebufline')
    call deletebufline('', a:start, a:end)
  else
    call execute(printf('%d,%d d_', a:start, a:end))
  endif
endfunction

function! s:ensureWorkspace(dir)
  let l:modroot = go#util#ModuleRoot(a:dir)
  if l:modroot is -1 || l:modroot is ''
    return
  endif


  let l:lsp = s:lspfactory.get()

  for l:dir in l:lsp.workspaceDirectories
    if l:dir == l:modroot
      return
    endif
  endfor

  " Do not add directories that reside in the module cache if there's any
  " other directories already in the workspace. In such a case, adding a
  " module directory can potentially break jumping to definitions and finding
  " references if the module in the cache has a replace directive in it the
  " refers to a relative path.
  if len(l:lsp.workspaceDirectories) > 0
    let l:modcache = go#util#env('gomodcache')
    if l:modroot[0:len(l:modcache)-1] ==# l:modcache
      return
    endif
  endif

  call go#lsp#AddWorkspaceDirectory(l:modroot)
endfunction

function! s:dedup(list)
  let l:dict = {}
  for l:item in a:list
    let l:dict[l:item] = 1
  endfor

  return sort(keys(l:dict))
endfunction

function! s:lineinfile(fname, line) abort
  let l:bufnr = bufnr(a:fname)
  let l:bufinfo = getbufinfo(a:fname)

  try
    if l:bufnr == -1 || len(l:bufinfo) == 0 || l:bufinfo[0].loaded == 0
      let l:filecontents = readfile(a:fname, '', a:line)
    else
      let l:filecontents = getbufline(a:fname, a:line)
    endif

    if len(l:filecontents) == 0
      return -1
    endif

    return l:filecontents[-1]
  catch
    "call go#util#EchoError(printf('%s (line %s): %s at %s', a:fname, a:line, v:exception, v:throwpoint))
    return -1
  endtry
endfunction

function! s:within(range, line, character) abort
  if a:line < a:range.start.line
    return 0
  endif

  if a:line > a:range.end.line
    return 0
  endif

  if a:line == a:range.start.line && a:character < a:range.start.character
    return 0
  endif

  if a:line == a:range.end.line && a:character > a:range.end.character
    return 0
  endif

  return 1
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
