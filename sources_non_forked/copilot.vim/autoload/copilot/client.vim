scriptencoding utf-8

let s:plugin_version = copilot#version#String()

let s:error_canceled = {'code': -32800, 'message': 'Canceled'}
let s:error_exit = {'code': -32097, 'message': 'Process exited'}
let s:error_connection_inactive = {'code': -32096, 'message': 'Connection inactive'}

let s:root = expand('<sfile>:h:h:h')

if !exists('s:instances')
  let s:instances = {}
endif

" allow sourcing this file to reload the Lua file too
if has('nvim')
  lua package.loaded._copilot = nil
endif

function! s:Warn(msg) abort
  if !empty(get(g:, 'copilot_no_startup_warnings'))
    return
  endif
  echohl WarningMsg
  echomsg 'Copilot: ' . a:msg
  echohl NONE
endfunction

function! s:VimClose() dict abort
  if !has_key(self, 'job')
    return
  endif
  let job = self.job
  if has_key(self, 'kill')
    call job_stop(job, 'kill')
    call copilot#logger#Warn('Process forcefully terminated')
    return
  endif
  let self.kill = v:true
  let self.shutdown = self.Request('shutdown', {}, function(self.Notify, ['exit']))
  call timer_start(2000, { _ -> job_stop(job, 'kill') })
  call copilot#logger#Debug('Process shutdown initiated')
endfunction

function! s:LogSend(request, line) abort
  return '--> ' . a:line
endfunction

function! s:RejectRequest(request, error) abort
  if a:request.status !=# 'running'
    return
  endif
  let a:request.waiting = {}
  call remove(a:request, 'resolve')
  let reject = remove(a:request, 'reject')
  let a:request.status = 'error'
  let a:request.error = deepcopy(a:error)
  for Cb in reject
    let a:request.waiting[timer_start(0, function('s:Callback', [a:request, 'error', Cb]))] = 1
  endfor
  if index([s:error_canceled.code, s:error_connection_inactive.code], a:error.code) != -1
    return
  endif
  let msg = 'Method ' . a:request.method . ' errored with E' . a:error.code . ': ' . json_encode(a:error.message)
  if empty(reject)
    call copilot#logger#Error(msg)
  else
    call copilot#logger#Debug(msg)
  endif
endfunction

function! s:AfterInitialized(fn, ...) dict abort
  call add(self.after_initialized, function(a:fn, a:000))
endfunction

function! s:Send(instance, request) abort
  if !has_key(a:instance, 'job')
    return v:false
  endif
  try
    call ch_sendexpr(a:instance.job, a:request)
    return v:true
  catch /^Vim\%((\a\+)\)\=:E906:/
    let a:instance.kill = v:true
    let job = remove(a:instance, 'job')
    call job_stop(job)
    call timer_start(2000, { _ -> job_stop(job, 'kill') })
    call copilot#logger#Warn('Terminating process after failed write')
    return v:false
  catch /^Vim\%((\a\+)\)\=:E631:/
    return v:false
  endtry
endfunction

function! s:VimNotify(method, params) dict abort
  let request = {'method': a:method, 'params': a:params}
  call self.AfterInitialized(function('s:Send', [self, request]))
endfunction

function! s:RequestWait() dict abort
  while self.status ==# 'running'
    sleep 1m
  endwhile
  while !empty(get(self, 'waiting', {}))
    sleep 1m
  endwhile
  return self
endfunction

function! s:RequestAwait() dict abort
  call self.Wait()
  if has_key(self, 'result')
    return self.result
  endif
  throw 'Copilot:E' . self.error.code . ': ' . self.error.message
endfunction

function! s:RequestClient() dict abort
  return get(s:instances, self.client_id, v:null)
endfunction

if !exists('s:id')
  let s:id = 0
endif
if !exists('s:progress_token_id')
  let s:progress_token_id = 0
endif

function! s:SetUpRequest(instance, id, method, params, progress, ...) abort
  let request = {
        \ 'client_id': a:instance.id,
        \ 'id': a:id,
        \ 'method': a:method,
        \ 'params': a:params,
        \ 'Client': function('s:RequestClient'),
        \ 'Wait': function('s:RequestWait'),
        \ 'Await': function('s:RequestAwait'),
        \ 'Cancel': function('s:RequestCancel'),
        \ 'resolve': [],
        \ 'reject': [],
        \ 'progress': a:progress,
        \ 'status': 'running'}
  let args = a:000[2:-1]
  if len(args)
    if !empty(a:1)
      call add(request.resolve, { v -> call(a:1, [v] + args)})
    endif
    if !empty(a:2)
      call add(request.reject, { v -> call(a:2, [v] + args)})
    endif
    return request
  endif
  if a:0 && !empty(a:1)
    call add(request.resolve, a:1)
  endif
  if a:0 > 1 && !empty(a:2)
    call add(request.reject, a:2)
  endif
  return request
endfunction

function! s:UrlEncode(str) abort
  return substitute(iconv(a:str, 'latin1', 'utf-8'),'[^A-Za-z0-9._~!$&''()*+,;=:@/-]','\="%".printf("%02X",char2nr(submatch(0)))','g')
endfunction

let s:slash = exists('+shellslash') ? '\' : '/'
function! s:UriFromBufnr(bufnr) abort
  let absolute = tr(bufname(a:bufnr), s:slash, '/')
  if absolute !~# '^\a\+:\|^/\|^$' && getbufvar(a:bufnr, 'buftype') =~# '^\%(nowrite\)\=$'
    let absolute = substitute(tr(getcwd(), s:slash, '/'), '/\=$', '/', '') . absolute
  endif
  return s:UriFromPath(absolute)
endfunction

function! s:UriFromPath(absolute) abort
  let absolute = a:absolute
  if has('win32') && absolute =~# '^\a://\@!'
    return 'file:///' . strpart(absolute, 0, 2) . s:UrlEncode(strpart(absolute, 2))
  elseif absolute =~# '^/'
    return 'file://' . s:UrlEncode(absolute)
  elseif absolute =~# '^\a[[:alnum:].+-]*:\|^$'
    return absolute
  else
    return ''
  endif
endfunction

function! s:BufferText(bufnr) abort
  return join(getbufline(a:bufnr, 1, '$'), "\n") . "\n"
endfunction

let s:valid_request_key = '^\%(id\|method\|params\)$'
function! s:SendRequest(instance, request, ...) abort
  if !has_key(a:instance, 'job') || get(a:instance, 'shutdown', a:request) isnot# a:request
    return s:RejectRequest(a:request, s:error_connection_inactive)
  endif
  let json = filter(copy(a:request), 'v:key =~# s:valid_request_key')
  if empty(s:Send(a:instance, json)) && has_key(a:request, 'id') && has_key(a:instance.requests, a:request.id)
    call s:RejectRequest(remove(a:instance.requests, a:request.id), {'code': -32099, 'message': 'Write failed'})
  endif
endfunction

function! s:RegisterWorkspaceFolderForBuffer(instance, buf) abort
  let root = getbufvar(a:buf, 'workspace_folder')
  if type(root) != v:t_string
    return
  endif
  let root = s:UriFromPath(substitute(root, '[\/]$', '', ''))
  if empty(root) || has_key(a:instance.workspaceFolders, root)
    return
  endif
  let a:instance.workspaceFolders[root] = v:true
  call a:instance.Notify('workspace/didChangeWorkspaceFolders', {'event': {'added': [{'uri': root, 'name': fnamemodify(root, ':t')}], 'removed': []}})
endfunction

function! s:PreprocessParams(instance, params) abort
  let bufnr = v:null
  for doc in filter([get(a:params, 'textDocument', {})], 'type(get(v:val, "uri", "")) == v:t_number')
    let bufnr = doc.uri
    call s:RegisterWorkspaceFolderForBuffer(a:instance, bufnr)
    call extend(doc, a:instance.Attach(bufnr))
  endfor
  let progress_tokens = []
  for key in keys(a:params)
    if key =~# 'Token$' && type(a:params[key]) == v:t_func
      let s:progress_token_id += 1
      let a:instance.progress[s:progress_token_id] = a:params[key]
      call add(progress_tokens, s:progress_token_id)
      let a:params[key] = s:progress_token_id
    endif
  endfor
  return [bufnr, progress_tokens]
endfunction

function! s:VimAttach(bufnr) dict abort
  if !bufloaded(a:bufnr)
    return {'uri': '', 'version': 0}
  endif
  let bufnr = a:bufnr
  let doc = {
        \ 'uri': s:UriFromBufnr(bufnr),
        \ 'version': getbufvar(bufnr, 'changedtick', 0),
        \ 'languageId': getbufvar(bufnr, '&filetype'),
        \ }
  if has_key(self.open_buffers, bufnr) && (
        \ self.open_buffers[bufnr].uri !=# doc.uri ||
        \ self.open_buffers[bufnr].languageId !=# doc.languageId)
    call self.Notify('textDocument/didClose', {'textDocument': {'uri': self.open_buffers[bufnr].uri}})
    call remove(self.open_buffers, bufnr)
  endif
  if !has_key(self.open_buffers, bufnr)
    call self.Notify('textDocument/didOpen', {'textDocument': extend({'text': s:BufferText(bufnr)}, doc)})
    let self.open_buffers[bufnr] = doc
  else
    call self.Notify('textDocument/didChange', {
          \ 'textDocument': {'uri': doc.uri, 'version': doc.version},
          \ 'contentChanges': [{'text': s:BufferText(bufnr)}]})
    let self.open_buffers[bufnr].version = doc.version
  endif
  return doc
endfunction

function! s:VimIsAttached(bufnr) dict abort
  return bufloaded(a:bufnr) && has_key(self.open_buffers, a:bufnr) ? v:true : v:false
endfunction

function! s:VimRequest(method, params, ...) dict abort
  let s:id += 1
  let params = deepcopy(a:params)
  let [_, progress] = s:PreprocessParams(self, params)
  let request = call('s:SetUpRequest', [self, s:id, a:method, params, progress] + a:000)
  call self.AfterInitialized(function('s:SendRequest', [self, request]))
  let self.requests[s:id] = request
  return request
endfunction

function! s:Call(method, params, ...) dict abort
  let request = call(self.Request, [a:method, a:params] + a:000)
  if a:0
    return request
  endif
  return request.Await()
endfunction

function! s:Cancel(request) dict abort
  if has_key(self.requests, get(a:request, 'id', ''))
    call self.Notify('$/cancelRequest', {'id': a:request.id})
    call s:RejectRequest(remove(self.requests, a:request.id), s:error_canceled)
  endif
endfunction

function! s:RequestCancel() dict abort
  let instance = self.Client()
  if !empty(instance)
    call instance.Cancel(self)
  elseif get(self, 'status', '') ==# 'running'
    call s:RejectRequest(self, s:error_canceled)
  endif
  return self
endfunction

function! s:DispatchMessage(instance, method, handler, id, params, ...) abort
  try
    let response = {'result': call(a:handler, [a:params, a:instance])}
    if response.result is# 0
      let response.result = v:null
    endif
  catch
    call copilot#logger#Exception('lsp.request.' . a:method)
    let response = {'error': {'code': -32000, 'message': v:exception}}
  endtry
  if a:id isnot# v:null
    call s:Send(a:instance, extend({'id': a:id}, response))
  endif
  if !has_key(s:notifications, a:method)
    return response
  endif
endfunction

function! s:OnMessage(instance, body, ...) abort
  if !has_key(a:body, 'method')
    return s:OnResponse(a:instance, a:body)
  endif
  let request = a:body
  let id = get(request, 'id', v:null)
  let params = get(request, 'params', v:null)
  if has_key(a:instance.methods, request.method)
    return s:DispatchMessage(a:instance, request.method, a:instance.methods[request.method], id, params)
  elseif id isnot# v:null
    call s:Send(a:instance, {"id": id, "error": {"code": -32700, "message": "Method not found: " . request.method}})
    call copilot#logger#Debug('Unexpected request ' . request.method . ' called with ' . json_encode(params))
  elseif request.method !~# '^\$/'
    call copilot#logger#Debug('Unexpected notification ' . request.method . ' called with ' . json_encode(params))
  endif
endfunction

function! s:OnResponse(instance, response, ...) abort
  let response = a:response
  let id = get(a:response, 'id', v:null)
  if !has_key(a:instance.requests, id)
    return
  endif
  let request = remove(a:instance.requests, id)
  for progress_token in request.progress
    if has_key(a:instance.progress, progress_token)
      call remove(a:instance.progress, progress_token)
    endif
  endfor
  if request.status !=# 'running'
    return
  endif
  if has_key(response, 'result')
    let request.waiting = {}
    let resolve = remove(request, 'resolve')
    call remove(request, 'reject')
    let request.status = 'success'
    let request.result = response.result
    for Cb in resolve
      let request.waiting[timer_start(0, function('s:Callback', [request, 'result', Cb]))] = 1
    endfor
  else
    call s:RejectRequest(request, response.error)
  endif
endfunction

function! s:OnErr(instance, ch, line, ...) abort
  if !has_key(a:instance, 'serverInfo')
    call copilot#logger#Bare('<-! ' . a:line)
  endif
endfunction

function! s:OnExit(instance, code, ...) abort
  let a:instance.exit_status = a:code
  if has_key(a:instance, 'job')
    call remove(a:instance, 'job')
  endif
  if has_key(a:instance, 'client_id')
    call remove(a:instance, 'client_id')
  endif
  let message = 'Process exited with status ' . a:code
  if a:code >= 18 && a:code < 100
    let message = 'Node.js too old.  ' .
          \ (get(a:instance.node, 0, 'node') ==# 'node' ? 'Upgrade' : 'Change g:copilot_node_command') .
          \ ' to ' . a:code . '.x or newer'
  endif
  if !has_key(a:instance, 'serverInfo') && !has_key(a:instance, 'startup_error')
    let a:instance.startup_error = message
  endif
  for id in sort(keys(a:instance.requests), { a, b -> +a > +b })
    call s:RejectRequest(remove(a:instance.requests, id), s:error_exit)
  endfor
  if has_key(a:instance, 'after_initialized')
    let a:instance.AfterInitialized = function('copilot#util#Defer')
    for Fn in remove(a:instance, 'after_initialized')
      call copilot#util#Defer(Fn)
    endfor
  endif
  call copilot#util#Defer({ -> get(s:instances, a:instance.id) is# a:instance ? remove(s:instances, a:instance.id) : {} })
  if a:code == 0
    call copilot#logger#Info(message)
  else
    call copilot#logger#Warn(message)
    if !has_key(a:instance, 'kill')
      call copilot#util#Defer(function('s:Warn'), message)
    endif
  endif
endfunction

function! copilot#client#LspInit(id, initialize_result) abort
  if !has_key(s:instances, a:id)
    return
  endif
  call s:PostInit(a:initialize_result, s:instances[a:id])
endfunction

function! copilot#client#LspExit(id, code, signal) abort
  if !has_key(s:instances, a:id)
    return
  endif
  let instance = remove(s:instances, a:id)
  call s:OnExit(instance, a:code)
endfunction

function! copilot#client#LspResponse(id, opts, ...) abort
  if !has_key(s:instances, a:id)
    return
  endif
  call s:OnResponse(s:instances[a:id], a:opts)
endfunction

function! s:NvimAttach(bufnr) dict abort
  if !bufloaded(a:bufnr)
    return {'uri': '', 'version': 0}
  endif
  call luaeval('pcall(vim.lsp.buf_attach_client, _A[1], _A[2])', [a:bufnr, self.id])
  return luaeval('{uri = vim.uri_from_bufnr(_A), version = vim.lsp.util.buf_versions[_A]}', a:bufnr)
endfunction

function! s:NvimIsAttached(bufnr) dict abort
  return bufloaded(a:bufnr) ? luaeval('vim.lsp.buf_is_attached(_A[1], _A[2])', [a:bufnr, self.id]) : v:false
endfunction

function! s:NvimRequest(method, params, ...) dict abort
  let params = deepcopy(a:params)
  let [bufnr, progress] = s:PreprocessParams(self, params)
  let request = call('s:SetUpRequest', [self, v:null, a:method, params, progress] + a:000)
  call self.AfterInitialized(function('s:NvimDoRequest', [self, request, bufnr]))
  return request
endfunction

function! s:NvimDoRequest(client, request, bufnr) abort
  let request = a:request
  if has_key(a:client, 'client_id') && !has_key(a:client, 'kill')
    let request.id = eval("v:lua.require'_copilot'.lsp_request(a:client.id, a:request.method, a:request.params, a:bufnr)")
  endif
  if request.id isnot# v:null
    let a:client.requests[request.id] = request
  else
    if has_key(a:client, 'client_id')
      call copilot#client#LspExit(a:client.client_id, -1, -1)
    endif
    call copilot#util#Defer(function('s:RejectRequest'), request, s:error_connection_inactive)
  endif
  return request
endfunction

function! s:NvimClose() dict abort
  if !has_key(self, 'client_id')
    return
  endif
  let self.kill = v:true
  return luaeval('vim.lsp.get_client_by_id(_A).stop()', self.client_id)
endfunction

function! s:NvimNotify(method, params) dict abort
  call self.AfterInitialized(function('s:NvimDoNotify', [self.client_id, a:method, a:params]))
endfunction

function! s:NvimDoNotify(client_id, method, params) abort
  return eval("v:lua.require'_copilot'.rpc_notify(a:client_id, a:method, a:params)")
endfunction

function! copilot#client#LspHandle(id, request) abort
  if !has_key(s:instances, a:id)
    return
  endif
  return s:OnMessage(s:instances[a:id], a:request)
endfunction

let s:script_name = 'dist/language-server.js'
function! s:Command() abort
  if !has('nvim-0.7') && v:version < 900
    return [[], [], 'Vim version too old']
  endif
  let script = get(g:, 'copilot_command', '')
  if type(script) == type('')
    let script = [expand(script)]
  endif
  if empty(script) || !filereadable(script[0])
    let script = [s:root . '/' . s:script_name]
    if !filereadable(script[0])
      return [[], [], 'Could not find ' . s:script_name . ' (bad install?)']
    endif
  elseif script[0] !~# '\.js$'
    return [[], script + ['--stdio'], '']
  endif
  let node = get(g:, 'copilot_node_command', '')
  if empty(node)
    let node = ['node']
  elseif type(node) == type('')
    let node = [expand(node)]
  endif
  if !executable(get(node, 0, ''))
    if get(node, 0, '') ==# 'node'
      return [[], [], 'Node.js not found in PATH']
    else
      return [[], [], 'Node.js executable `' . get(node, 0, '') . "' not found"]
    endif
  endif
  return [node, script + ['--stdio'], '']
endfunction

function! s:UrlDecode(str) abort
  return substitute(a:str, '%\(\x\x\)', '\=iconv(nr2char("0x".submatch(1)), "utf-8", "latin1")', 'g')
endfunction

function! copilot#client#EditorInfo() abort
  if !exists('s:editor_version')
    if has('nvim')
      let s:editor_version = matchstr(execute('version'), 'NVIM v\zs[^[:space:]]\+')
    else
      let s:editor_version = (v:version / 100) . '.' . (v:version % 100) . (exists('v:versionlong') ? printf('.%04d', v:versionlong % 10000) : '')
    endif
  endif
  return {'name': has('nvim') ? 'Neovim': 'Vim', 'version': s:editor_version}
endfunction

function! copilot#client#EditorPluginInfo() abort
  return {'name': 'copilot.vim', 'version': s:plugin_version}
endfunction

function! copilot#client#Settings() abort
  let settings = {
        \ 'http': {
        \   'proxy': get(g:, 'copilot_proxy', v:null),
        \   'proxyStrictSSL': get(g:, 'copilot_proxy_strict_ssl', v:null)},
        \ 'github-enterprise': {'uri': get(g:, 'copilot_auth_provider_url', v:null)},
        \ }
  if type(settings.http.proxy) ==# v:t_string && settings.http.proxy =~# '^[^/]\+$'
    let settings.http.proxy = 'http://' . settings.http.proxy
  endif
  if type(get(g:, 'copilot_settings')) == v:t_dict
    call extend(settings, g:copilot_settings)
  endif
  return settings
endfunction

function! s:PostInit(result, instance) abort
  let a:instance.serverInfo = get(a:result, 'serverInfo', {})
  if !has_key(a:instance, 'node_version') && has_key(a:result.serverInfo, 'nodeVersion')
    let a:instance.node_version = a:result.serverInfo.nodeVersion
  endif
  let a:instance.AfterInitialized = function('copilot#util#Defer')
  for Fn in remove(a:instance, 'after_initialized')
    call copilot#util#Defer(Fn)
  endfor
endfunction

function! s:InitializeResult(result, instance) abort
  call s:Send(a:instance, {'method': 'initialized', 'params': {}})
  call s:PostInit(a:result, a:instance)
endfunction

function! s:InitializeError(error, instance) abort
  if !has_key(a:instance, 'startup_error')
    let a:instance.startup_error = 'Unexpected error E' . a:error.code . ' initializing language server: ' . a:error.message
    call a:instance.Close()
  endif
endfunction

function! s:StartupError() dict abort
  while (has_key(self, 'job') || has_key(self, 'client_id')) && !has_key(self, 'startup_error') && !has_key(self, 'serverInfo')
    sleep 10m
  endwhile
  if has_key(self, 'serverInfo')
    return ''
  else
    return get(self, 'startup_error', 'Something unexpected went wrong spawning the language server')
  endif
endfunction

function! s:StatusNotification(params, instance) abort
  let a:instance.status = a:params
endfunction

function! s:Nop(...) abort
  return v:null
endfunction

function! s:False(...) abort
  return v:false
endfunction

function! s:Progress(params, instance) abort
  if has_key(a:instance.progress, a:params.token)
    call a:instance.progress[a:params.token](a:params.value)
  endif
endfunction

let s:notifications = {
      \ '$/progress': function('s:Progress'),
      \ 'featureFlagsNotification': function('s:Nop'),
      \ 'statusNotification': function('s:StatusNotification'),
      \ 'window/logMessage': function('copilot#handlers#window_logMessage'),
      \ }

let s:vim_handlers = {
      \ 'window/showMessageRequest': function('copilot#handlers#window_showMessageRequest'),
      \ 'window/showDocument': function('copilot#handlers#window_showDocument'),
      \ }

let s:vim_capabilities = {
      \ 'workspace': {'workspaceFolders': v:true},
      \ 'window': {'showDocument': {'support': v:true}},
      \ }

function! copilot#client#New(...) abort
  let opts = a:0 ? a:1 : {}
  let instance = {'requests': {},
        \ 'progress': {},
        \ 'workspaceFolders': {},
        \ 'after_initialized': [],
        \ 'status': {'status': 'Starting', 'message': ''},
        \ 'AfterInitialized': function('s:AfterInitialized'),
        \ 'Close': function('s:Nop'),
        \ 'Notify': function('s:False'),
        \ 'Request': function('s:VimRequest'),
        \ 'Attach': function('s:Nop'),
        \ 'IsAttached': function('s:False'),
        \ 'Call': function('s:Call'),
        \ 'Cancel': function('s:Cancel'),
        \ 'StartupError': function('s:StartupError'),
        \ }
  let instance.methods = copy(s:notifications)
  let [node, argv, command_error] = s:Command()
  if !empty(command_error)
    let instance.id = -1
    let instance.startup_error = command_error
    call copilot#logger#Error(command_error)
    return instance
  endif
  let instance.node = node
  let command = node + argv
  let opts = {}
  let opts.initializationOptions = {
        \ 'editorInfo': copilot#client#EditorInfo(),
        \ 'editorPluginInfo': copilot#client#EditorPluginInfo(),
        \ }
  let opts.workspaceFolders = []
  let settings = extend(copilot#client#Settings(), get(opts, 'editorConfiguration', {}))
  if type(get(g:, 'copilot_workspace_folders')) == v:t_list
    for folder in g:copilot_workspace_folders
      if type(folder) == v:t_string && !empty(folder) && folder !~# '\*\*\|^/$'
        for path in glob(folder . '/', 0, 1)
          let uri = s:UriFromPath(substitute(path, '[\/]*$', '', ''))
          call add(opts.workspaceFolders, {'uri': uri, 'name': fnamemodify(uri, ':t')})
        endfor
      elseif type(folder) == v:t_dict && has_key(v:t_dict, 'uri') && !empty(folder.uri) && has_key(folder, 'name')
        call add(opts.workspaceFolders, folder)
      endif
    endfor
  endif
  for folder in opts.workspaceFolders
    let instance.workspaceFolders[folder.uri] = v:true
  endfor
  if has('nvim')
    call extend(instance, {
          \ 'Close': function('s:NvimClose'),
          \ 'Notify': function('s:NvimNotify'),
          \ 'Request': function('s:NvimRequest'),
          \ 'Attach': function('s:NvimAttach'),
          \ 'IsAttached': function('s:NvimIsAttached'),
          \ })
    let instance.client_id = eval("v:lua.require'_copilot'.lsp_start_client(command, keys(instance.methods), opts, settings)")
    let instance.id = instance.client_id
  else
    call extend(instance, {
          \ 'Close': function('s:VimClose'),
          \ 'Notify': function('s:VimNotify'),
          \ 'Attach': function('s:VimAttach'),
          \ 'IsAttached': function('s:VimIsAttached'),
          \ })
    let state = {'headers': {}, 'mode': 'headers', 'buffer': ''}
    let instance.open_buffers = {}
    let instance.methods = extend(s:vim_handlers, instance.methods)
    let instance.job = job_start(command, {
          \ 'cwd': copilot#job#Cwd(),
          \ 'noblock': 1,
          \ 'stoponexit': '',
          \ 'in_mode': 'lsp',
          \ 'out_mode': 'lsp',
          \ 'out_cb': { j, d -> copilot#util#Defer(function('s:OnMessage'), instance, d) },
          \ 'err_cb': function('s:OnErr', [instance]),
          \ 'exit_cb': { j, d -> copilot#util#Defer(function('s:OnExit'), instance, d) },
          \ })
    let instance.id = job_info(instance.job).process
    let opts.capabilities = s:vim_capabilities
    let opts.processId = getpid()
    let request = instance.Request('initialize', opts, function('s:InitializeResult'), function('s:InitializeError'), instance)
    call call(remove(instance.after_initialized, 0), [])
    call instance.Notify('workspace/didChangeConfiguration', {'settings': settings})
  endif
  let s:instances[instance.id] = instance
  return instance
endfunction

function! copilot#client#Cancel(request) abort
  if type(a:request) == type({}) && has_key(a:request, 'Cancel')
    call a:request.Cancel()
  endif
endfunction

function! s:Callback(request, type, callback, timer) abort
  call remove(a:request.waiting, a:timer)
  if has_key(a:request, a:type)
    call a:callback(a:request[a:type])
  endif
endfunction

function! copilot#client#Result(request, callback) abort
  if has_key(a:request, 'resolve')
    call add(a:request.resolve, a:callback)
  elseif has_key(a:request, 'result')
    let a:request.waiting[timer_start(0, function('s:Callback', [a:request, 'result', a:callback]))] = 1
  endif
endfunction

function! copilot#client#Error(request, callback) abort
  if has_key(a:request, 'reject')
    call add(a:request.reject, a:callback)
  elseif has_key(a:request, 'error')
    let a:request.waiting[timer_start(0, function('s:Callback', [a:request, 'error', a:callback]))] = 1
  endif
endfunction

function! s:CloseBuffer(bufnr) abort
  for instance in values(s:instances)
    try
      if has_key(instance, 'job') && has_key(instance.open_buffers, a:bufnr)
        let buffer = remove(instance.open_buffers, a:bufnr)
        call instance.Notify('textDocument/didClose', {'textDocument': {'uri': buffer.uri}})
      endif
    catch
      call copilot#logger#Exception()
    endtry
  endfor
endfunction

augroup copilot_close
  autocmd!
  if !has('nvim')
    autocmd BufUnload * call s:CloseBuffer(+expand('<abuf>'))
  endif
augroup END
