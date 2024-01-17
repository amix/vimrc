if exists('g:autoloaded_copilot')
  finish
endif
let g:autoloaded_copilot = 1

scriptencoding utf-8

let s:has_nvim_ghost_text = has('nvim-0.6') && exists('*nvim_buf_get_mark')
let s:vim_minimum_version = '9.0.0185'
let s:has_vim_ghost_text = has('patch-' . s:vim_minimum_version) && has('textprop')
let s:has_ghost_text = s:has_nvim_ghost_text || s:has_vim_ghost_text

let s:hlgroup = 'CopilotSuggestion'
let s:annot_hlgroup = 'CopilotAnnotation'

if s:has_vim_ghost_text && empty(prop_type_get(s:hlgroup))
  call prop_type_add(s:hlgroup, {'highlight': s:hlgroup})
endif
if s:has_vim_ghost_text && empty(prop_type_get(s:annot_hlgroup))
  call prop_type_add(s:annot_hlgroup, {'highlight': s:annot_hlgroup})
endif

function! s:Echo(msg) abort
  if has('nvim') && &cmdheight == 0
    call v:lua.vim.notify(a:msg, v:null, {'title': 'GitHub Copilot'})
  else
    echo a:msg
  endif
endfunction

function! s:EditorConfiguration() abort
  let filetypes = copy(s:filetype_defaults)
  if type(get(g:, 'copilot_filetypes')) == v:t_dict
    call extend(filetypes, g:copilot_filetypes)
  endif
  return {
        \ 'enableAutoCompletions': empty(get(g:, 'copilot_enabled', 1)) ? v:false : v:true,
        \ 'disabledLanguages': map(sort(keys(filter(filetypes, { k, v -> empty(v) }))), { _, v -> {'languageId': v}}),
        \ }
endfunction

function! s:StatusNotification(params, ...) abort
  let status = get(a:params, 'status', '')
  if status ==? 'error'
    let s:agent_error = a:params.message
  else
    unlet! s:agent_error
  endif
endfunction

function! copilot#Init(...) abort
  call timer_start(0, { _ -> s:Start() })
endfunction

function! s:Running() abort
  return exists('s:agent.job') || exists('s:agent.client_id')
endfunction

function! s:Start() abort
  if s:Running()
    return
  endif
  let s:agent = copilot#agent#New({'methods': {
        \ 'statusNotification': function('s:StatusNotification'),
        \ 'PanelSolution': function('copilot#panel#Solution'),
        \ 'PanelSolutionsDone': function('copilot#panel#SolutionsDone'),
        \ 'copilot/openURL': function('s:OpenURL'),
        \ },
        \ 'editorConfiguration' : s:EditorConfiguration()})
endfunction

function! s:Stop() abort
  if exists('s:agent')
    let agent = remove(s:, 'agent')
    call agent.Close()
  endif
endfunction

function! copilot#Agent() abort
  call s:Start()
  return s:agent
endfunction

function! copilot#RunningAgent() abort
  if s:Running()
    return s:agent
  else
    return v:null
  endif
endfunction

function! s:NodeVersionWarning() abort
  if exists('s:agent.node_version') && s:agent.node_version =~# '^16\.'
    echohl WarningMsg
    echo "Warning: Node.js 16 is approaching end of life and support will be dropped in a future release of copilot.vim."
    echohl NONE
  elseif exists('s:agent.node_version_warning')
    echohl WarningMsg
    echo 'Warning:' s:agent.node_version_warning
    echohl NONE
  endif
endfunction

function! copilot#Request(method, params, ...) abort
  let agent = copilot#Agent()
  return call(agent.Request, [a:method, a:params] + a:000)
endfunction

function! copilot#Call(method, params, ...) abort
  let agent = copilot#Agent()
  return call(agent.Call, [a:method, a:params] + a:000)
endfunction

function! copilot#Notify(method, params, ...) abort
  let agent = copilot#Agent()
  return call(agent.Notify, [a:method, a:params] + a:000)
endfunction

function! copilot#NvimNs() abort
  return nvim_create_namespace('github-copilot')
endfunction

function! copilot#Clear() abort
  if exists('g:_copilot_timer')
    call timer_stop(remove(g:, '_copilot_timer'))
  endif
  if exists('b:_copilot')
    call copilot#agent#Cancel(get(b:_copilot, 'first', {}))
    call copilot#agent#Cancel(get(b:_copilot, 'cycling', {}))
  endif
  call s:UpdatePreview()
  unlet! b:_copilot
  return ''
endfunction

function! s:Reject(bufnr) abort
  try
    let dict = getbufvar(a:bufnr, '_copilot')
    if type(dict) == v:t_dict && !empty(get(dict, 'shown_choices', {}))
      call copilot#Request('notifyRejected', {'uuids': keys(dict.shown_choices)})
      let dict.shown_choices = {}
    endif
  catch
    call copilot#logger#Exception()
  endtry
endfunction

function! copilot#Dismiss() abort
  call s:Reject('%')
  call copilot#Clear()
  call s:UpdatePreview()
  return ''
endfunction

let s:filetype_defaults = {
      \ 'yaml': 0,
      \ 'markdown': 0,
      \ 'help': 0,
      \ 'gitcommit': 0,
      \ 'gitrebase': 0,
      \ 'hgcommit': 0,
      \ 'svn': 0,
      \ 'cvs': 0,
      \ '.': 0}

function! s:BufferDisabled() abort
  if &buftype =~# '^\%(help\|prompt\|quickfix\|terminal\)$'
    return 5
  endif
  if exists('b:copilot_disabled')
    return empty(b:copilot_disabled) ? 0 : 3
  endif
  if exists('b:copilot_enabled')
    return empty(b:copilot_enabled) ? 4 : 0
  endif
  let short = empty(&l:filetype) ? '.' : split(&l:filetype, '\.', 1)[0]
  let config = {}
  if type(get(g:, 'copilot_filetypes')) == v:t_dict
    let config = g:copilot_filetypes
  endif
  if has_key(config, &l:filetype)
    return empty(config[&l:filetype])
  elseif has_key(config, short)
    return empty(config[short])
  elseif has_key(config, '*')
    return empty(config['*'])
  else
    return get(s:filetype_defaults, short, 1) == 0 ? 2 : 0
  endif
endfunction

function! copilot#Enabled() abort
  return get(g:, 'copilot_enabled', 1)
        \ && empty(s:BufferDisabled())
        \ && empty(copilot#Agent().StartupError())
endfunction

function! copilot#Complete(...) abort
  if exists('g:_copilot_timer')
    call timer_stop(remove(g:, '_copilot_timer'))
  endif
  let params = copilot#doc#Params()
  if !exists('b:_copilot.params') || b:_copilot.params !=# params
    let b:_copilot = {'params': params, 'first':
          \ copilot#Request('getCompletions', params)}
    let g:_copilot_last = b:_copilot
  endif
  let completion = b:_copilot.first
  if !a:0
    return completion.Await()
  else
    call copilot#agent#Result(completion, a:1)
    if a:0 > 1
      call copilot#agent#Error(completion, a:2)
    endif
  endif
endfunction

function! s:HideDuringCompletion() abort
  return get(g:, 'copilot_hide_during_completion', 1)
endfunction

function! s:SuggestionTextWithAdjustments() abort
  try
    if mode() !~# '^[iR]' || (s:HideDuringCompletion() && pumvisible()) || !exists('b:_copilot.suggestions')
      return ['', 0, 0, '']
    endif
    let choice = get(b:_copilot.suggestions, b:_copilot.choice, {})
    if !has_key(choice, 'range') || choice.range.start.line != line('.') - 1 || type(choice.text) !=# v:t_string
      return ['', 0, 0, '']
    endif
    let line = getline('.')
    let offset = col('.') - 1
    let choice_text = strpart(line, 0, copilot#doc#UTF16ToByteIdx(line, choice.range.start.character)) . choice.text
    let typed = strpart(line, 0, offset)
    let end_offset = copilot#doc#UTF16ToByteIdx(line, choice.range.end.character)
    if end_offset < 0
      let end_offset = len(line)
    endif
    let delete = strpart(line, offset, end_offset - offset)
    let uuid = get(choice, 'uuid', '')
    if typed =~# '^\s*$'
      let leading = matchstr(choice_text, '^\s\+')
      let unindented = strpart(choice_text, len(leading))
      if strpart(typed, 0, len(leading)) == leading && unindented !=# delete
        return [unindented, len(typed) - len(leading), strchars(delete), uuid]
      endif
    elseif typed ==# strpart(choice_text, 0, offset)
      return [strpart(choice_text, offset), 0, strchars(delete), uuid]
    endif
  catch
    call copilot#logger#Exception()
  endtry
  return ['', 0, 0, '']
endfunction


function! s:Advance(count, context, ...) abort
  if a:context isnot# get(b:, '_copilot', {})
    return
  endif
  let a:context.choice += a:count
  if a:context.choice < 0
    let a:context.choice += len(a:context.suggestions)
  endif
  let a:context.choice %= len(a:context.suggestions)
  call s:UpdatePreview()
endfunction

function! s:GetSuggestionsCyclingCallback(context, result) abort
  let callbacks = remove(a:context, 'cycling_callbacks')
  let seen = {}
  for suggestion in a:context.suggestions
    let seen[suggestion.text] = 1
  endfor
  for suggestion in get(a:result, 'completions', [])
    if !has_key(seen, suggestion.text)
      call add(a:context.suggestions, suggestion)
      let seen[suggestion.text] = 1
    endif
  endfor
  for Callback in callbacks
    call Callback(a:context)
  endfor
endfunction

function! s:GetSuggestionsCycling(callback) abort
  if exists('b:_copilot.cycling_callbacks')
    call add(b:_copilot.cycling_callbacks, a:callback)
  elseif exists('b:_copilot.cycling')
    call a:callback(b:_copilot)
  elseif exists('b:_copilot.suggestions')
    let b:_copilot.cycling_callbacks = [a:callback]
    let b:_copilot.cycling = copilot#Request('getCompletionsCycling',
          \ b:_copilot.first.params,
          \ function('s:GetSuggestionsCyclingCallback', [b:_copilot]),
          \ function('s:GetSuggestionsCyclingCallback', [b:_copilot]),
          \ )
    call s:UpdatePreview()
  endif
  return ''
endfunction

function! copilot#Next() abort
  return s:GetSuggestionsCycling(function('s:Advance', [1]))
endfunction

function! copilot#Previous() abort
  return s:GetSuggestionsCycling(function('s:Advance', [-1]))
endfunction

function! copilot#GetDisplayedSuggestion() abort
  let [text, outdent, delete, uuid] = s:SuggestionTextWithAdjustments()

  return {
        \ 'uuid': uuid,
        \ 'text': text,
        \ 'outdentSize': outdent,
        \ 'deleteSize': delete}
endfunction

function! s:ClearPreview() abort
  if s:has_nvim_ghost_text
    call nvim_buf_del_extmark(0, copilot#NvimNs(), 1)
  elseif s:has_vim_ghost_text
    call prop_remove({'type': s:hlgroup, 'all': v:true})
    call prop_remove({'type': s:annot_hlgroup, 'all': v:true})
  endif
endfunction

function! s:UpdatePreview() abort
  try
    let [text, outdent, delete, uuid] = s:SuggestionTextWithAdjustments()
    let text = split(text, "\n", 1)
    if empty(text[-1])
      call remove(text, -1)
    endif
    if empty(text) || !s:has_ghost_text
      return s:ClearPreview()
    endif
    if exists('b:_copilot.cycling_callbacks')
      let annot = '(1/…)'
    elseif exists('b:_copilot.cycling')
      let annot = '(' . (b:_copilot.choice + 1) . '/' . len(b:_copilot.suggestions) . ')'
    else
      let annot = ''
    endif
    call s:ClearPreview()
    if s:has_nvim_ghost_text
      let data = {'id': 1}
      let data.virt_text_win_col = virtcol('.') - 1
      let append = strpart(getline('.'), col('.') - 1 + delete)
      let data.virt_text = [[text[0] . append . repeat(' ', delete - len(text[0])), s:hlgroup]]
      if len(text) > 1
        let data.virt_lines = map(text[1:-1], { _, l -> [[l, s:hlgroup]] })
        if !empty(annot)
          let data.virt_lines[-1] += [[' '], [annot, s:annot_hlgroup]]
        endif
      elseif len(annot)
        let data.virt_text += [[' '], [annot, s:annot_hlgroup]]
      endif
      let data.hl_mode = 'combine'
      call nvim_buf_set_extmark(0, copilot#NvimNs(), line('.')-1, col('.')-1, data)
    else
      call prop_add(line('.'), col('.'), {'type': s:hlgroup, 'text': text[0]})
      for line in text[1:]
        call prop_add(line('.'), 0, {'type': s:hlgroup, 'text_align': 'below', 'text': line})
      endfor
      if !empty(annot)
        call prop_add(line('.'), col('$'), {'type': s:annot_hlgroup, 'text': ' ' . annot})
      endif
    endif
    if !has_key(b:_copilot.shown_choices, uuid)
      let b:_copilot.shown_choices[uuid] = v:true
      call copilot#Request('notifyShown', {'uuid': uuid})
    endif
  catch
    return copilot#logger#Exception()
  endtry
endfunction

function! s:HandleTriggerResult(result) abort
  if !exists('b:_copilot')
    return
  endif
  let b:_copilot.suggestions = get(a:result, 'completions', [])
  let b:_copilot.choice = 0
  let b:_copilot.shown_choices = {}
  call s:UpdatePreview()
endfunction

function! copilot#Suggest() abort
  try
    call copilot#Complete(function('s:HandleTriggerResult'), function('s:HandleTriggerResult'))
  catch
    call copilot#logger#Exception()
  endtry
  return ''
endfunction

function! s:Trigger(bufnr, timer) abort
  let timer = get(g:, '_copilot_timer', -1)
  unlet! g:_copilot_timer
  if a:bufnr !=# bufnr('') || a:timer isnot# timer || mode() !=# 'i'
    return
  endif
  return copilot#Suggest()
endfunction

function! copilot#IsMapped() abort
  return get(g:, 'copilot_assume_mapped') ||
        \ hasmapto('copilot#Accept(', 'i')
endfunction

function! copilot#Schedule(...) abort
  if !s:has_ghost_text || !copilot#Enabled() || !copilot#IsMapped()
    call copilot#Clear()
    return
  endif
  call s:UpdatePreview()
  let delay = a:0 ? a:1 : get(g:, 'copilot_idle_delay', 15)
  let g:_copilot_timer = timer_start(delay, function('s:Trigger', [bufnr('')]))
endfunction

function! copilot#OnInsertLeave() abort
  return copilot#Clear()
endfunction

function! copilot#OnInsertEnter() abort
  return copilot#Schedule()
endfunction

function! copilot#OnCompleteChanged() abort
  if s:HideDuringCompletion()
    return copilot#Clear()
  else
    return copilot#Schedule()
  endif
endfunction

function! copilot#OnCursorMovedI() abort
  return copilot#Schedule()
endfunction

function! copilot#OnBufUnload() abort
  call s:Reject(+expand('<abuf>'))
endfunction

function! copilot#OnVimLeavePre() abort
endfunction

function! copilot#TextQueuedForInsertion() abort
  try
    return remove(s:, 'suggestion_text')
  catch
    return ''
  endtry
endfunction

function! copilot#Accept(...) abort
  let s = copilot#GetDisplayedSuggestion()
  if !empty(s.text)
    unlet! b:_copilot
    let text = ''
    if a:0 > 1
      let text = substitute(matchstr(s.text, "\n*" . '\%(' . a:2 .'\)'), "\n*$", '', '')
    endif
    if empty(text)
      let text = s.text
    endif
    call copilot#Request('notifyAccepted', {'uuid': s.uuid, 'acceptedLength': copilot#doc#UTF16Width(text)})
    call s:ClearPreview()
    let s:suggestion_text = text
    return repeat("\<Left>\<Del>", s.outdentSize) . repeat("\<Del>", s.deleteSize) .
            \ "\<C-R>\<C-O>=copilot#TextQueuedForInsertion()\<CR>" . (a:0 > 1 ? '' : "\<End>")
  endif
  let default = get(g:, 'copilot_tab_fallback', pumvisible() ? "\<C-N>" : "\t")
  if !a:0
    return default
  elseif type(a:1) == v:t_string
    return a:1
  elseif type(a:1) == v:t_func
    try
      return call(a:1, [])
    catch
      return default
    endtry
  else
    return default
  endif
endfunction

function! copilot#AcceptWord(...) abort
  return copilot#Accept(a:0 ? a:1 : '', '\%(\k\@!.\)*\k*')
endfunction

function! copilot#AcceptLine(...) abort
  return copilot#Accept(a:0 ? a:1 : "\r", "[^\n]\\+")
endfunction

function! s:BrowserCallback(into, code) abort
  let a:into.code = a:code
endfunction

function! copilot#Browser() abort
  if type(get(g:, 'copilot_browser')) == v:t_list
    let cmd = copy(g:copilot_browser)
  elseif type(get(g:, 'open_command')) == v:t_list
    let cmd = copy(g:open_command)
  elseif has('win32')
    let cmd = ['rundll32', 'url.dll,FileProtocolHandler']
  elseif has('mac')
    let cmd = ['open']
  elseif executable('wslview')
    return ['wslview']
  elseif executable('xdg-open')
    return ['xdg-open']
  else
    return []
  endif
  if executable(get(cmd, 0, ''))
    return cmd
  else
    return []
  endif
endfunction

function! s:OpenURL(params) abort
  echo a:params.target
  let browser = copilot#Browser()
  if empty(browser)
    return v:false
  endif
  let status = {}
  call copilot#job#Stream(browser + [a:params.target], v:null, v:null, function('s:BrowserCallback', [status]))
  let time = reltime()
  while empty(status) && reltimefloat(reltime(time)) < 1
    sleep 10m
  endwhile
  return get(status, 'code') ? v:false : v:true
endfunction

let s:commands = {}

function! s:EnabledStatusMessage() abort
  let buf_disabled = s:BufferDisabled()
  if !s:has_ghost_text
    if has('nvim')
      return "Neovim 0.6 required to support ghost text"
    else
      return "Vim " . s:vim_minimum_version . " required to support ghost text"
    endif
  elseif !copilot#IsMapped()
    return '<Tab> map has been disabled or is claimed by another plugin'
  elseif !get(g:, 'copilot_enabled', 1)
    return 'Disabled globally by :Copilot disable'
  elseif buf_disabled is# 5
    return 'Disabled for current buffer by buftype=' . &buftype
  elseif buf_disabled is# 4
    return 'Disabled for current buffer by b:copilot_enabled'
  elseif buf_disabled is# 3
    return 'Disabled for current buffer by b:copilot_disabled'
  elseif buf_disabled is# 2
    return 'Disabled for filetype=' . &filetype . ' by internal default'
  elseif buf_disabled
    return 'Disabled for filetype=' . &filetype . ' by g:copilot_filetypes'
  elseif !copilot#Enabled()
    return 'BUG: Something is wrong with enabling/disabling'
  else
    return ''
  endif
endfunction

function! s:VerifySetup() abort
  let error = copilot#Agent().StartupError()
  if !empty(error)
    echo 'Copilot: ' . error
    return
  endif

  let status = copilot#Call('checkStatus', {})

  if !has_key(status, 'user')
    echo 'Copilot: Not authenticated. Invoke :Copilot setup'
    return
  endif

  if status.status ==# 'NoTelemetryConsent'
    echo 'Copilot: Telemetry terms not accepted. Invoke :Copilot setup'
    return
  endif
  return 1
endfunction

function! s:commands.status(opts) abort
  if !s:VerifySetup()
    return
  endif

  let status = s:EnabledStatusMessage()
  if !empty(status)
    echo 'Copilot: ' . status
    return
  endif

  let startup_error = copilot#Agent().StartupError()
  if !empty(startup_error)
      echo 'Copilot: ' . startup_error
      return
  endif

  if exists('s:agent_error')
    echo 'Copilot: ' . s:agent_error
    return
  endif

  let status = copilot#Call('checkStatus', {})
  if status.status ==# 'NotAuthorized'
    echo 'Copilot: Not authorized'
    return
  endif

  echo 'Copilot: Enabled and online'
  call s:NodeVersionWarning()
endfunction

function! s:commands.signout(opts) abort
  let status = copilot#Call('checkStatus', {'options': {'localChecksOnly': v:true}})
  if has_key(status, 'user')
    echo 'Copilot: Signed out as GitHub user ' . status.user
  else
    echo 'Copilot: Not signed in'
  endif
  call copilot#Call('signOut', {})
endfunction

function! s:commands.setup(opts) abort
  let startup_error = copilot#Agent().StartupError()
  if !empty(startup_error)
      echo 'Copilot: ' . startup_error
      return
  endif

  let browser = copilot#Browser()

  let status = copilot#Call('checkStatus', {})
  if has_key(status, 'user')
    let data = {}
  else
    let data = copilot#Call('signInInitiate', {})
  endif

  if has_key(data, 'verificationUri')
    let uri = data.verificationUri
    if has('clipboard')
      let @+ = data.userCode
      let @* = data.userCode
    endif
    call s:Echo("First copy your one-time code: " . data.userCode)
    try
      if len(&mouse)
        let mouse = &mouse
        set mouse=
      endif
      if get(a:opts, 'bang')
        call s:Echo("In your browser, visit " . uri)
      elseif len(browser)
        call s:Echo("Press ENTER to open GitHub in your browser")
        let c = getchar()
        while c isnot# 13 && c isnot# 10 && c isnot# 0
          let c = getchar()
        endwhile
        let status = {}
        call copilot#job#Stream(browser + [uri], v:null, v:null, function('s:BrowserCallback', [status]))
        let time = reltime()
        while empty(status) && reltimefloat(reltime(time)) < 5
          sleep 10m
        endwhile
        if get(status, 'code', browser[0] !=# 'xdg-open') != 0
          call s:Echo("Failed to open browser.  Visit " . uri)
        else
          call s:Echo("Opened " . uri)
        endif
      else
        call s:Echo("Could not find browser.  Visit " . uri)
      endif
      call s:Echo("Waiting (could take up to 5 seconds)")
      let request = copilot#Request('signInConfirm', {'userCode': data.userCode}).Wait()
    finally
      if exists('mouse')
        let &mouse = mouse
      endif
    endtry
    if request.status ==# 'error'
      return 'echoerr ' . string('Copilot: Authentication failure: ' . request.error.message)
    else
      let status = request.result
    endif
  endif

  let user = get(status, 'user', '<unknown>')

  echo 'Copilot: Authenticated as GitHub user ' . user
endfunction

let s:commands.auth = s:commands.setup

function! s:commands.help(opts) abort
  return a:opts.mods . ' help ' . (len(a:opts.arg) ? ':Copilot_' . a:opts.arg : 'copilot')
endfunction

function! s:commands.version(opts) abort
  let info = copilot#agent#EditorInfo()
  echo 'copilot.vim ' .info.editorPluginInfo.version
  echo info.editorInfo.name . ' ' . info.editorInfo.version
  if exists('s:agent.node_version')
    echo 'dist/agent.js ' . s:agent.Call('getVersion', {}).version
    echo 'Node.js ' . s:agent.node_version
    call s:NodeVersionWarning()
  else
    echo 'dist/agent.js not running'
  endif
endfunction

function! s:UpdateEditorConfiguration() abort
  try
    if s:Running()
      call copilot#Notify('notifyChangeConfiguration', {'settings': s:EditorConfiguration()})
    endif
  catch
    call copilot#logger#Exception()
  endtry
endfunction

let s:feedback_url = 'https://github.com/orgs/community/discussions/categories/copilot'
function! s:commands.feedback(opts) abort
  echo s:feedback_url
  let browser = copilot#Browser()
  if len(browser)
    call copilot#job#Stream(browser + [s:feedback_url], v:null, v:null, v:null)
  endif
endfunction

function! s:commands.restart(opts) abort
  call s:Stop()
  let err = copilot#Agent().StartupError()
  if !empty(err)
    return 'echoerr ' . string('Copilot: ' . err)
  endif
  echo 'Copilot: Restarting agent.'
endfunction

function! s:commands.disable(opts) abort
  let g:copilot_enabled = 0
  call s:UpdateEditorConfiguration()
endfunction

function! s:commands.enable(opts) abort
  let g:copilot_enabled = 1
  call s:UpdateEditorConfiguration()
endfunction

function! s:commands.panel(opts) abort
  if s:VerifySetup()
    return copilot#panel#Open(a:opts)
  endif
endfunction

function! copilot#CommandComplete(arg, lead, pos) abort
  let args = matchstr(strpart(a:lead, 0, a:pos), 'C\%[opilot][! ] *\zs.*')
  if args !~# ' '
    return sort(filter(map(keys(s:commands), { k, v -> tr(v, '_', '-') }),
          \ { k, v -> strpart(v, 0, len(a:arg)) ==# a:arg }))
  else
    return []
  endif
endfunction

function! copilot#Command(line1, line2, range, bang, mods, arg) abort
  let cmd = matchstr(a:arg, '^\%(\\.\|\S\)\+')
  let arg = matchstr(a:arg, '\s\zs\S.*')
  if cmd ==# 'log'
    return a:mods . ' split +$ ' . fnameescape(copilot#logger#File())
  endif
  if !empty(cmd) && !has_key(s:commands, tr(cmd, '-', '_'))
    return 'echoerr ' . string('Copilot: unknown command ' . string(cmd))
  endif
  try
    let err = copilot#Agent().StartupError()
    if !empty(err)
      return 'echo ' . string('Copilot: ' . err)
    endif
    try
      let opts = copilot#Call('checkStatus', {'options': {'localChecksOnly': v:true}})
    catch
      call copilot#logger#Exception()
      let opts = {'status': 'VimException'}
    endtry
    if empty(cmd)
      if opts.status ==# 'VimException'
        return a:mods . ' split +$ ' . fnameescape(copilot#logger#File())
      elseif opts.status !=# 'OK' && opts.status !=# 'MaybeOK'
        let cmd = 'setup'
      else
        let cmd = 'panel'
      endif
    endif
    call extend(opts, {'line1': a:line1, 'line2': a:line2, 'range': a:range, 'bang': a:bang, 'mods': a:mods, 'arg': arg})
    let retval = s:commands[tr(cmd, '-', '_')](opts)
    if type(retval) == v:t_string
      return retval
    else
      return ''
    endif
  catch /^Copilot:/
    return 'echoerr ' . string(v:exception)
  endtry
endfunction
