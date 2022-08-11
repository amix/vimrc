scriptencoding utf-8
let g:coc#_context = {'start': 0, 'preselect': -1,'candidates': []}
let g:coc_user_config = get(g:, 'coc_user_config', {})
let g:coc_global_extensions = get(g:, 'coc_global_extensions', [])
let g:coc_selected_text = ''
let g:coc_vim_commands = []
let s:watched_keys = []
let s:is_vim = !has('nvim')
let s:error_sign = get(g:, 'coc_status_error_sign', has('mac') ? '❌ ' : 'E')
let s:warning_sign = get(g:, 'coc_status_warning_sign', has('mac') ? '⚠️ ' : 'W')
let s:select_api = exists('*nvim_select_popupmenu_item')
let s:callbacks = {}
let s:hide_pum = has('nvim-0.6.1') || has('patch-8.2.3389')

function! coc#expandable() abort
  return coc#rpc#request('snippetCheck', [1, 0])
endfunction

function! coc#jumpable() abort
  return coc#rpc#request('snippetCheck', [0, 1])
endfunction

function! coc#expandableOrJumpable() abort
  return coc#rpc#request('snippetCheck', [1, 1])
endfunction

" add vim command to CocCommand list
function! coc#add_command(id, cmd, ...)
  let config = {'id':a:id, 'cmd':a:cmd, 'title': get(a:,1,'')}
  call add(g:coc_vim_commands, config)
  if !coc#rpc#ready() | return | endif
  call coc#rpc#notify('addCommand', [config])
endfunction

function! coc#on_enter()
  call coc#rpc#notify('CocAutocmd', ['Enter', bufnr('%')])
  return ''
endfunction

function! coc#_insert_key(method, key, ...) abort
  let prefix = ''
  if get(a:, 1, 1)
    if pumvisible()
      if s:hide_pum
        let prefix = "\<C-x>\<C-z>"
      else
        let g:coc_disable_space_report = 1
        let prefix = "\<space>\<bs>"
      endif
    endif
  endif
  return prefix."\<c-r>=coc#rpc#".a:method."('doKeymap', ['".a:key."'])\<CR>"
endfunction

function! coc#_complete() abort
  let items = get(g:coc#_context, 'candidates', [])
  let preselect = get(g:coc#_context, 'preselect', -1)
  let startcol = g:coc#_context.start + 1
  if s:select_api && len(items) && preselect != -1
    noa call complete(startcol, items)
    call nvim_select_popupmenu_item(preselect, v:false, v:false, {})
    " use <cmd> specific key to preselect item at once
    call feedkeys("\<Cmd>\<CR>" , 'i')
  else
    if pumvisible()
      let g:coc_disable_complete_done = 1
    endif
    call complete(startcol, items)
  endif
  return ''
endfunction

function! coc#_do_complete(start, items, preselect, changedtick)
  if b:changedtick != a:changedtick
    return
  endif
  let g:coc#_context = {
        \ 'start': a:start,
        \ 'candidates': a:items,
        \ 'preselect': a:preselect
        \}
  if mode() =~# 'i'
    call coc#_complete()
  endif
endfunction

function! coc#_cancel(...)
  call coc#pum#close()
endfunction

" used for statusline
function! coc#status()
  let info = get(b:, 'coc_diagnostic_info', {})
  let msgs = []
  if !empty(info) && get(info, 'error', 0)
    call add(msgs, s:error_sign . info['error'])
  endif
  if !empty(info) && get(info, 'warning', 0)
    call add(msgs, s:warning_sign . info['warning'])
  endif
  return coc#compat#trim(join(msgs, ' ') . ' ' . get(g:, 'coc_status', ''))
endfunction

function! coc#config(section, value)
  let g:coc_user_config[a:section] = a:value
  call coc#rpc#notify('updateConfig', [a:section, a:value])
endfunction

function! coc#add_extension(...)
  if a:0 == 0 | return | endif
  call extend(g:coc_global_extensions, a:000)
endfunction

function! coc#_watch(key)
  if s:is_vim | return | endif
  if index(s:watched_keys, a:key) == -1
    call add(s:watched_keys, a:key)
    call dictwatcheradd(g:, a:key, function('s:GlobalChange'))
  endif
endfunction

function! coc#_unwatch(key)
  if s:is_vim | return | endif
  let idx = index(s:watched_keys, a:key)
  if idx != -1
    call remove(s:watched_keys, idx)
    call dictwatcherdel(g:, a:key, function('s:GlobalChange'))
  endif
endfunction

function! s:GlobalChange(dict, key, val)
  call coc#rpc#notify('GlobalChange', [a:key, get(a:val, 'old', v:null), get(a:val, 'new', v:null)])
endfunction

function! coc#on_notify(id, method, Cb)
  let key = a:id. '-'.a:method
  let s:callbacks[key] = a:Cb
  call coc#rpc#notify('registNotification', [a:id, a:method])
endfunction

function! coc#do_notify(id, method, result)
  let key = a:id. '-'.a:method
  let Fn = s:callbacks[key]
  if !empty(Fn)
    call Fn(a:result)
  endif
endfunction

function! coc#start(...)
  let opt = coc#util#get_complete_option()
  call CocActionAsync('startCompletion', extend(opt, get(a:, 1, {})))
  return ''
endfunction

function! coc#refresh() abort
  return "\<c-r>=coc#start()\<CR>"
endfunction

function! coc#_select_confirm() abort
  call timer_start(10, { -> coc#pum#select_confirm()})
  return s:is_vim || has('nvim-0.5.0') ? "\<Ignore>" : "\<space>\<bs>" 
endfunction

function! coc#complete_indent() abort
  let curpos = getcurpos()
  let indent_len = len(matchstr(getline('.'), '^\s*'))
  let startofline = &startofline
  let virtualedit = &virtualedit
  set nostartofline
  set virtualedit=all
  normal! ==
  let &startofline = startofline
  let &virtualedit = virtualedit
  let shift = len(matchstr(getline('.'), '^\s*')) - indent_len
  let curpos[2] += shift
  let curpos[4] += shift
  call cursor(curpos[1:])
   if shift != 0
    if s:is_vim
      call timer_start(0, { -> execute('redraw')})
    endif
  endif
endfunction
