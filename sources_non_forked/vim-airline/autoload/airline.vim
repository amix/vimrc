" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let g:airline_statusline_funcrefs = get(g:, 'airline_statusline_funcrefs', [])

let s:sections = ['a','b','c','gutter','x','y','z','warning']
let s:inactive_funcrefs = []

function! airline#add_statusline_func(name)
  call airline#add_statusline_funcref(function(a:name))
endfunction

function! airline#add_statusline_funcref(function)
  if index(g:airline_statusline_funcrefs, a:function) >= 0
    echohl WarningMsg
    echo 'The airline statusline funcref '.string(a:function).' has already been added.'
    echohl NONE
    return
  endif
  call add(g:airline_statusline_funcrefs, a:function)
endfunction

function! airline#remove_statusline_func(name)
  let i = index(g:airline_statusline_funcrefs, function(a:name))
  if i > -1
    call remove(g:airline_statusline_funcrefs, i)
  endif
endfunction

function! airline#add_inactive_statusline_func(name)
  call add(s:inactive_funcrefs, function(a:name))
endfunction

function! airline#load_theme()
  if exists('*airline#themes#{g:airline_theme}#refresh')
    call airline#themes#{g:airline_theme}#refresh()
  endif

  let palette = g:airline#themes#{g:airline_theme}#palette
  call airline#themes#patch(palette)

  if exists('g:airline_theme_patch_func')
    let Fn = function(g:airline_theme_patch_func)
    call Fn(palette)
  endif

  call airline#highlighter#load_theme()
  call airline#extensions#load_theme()
endfunction

function! airline#switch_theme(name)
  try
    let palette = g:airline#themes#{a:name}#palette "also lazy loads the theme
    let g:airline_theme = a:name
  catch
    echohl WarningMsg | echo 'The specified theme cannot be found.' | echohl NONE
    if exists('g:airline_theme')
      return
    else
      let g:airline_theme = 'dark'
    endif
  endtry

  let w:airline_lastmode = ''
  call airline#update_statusline()
  call airline#load_theme()

  " this is required to prevent clobbering the startup info message, i don't know why...
  call airline#check_mode(winnr())
endfunction

function! airline#switch_matching_theme()
  if exists('g:colors_name')
    try
      let palette = g:airline#themes#{g:colors_name}#palette
      call airline#switch_theme(g:colors_name)
      return 1
    catch
      for map in items(g:airline_theme_map)
        if match(g:colors_name, map[0]) > -1
          call airline#switch_theme(map[1])
          return 1
        endif
      endfor
    endtry
  endif
  return 0
endfunction

function! airline#update_statusline()
  for nr in filter(range(1, winnr('$')), 'v:val != winnr()')
    call setwinvar(nr, 'airline_active', 0)
    let context = { 'winnr': nr, 'active': 0, 'bufnr': winbufnr(nr) }
    call s:invoke_funcrefs(context, s:inactive_funcrefs)
  endfor

  unlet! w:airline_render_left
  unlet! w:airline_render_right
  for section in s:sections
    unlet! w:airline_section_{section}
  endfor

  let w:airline_active = 1
  let context = { 'winnr': winnr(), 'active': 1, 'bufnr': winbufnr(winnr()) }
  call s:invoke_funcrefs(context, g:airline_statusline_funcrefs)
endfunction

let s:contexts = {}
let s:core_funcrefs = [
      \ function('airline#extensions#apply'),
      \ function('airline#extensions#default#apply') ]
function! s:invoke_funcrefs(context, funcrefs)
  let builder = airline#builder#new(a:context)
  let err = airline#util#exec_funcrefs(a:funcrefs + s:core_funcrefs, builder, a:context)
  if err == 1
    let a:context.line = builder.build()
    let s:contexts[a:context.winnr] = a:context
    call setwinvar(a:context.winnr, '&statusline', '%!airline#statusline('.a:context.winnr.')')
  endif
endfunction

function! airline#statusline(winnr)
  if has_key(s:contexts, a:winnr)
    return '%{airline#check_mode('.a:winnr.')}'.s:contexts[a:winnr].line
  endif

  " in rare circumstances this happens...see #276
  return ''
endfunction

function! airline#check_mode(winnr)
  let context = s:contexts[a:winnr]

  if get(w:, 'airline_active', 1)
    let l:m = mode()
    if l:m ==# "i"
      let l:mode = ['insert']
    elseif l:m ==# "R"
      let l:mode = ['replace']
    elseif l:m =~# '\v(v|V||s|S|)'
      let l:mode = ['visual']
    else
      let l:mode = ['normal']
    endif
    let w:airline_current_mode = get(g:airline_mode_map, l:m, l:m)
  else
    let l:mode = ['inactive']
    let w:airline_current_mode = get(g:airline_mode_map, '__')
  endif

  if g:airline_detect_modified && &modified
    call add(l:mode, 'modified')
  endif

  if g:airline_detect_paste && &paste
    call add(l:mode, 'paste')
  endif

  if &readonly
    call add(l:mode, 'readonly')
  endif

  let mode_string = join(l:mode)
  if get(w:, 'airline_lastmode', '') != mode_string
    call airline#highlighter#highlight_modified_inactive(context.bufnr)
    call airline#highlighter#highlight(l:mode)
    let w:airline_lastmode = mode_string
  endif

  return ''
endfunction

