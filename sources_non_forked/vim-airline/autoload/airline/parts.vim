" MIT License. Copyright (c) 2013-2015 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:parts = {}

" PUBLIC API {{{

function! airline#parts#define(key, config)
  let s:parts[a:key] = get(s:parts, a:key, {})
  if exists('g:airline#init#bootstrapping')
    call extend(s:parts[a:key], a:config, 'keep')
  else
    call extend(s:parts[a:key], a:config, 'force')
  endif
endfunction

function! airline#parts#define_function(key, name)
  call airline#parts#define(a:key, { 'function': a:name })
endfunction

function! airline#parts#define_text(key, text)
  call airline#parts#define(a:key, { 'text': a:text })
endfunction

function! airline#parts#define_raw(key, raw)
  call airline#parts#define(a:key, { 'raw': a:raw })
endfunction

function! airline#parts#define_minwidth(key, width)
  call airline#parts#define(a:key, { 'minwidth': a:width })
endfunction

function! airline#parts#define_condition(key, predicate)
  call airline#parts#define(a:key, { 'condition': a:predicate })
endfunction

function! airline#parts#define_accent(key, accent)
  call airline#parts#define(a:key, { 'accent': a:accent })
endfunction

function! airline#parts#define_empty(keys)
  for key in a:keys
    call airline#parts#define_raw(key, '')
  endfor
endfunction

function! airline#parts#get(key)
  return get(s:parts, a:key, {})
endfunction

" }}}

function! airline#parts#mode()
  return get(w:, 'airline_current_mode', '')
endfunction

function! airline#parts#paste()
  return g:airline_detect_paste && &paste ? g:airline_symbols.paste : ''
endfunction

function! airline#parts#iminsert()
  if g:airline_detect_iminsert && &iminsert && exists('b:keymap_name')
    return toupper(b:keymap_name)
  endif
  return ''
endfunction

function! airline#parts#readonly()
  return &readonly ? g:airline_symbols.readonly : ''
endfunction

function! airline#parts#filetype()
  return &filetype
endfunction

function! airline#parts#ffenc()
  return printf('%s%s', &fenc, strlen(&ff) > 0 ? '['.&ff.']' : '')
endfunction

