" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

" http://got-ravings.blogspot.com/2008/10/vim-pr0n-statusline-whitespace-flags.html

" for backwards compatibility
if exists('g:airline_detect_whitespace')
  let s:show_message = g:airline_detect_whitespace == 1
else
  let s:show_message = get(g:, 'airline#extensions#whitespace#show_message', 1)
endif

let s:symbol = get(g:, 'airline#extensions#whitespace#symbol', g:airline_symbols.whitespace)
let s:default_checks = ['indent', 'trailing']

let s:trailing_format = get(g:, 'airline#extensions#whitespace#trailing_format', 'trailing[%s]')
let s:mixed_indent_format = get(g:, 'airline#extensions#whitespace#mixed_indent_format', 'mixed-indent[%s]')
let s:indent_algo = get(g:, 'airline#extensions#whitespace#mixed_indent_algo', 0)

let s:max_lines = get(g:, 'airline#extensions#whitespace#max_lines', 20000)

let s:enabled = get(g:, 'airline#extensions#whitespace#enabled', 1)

function! s:check_mixed_indent()
  if s:indent_algo == 1
    " [<tab>]<space><tab>
    " spaces before or between tabs are not allowed
    let t_s_t = '(^\t* +\t\s*\S)'
    " <tab>(<space> x count)
    " count of spaces at the end of tabs should be less then tabstop value
    let t_l_s = '(^\t+ {' . &ts . ',}' . '\S)'
    return search('\v' . t_s_t . '|' . t_l_s, 'nw')
  else
    return search('\v(^\t+ +)|(^ +\t+)', 'nw')
  endif
endfunction

function! airline#extensions#whitespace#check()
  if &readonly || !&modifiable || !s:enabled || line('$') > s:max_lines
    return ''
  endif

  if !exists('b:airline_whitespace_check')
    let b:airline_whitespace_check = ''
    let checks = get(g:, 'airline#extensions#whitespace#checks', s:default_checks)

    let trailing = 0
    if index(checks, 'trailing') > -1
      let trailing = search('\s$', 'nw')
    endif

    let mixed = 0
    if index(checks, 'indent') > -1
      let mixed = s:check_mixed_indent()
    endif

    if trailing != 0 || mixed != 0
      let b:airline_whitespace_check = s:symbol
      if s:show_message
        if trailing != 0
          let b:airline_whitespace_check .= (g:airline_symbols.space).printf(s:trailing_format, trailing)
        endif
        if mixed != 0
          let b:airline_whitespace_check .= (g:airline_symbols.space).printf(s:mixed_indent_format, mixed)
        endif
      endif
    endif
  endif
  return b:airline_whitespace_check
endfunction

function! airline#extensions#whitespace#toggle()
  if s:enabled
    augroup airline_whitespace
      autocmd!
    augroup END
    augroup! airline_whitespace
    let s:enabled = 0
  else
    call airline#extensions#whitespace#init()
    let s:enabled = 1
  endif

  if exists("g:airline#extensions#whitespace#enabled")
    let g:airline#extensions#whitespace#enabled = s:enabled
    if s:enabled && match(g:airline_section_warning, '#whitespace#check') < 0
      let g:airline_section_warning .= airline#section#create(['whitespace'])
      call airline#update_statusline()
    endif
  endif
  echo 'Whitespace checking: '.(s:enabled ? 'Enabled' : 'Disabled')
endfunction

function! airline#extensions#whitespace#init(...)
  call airline#parts#define_function('whitespace', 'airline#extensions#whitespace#check')

  unlet! b:airline_whitespace_check
  augroup airline_whitespace
    autocmd!
    autocmd CursorHold,BufWritePost * unlet! b:airline_whitespace_check
  augroup END
endfunction

