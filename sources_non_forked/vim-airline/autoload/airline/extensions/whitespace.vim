" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

" http://got-ravings.blogspot.com/2008/10/vim-pr0n-statusline-whitespace-flags.html

let s:show_message = get(g:, 'airline#extensions#whitespace#show_message', 1)
let s:symbol = get(g:, 'airline#extensions#whitespace#symbol', g:airline_symbols.whitespace)
let s:default_checks = ['indent', 'trailing', 'mixed-indent-file']

let s:trailing_format = get(g:, 'airline#extensions#whitespace#trailing_format', 'trailing[%s]')
let s:mixed_indent_format = get(g:, 'airline#extensions#whitespace#mixed_indent_format', 'mixed-indent[%s]')
let s:long_format = get(g:, 'airline#extensions#whitespace#long_format', 'long[%s]')
let s:mixed_indent_file_format = get(g:, 'airline#extensions#whitespace#mixed_indent_file_format', 'mix-indent-file[%s]')
let s:indent_algo = get(g:, 'airline#extensions#whitespace#mixed_indent_algo', 0)
let s:skip_check_ft = {'make': ['indent', 'mixed-indent-file'] }

let s:max_lines = get(g:, 'airline#extensions#whitespace#max_lines', 20000)

let s:enabled = get(g:, 'airline#extensions#whitespace#enabled', 1)

function! s:check_mixed_indent()
  if s:indent_algo == 1
    " [<tab>]<space><tab>
    " spaces before or between tabs are not allowed
    let t_s_t = '(^\t* +\t\s*\S)'
    " <tab>(<space> x count)
    " count of spaces at the end of tabs should be less than tabstop value
    let t_l_s = '(^\t+ {' . &ts . ',}' . '\S)'
    return search('\v' . t_s_t . '|' . t_l_s, 'nw')
  elseif s:indent_algo == 2
    return search('\v(^\t* +\t\s*\S)', 'nw')
  else
    return search('\v(^\t+ +)|(^ +\t+)', 'nw')
  endif
endfunction

function! s:check_mixed_indent_file()
  let indent_tabs = search('\v(^\t+)', 'nw')
  let indent_spc  = search('\v(^ +)', 'nw')
  if indent_tabs > 0 && indent_spc > 0
    return printf("%d:%d", indent_tabs, indent_spc)
  else
    return ''
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
      try
        let regexp = get(g:, 'airline#extensions#whitespace#trailing_regexp', '\s$')
        let trailing = search(regexp, 'nw')
      catch
        echomsg 'airline#whitespace: error occured evaluating '. regexp
        echomsg v:exception
        return ''
      endtry
    endif

    let mixed = 0
    let check = 'indent'
    if index(checks, check) > -1 && index(get(s:skip_check_ft, &ft, []), check) < 0
      let mixed = s:check_mixed_indent()
    endif

    let mixed_file = ''
    let check = 'mixed-indent-file'
    if index(checks, check) > -1 && index(get(s:skip_check_ft, &ft, []), check) < 0
      let mixed_file = s:check_mixed_indent_file()
    endif

    let long = 0
    if index(checks, 'long') > -1 && &tw > 0
      let long = search('\%>'.&tw.'v.\+', 'nw')
    endif

    if trailing != 0 || mixed != 0 || long != 0 || !empty(mixed_file)
      let b:airline_whitespace_check = s:symbol
      if s:show_message
        if trailing != 0
          let b:airline_whitespace_check .= (g:airline_symbols.space).printf(s:trailing_format, trailing)
        endif
        if mixed != 0
          let b:airline_whitespace_check .= (g:airline_symbols.space).printf(s:mixed_indent_format, mixed)
        endif
        if long != 0
          let b:airline_whitespace_check .= (g:airline_symbols.space).printf(s:long_format, long)
        endif
        if !empty(mixed_file)
          let b:airline_whitespace_check .= (g:airline_symbols.space).printf(s:mixed_indent_file_format, mixed_file)
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

