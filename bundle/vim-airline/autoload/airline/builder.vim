" MIT License. Copyright (c) 2013-2014 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:prototype = {}

function! s:prototype.split(...)
  let self._side = 0
  let self._line .= '%#'.self._curgroup.'#'.(a:0 ? a:1 : '%=')
endfunction

function! s:prototype.add_section_spaced(group, contents)
  call self.add_section(a:group, (g:airline_symbols.space).a:contents.(g:airline_symbols.space))
endfunction

function! s:prototype.add_section(group, contents)
  if self._curgroup != ''
    if self._curgroup == a:group
      let self._line .= self._side ? self._context.left_alt_sep : self._context.right_alt_sep
    else
      call airline#highlighter#add_separator(self._curgroup, a:group, self._side)
      let self._line .= '%#'.self._curgroup.'_to_'.a:group.'#'
      let self._line .= self._side ? self._context.left_sep : self._context.right_sep
    endif
  endif

  if self._curgroup != a:group
    let self._line .= '%#'.a:group.'#'
  endif

  if self._context.active
    let contents = []
    let content_parts = split(a:contents, '__accent')
    for cpart in content_parts
      let accent = matchstr(cpart, '_\zs[^#]*\ze')
      call add(contents, cpart)
    endfor
    let line = join(contents, a:group)
    let line = substitute(line, '__restore__', a:group, 'g')
  else
    let line = substitute(a:contents, '%#__accent[^#]*#', '', 'g')
    let line = substitute(line, '%#__restore__#', '', 'g')
  endif

  let self._line .= line
  let self._curgroup = a:group
endfunction

function! s:prototype.add_raw(text)
  let self._line .= a:text
endfunction

function! s:prototype.build()
  if !self._context.active
    let self._line = substitute(self._line, '%#.\{-}\ze#', '\0_inactive', 'g')
  endif
  return self._line
endfunction

function! airline#builder#new(context)
  let builder = copy(s:prototype)
  let builder._context = a:context
  let builder._side = 1
  let builder._curgroup = ''
  let builder._line = ''

  call extend(builder._context, {
        \ 'left_sep': g:airline_left_sep,
        \ 'left_alt_sep': g:airline_left_alt_sep,
        \ 'right_sep': g:airline_right_sep,
        \ 'right_alt_sep': g:airline_right_alt_sep,
        \ }, 'keep')
  return builder
endfunction

