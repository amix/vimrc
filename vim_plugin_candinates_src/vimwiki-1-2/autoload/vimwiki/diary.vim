" vim:tabstop=2:shiftwidth=2:expandtab:foldmethod=marker:textwidth=79
" Vimwiki autoload plugin file
" Desc: Handle diary notes
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/

" Load only once {{{
if exists("g:loaded_vimwiki_diary_auto") || &cp
  finish
endif
let g:loaded_vimwiki_diary_auto = 1
"}}}

function! s:prefix_zero(num) "{{{
  if a:num < 10
    return '0'.a:num
  endif
  return a:num
endfunction "}}}

function! s:desc(d1, d2) "{{{
  return a:d1 == a:d2 ? 0 : a:d1 < a:d2 ? 1 : -1
endfunction "}}}
    
function! s:get_date_link(fmt) "{{{
  return strftime(a:fmt)
endfunction "}}}

function! s:link_exists(lines, link) "{{{
  let link_exists = 0
  for line in a:lines
    if line =~ escape(a:link, '[]\')
      let link_exists = 1
      break
    endif
  endfor
  return link_exists
endfunction "}}}

function! s:diary_path() "{{{
  return VimwikiGet('path').VimwikiGet('diary_rel_path')
endfunction "}}}

function! s:diary_index() "{{{
  return s:diary_path().VimwikiGet('diary_index').VimwikiGet('ext')
endfunction "}}}

function! s:get_diary_range(lines, header) "{{{
  let rx = '\[\[\d\{4}-\d\d-\d\d\]\]'
  let idx = 0
  let ln_start = -1
  let ln_end = -1
  for line in a:lines
    if ln_start != -1 
      if line =~ '^\s*\(=\)\+.*\1\s*$' || (line !~ rx && line !~ '^\s*$')
        break
      endif
    endif
    if line =~ '^\s*\(=\)\+\s*'.a:header.'\s*\1\s*$'
      let ln_start = idx + 1
    endif
    let idx += 1
  endfor

  let ln_end = idx
  return [ln_start, ln_end]
endfunction "}}}

function! s:diary_date_link() "{{{
  return s:get_date_link(VimwikiGet('diary_link_fmt'))
endfunction "}}}

function! s:get_file_contents(file_name) "{{{
  let lines = []
  let bufnr = bufnr(expand(a:file_name))
  if bufnr != -1
    let lines = getbufline(bufnr, 1, '$')
  else
    try
      let lines = readfile(expand(a:file_name))
    catch
    endtry
  endif
  return [lines, bufnr]
endfunction "}}}

function! s:get_links() "{{{
  let rx = '\d\{4}-\d\d-\d\d'
  let s_links = glob(VimwikiGet('path').VimwikiGet('diary_rel_path').
        \ '*'.VimwikiGet('ext'))

  let s_links = substitute(s_links, '\'.VimwikiGet('ext'), "", "g")
  let links = split(s_links, '\n')

  " remove backup files (.wiki~)
  call filter(links, 'v:val !~ ''.*\~$''')
  
  " remove paths
  call map(links, 'fnamemodify(v:val, ":t")')

  call filter(links, 'v:val =~ "'.escape(rx, '\').'"')
  return links
endfunction "}}}

function! s:get_position_links(link) "{{{
  let idx = -1
  let links = []
  if a:link =~ '\d\{4}-\d\d-\d\d'
    let links = s:get_links()
    " include 'today' into links
    if index(links, s:diary_date_link()) == -1
      call add(links, s:diary_date_link())
    endif
    call sort(links)
    let idx = index(links, a:link)
  endif
  return [idx, links]
endfunction "}}}

function! s:format_links(links) "{{{
  let lines = []
  let line = '| '
  let idx = 0
  let trigger = 0
  while idx < len(a:links)
    if idx/VimwikiGet('diary_link_count') > trigger
      let trigger = idx/VimwikiGet('diary_link_count')
      call add(lines, substitute(line, '\s\+$', '', ''))
      let line = '| '
    endif
    let line .= a:links[idx].' | '
    let idx += 1
  endwhile
  call add(lines, substitute(line, '\s\+$', '', ''))
  call extend(lines, [''])

  return lines
endfunction "}}}

function! s:add_link(page, header, link) "{{{
  let [lines, bufnr] = s:get_file_contents(a:page)

  let [ln_start, ln_end] = s:get_diary_range(lines, a:header)

  let link = '[['.a:link.']]'

  let link_exists = s:link_exists(lines[ln_start : ln_end], link)

  if !link_exists

    if ln_start == -1
      call insert(lines, '= '.a:header.' =')
      let ln_start = 1
      let ln_end = 1
    endif

    " removing 'old' links
    let idx = ln_end - ln_start
    while idx > 0
      call remove(lines, ln_start)
      let idx -= 1
    endwhile
    
    " get all diary links from filesystem
    let links = s:get_links()
    call map(links, '"[[".v:val."]]"')
    
    " add current link
    if index(links, link) == -1
      call add(links, link)
    endif

    let links = sort(links, 's:desc')
    call extend(lines, s:format_links(links), ln_start)

    if bufnr != -1
      exe 'buffer '.bufnr
      if !&readonly
        1,$delete _
        call append(1, lines)
        1,1delete _
      endif
    else
      call writefile(lines, expand(a:page))
    endif
  endif
endfunction "}}}

function! s:make_date_link(...) "{{{
  if a:0
    let link = a:1
  else
    let link = s:diary_date_link()
  endif
  let header = VimwikiGet('diary_header')
  call s:add_link(s:diary_index(), header, link)
  return VimwikiGet('diary_rel_path').link
endfunction "}}}

function! vimwiki#diary#make_note(index, ...) "{{{
  call vimwiki#base#select(a:index)
  call vimwiki#base#mkdir(VimwikiGet('path').VimwikiGet('diary_rel_path'))
  if a:0
    let link = s:make_date_link(a:1)
  else
    let link = s:make_date_link()
  endif
  call vimwiki#base#open_link(':e ', link, s:diary_index())
endfunction "}}}

function! vimwiki#diary#goto_index(index) "{{{
  call vimwiki#base#select(a:index)
  call vimwiki#base#edit_file(':e', s:diary_index())
endfunction "}}}

" Calendar.vim callback function.
function! vimwiki#diary#calendar_action(day, month, year, week, dir) "{{{
  let day = s:prefix_zero(a:day)
  let month = s:prefix_zero(a:month)

  let link = a:year.'-'.month.'-'.day
  if winnr('#') == 0
    if a:dir == 'V'
      vsplit
    else
      split
    endif
  else
    wincmd p
    if !&hidden && &modified
      new
    endif
  endif

  " Create diary note for a selected date in default wiki.
  call vimwiki#diary#make_note(1, link)
endfunction "}}}

" Calendar.vim sign function.
function vimwiki#diary#calendar_sign(day, month, year) "{{{
  let day = s:prefix_zero(a:day)
  let month = s:prefix_zero(a:month)
  let sfile = VimwikiGet('path').VimwikiGet('diary_rel_path').
        \ a:year.'-'.month.'-'.day.VimwikiGet('ext')
  return filereadable(expand(sfile))
endfunction "}}}

function! vimwiki#diary#goto_next_day() "{{{
  let link = ''
  let [idx, links] = s:get_position_links(expand('%:t:r'))

  if idx == (len(links) - 1)
    return
  endif

  if idx != -1 && idx < len(links) - 1
    let link = VimwikiGet('diary_rel_path').links[idx+1]
  else
    " goto today
    let link = VimwikiGet('diary_rel_path').s:diary_date_link()
  endif

  if len(link)
    call vimwiki#base#open_link(':e ', link)
  endif
endfunction "}}}

function! vimwiki#diary#goto_prev_day() "{{{
  let link = ''
  let [idx, links] = s:get_position_links(expand('%:t:r'))

  if idx == 0
    return
  endif

  if idx > 0
    let link = VimwikiGet('diary_rel_path').links[idx-1]
  else
    " goto today
    let link = VimwikiGet('diary_rel_path').s:diary_date_link()
  endif

  if len(link)
    call vimwiki#base#open_link(':e ', link)
  endif
endfunction "}}}
