" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:is_win32term = (has('win32') || has('win64')) && !has('gui_running') && (empty($CONEMUBUILD) || &term !=? 'xterm')

let s:separators = {}
let s:accents = {}

function! s:gui2cui(rgb, fallback)
  if a:rgb == ''
    return a:fallback
  elseif match(a:rgb, '^\%(NONE\|[fb]g\)$') > -1
    return a:rgb
  endif
  let rgb = map(split(a:rgb[1:], '..\zs'), '0 + ("0x".v:val)')
  return airline#msdos#round_msdos_colors(rgb)
endfunction

function! s:get_syn(group, what)
  if !exists("g:airline_gui_mode")
    let g:airline_gui_mode = airline#init#gui_mode()
  endif
  let color = synIDattr(synIDtrans(hlID(a:group)), a:what, g:airline_gui_mode)
  if empty(color) || color == -1
    let color = synIDattr(synIDtrans(hlID('Normal')), a:what, g:airline_gui_mode)
  endif
  if empty(color) || color == -1
    let color = 'NONE'
  endif
  return color
endfunction

function! s:get_array(fg, bg, opts)
  let fg = a:fg
  let bg = a:bg
  return g:airline_gui_mode ==# 'gui'
        \ ? [ fg, bg, '', '', join(a:opts, ',') ]
        \ : [ '', '', fg, bg, join(a:opts, ',') ]
endfunction

function! airline#highlighter#get_highlight(group, ...)
  let fg = s:get_syn(a:group, 'fg')
  let bg = s:get_syn(a:group, 'bg')
  let reverse = g:airline_gui_mode ==# 'gui'
        \ ? synIDattr(synIDtrans(hlID(a:group)), 'reverse', 'gui')
        \ : synIDattr(synIDtrans(hlID(a:group)), 'reverse', 'cterm')
        \|| synIDattr(synIDtrans(hlID(a:group)), 'reverse', 'term')
  return reverse ? s:get_array(bg, fg, a:000) : s:get_array(fg, bg, a:000)
endfunction

function! airline#highlighter#get_highlight2(fg, bg, ...)
  let fg = s:get_syn(a:fg[0], a:fg[1])
  let bg = s:get_syn(a:bg[0], a:bg[1])
  return s:get_array(fg, bg, a:000)
endfunction

function! airline#highlighter#exec(group, colors)
  if pumvisible()
    return
  endif
  let colors = a:colors
  if s:is_win32term
    let colors[2] = s:gui2cui(get(colors, 0, ''), get(colors, 2, ''))
    let colors[3] = s:gui2cui(get(colors, 1, ''), get(colors, 3, ''))
  endif
  let cmd= printf('hi %s %s %s %s %s %s %s %s',
        \ a:group, s:Get(colors, 0, 'guifg=', ''), s:Get(colors, 1, 'guibg=', ''),
        \ s:Get(colors, 2, 'ctermfg=', ''), s:Get(colors, 3, 'ctermbg=', ''),
        \ s:Get(colors, 4, 'gui=', ''), s:Get(colors, 4, 'cterm=', ''),
        \ s:Get(colors, 4, 'term=', ''))
  let old_hi = airline#highlighter#get_highlight(a:group)
  if len(colors) == 4
    call add(colors, '')
  endif
  if old_hi != colors
    exe cmd
  endif
endfunction

function! s:Get(dict, key, prefix, default)
  if get(a:dict, a:key, a:default) isnot# a:default
    return a:prefix. get(a:dict, a:key)
  else
    return ''
  endif
endfunction

function! s:exec_separator(dict, from, to, inverse, suffix)
  if pumvisible()
    return
  endif
  let l:from = airline#themes#get_highlight(a:from.a:suffix)
  let l:to = airline#themes#get_highlight(a:to.a:suffix)
  let group = a:from.'_to_'.a:to.a:suffix
  if a:inverse
    let colors = [ l:from[1], l:to[1], l:from[3], l:to[3] ]
  else
    let colors = [ l:to[1], l:from[1], l:to[3], l:from[3] ]
  endif
  let a:dict[group] = colors
  call airline#highlighter#exec(group, colors)
endfunction

function! airline#highlighter#load_theme()
  if pumvisible()
    return
  endif
  for winnr in filter(range(1, winnr('$')), 'v:val != winnr()')
    call airline#highlighter#highlight_modified_inactive(winbufnr(winnr))
  endfor
  call airline#highlighter#highlight(['inactive'])
  call airline#highlighter#highlight(['normal'])
endfunction

function! airline#highlighter#add_separator(from, to, inverse)
  let s:separators[a:from.a:to] = [a:from, a:to, a:inverse]
  call <sid>exec_separator({}, a:from, a:to, a:inverse, '')
endfunction

function! airline#highlighter#add_accent(accent)
  let s:accents[a:accent] = 1
endfunction

function! airline#highlighter#highlight_modified_inactive(bufnr)
  if getbufvar(a:bufnr, '&modified')
    let colors = exists('g:airline#themes#{g:airline_theme}#palette.inactive_modified.airline_c')
          \ ? g:airline#themes#{g:airline_theme}#palette.inactive_modified.airline_c : []
  else
    let colors = exists('g:airline#themes#{g:airline_theme}#palette.inactive.airline_c')
          \ ? g:airline#themes#{g:airline_theme}#palette.inactive.airline_c : []
  endif

  if !empty(colors)
    call airline#highlighter#exec('airline_c'.(a:bufnr).'_inactive', colors)
  endif
endfunction

function! airline#highlighter#highlight(modes)
  let p = g:airline#themes#{g:airline_theme}#palette

  " draw the base mode, followed by any overrides
  let mapped = map(a:modes, 'v:val == a:modes[0] ? v:val : a:modes[0]."_".v:val')
  let suffix = a:modes[0] == 'inactive' ? '_inactive' : ''
  for mode in mapped
    if exists('g:airline#themes#{g:airline_theme}#palette[mode]')
      let dict = g:airline#themes#{g:airline_theme}#palette[mode]
      for kvp in items(dict)
        let mode_colors = kvp[1]
        call airline#highlighter#exec(kvp[0].suffix, mode_colors)

        for accent in keys(s:accents)
          if !has_key(p.accents, accent)
            continue
          endif
          let colors = copy(mode_colors)
          if p.accents[accent][0] != ''
            let colors[0] = p.accents[accent][0]
          endif
          if p.accents[accent][2] != ''
            let colors[2] = p.accents[accent][2]
          endif
          if len(colors) >= 5
            let colors[4] = get(p.accents[accent], 4, '')
          else
            call add(colors, get(p.accents[accent], 4, ''))
          endif
          call airline#highlighter#exec(kvp[0].suffix.'_'.accent, colors)
        endfor
      endfor

      " TODO: optimize this
      for sep in items(s:separators)
        call <sid>exec_separator(dict, sep[1][0], sep[1][1], sep[1][2], suffix)
      endfor
    endif
  endfor
endfunction

