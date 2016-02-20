" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

if !get(g:, 'loaded_nrrw_rgn', 0)
  finish
endif

function! airline#extensions#nrrwrgn#apply(...)
  if exists(":WidenRegion") == 2
    let spc = g:airline_symbols.space
    if !exists("*nrrwrgn#NrrwRgnStatus()") || empty(nrrwrgn#NrrwRgnStatus())
      call a:1.add_section('airline_a', printf('%s[Narrowed%s#%d]', spc, spc, b:nrrw_instn))
      let bufname=(get(b:, 'orig_buf', 0) ? bufname(b:orig_buf) : substitute(bufname('%'), '^Nrrwrgn_\zs.*\ze_\d\+$', submatch(0), ''))
      call a:1.add_section('airline_c', spc.bufname.spc)
      call a:1.split()
    else
      let dict=nrrwrgn#NrrwRgnStatus()
      let vmode = { 'v': 'Char ', 'V': 'Line ', '': 'Block '}
      let mode = dict.visual ? vmode[dict.visual] : vmode['V']
      let winwidth = winwidth(0)
      if winwidth < 80
        let mode = mode[0]
      endif
      let title = (winwidth < 80 ? "Nrrw" : "Narrowed ")
      let multi = (winwidth < 80 ? 'M' : 'Multi')
      call a:1.add_section('airline_a', printf('[%s%s%s#%d]%s', (dict.multi ? multi : ""),
        \ title, mode, b:nrrw_instn, spc))
      let name = dict.fullname
      if name !=# '[No Name]'
        if winwidth > 100
          " need some space
          let name = fnamemodify(dict.fullname, ':~')
          if strlen(name) > 8
            " shorten name
            let name = substitute(name, '\(.\)[^/\\]*\([/\\]\)', '\1\2', 'g')
          endif
        else
          let name = fnamemodify(dict.fullname, ':t')
        endif
      endif
      let range=(dict.multi ? '' : printf("[%d-%d]", dict.start[1], dict.end[1]))
      call a:1.add_section('airline_c', printf("%s %s %s", name, range,
        \ dict.enabled ? (&encoding ==? 'utf-8'  ? "\u2713"  : '')  : '!'))
      call a:1.split()
      call a:1.add_section('airline_x', get(g:, 'airline_section_x').spc)
      call a:1.add_section('airline_y', spc.get(g:, 'airline_section_y').spc)
      call a:1.add_section('airline_z', spc.get(g:, 'airline_section_z'))
    endif
    return 1
  endif
endfunction

function! airline#extensions#nrrwrgn#init(ext)
  call a:ext.add_statusline_func('airline#extensions#nrrwrgn#apply')
endfunction
