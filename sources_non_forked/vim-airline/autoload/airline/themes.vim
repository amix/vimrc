" MIT License. Copyright (c) 2013 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

" generates a dictionary which defines the colors for each highlight group
function! airline#themes#generate_color_map(sect1, sect2, sect3, ...)
  let palette = {
        \ 'airline_a': [ a:sect1[0] , a:sect1[1] , a:sect1[2] , a:sect1[3] , get(a:sect1 , 4 , '') ] ,
        \ 'airline_b': [ a:sect2[0] , a:sect2[1] , a:sect2[2] , a:sect2[3] , get(a:sect2 , 4 , '') ] ,
        \ 'airline_c': [ a:sect3[0] , a:sect3[1] , a:sect3[2] , a:sect3[3] , get(a:sect3 , 4 , '') ] ,
        \ }

  if a:0 > 0
    call extend(palette, {
          \ 'airline_x': [ a:1[0] , a:1[1] , a:1[2] , a:1[3] , get(a:1 , 4 , '' ) ] ,
          \ 'airline_y': [ a:2[0] , a:2[1] , a:2[2] , a:2[3] , get(a:2 , 4 , '' ) ] ,
          \ 'airline_z': [ a:3[0] , a:3[1] , a:3[2] , a:3[3] , get(a:3 , 4 , '' ) ] ,
          \ })
  else
    call extend(palette, {
          \ 'airline_x': [ a:sect3[0] , a:sect3[1] , a:sect3[2] , a:sect3[3] , '' ] ,
          \ 'airline_y': [ a:sect2[0] , a:sect2[1] , a:sect2[2] , a:sect2[3] , '' ] ,
          \ 'airline_z': [ a:sect1[0] , a:sect1[1] , a:sect1[2] , a:sect1[3] , '' ] ,
          \ })
  endif

  return palette
endfunction

function! airline#themes#get_highlight(group, ...)
  return call('airline#highlighter#get_highlight', [a:group] + a:000)
endfunction

function! airline#themes#get_highlight2(fg, bg, ...)
  return call('airline#highlighter#get_highlight2', [a:fg, a:bg] + a:000)
endfunction

function! airline#themes#patch(palette)
  for mode in keys(a:palette)
    if !has_key(a:palette[mode], 'airline_warning')
      let a:palette[mode]['airline_warning'] = [ '#000000', '#df5f00', 232, 166 ]
    endif
  endfor

  let a:palette.accents = get(a:palette, 'accents', {})
  let a:palette.accents.bold = [ '', '', '', '', 'bold' ]
  let a:palette.accents.italic = [ '', '', '', '', 'italic' ]

  if !has_key(a:palette.accents, 'red')
    let a:palette.accents.red = [ '#ff0000' , '' , 160 , '' ]
  endif
  if !has_key(a:palette.accents, 'green')
    let a:palette.accents.green = [ '#008700' , '' , 22  , '' ]
  endif
  if !has_key(a:palette.accents, 'blue')
    let a:palette.accents.blue = [ '#005fff' , '' , 27  , '' ]
  endif
  if !has_key(a:palette.accents, 'yellow')
    let a:palette.accents.yellow = [ '#dfff00' , '' , 190 , '' ]
  endif
  if !has_key(a:palette.accents, 'orange')
    let a:palette.accents.orange = [ '#df5f00' , '' , 166 , '' ]
  endif
  if !has_key(a:palette.accents, 'purple')
    let a:palette.accents.purple = [ '#af00df' , '' , 128 , '' ]
  endif
endfunction

