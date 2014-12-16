let g:airline#themes#jellybeans#palette = {}

" The name of the function must be 'refresh'.
function! airline#themes#jellybeans#refresh()
  " This theme is an example of how to use helper functions to extract highlight
  " values from the corresponding colorscheme. It was written in a hurry, so it
  " is very minimalistic. If you are a jellybeans user and want to make updates,
  " please send pull requests.

  " Here are examples where the entire highlight group is copied and an airline
  " compatible color array is generated.
  let s:N1 = airline#themes#get_highlight('DbgCurrent', 'bold')
  let s:N2 = airline#themes#get_highlight('Folded')
  let s:N3 = airline#themes#get_highlight('NonText')

  let g:airline#themes#jellybeans#palette.accents = {
        \ 'red': airline#themes#get_highlight('Constant'),
        \ }

  let g:airline#themes#jellybeans#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
  let g:airline#themes#jellybeans#palette.normal_modified = {
        \ 'airline_c': [ '#ffb964', '', 215, '', '' ]
        \ }

  let s:I1 = airline#themes#get_highlight('DiffAdd', 'bold')
  let s:I2 = s:N2
  let s:I3 = s:N3
  let g:airline#themes#jellybeans#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
  let g:airline#themes#jellybeans#palette.insert_modified = g:airline#themes#jellybeans#palette.normal_modified

  let s:R1 = airline#themes#get_highlight('WildMenu', 'bold')
  let s:R2 = s:N2
  let s:R3 = s:N3
  let g:airline#themes#jellybeans#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)
  let g:airline#themes#jellybeans#palette.replace_modified = g:airline#themes#jellybeans#palette.normal_modified

  " Sometimes you want to mix and match colors from different groups, you can do
  " that with this method.
  let s:V1 = airline#themes#get_highlight2(['TabLineSel', 'bg'], ['DiffDelete', 'bg'], 'bold')
  let s:V2 = s:N2
  let s:V3 = s:N3
  let g:airline#themes#jellybeans#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
  let g:airline#themes#jellybeans#palette.visual_modified = g:airline#themes#jellybeans#palette.normal_modified

  " And of course, you can always do it manually as well.
  let s:IA = [ '#444444', '#1c1c1c', 237, 234 ]
  let g:airline#themes#jellybeans#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
  let g:airline#themes#jellybeans#palette.inactive_modified = g:airline#themes#jellybeans#palette.normal_modified
endfunction

call airline#themes#jellybeans#refresh()

