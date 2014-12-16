if get(g:, 'airline#themes#base16#constant', 0)
  let g:airline#themes#base16#palette = {}

  " Color palette
  let s:gui_dark_gray = '#202020'
  let s:cterm_dark_gray = 234
  let s:gui_med_gray_hi = '#303030'
  let s:cterm_med_gray_hi = 236
  let s:gui_med_gray_lo = '#3a3a3a'
  let s:cterm_med_gray_lo = 237
  let s:gui_light_gray = '#505050'
  let s:cterm_light_gray = 239
  let s:gui_green = '#99cc99'
  let s:cterm_green = 151
  let s:gui_blue = '#6a9fb5'
  let s:cterm_blue = 67
  let s:gui_purple = '#aa759f'
  let s:cterm_purple = 139
  let s:gui_orange = '#d28445'
  let s:cterm_orange = 173
  let s:gui_red = '#ac4142'
  let s:cterm_red = 131
  let s:gui_pink = '#d7afd7'
  let s:cterm_pink = 182

  " Normal mode
  let s:N1 = [s:gui_dark_gray, s:gui_green, s:cterm_dark_gray, s:cterm_green]
  let s:N2 = [s:gui_light_gray, s:gui_med_gray_lo, s:cterm_light_gray, s:cterm_med_gray_lo]
  let s:N3 = [s:gui_green, s:gui_med_gray_hi, s:cterm_green, s:cterm_med_gray_hi]
  let g:airline#themes#base16#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
  let g:airline#themes#base16#palette.normal_modified = {
        \ 'airline_c': [s:gui_orange, s:gui_med_gray_hi, s:cterm_orange, s:cterm_med_gray_hi, ''],
        \ }

  " Insert mode
  let s:I1 = [s:gui_med_gray_hi, s:gui_blue, s:cterm_med_gray_hi, s:cterm_blue]
  let s:I3 = [s:gui_blue, s:gui_med_gray_hi, s:cterm_blue, s:cterm_med_gray_hi]
  let g:airline#themes#base16#palette.insert = airline#themes#generate_color_map(s:I1, s:N2, s:I3)
  let g:airline#themes#base16#palette.insert_modified = copy(g:airline#themes#base16#palette.normal_modified)
  let g:airline#themes#base16#palette.insert_paste = {
        \ 'airline_a': [s:gui_dark_gray, s:gui_orange, s:cterm_dark_gray, s:cterm_orange, ''],
        \ }

  " Replace mode
  let g:airline#themes#base16#palette.replace = {
        \ 'airline_a': [s:gui_dark_gray, s:gui_red, s:cterm_dark_gray, s:cterm_red, ''],
        \ 'airline_c': [s:gui_red, s:gui_med_gray_hi, s:cterm_red, s:cterm_med_gray_hi, ''],
        \ }
  let g:airline#themes#base16#palette.replace_modified = copy(g:airline#themes#base16#palette.insert_modified)

  " Visual mode
  let s:V1 = [s:gui_dark_gray, s:gui_pink, s:cterm_dark_gray, s:cterm_pink]
  let s:V3 = [s:gui_pink, s:gui_med_gray_hi, s:cterm_pink, s:cterm_med_gray_hi]
  let g:airline#themes#base16#palette.visual = airline#themes#generate_color_map(s:V1, s:N2, s:V3)
  let g:airline#themes#base16#palette.visual_modified = copy(g:airline#themes#base16#palette.insert_modified)

  " Inactive window
  let s:IA = [s:gui_dark_gray, s:gui_med_gray_hi, s:cterm_dark_gray, s:cterm_med_gray_hi, '']
  let g:airline#themes#base16#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
  let g:airline#themes#base16#palette.inactive_modified = {
        \ 'airline_c': [s:gui_orange, '', s:cterm_orange, '', ''],
        \ }
else
  function! airline#themes#base16#refresh()
    let g:airline#themes#base16#palette = {}

    let g:airline#themes#base16#palette.accents = {
          \ 'red': airline#themes#get_highlight('Constant'),
          \ }

    let s:N1 = airline#themes#get_highlight2(['DiffText', 'bg'], ['DiffText', 'fg'], 'bold')
    let s:N2 = airline#themes#get_highlight('Visual')
    let s:N3 = airline#themes#get_highlight('CursorLine')
    let g:airline#themes#base16#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

    let group = airline#themes#get_highlight('vimCommand')
    let g:airline#themes#base16#palette.normal_modified = {
          \ 'statusline': [ group[0], '', group[2], '', '' ]
          \ }

    let s:I1 = airline#themes#get_highlight2(['DiffAdded', 'bg'], ['DiffAdded', 'fg'], 'bold')
    let s:I2 = airline#themes#get_highlight2(['DiffAdded', 'fg'], ['Normal', 'bg'])
    let s:I3 = s:N3
    let g:airline#themes#base16#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
    let g:airline#themes#base16#palette.insert_modified = g:airline#themes#base16#palette.normal_modified

    let s:R1 = airline#themes#get_highlight2(['WarningMsg', 'bg'], ['WarningMsg', 'fg'], 'bold')
    let s:R2 = s:N2
    let s:R3 = s:N3
    let g:airline#themes#base16#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)
    let g:airline#themes#base16#palette.replace_modified = g:airline#themes#base16#palette.normal_modified

    let s:V1 = airline#themes#get_highlight2(['Normal', 'bg'], ['Constant', 'fg'], 'bold')
    let s:V2 = airline#themes#get_highlight2(['Constant', 'fg'], ['Normal', 'bg'])
    let s:V3 = s:N3
    let g:airline#themes#base16#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
    let g:airline#themes#base16#palette.visual_modified = g:airline#themes#base16#palette.normal_modified

    let s:IA = airline#themes#get_highlight2(['NonText', 'fg'], ['CursorLine', 'bg'])
    let g:airline#themes#base16#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
    let g:airline#themes#base16#palette.inactive_modified = {
          \ 'airline_c': [ group[0], '', group[2], '', '' ]
          \ }
  endfunction
  call airline#themes#base16#refresh()
endif

