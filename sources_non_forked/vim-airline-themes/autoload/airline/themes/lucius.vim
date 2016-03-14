let g:airline#themes#lucius#palette = {}

function! airline#themes#lucius#refresh()

    let s:N1 = airline#themes#get_highlight('StatusLine')
    let s:N2 = airline#themes#get_highlight('Folded')
    let s:N3 = airline#themes#get_highlight('CursorLine')
    let g:airline#themes#lucius#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

    let modified_group = airline#themes#get_highlight('Statement')
    let g:airline#themes#lucius#palette.normal_modified = {
                \ 'airline_c': [modified_group[0], '', modified_group[2], '', '']
                \ }

    let warning_group = airline#themes#get_highlight('DiffDelete')
    let g:airline#themes#lucius#palette.normal.airline_warning = warning_group
    let g:airline#themes#lucius#palette.normal_modified.airline_warning = warning_group

    let s:I1 = airline#themes#get_highlight('DiffAdd')
    let s:I2 = s:N2
    let s:I3 = s:N3
    let g:airline#themes#lucius#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
    let g:airline#themes#lucius#palette.insert_modified = g:airline#themes#lucius#palette.normal_modified
    let g:airline#themes#lucius#palette.insert.airline_warning = g:airline#themes#lucius#palette.normal.airline_warning
    let g:airline#themes#lucius#palette.insert_modified.airline_warning = g:airline#themes#lucius#palette.normal_modified.airline_warning

    let s:R1 = airline#themes#get_highlight('DiffChange')
    let s:R2 = s:N2
    let s:R3 = s:N3
    let g:airline#themes#lucius#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)
    let g:airline#themes#lucius#palette.replace_modified = g:airline#themes#lucius#palette.normal_modified
    let g:airline#themes#lucius#palette.replace.airline_warning = g:airline#themes#lucius#palette.normal.airline_warning
    let g:airline#themes#lucius#palette.replace_modified.airline_warning = g:airline#themes#lucius#palette.normal_modified.airline_warning

    let s:V1 = airline#themes#get_highlight('Cursor')
    let s:V2 = s:N2
    let s:V3 = s:N3
    let g:airline#themes#lucius#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
    let g:airline#themes#lucius#palette.visual_modified = g:airline#themes#lucius#palette.normal_modified
    let g:airline#themes#lucius#palette.visual.airline_warning = g:airline#themes#lucius#palette.normal.airline_warning
    let g:airline#themes#lucius#palette.visual_modified.airline_warning = g:airline#themes#lucius#palette.normal_modified.airline_warning

    let s:IA = airline#themes#get_highlight('StatusLineNC')
    let g:airline#themes#lucius#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
    let g:airline#themes#lucius#palette.inactive_modified = {
                \ 'airline_c': [ modified_group[0], '', modified_group[2], '', '' ]
                \ }

    let g:airline#themes#lucius#palette.accents = {
                \ 'red': airline#themes#get_highlight('Constant'),
                \ }

    " Extra tabline colors
    let s:TS = airline#themes#get_highlight('TabLineSel')
    let g:airline#themes#lucius#palette.tabline = {}
    let g:airline#themes#lucius#palette.tabline.airline_tabsel = s:TS
    let g:airline#themes#lucius#palette.tabline.airline_tabsel_right = s:TS

endfunction

call airline#themes#lucius#refresh()

