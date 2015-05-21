let g:airline#themes#raven#palette = {}

let g:airline#themes#raven#palette.accents = {
      \ 'red': [ '#ff2121' , '' , 196 , '' , '' ],
      \ }

let s:N1 = [ '#c8c8c8' , '#2e2e2e' , 188 , 235 ]
let s:N2 = [ '#c8c8c8' , '#2e2e2e' , 188 , 235 ]
let s:N3 = [ '#c8c8c8' , '#2e2e2e' , 188 , 235 ]
let g:airline#themes#raven#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#raven#palette.normal_modified = {
      \ 'airline_c': [ '#e25000' , '#2e2e2e' , 166 , 235 , '' ] ,
      \ }

let s:I1 = [ '#11c279' , '#2e2e2e' , 36 , 235 ]
let s:I2 = [ '#11c279' , '#2e2e2e' , 36 , 235 ]
let s:I3 = [ '#11c279' , '#2e2e2e' , 36 , 235 ]
let g:airline#themes#raven#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#raven#palette.insert_modified = {
      \ 'airline_c': [ '#e25000' , '#2e2e2e' , 166 , 235 , '' ] ,
      \ }
let g:airline#themes#raven#palette.insert_paste = {
      \ 'airline_a': [ s:I1[0]   , '#2e2e2e' , s:I1[2] , 235 , '' ] ,
      \ }

let g:airline#themes#raven#palette.replace = copy(g:airline#themes#raven#palette.insert)
let g:airline#themes#raven#palette.replace.airline_a = [ '#e60000' , s:I1[1] , 160 , s:I1[3] , '' ]
let g:airline#themes#raven#palette.replace.airline_z = [ '#e60000' , s:I1[1] , 160 , s:I1[3] , '' ]
let g:airline#themes#raven#palette.replace_modified = g:airline#themes#raven#palette.insert_modified

let s:V1 = [ '#6565ff' , '#2e2e2e' , 63 , 235 ]
let s:V2 = [ '#6565ff' , '#2e2e2e' , 63 , 235 ]
let s:V3 = [ '#6565ff' , '#2e2e2e' , 63 , 235 ]
let g:airline#themes#raven#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#raven#palette.visual_modified = {
      \ 'airline_c': [ '#e25000' , '#2e2e2e' , 166 , 235 , '' ] ,
      \ }

let s:IA = [ '#5e5e5e' , '#222222' , 59 , 235 , '' ]
let g:airline#themes#raven#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
let g:airline#themes#raven#palette.inactive_modified = {
      \ 'airline_c': [ '#e25000' , '' , 166 , '' , '' ] ,
       \ }

let g:airline#themes#raven#palette.tabline = {
      \ 'airline_tab':      ['#c8c8c8' , '#2e2e2e' , 188 , 235 , '' ],
      \ 'airline_tabsel':   ['#2e2e2e' , '#a4c639' , 235 , 149 , '' ],
      \ 'airline_tabtype':  ['#c8c8c8' , '#2e2e2e' , 188 , 235 , '' ],
      \ 'airline_tabfill':  ['#c8c8c8' , '#2e2e2e' , 188 , 235 , '' ],
      \ 'airline_tabmod':   ['#2e2e2e' , '#a4c639' , 235 , 149 , '' ],
      \ }

let s:WI = [ '#ff0000', '#2e2e2e', 196, 235 ]
let g:airline#themes#raven#palette.normal.airline_warning = [
     \ s:WI[0], s:WI[1], s:WI[2], s:WI[3]
     \ ]

let g:airline#themes#raven#palette.normal_modified.airline_warning =
    \ g:airline#themes#raven#palette.normal.airline_warning

let g:airline#themes#raven#palette.insert.airline_warning =
    \ g:airline#themes#raven#palette.normal.airline_warning

let g:airline#themes#raven#palette.insert_modified.airline_warning =
    \ g:airline#themes#raven#palette.normal.airline_warning

let g:airline#themes#raven#palette.visual.airline_warning =
    \ g:airline#themes#raven#palette.normal.airline_warning

let g:airline#themes#raven#palette.visual_modified.airline_warning =
    \ g:airline#themes#raven#palette.normal.airline_warning

let g:airline#themes#raven#palette.replace.airline_warning =
    \ g:airline#themes#raven#palette.normal.airline_warning

let g:airline#themes#raven#palette.replace_modified.airline_warning =
    \ g:airline#themes#raven#palette.normal.airline_warning

if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#raven#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ '#c8c8c8' , '#2e2e2e' , 188 , 235 , ''     ] ,
      \ [ '#c8c8c8' , '#2e2e2e' , 188 , 235 , ''     ] ,
      \ [ '#2e2e2e' , '#a4c639' , 235 , 149 , ''     ] )
