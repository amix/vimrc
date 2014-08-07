" vim-airline companion theme of Sol
" (https://github.com/Pychimp/vim-sol)

let g:airline#themes#sol#palette = {}

let g:airline#themes#sol#palette.accents = {
      \ 'red': [ '#ffffff' , '' , 231 , '' , '' ],
      \ }

let s:N1 = [ '#343434' , '#a0a0a0' , 237 , 248 ]
let s:N2 = [ '#343434' , '#b3b3b3' , 237 , 250 ]
let s:N3 = [ '#343434' , '#c7c7c7' , 237 , 252 ]
let g:airline#themes#sol#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#sol#palette.normal_modified = {
      \ 'airline_c': [ '#ffffff' , '#ff6868' , 237 , 209 , '' ] ,
      \ }


let s:I1 = [ '#eeeeee' , '#09643f' , 255 , 30  ]
let s:I2 = [ '#343434' , '#a3a3a3' , 237 , 249 ]
let s:I3 = [ '#343434' , '#b0b0b0' , 237 , 250 ]
let g:airline#themes#sol#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#sol#palette.insert_modified = {
      \ 'airline_c': [ '#343434' , '#ffdbc7' , 237 , 216 , '' ] ,
      \ }
let g:airline#themes#sol#palette.insert_paste = {
      \ 'airline_a': [ s:I1[0]   , '#09643f' , s:I1[2] , 30 , '' ] ,
      \ }


let g:airline#themes#sol#palette.replace = copy(g:airline#themes#sol#palette.insert)
let g:airline#themes#sol#palette.replace.airline_a = [ s:I1[0] , '#ff2121' , s:I1[2] , 196 , '' ]
let g:airline#themes#sol#palette.replace.airline_z = [ s:I1[0] , '#ff2121' , s:I1[2] , 196 , '' ]
let g:airline#themes#sol#palette.replace_modified = g:airline#themes#sol#palette.insert_modified

let s:V1 = [ '#ffff9a' , '#ff6003' , 222 , 202 ]
let s:V2 = [ '#343434' , '#a3a3a3' , 237 , 249 ]
let s:V3 = [ '#343434' , '#b0b0b0' , 237 , 250 ]
let g:airline#themes#sol#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#sol#palette.visual_modified = {
      \ 'airline_c': [ '#343434' , '#ffdbc7' , 237 , 216 , '' ] ,
      \ }

let s:IA = [ '#777777' , '#c7c7c7' , 244 , 251 , '' ]
let g:airline#themes#sol#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
let g:airline#themes#sol#palette.inactive_modified = {
      \ 'airline_c': [ '#ff3535' , '' , 203 , '' , '' ] ,
       \ }

let g:airline#themes#sol#palette.tabline = {
      \ 'airline_tab':      ['#343434', '#b3b3b3',  237, 250, ''],
      \ 'airline_tabsel':   ['#ffffff', '#004b9a',  231, 31 , ''],
      \ 'airline_tabtype':  ['#343434', '#a0a0a0',  237, 248, ''],
      \ 'airline_tabfill':  ['#343434', '#c7c7c7',  237, 251, ''],
      \ 'airline_tabmod':   ['#343434', '#ffdbc7',  237, 216, ''],
      \ }

let s:WI = [ '#eeeeee', '#e33900', 255, 166 ]
let g:airline#themes#sol#palette.normal.airline_warning = [
     \ s:WI[0], s:WI[1], s:WI[2], s:WI[3]
     \ ]

let g:airline#themes#sol#palette.normal_modified.airline_warning =
    \ g:airline#themes#sol#palette.normal.airline_warning

let g:airline#themes#sol#palette.insert.airline_warning =
    \ g:airline#themes#sol#palette.normal.airline_warning

let g:airline#themes#sol#palette.insert_modified.airline_warning =
    \ g:airline#themes#sol#palette.normal.airline_warning

let g:airline#themes#sol#palette.visual.airline_warning =
    \ g:airline#themes#sol#palette.normal.airline_warning

let g:airline#themes#sol#palette.visual_modified.airline_warning =
    \ g:airline#themes#sol#palette.normal.airline_warning

let g:airline#themes#sol#palette.replace.airline_warning =
    \ g:airline#themes#sol#palette.normal.airline_warning

let g:airline#themes#sol#palette.replace_modified.airline_warning =
    \ g:airline#themes#sol#palette.normal.airline_warning

if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#sol#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ '#343434' , '#c7c7c7' , 237 , 251 , ''     ] ,
      \ [ '#343434' , '#b3b3b3' , 237 , 250 , ''     ] ,
      \ [ '#eeeeee' , '#007fff' , 255 , 27  , ''     ] )
