let g:airline#themes#papercolor#palette = {}

let g:airline#themes#papercolor#palette.accents = {
      \ 'red': [ '#66d9ef' , '' , 81 , '' , '' ],
      \ }

" Normal Mode:
let s:N1 = [ '#585858' , '#e4e4e4' , 240 , 254 ] " Mode
let s:N2 = [ '#e4e4e4' , '#0087af' , 254 , 31  ] " Info
let s:N3 = [ '#eeeeee' , '#005f87' , 255 , 24  ] " StatusLine


let g:airline#themes#papercolor#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#papercolor#palette.normal_modified = {
      \ 'airline_c': [ '#eeeeee' , '#005f87' , 255 , 24 , '' ] ,
      \ }


" Insert Mode:
let s:I1 = [ '#585858' , '#e4e4e4' , 240 , 254 ] " Mode
let s:I2 = [ '#e4e4e4' , '#0087af' , 254 , 31  ] " Info
let s:I3 = [ '#eeeeee' , '#005f87' , 255 , 24  ] " StatusLine


let g:airline#themes#papercolor#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#papercolor#palette.insert_modified = {
      \ 'airline_c': [ '#eeeeee' , '#005f87' , 255 , 24 , '' ] ,
      \ }


" Replace Mode:
let g:airline#themes#papercolor#palette.replace = copy(g:airline#themes#papercolor#palette.insert)
let g:airline#themes#papercolor#palette.replace.airline_a = [ '#d7005f'   , '#e4e4e4' , 161 , 254, ''     ]
let g:airline#themes#papercolor#palette.replace_modified = {
      \ 'airline_c': [ '#eeeeee' , '#005f87' , 255 , 24 , '' ] ,
      \ }


" Visual Mode:
let s:V1 = [ '#005f87', '#e4e4e4', 24,  254 ]
let s:V2 = [ '',        '#0087af', '',  31  ]
let s:V3 = [ '#e4e4e4', '#005f87', 254, 24  ]

let g:airline#themes#papercolor#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#papercolor#palette.visual_modified = {
      \ 'airline_c': [ '#e4e4e4', '#005f87', 254, 24  ] ,
      \ }

" Inactive:
let s:IA = [ '#585858' , '#e4e4e4' , 240 , 254 , '' ]
let g:airline#themes#papercolor#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
let g:airline#themes#papercolor#palette.inactive_modified = {
      \ 'airline_c': [ '#585858' , '#e4e4e4' , 240 , 254 , '' ] ,
      \ }


" CtrlP:
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#papercolor#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ '#e4e4e4' , '#005f87' , 254 , 24  , ''     ] ,
      \ [ '#e4e4e4' , '#0087af' , 254 , 31  , ''     ] ,
      \ [ '#585858' , '#e4e4e4' , 240 , 254 , 'bold' ] )

