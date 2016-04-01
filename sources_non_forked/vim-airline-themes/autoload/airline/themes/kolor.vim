let g:airline#themes#kolor#palette = {}

let s:N1 = [ '#e2e2e2' , '#4f3598' , 254 , 56  ]
let s:N2 = [ '#ff5fd7' , '#242322' , 206 , 234 ]
let s:N3 = [ '#e2e2e2' , '#4a4a4a' , 254 , 238 ]

let g:airline#themes#kolor#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let g:airline#themes#kolor#palette.normal_modified = {
      \ 'airline_c': [ '#e2e2e2' , '#4f3598' , 254     , 56      , ''     ] ,
      \ }


let s:I1 = [ '#242322' , '#7eaefd' , 234 , 111 ]
let s:I2 = [ '#75d7d8' , '#242322' , 80  , 234 ]
let s:I3 = [ '#e2e2e2' , '#4a4a4a' , 254 , 238 ]
let g:airline#themes#kolor#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#kolor#palette.insert_modified = {
      \ 'airline_c': [ '#242322' , '#7eaefd' , 234     , 111     , ''     ] ,
      \ }


let g:airline#themes#kolor#palette.replace = copy(g:airline#themes#kolor#palette.insert)
let g:airline#themes#kolor#palette.replace.airline_a = [ s:I2[0]   , '#005154' , s:I2[2] , 23      , ''     ]
let g:airline#themes#kolor#palette.replace_modified = {
      \ 'airline_c': [ '#e2e2e2' , '#005154' , 254 , 23  , '' ] ,
      \ }


let s:V1 = [ '#242322' , '#e6987a' , 234 , 180 ]
let s:V2 = [ '#dbc570' , '#242322' , 186 , 234 ]
let s:V3 = [ '#e2e2e2' , '#4a4a4a' , 254 , 238 ]
let g:airline#themes#kolor#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#kolor#palette.visual_modified = {
      \ 'airline_c': [ '#242322' , '#e6987a' , 234     , 180      , ''     ] ,
      \ }


let s:IA1 = [ '#b2b2b2' , '#4a4a4a' , 247 , 238 , '' ]
let s:IA2 = [ '#b2b2b2' , '#4a4a4a' , 247 , 238 ]
let s:IA3 = [ '#b2b2b2' , '#4a4a4a' , 247 , 238 , '' ]
let g:airline#themes#kolor#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)
let g:airline#themes#kolor#palette.inactive_modified = {
      \ 'airline_c': [ '#875faf' , '' , 97 , '' , '' ] ,
      \ }


let g:airline#themes#kolor#palette.accents = {
      \ 'red': [ '#d96e8a' , '' , 168 , ''  ]
      \ }


if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#kolor#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ '#e2e2e2' , '#4a4a4a' , 254 , 238 , ''     ],
      \ [ '#e2e2e2' , '#242322' , 254 , 234 , ''     ],
      \ [ '#e2e2e2' , '#4f3598' , 254 , 56  , 'bold' ])
