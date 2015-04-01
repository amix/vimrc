let g:airline#themes#durant#palette = {}


let s:N1 = [ '#005f00' , '#afd700' , 22  , 148 ]
let s:N2 = [ '#93a1a1' , '#586e75' , 245 , 240 ]
let s:N3 = [ '#93a1a1' , '#073642' , 240 , 233 ]
let g:airline#themes#durant#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)


let g:airline#themes#durant#normal_modified = {
      \ 'airline_c': [ '#ffffff' , '#5f005f' , 255     , 53      , ''     ] ,
      \ }



let s:I1 = [ '#ffffff' , '#00875f' , 255 , 29  ]
let s:I2 = [ '#9e9e9e' , '#303030' , 247 , 236 ]
let s:I3 = [ '#87d7ff' , '#005f87' , 117 , 24  ]
let g:airline#themes#durant#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#durant#palette.insert_modified = {
      \ 'airline_c': [ '#ffffff' , '#5f005f' , 255     , 53      , ''     ] ,
      \ }
let g:airline#themes#durant#palette.insert_paste = {
      \ 'airline_a': [ s:I1[0]   , '#d78700' , s:I1[2] , 172     , ''     ] ,
      \ }


let g:airline#themes#durant#palette.replace = copy(g:airline#themes#durant#palette.insert)
let g:airline#themes#durant#palette.replace.airline_a = [ s:I2[0]   , '#af0000' , s:I2[2] , 124     , ''     ]

let g:airline#themes#durant#palette.replace_modified = g:airline#themes#durant#palette.insert_modified

let s:V1 = [ '#1a1a18' , '#ffffff' , 232 , 255 ]
let s:V2 = [ '#ffffff' , '#44403a' , 255, 238 ]
let s:V3 = [ '#90a680' , '#2e2d2a' , 64, 235 ]
let g:airline#themes#durant#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#durant#palette.visual_modified = {
      \ 'airline_c': [ '#ffffff' , '#5f005f' , 255     , 53      , ''     ] ,
      \ }


let s:IA1 = [ '#4e4e4e' , '#1c1c1c' , 239 , 234 , '' ]
let s:IA2 = [ '#4e4e4e' , '#262626' , 239 , 235 , '' ]
let s:IA3 = [ '#4e4e4e' , '#303030' , 239 , 236 , '' ]
let g:airline#themes#durant#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)
let g:airline#themes#durant#palette.inactive_modified = {
      \ 'airline_c': [ '#875faf' , '' , 97 , '' , '' ] ,
      \ }


let g:airline#themes#durant#palette.accents = {
      \ 'red': [ '#ff0000' , '' , 160 , ''  ]
      \ }

if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
  let g:airline#themes#durant#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
        \ [ '#d7d7ff' , '#5f00af' , 189 , 55  , ''     ],
        \ [ '#ffffff' , '#875fd7' , 231 , 98  , ''     ],
        \ [ '#5f00af' , '#ffffff' , 55  , 231 , 'bold' ])

