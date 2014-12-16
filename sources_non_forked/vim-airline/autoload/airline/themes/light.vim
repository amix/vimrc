let g:airline#themes#light#palette = {}

let s:N1 = [ '#ffffff' , '#005fff' , 255 , 27  ]
let s:N2 = [ '#000087' , '#00dfff' , 18  , 45  ]
let s:N3 = [ '#005fff' , '#afffff' , 27  , 159 ]
let g:airline#themes#light#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#light#palette.normal_modified = {
      \ 'airline_c': [ '#df0000' , '#ffdfdf' , 160     , 224     , ''     ] ,
      \ }


let s:I1 = [ '#ffffff' , '#00875f' , 255 , 29  ]
let s:I2 = [ '#005f00' , '#00df87' , 22  , 42  ]
let s:I3 = [ '#005f5f' , '#afff87' , 23  , 156 ]
let g:airline#themes#light#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#light#palette.insert_modified = {
      \ 'airline_c': [ '#df0000' , '#ffdfdf' , 160     , 224     , ''     ] ,
      \ }
let g:airline#themes#light#palette.insert_paste = {
      \ 'airline_a': [ s:I1[0]   , '#d78700' , s:I1[2] , 172     , ''     ] ,
      \ }


let g:airline#themes#light#palette.replace = copy(g:airline#themes#light#palette.insert)
let g:airline#themes#light#palette.replace.airline_a = [ s:I2[0]   , '#ff0000' , s:I1[2] , 196     , ''     ]
let g:airline#themes#light#palette.replace_modified = g:airline#themes#light#palette.insert_modified


let s:V1 = [ '#ffffff' , '#ff5f00' , 255 , 202 ]
let s:V2 = [ '#5f0000' , '#ffaf00' , 52  , 214 ]
let s:V3 = [ '#df5f00' , '#ffff87' , 166 , 228 ]
let g:airline#themes#light#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#light#palette.visual_modified = {
      \ 'airline_c': [ '#df0000' , '#ffdfdf' , 160     , 224     , ''     ] ,
      \ }


let s:IA1 = [ '#666666' , '#b2b2b2' , 242 , 249 , '' ]
let s:IA2 = [ '#8a8a8a' , '#d0d0d0' , 245 , 252 , '' ]
let s:IA3 = [ '#a8a8a8' , '#ffffff' , 248 , 255 , '' ]
let g:airline#themes#light#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)
let g:airline#themes#light#palette.inactive_modified = {
      \ 'airline_c': [ '#df0000' , ''        , 160     , ''      , ''     ] ,
      \ }

