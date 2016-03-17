let g:airline#themes#xtermlight#palette = {}

let s:N1 = [ '#eeeeee' , '#005fff' , 255 , 27  ]
let s:N2 = [ '#000087' , '#00d7ff' , 18  , 45  ]
let s:N3 = [ '#005fff' , '#afffff' , 27  , 159 ]
let g:airline#themes#xtermlight#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#xtermlight#palette.normal_modified = {
      \ 'airline_c': [ '#d70000' , '#ffdfdf' , 160     , 224     , ''     ] ,
      \ }


let s:I1 = [ '#eeeeee' , '#00875f' , 255 , 29  ]
let s:I2 = [ '#005f00' , '#00d787' , 22  , 42  ]
let s:I3 = [ '#005f5f' , '#afff87' , 23  , 156 ]
let g:airline#themes#xtermlight#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#xtermlight#palette.insert_modified = {
      \ 'airline_c': [ '#d70000' , '#ffdfdf' , 160     , 224     , ''     ] ,
      \ }
let g:airline#themes#xtermlight#palette.insert_paste = {
      \ 'airline_a': [ s:I1[0]   , '#d78700' , s:I1[2] , 172     , ''     ] ,
      \ }


let g:airline#themes#xtermlight#palette.replace = copy(g:airline#themes#xtermlight#palette.insert)
let g:airline#themes#xtermlight#palette.replace.airline_a = [ s:I2[0]   , '#ff0000' , s:I1[2] , 196     , ''     ]
let g:airline#themes#xtermlight#palette.replace_modified = g:airline#themes#xtermlight#palette.insert_modified


let s:V1 = [ '#eeeeee' , '#ff5f00' , 255 , 202 ]
let s:V2 = [ '#5f0000' , '#ffaf00' , 52  , 214 ]
let s:V3 = [ '#d75f00' , '#ffff87' , 166 , 228 ]
let g:airline#themes#xtermlight#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#xtermlight#palette.visual_modified = {
      \ 'airline_c': [ '#d70000' , '#ffdfdf' , 160     , 224     , ''     ] ,
      \ }


let s:IA1 = [ '#6c6c6c' , '#b2b2b2' , 242 , 249 , '' ]
let s:IA2 = [ '#8a8a8a' , '#d0d0d0' , 245 , 252 , '' ]
let s:IA3 = [ '#a8a8a8' , '#eeeeee' , 248 , 255 , '' ]
let g:airline#themes#xtermlight#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)
let g:airline#themes#xtermlight#palette.inactive_modified = {
      \ 'airline_c': [ '#d70000' , ''        , 160     , ''      , ''     ] ,
      \ }

