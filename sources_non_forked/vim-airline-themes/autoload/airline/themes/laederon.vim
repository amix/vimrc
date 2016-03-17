" vim-airline companion theme of Laederon
" (https://github.com/Donearm/Laederon)

" Normal mode
let s:N1 = [ '#1a1a18' , '#ffffff' , 232 , 255 ] " blackestgravel & snow
let s:N2 = [ '#ffffff' , '#44403a' , 255, 238 ] " snow & deepgravel
let s:N3 = [ '#90a680' , '#2e2d2a' , 64, 235 ] " dilutedpaint & darkgravel
let s:N4 = [ '#777470' , 240 ] " gravel

" Insert mode
let s:I1 = [ '#1a1a18' , '#1693a5' , 232 , 62 ] " blackestgravel & crystallake
let s:I2 = [ '#515744' , '#44403a' , 101 , 238 ] " lichen & deepgravel
let s:I3 = [ '#1693a5' , '#2e2d2a' , 39 , 235 ] " crystallake & darkgravel

" Visual mode
let s:V1 = [ '#1a1a18' , '#ab3e5d' , 232 , 161 ] " blackestgravel & raspberry
let s:V2 = [ '#000000' , '#908571' , 16 , 252 ] " coal & winterterrain
let s:V3 = [ '#ab3e5d' , '#8c7f77' , 161 , 245 ] " raspberry & wetcoldterrain
let s:V4 = [ '#515744' , 101 ] " lichen

" Replace mode
let s:RE = [ '#233e09' , 22 ] " oakleaf

" Paste mode
let s:PA = [ '#ab3e5d' , 161 ] " raspberry

let s:IA = [ s:N2[1] , s:N3[1] , s:N2[3], s:N3[3] , '' ]


let g:airline#themes#laederon#palette = {}

let g:airline#themes#laederon#palette.accents = {
      \ 'red': [ '#ef393d' , '' , 196 , '' , '' ]
      \ }

let g:airline#themes#laederon#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#laederon#palette.normal_modified = {
      \ 'airline_a' : [ s:N2[0] , s:N4[0] , s:N2[2] , s:N4[1] , '' ] ,
      \ 'airline_c' : [ s:V1[1] , s:N2[1] , s:V1[3] , s:N2[3] , '' ] }


let g:airline#themes#laederon#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#laederon#palette.insert_modified = {
      \ 'airline_c' : [ s:V2[1] , s:N2[1] , s:V2[3] , s:N2[3] , '' ] }
let g:airline#themes#laederon#palette.insert_paste = {
      \ 'airline_a' : [ s:I1[0] , s:PA[0] , s:I1[2] , s:PA[1] , '' ] }


let g:airline#themes#laederon#palette.replace = copy(airline#themes#laederon#palette.insert)
let g:airline#themes#laederon#palette.replace.airline_a = [ s:I1[0] , s:RE[0] , s:I1[2] , s:RE[1] , '' ]
let g:airline#themes#laederon#palette.replace_modified = g:airline#themes#laederon#palette.insert_modified


let g:airline#themes#laederon#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#laederon#palette.visual_modified = {
      \ 'airline_c' : [ s:V3[0] , s:V4[0] , s:V3[2] , s:V4[1] , '' ] }


let g:airline#themes#laederon#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
let g:airline#themes#laederon#palette.inactive_modified = {
      \ 'airline_c' : [ s:V1[1] , ''      , s:V1[3] , ''      , '' ] }

