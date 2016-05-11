" vim-airline companion theme of Wombat
" looks great with wombat256 vim colorscheme

" Normal mode
"          [ guifg, guibg, ctermfg, ctermbg, opts ]
let s:N1 = [ '#141413' , '#CAE682' , 232 , 192 ] " mode
let s:N2 = [ '#CAE682' , '#32322F' , 192 , 238 ] " info
let s:N3 = [ '#CAE682' , '#242424' , 192 , 235 ] " statusline
let s:N4 = [ '#86CD74' , 113 ]                   " mode modified

" Insert mode
let s:I1 = [ '#141413' , '#FDE76E' , 232 , 227 ]
let s:I2 = [ '#FDE76E' , '#32322F' , 227 , 238 ]
let s:I3 = [ '#FDE76E' , '#242424' , 227 , 235 ]
let s:I4 = [ '#FADE3E' , 221 ]

" Visual mode
let s:V1 = [ '#141413' , '#B5D3F3' , 232 , 153 ]
let s:V2 = [ '#B5D3F3' , '#32322F' , 153 , 238 ]
let s:V3 = [ '#B5D3F3' , '#242424' , 153 , 235 ]
let s:V4 = [ '#7CB0E6' , 111 ]

" Replace mode
let s:R1 = [ '#141413' , '#E5786D' , 232 , 173 ]
let s:R2 = [ '#E5786D' , '#32322F' , 173 , 238 ]
let s:R3 = [ '#E5786D' , '#242424' , 173 , 235 ]
let s:R4 = [ '#E55345' , 203 ]

" Paste mode
let s:PA = [ '#94E42C' , 47 ]

" Info modified
let s:IM = [ '#40403C' , 238 ]

" Inactive mode
let s:IA = [ '#767676' , s:N3[1] , 243 , s:N3[3] , '' ]

let g:airline#themes#wombat#palette = {}

let g:airline#themes#wombat#palette.accents = {
      \ 'red': [ '#E5786D' , '' , 203 , '' , '' ],
      \ }

let g:airline#themes#wombat#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#wombat#palette.normal_modified = {
    \ 'airline_a': [ s:N1[0] , s:N4[0] , s:N1[2] , s:N4[1] , ''     ] ,
    \ 'airline_b': [ s:N4[0] , s:IM[0] , s:N4[1] , s:IM[1] , ''     ] ,
    \ 'airline_c': [ s:N4[0] , s:N3[1] , s:N4[1] , s:N3[3] , ''     ] }


let g:airline#themes#wombat#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#wombat#palette.insert_modified = {
    \ 'airline_a': [ s:I1[0] , s:I4[0] , s:I1[2] , s:I4[1] , ''     ] ,
    \ 'airline_b': [ s:I4[0] , s:IM[0] , s:I4[1] , s:IM[1] , ''     ] ,
    \ 'airline_c': [ s:I4[0] , s:N3[1] , s:I4[1] , s:N3[3] , ''     ] }


let g:airline#themes#wombat#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#wombat#palette.visual_modified = {
    \ 'airline_a': [ s:V1[0] , s:V4[0] , s:V1[2] , s:V4[1] , ''     ] ,
    \ 'airline_b': [ s:V4[0] , s:IM[0] , s:V4[1] , s:IM[1] , ''     ] ,
    \ 'airline_c': [ s:V4[0] , s:N3[1] , s:V4[1] , s:N3[3] , ''     ] }


let g:airline#themes#wombat#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)
let g:airline#themes#wombat#palette.replace_modified = {
    \ 'airline_a': [ s:R1[0] , s:R4[0] , s:R1[2] , s:R4[1] , ''     ] ,
    \ 'airline_b': [ s:R4[0] , s:IM[0] , s:R4[1] , s:IM[1] , ''     ] ,
    \ 'airline_c': [ s:R4[0] , s:N3[1] , s:R4[1] , s:N3[3] , ''     ] }


let g:airline#themes#wombat#palette.insert_paste = {
    \ 'airline_a': [ s:I1[0] , s:PA[0] , s:I1[2] , s:PA[1] , ''     ] ,
    \ 'airline_b': [ s:PA[0] , s:IM[0] , s:PA[1] , s:IM[1] , ''     ] ,
    \ 'airline_c': [ s:PA[0] , s:N3[1] , s:PA[1] , s:N3[3] , ''     ] }


let g:airline#themes#wombat#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
let g:airline#themes#wombat#palette.inactive_modified = {
    \ 'airline_c': [ s:N4[0] , ''      , s:N4[1] , ''      , ''     ] }


if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#wombat#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ '#DADADA' , '#242424' , 253 , 234 , ''     ] ,
      \ [ '#DADADA' , '#40403C' , 253 , 238 , ''     ] ,
      \ [ '#141413' , '#DADADA' , 232 , 253 , 'bold' ] )

