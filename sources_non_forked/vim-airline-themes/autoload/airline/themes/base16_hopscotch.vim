" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Hopscotch Scheme by Jan T. Sott
let g:airline#themes#base16_hopscotch#palette = {}
let s:gui00 = "#322931"
let s:gui01 = "#433b42"
let s:gui02 = "#5c545b"
let s:gui03 = "#797379"
let s:gui04 = "#989498"
let s:gui05 = "#b9b5b8"
let s:gui06 = "#d5d3d5"
let s:gui07 = "#ffffff"
let s:gui08 = "#dd464c"
let s:gui09 = "#fd8b19"
let s:gui0A = "#fdcc59"
let s:gui0B = "#8fc13e"
let s:gui0C = "#149b93"
let s:gui0D = "#1290bf"
let s:gui0E = "#c85e7c"
let s:gui0F = "#b33508"

let s:cterm00 = 53
let s:cterm01 = 59
let s:cterm02 = 59
let s:cterm03 = 96
let s:cterm04 = 102
let s:cterm05 = 145
let s:cterm06 = 188
let s:cterm07 = 15
let s:cterm08 = 167
let s:cterm09 = 208
let s:cterm0A = 221
let s:cterm0B = 107
let s:cterm0C = 30
let s:cterm0D = 31
let s:cterm0E = 168
let s:cterm0F = 130

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_hopscotch#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_hopscotch#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_hopscotch#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_hopscotch#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_hopscotch#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_hopscotch#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
