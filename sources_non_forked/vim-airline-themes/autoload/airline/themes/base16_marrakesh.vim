" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Marrakesh Scheme by Alexandre Gavioli (http://github.com/Alexx2/)
let g:airline#themes#base16_marrakesh#palette = {}
let s:gui00 = "#201602"
let s:gui01 = "#302e00"
let s:gui02 = "#5f5b17"
let s:gui03 = "#6c6823"
let s:gui04 = "#86813b"
let s:gui05 = "#948e48"
let s:gui06 = "#ccc37a"
let s:gui07 = "#faf0a5"
let s:gui08 = "#c35359"
let s:gui09 = "#b36144"
let s:gui0A = "#a88339"
let s:gui0B = "#18974e"
let s:gui0C = "#75a738"
let s:gui0D = "#477ca1"
let s:gui0E = "#8868b3"
let s:gui0F = "#b3588e"

let s:cterm00 = 0
let s:cterm01 = 52
let s:cterm02 = 58
let s:cterm03 = 58
let s:cterm04 = 101
let s:cterm05 = 101
let s:cterm06 = 180
let s:cterm07 = 229
let s:cterm08 = 131
let s:cterm09 = 131
let s:cterm0A = 137
let s:cterm0B = 29
let s:cterm0C = 107
let s:cterm0D = 67
let s:cterm0E = 97
let s:cterm0F = 132

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_marrakesh#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_marrakesh#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_marrakesh#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_marrakesh#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_marrakesh#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_marrakesh#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
