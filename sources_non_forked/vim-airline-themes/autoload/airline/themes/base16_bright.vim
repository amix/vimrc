" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Bright Scheme by Chris Kempson (http://chriskempson.com)
let g:airline#themes#base16_bright#palette = {}
let s:gui00 = "#000000"
let s:gui01 = "#303030"
let s:gui02 = "#505050"
let s:gui03 = "#b0b0b0"
let s:gui04 = "#d0d0d0"
let s:gui05 = "#e0e0e0"
let s:gui06 = "#f5f5f5"
let s:gui07 = "#ffffff"
let s:gui08 = "#fb0120"
let s:gui09 = "#fc6d24"
let s:gui0A = "#fda331"
let s:gui0B = "#a1c659"
let s:gui0C = "#76c7b7"
let s:gui0D = "#6fb3d2"
let s:gui0E = "#d381c3"
let s:gui0F = "#be643c"

let s:cterm00 = 0
let s:cterm01 = 236
let s:cterm02 = 239
let s:cterm03 = 249
let s:cterm04 = 252
let s:cterm05 = 253
let s:cterm06 = 15
let s:cterm07 = 15
let s:cterm08 = 9
let s:cterm09 = 202
let s:cterm0A = 215
let s:cterm0B = 149
let s:cterm0C = 115
let s:cterm0D = 74
let s:cterm0E = 175
let s:cterm0F = 131

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_bright#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_bright#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_bright#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_bright#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_bright#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_bright#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
