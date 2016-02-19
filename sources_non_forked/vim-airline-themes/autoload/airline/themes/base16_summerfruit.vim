" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Summerfruit Scheme by Christopher Corley (http://cscorley.github.io/)
let g:airline#themes#base16_summerfruit#palette = {}
let s:gui00 = "#151515"
let s:gui01 = "#202020"
let s:gui02 = "#303030"
let s:gui03 = "#505050"
let s:gui04 = "#B0B0B0"
let s:gui05 = "#D0D0D0"
let s:gui06 = "#E0E0E0"
let s:gui07 = "#FFFFFF"
let s:gui08 = "#FF0086"
let s:gui09 = "#FD8900"
let s:gui0A = "#ABA800"
let s:gui0B = "#00C918"
let s:gui0C = "#1faaaa"
let s:gui0D = "#3777E6"
let s:gui0E = "#AD00A1"
let s:gui0F = "#cc6633"

let s:cterm00 = 233
let s:cterm01 = 234
let s:cterm02 = 236
let s:cterm03 = 239
let s:cterm04 = 249
let s:cterm05 = 252
let s:cterm06 = 253
let s:cterm07 = 15
let s:cterm08 = 198
let s:cterm09 = 208
let s:cterm0A = 142
let s:cterm0B = 2
let s:cterm0C = 37
let s:cterm0D = 68
let s:cterm0E = 127
let s:cterm0F = 167

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_summerfruit#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_summerfruit#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_summerfruit#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_summerfruit#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_summerfruit#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_summerfruit#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
