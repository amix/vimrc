" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 3024 Scheme by Jan T. Sott (http://github.com/idleberg)
let g:airline#themes#base16_3024#palette = {}
let s:gui00 = "#090300"
let s:gui01 = "#3a3432"
let s:gui02 = "#4a4543"
let s:gui03 = "#5c5855"
let s:gui04 = "#807d7c"
let s:gui05 = "#a5a2a2"
let s:gui06 = "#d6d5d4"
let s:gui07 = "#f7f7f7"
let s:gui08 = "#db2d20"
let s:gui09 = "#e8bbd0"
let s:gui0A = "#fded02"
let s:gui0B = "#01a252"
let s:gui0C = "#b5e4f4"
let s:gui0D = "#01a0e4"
let s:gui0E = "#a16a94"
let s:gui0F = "#cdab53"

let s:cterm00 = 0
let s:cterm01 = 59
let s:cterm02 = 59
let s:cterm03 = 59
let s:cterm04 = 102
let s:cterm05 = 145
let s:cterm06 = 188
let s:cterm07 = 15
let s:cterm08 = 1
let s:cterm09 = 182
let s:cterm0A = 11
let s:cterm0B = 35
let s:cterm0C = 153
let s:cterm0D = 38
let s:cterm0E = 132
let s:cterm0F = 179

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_3024#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_3024#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_3024#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_3024#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_3024#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_3024#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
