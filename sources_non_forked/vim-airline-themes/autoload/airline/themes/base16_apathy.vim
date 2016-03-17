" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Apathy Scheme by Jannik Siebert (https://github.com/janniks)
let g:airline#themes#base16_apathy#palette = {}
let s:gui00 = "#031A16"
let s:gui01 = "#0B342D"
let s:gui02 = "#184E45"
let s:gui03 = "#2B685E"
let s:gui04 = "#5F9C92"
let s:gui05 = "#81B5AC"
let s:gui06 = "#A7CEC8"
let s:gui07 = "#D2E7E4"
let s:gui08 = "#3E9688"
let s:gui09 = "#3E7996"
let s:gui0A = "#3E4C96"
let s:gui0B = "#883E96"
let s:gui0C = "#963E4C"
let s:gui0D = "#96883E"
let s:gui0E = "#4C963E"
let s:gui0F = "#3E965B"

let s:cterm00 = 0
let s:cterm01 = 22
let s:cterm02 = 23
let s:cterm03 = 23
let s:cterm04 = 72
let s:cterm05 = 109
let s:cterm06 = 152
let s:cterm07 = 188
let s:cterm08 = 66
let s:cterm09 = 66
let s:cterm0A = 60
let s:cterm0B = 96
let s:cterm0C = 95
let s:cterm0D = 101
let s:cterm0E = 65
let s:cterm0F = 65

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_apathy#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_apathy#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_apathy#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_apathy#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_apathy#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_apathy#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
