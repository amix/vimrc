" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Atelier Seaside Scheme by Bram de Haan (http://atelierbram.github.io/syntax-highlighting/atelier-schemes/seaside/)
let g:airline#themes#base16_atelierseaside#palette = {}
let s:gui00 = "#131513"
let s:gui01 = "#242924"
let s:gui02 = "#5e6e5e"
let s:gui03 = "#687d68"
let s:gui04 = "#809980"
let s:gui05 = "#8ca68c"
let s:gui06 = "#cfe8cf"
let s:gui07 = "#f0fff0"
let s:gui08 = "#e6193c"
let s:gui09 = "#87711d"
let s:gui0A = "#c3c322"
let s:gui0B = "#29a329"
let s:gui0C = "#1999b3"
let s:gui0D = "#3d62f5"
let s:gui0E = "#ad2bee"
let s:gui0F = "#e619c3"

let s:cterm00 = 0
let s:cterm01 = 0
let s:cterm02 = 59
let s:cterm03 = 65
let s:cterm04 = 102
let s:cterm05 = 108
let s:cterm06 = 188
let s:cterm07 = 15
let s:cterm08 = 161
let s:cterm09 = 94
let s:cterm0A = 3
let s:cterm0B = 2
let s:cterm0C = 31
let s:cterm0D = 12
let s:cterm0E = 129
let s:cterm0F = 5

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierseaside#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierseaside#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierseaside#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierseaside#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_atelierseaside#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_atelierseaside#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
