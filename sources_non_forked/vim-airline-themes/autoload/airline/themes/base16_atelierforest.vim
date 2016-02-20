" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Atelier Forest Scheme by Bram de Haan (http://atelierbram.github.io/syntax-highlighting/atelier-schemes/forest)
let g:airline#themes#base16_atelierforest#palette = {}
let s:gui00 = "#1b1918"
let s:gui01 = "#2c2421"
let s:gui02 = "#68615e"
let s:gui03 = "#766e6b"
let s:gui04 = "#9c9491"
let s:gui05 = "#a8a19f"
let s:gui06 = "#e6e2e0"
let s:gui07 = "#f1efee"
let s:gui08 = "#f22c40"
let s:gui09 = "#df5320"
let s:gui0A = "#d5911a"
let s:gui0B = "#5ab738"
let s:gui0C = "#00ad9c"
let s:gui0D = "#407ee7"
let s:gui0E = "#6666ea"
let s:gui0F = "#c33ff3"

let s:cterm00 = 0
let s:cterm01 = 0
let s:cterm02 = 59
let s:cterm03 = 95
let s:cterm04 = 138
let s:cterm05 = 145
let s:cterm06 = 188
let s:cterm07 = 15
let s:cterm08 = 197
let s:cterm09 = 166
let s:cterm0A = 172
let s:cterm0B = 71
let s:cterm0C = 37
let s:cterm0D = 68
let s:cterm0E = 62
let s:cterm0F = 135

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierforest#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierforest#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierforest#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierforest#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_atelierforest#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_atelierforest#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
