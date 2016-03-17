" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Atelier Heath Scheme by Bram de Haan (http://atelierbram.github.io/syntax-highlighting/atelier-schemes/heath)
let g:airline#themes#base16_atelierheath#palette = {}
let s:gui00 = "#1b181b"
let s:gui01 = "#292329"
let s:gui02 = "#695d69"
let s:gui03 = "#776977"
let s:gui04 = "#9e8f9e"
let s:gui05 = "#ab9bab"
let s:gui06 = "#d8cad8"
let s:gui07 = "#f7f3f7"
let s:gui08 = "#ca402b"
let s:gui09 = "#a65926"
let s:gui0A = "#bb8a35"
let s:gui0B = "#379a37"
let s:gui0C = "#159393"
let s:gui0D = "#516aec"
let s:gui0E = "#7b59c0"
let s:gui0F = "#cc33cc"

let s:cterm00 = 0
let s:cterm01 = 0
let s:cterm02 = 59
let s:cterm03 = 96
let s:cterm04 = 139
let s:cterm05 = 139
let s:cterm06 = 188
let s:cterm07 = 15
let s:cterm08 = 166
let s:cterm09 = 130
let s:cterm0A = 137
let s:cterm0B = 65
let s:cterm0C = 30
let s:cterm0D = 12
let s:cterm0E = 97
let s:cterm0F = 170

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierheath#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierheath#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierheath#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierheath#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_atelierheath#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_atelierheath#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
