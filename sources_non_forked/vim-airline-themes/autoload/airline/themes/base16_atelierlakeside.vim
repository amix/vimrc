" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Atelier Lakeside Scheme by Bram de Haan (http://atelierbram.github.io/syntax-highlighting/atelier-schemes/lakeside/)
let g:airline#themes#base16_atelierlakeside#palette = {}
let s:gui00 = "#161b1d"
let s:gui01 = "#1f292e"
let s:gui02 = "#516d7b"
let s:gui03 = "#5a7b8c"
let s:gui04 = "#7195a8"
let s:gui05 = "#7ea2b4"
let s:gui06 = "#c1e4f6"
let s:gui07 = "#ebf8ff"
let s:gui08 = "#d22d72"
let s:gui09 = "#935c25"
let s:gui0A = "#8a8a0f"
let s:gui0B = "#568c3b"
let s:gui0C = "#2d8f6f"
let s:gui0D = "#257fad"
let s:gui0E = "#5d5db1"
let s:gui0F = "#b72dd2"

let s:cterm00 = 0
let s:cterm01 = 0
let s:cterm02 = 60
let s:cterm03 = 66
let s:cterm04 = 67
let s:cterm05 = 109
let s:cterm06 = 153
let s:cterm07 = 195
let s:cterm08 = 161
let s:cterm09 = 94
let s:cterm0A = 100
let s:cterm0B = 65
let s:cterm0C = 29
let s:cterm0D = 31
let s:cterm0E = 61
let s:cterm0F = 5

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierlakeside#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierlakeside#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierlakeside#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_atelierlakeside#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_atelierlakeside#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_atelierlakeside#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
