" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Solarized Scheme by Ethan Schoonover (http://ethanschoonover.com/solarized)
let g:airline#themes#base16_solarized#palette = {}
let s:gui00 = "#002b36"
let s:gui01 = "#073642"
let s:gui02 = "#586e75"
let s:gui03 = "#657b83"
let s:gui04 = "#839496"
let s:gui05 = "#93a1a1"
let s:gui06 = "#eee8d5"
let s:gui07 = "#fdf6e3"
let s:gui08 = "#dc322f"
let s:gui09 = "#cb4b16"
let s:gui0A = "#b58900"
let s:gui0B = "#859900"
let s:gui0C = "#2aa198"
let s:gui0D = "#268bd2"
let s:gui0E = "#6c71c4"
let s:gui0F = "#d33682"

let s:cterm00 = 17
let s:cterm01 = 23
let s:cterm02 = 60
let s:cterm03 = 66
let s:cterm04 = 102
let s:cterm05 = 109
let s:cterm06 = 224
let s:cterm07 = 230
let s:cterm08 = 166
let s:cterm09 = 166
let s:cterm0A = 136
let s:cterm0B = 100
let s:cterm0C = 36
let s:cterm0D = 32
let s:cterm0E = 12
let s:cterm0F = 168

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_solarized#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_solarized#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_solarized#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_solarized#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_solarized#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_solarized#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
