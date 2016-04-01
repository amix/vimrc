" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Colors Scheme by mrmrs (http://clrs.cc)
let g:airline#themes#base16_colors#palette = {}
let s:gui00 = "#111111"
let s:gui01 = "#333333"
let s:gui02 = "#555555"
let s:gui03 = "#777777"
let s:gui04 = "#999999"
let s:gui05 = "#bbbbbb"
let s:gui06 = "#dddddd"
let s:gui07 = "#ffffff"
let s:gui08 = "#ff4136"
let s:gui09 = "#ff851b"
let s:gui0A = "#ffdc00"
let s:gui0B = "#2ecc40"
let s:gui0C = "#7fdbff"
let s:gui0D = "#0074d9"
let s:gui0E = "#b10dc9"
let s:gui0F = "#85144b"

let s:cterm00 = 233
let s:cterm01 = 236
let s:cterm02 = 240
let s:cterm03 = 243
let s:cterm04 = 246
let s:cterm05 = 250
let s:cterm06 = 253
let s:cterm07 = 15
let s:cterm08 = 203
let s:cterm09 = 208
let s:cterm0A = 220
let s:cterm0B = 41
let s:cterm0C = 117
let s:cterm0D = 32
let s:cterm0E = 128
let s:cterm0F = 89

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_colors#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_colors#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_colors#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_colors#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_colors#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_colors#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
