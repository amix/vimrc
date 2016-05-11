" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 harmonic16 Scheme by Jannik Siebert (https://github.com/janniks)
let g:airline#themes#base16_harmonic16#palette = {}
let s:gui00 = "#0b1c2c"
let s:gui01 = "#223b54"
let s:gui02 = "#405c79"
let s:gui03 = "#627e99"
let s:gui04 = "#aabcce"
let s:gui05 = "#cbd6e2"
let s:gui06 = "#e5ebf1"
let s:gui07 = "#f7f9fb"
let s:gui08 = "#bf8b56"
let s:gui09 = "#bfbf56"
let s:gui0A = "#8bbf56"
let s:gui0B = "#56bf8b"
let s:gui0C = "#568bbf"
let s:gui0D = "#8b56bf"
let s:gui0E = "#bf568b"
let s:gui0F = "#bf5656"

let s:cterm00 = 0
let s:cterm01 = 23
let s:cterm02 = 60
let s:cterm03 = 66
let s:cterm04 = 146
let s:cterm05 = 188
let s:cterm06 = 189
let s:cterm07 = 15
let s:cterm08 = 137
let s:cterm09 = 143
let s:cterm0A = 107
let s:cterm0B = 72
let s:cterm0C = 67
let s:cterm0D = 97
let s:cterm0E = 132
let s:cterm0F = 131

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_harmonic16#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_harmonic16#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_harmonic16#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_harmonic16#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_harmonic16#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_harmonic16#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
