" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Twilight Scheme by David Hart (http://hart-dev.com)
let g:airline#themes#base16_twilight#palette = {}
let s:gui00 = "#1e1e1e"
let s:gui01 = "#323537"
let s:gui02 = "#464b50"
let s:gui03 = "#5f5a60"
let s:gui04 = "#838184"
let s:gui05 = "#a7a7a7"
let s:gui06 = "#c3c3c3"
let s:gui07 = "#ffffff"
let s:gui08 = "#cf6a4c"
let s:gui09 = "#cda869"
let s:gui0A = "#f9ee98"
let s:gui0B = "#8f9d6a"
let s:gui0C = "#afc4db"
let s:gui0D = "#7587a6"
let s:gui0E = "#9b859d"
let s:gui0F = "#9b703f"

let s:cterm00 = 234
let s:cterm01 = 59
let s:cterm02 = 59
let s:cterm03 = 59
let s:cterm04 = 102
let s:cterm05 = 248
let s:cterm06 = 251
let s:cterm07 = 15
let s:cterm08 = 167
let s:cterm09 = 179
let s:cterm0A = 228
let s:cterm0B = 107
let s:cterm0C = 152
let s:cterm0D = 103
let s:cterm0E = 103
let s:cterm0F = 95

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_twilight#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_twilight#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_twilight#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_twilight#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_twilight#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_twilight#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
