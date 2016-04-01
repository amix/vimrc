" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Railscasts Scheme by Ryan Bates (http://railscasts.com)
let g:airline#themes#base16_railscasts#palette = {}
let s:gui00 = "#2b2b2b"
let s:gui01 = "#272935"
let s:gui02 = "#3a4055"
let s:gui03 = "#5a647e"
let s:gui04 = "#d4cfc9"
let s:gui05 = "#e6e1dc"
let s:gui06 = "#f4f1ed"
let s:gui07 = "#f9f7f3"
let s:gui08 = "#da4939"
let s:gui09 = "#cc7833"
let s:gui0A = "#ffc66d"
let s:gui0B = "#a5c261"
let s:gui0C = "#519f50"
let s:gui0D = "#6d9cbe"
let s:gui0E = "#b6b3eb"
let s:gui0F = "#bc9458"

let s:cterm00 = 235
let s:cterm01 = 17
let s:cterm02 = 59
let s:cterm03 = 60
let s:cterm04 = 188
let s:cterm05 = 188
let s:cterm06 = 15
let s:cterm07 = 15
let s:cterm08 = 167
let s:cterm09 = 173
let s:cterm0A = 221
let s:cterm0B = 143
let s:cterm0C = 71
let s:cterm0D = 73
let s:cterm0E = 146
let s:cterm0F = 137

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_railscasts#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_railscasts#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_railscasts#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_railscasts#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_railscasts#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_railscasts#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
