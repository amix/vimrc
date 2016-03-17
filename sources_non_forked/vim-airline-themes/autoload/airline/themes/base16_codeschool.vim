" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Codeschool Scheme by brettof86
let g:airline#themes#base16_codeschool#palette = {}
let s:gui00 = "#232c31"
let s:gui01 = "#1c3657"
let s:gui02 = "#2a343a"
let s:gui03 = "#3f4944"
let s:gui04 = "#84898c"
let s:gui05 = "#9ea7a6"
let s:gui06 = "#a7cfa3"
let s:gui07 = "#b5d8f6"
let s:gui08 = "#2a5491"
let s:gui09 = "#43820d"
let s:gui0A = "#a03b1e"
let s:gui0B = "#237986"
let s:gui0C = "#b02f30"
let s:gui0D = "#484d79"
let s:gui0E = "#c59820"
let s:gui0F = "#c98344"

let s:cterm00 = 17
let s:cterm01 = 23
let s:cterm02 = 23
let s:cterm03 = 59
let s:cterm04 = 102
let s:cterm05 = 145
let s:cterm06 = 151
let s:cterm07 = 153
let s:cterm08 = 24
let s:cterm09 = 64
let s:cterm0A = 130
let s:cterm0B = 30
let s:cterm0C = 125
let s:cterm0D = 60
let s:cterm0E = 172
let s:cterm0F = 173

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_codeschool#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_codeschool#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_codeschool#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_codeschool#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_codeschool#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_codeschool#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
