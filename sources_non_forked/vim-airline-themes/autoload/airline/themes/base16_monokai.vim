" vim-airline template by chartoin (http://github.com/chartoin)
" Base 16 Monokai Scheme by Wimer Hazenberg (http://www.monokai.nl)
let g:airline#themes#base16_monokai#palette = {}
let s:gui00 = "#272822"
let s:gui01 = "#383830"
let s:gui02 = "#49483e"
let s:gui03 = "#75715e"
let s:gui04 = "#a59f85"
let s:gui05 = "#f8f8f2"
let s:gui06 = "#f5f4f1"
let s:gui07 = "#f9f8f5"
let s:gui08 = "#f92672"
let s:gui09 = "#fd971f"
let s:gui0A = "#f4bf75"
let s:gui0B = "#a6e22e"
let s:gui0C = "#a1efe4"
let s:gui0D = "#66d9ef"
let s:gui0E = "#ae81ff"
let s:gui0F = "#cc6633"

let s:cterm00 = 0
let s:cterm01 = 59
let s:cterm02 = 59
let s:cterm03 = 95
let s:cterm04 = 144
let s:cterm05 = 15
let s:cterm06 = 15
let s:cterm07 = 15
let s:cterm08 = 197
let s:cterm09 = 208
let s:cterm0A = 216
let s:cterm0B = 3
let s:cterm0C = 158
let s:cterm0D = 81
let s:cterm0E = 141
let s:cterm0F = 167

let s:N1   = [ s:gui01, s:gui0B, s:cterm01, s:cterm0B ]
let s:N2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:N3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_monokai#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)

let s:I1   = [ s:gui01, s:gui0D, s:cterm01, s:cterm0D ]
let s:I2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:I3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_monokai#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)

let s:R1   = [ s:gui01, s:gui08, s:cterm01, s:cterm08 ]
let s:R2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:R3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_monokai#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

let s:V1   = [ s:gui01, s:gui0E, s:cterm01, s:cterm0E ]
let s:V2   = [ s:gui06, s:gui02, s:cterm06, s:cterm02 ]
let s:V3   = [ s:gui09, s:gui01, s:cterm09, s:cterm01 ]
let g:airline#themes#base16_monokai#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)

let s:IA1   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA2   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let s:IA3   = [ s:gui05, s:gui01, s:cterm05, s:cterm01 ]
let g:airline#themes#base16_monokai#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" Here we define the color map for ctrlp.  We check for the g:loaded_ctrlp
" variable so that related functionality is loaded iff the user is using
" ctrlp. Note that this is optional, and if you do not define ctrlp colors
" they will be chosen automatically from the existing palette.
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#base16_monokai#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ [ s:gui07, s:gui02, s:cterm07, s:cterm02, '' ],
      \ [ s:gui07, s:gui04, s:cterm07, s:cterm04, '' ],
      \ [ s:gui05, s:gui01, s:cterm05, s:cterm01, 'bold' ])
