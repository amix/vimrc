let g:airline#themes#murmur#palette = {}

let s:termbg = 237  " Background for branch and file format blocks
let s:termbg2= 234  " Background for middle block
let s:normalbg= 27  " Background for normal mode and file position blocks
let s:insertbg= 70  " Background for insert mode and file position blocks
let s:visualbg= 166 " Background for visual mode and file position blocks
let s:replacebg=88  " Background for replace mode and file position blocks
let s:alert= 88     " Modefied file alert color

let s:BB = ['#AFAF87', '#5F5F5F', 144, s:termbg] " Branch and file format blocks

let s:N1 = ['#FFFFFF', '#5F87FF', 15, s:normalbg]   " Outside blocks in normal mode
let s:N2 = ['#AFAF87', '#5F5F5F', 39, s:termbg2]    " Middle block
let g:airline#themes#murmur#palette.normal = airline#themes#generate_color_map(s:N1, s:BB, s:N2)
let g:airline#themes#murmur#palette.normal_modified = {'airline_c': ['#ffffff', '#5f005f', s:alert, s:termbg2, 'bold'] ,}

let s:I1 = ['#FFFFFF', '#87AF5F', 15, s:insertbg]         " Outside blocks in insert mode
let s:I2 = ['#AFAF87', '#5F5F5F', s:insertbg, s:termbg2]  " Middle block
let g:airline#themes#murmur#palette.insert = airline#themes#generate_color_map(s:I1, s:BB, s:I2)
let g:airline#themes#murmur#palette.insert_modified = {'airline_c': ['#AFAF87', '#5F5F5F', s:alert, s:termbg2, 'bold'] ,}

let s:R1 = ['#FFFFFF', '#870000', 15, s:replacebg]  " Outside blocks in replace mode
let s:R2 = ['#AFAF87', '#5F5F5F', 255, s:termbg2]   " Middle block
let g:airline#themes#murmur#palette.replace = airline#themes#generate_color_map(s:R1, s:BB, s:R2)
let g:airline#themes#murmur#palette.replace_modified = {'airline_c': ['#AFAF87', '#5f005f', s:alert, s:termbg2, 'bold'] ,}

let s:V1 = ['#FFFFFF', '#AF5F00', 15, s:visualbg]         " Outside blocks in visual mode
let s:V2 = ['#AFAF87', '#5F5F5F', s:visualbg, s:termbg2]  " Middle block
let g:airline#themes#murmur#palette.visual = airline#themes#generate_color_map(s:V1, s:BB, s:V2)
let g:airline#themes#murmur#palette.visual_modified = {'airline_c': [ '#AFAF87', '#5f005f', s:alert, s:termbg2, 'bold'] ,}

" Inactive mode
let s:IA1 = ['#4E4E4E', '#1C1C1C', 239, 234, '']
let s:IA2 = ['#4E4E4E', '#1C1C1C', 239, 234, '']
let s:IA3 = ['#4E4E4E', '#1C1C1C', 239, 234, '']
let g:airline#themes#murmur#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

" CtrlP plugin colors
if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#murmur#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ ['#FFFFFF', '#5F87FF', 15, s:normalbg, ''],
      \ ['#AFAF87', '#5F5F5F', 144, s:termbg, ''],
      \ ['#AFAF87', '#5F5F5F', 39, s:termbg2, 'bold'])
