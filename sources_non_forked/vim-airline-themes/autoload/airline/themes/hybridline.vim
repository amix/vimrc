" vim-airline theme based on vim-hybrid and powerline
" (https://github.com/w0ng/vim-hybrid)
" (https://github.com/Lokaltog/powerline)

let g:airline#themes#hybridline#palette = {}

let s:N1 = [ '#282a2e' , '#c5c8c6' , 'black' , 15      ]
let s:N2 = [ '#c5c8c6' , '#373b41' , 15      , 8       ]
let s:N3 = [ '#ffffff' , '#282a2e' , 255     , 'black' ]
let g:airline#themes#hybridline#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#hybridline#palette.normal.airline_a = ['#005f00', '#b5bd68', 22, 10, '']

let s:I1 = [ '#005f5f' , '#8abeb7' , 23  , 14 ]
let s:I2 = [ '#c5c8c6' , '#0087af' , 15  , 31 ]
let s:I3 = [ '#ffffff' , '#005f87' , 255 , 24 ]
let g:airline#themes#hybridline#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#hybridline#palette.insert_paste = {
            \ 'airline_a': ['#000000', '#ac4142', 16 , 1, ''] ,
            \ }

let g:airline#themes#hybridline#palette.replace = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#hybridline#palette.replace.airline_a = ['#000000', '#CC6666', 16, 9]

let g:airline#themes#hybridline#palette.visual = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#hybridline#palette.visual.airline_a = ['#000000', '#de935f', 16, 3]

let s:IA1 = [ '#4e4e4e' , '#1c1c1c' , 239 , 234 , '' ]
let s:IA2 = [ '#4e4e4e' , '#262626' , 239 , 235 , '' ]
let s:IA3 = [ '#4e4e4e' , '#303030' , 239 , 236 , '' ]
let g:airline#themes#hybridline#palette.inactive = airline#themes#generate_color_map(s:IA1, s:IA2, s:IA3)

let g:airline#themes#hybridline#palette.accents = {
      \ 'red': [ '#ff0000' , '' , 160 , ''  ]
      \ }
