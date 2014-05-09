"
" Colorscheme: Kalisi for airline. Inspired by powerline.
" 06.02.2014 Arthur Jaron
" hifreeo@gmail.com
" 

" Insert mode                                    
let s:I1 = [ '#ffffff' , '#e80000' , 23  , 231 ] 
let s:I2 = [ '#c5c5c5' , '#901010' , 74  , 31  ] 
let s:I3 = [ '#c5c5c5' , '#500000' , 117 , 24  ] 

" Visual mode                                    
let s:V1 = [ '#005f5f' , '#ffffff' , 23  , 231 ] 
let s:V2 = [ '#5fafd7' , '#0087af' , 74  , 31  ] 
let s:V3 = [ '#87d7ff' , '#005f87' , 117 , 24  ] 

" Replace mode
let s:R1 = [ '#8e00da' , '#ffffff' , 23  , 231 ] 
let s:R2 = [ '#8e00da' , '#ce99ff' , 74  , 31  ] 
let s:R3 = [ '#ce99ff' , '#8e00da' , 117 , 24  ] 

let g:airline#themes#kalisi#palette = {}

function! airline#themes#kalisi#refresh()

  " Normal mode
  let s:N1 = [ '#005f00' , '#afd700' , 22  , 148 ]
  let s:N2 = [ '#afd700' , '#005f00' , 247 , 236 ]
  let s:N3 = airline#themes#get_highlight('StatusLine')

  " Tabline Plugin
  let g:airline#themes#kalisi#palette.tabline = {
        \ 'airline_tab':  ['#A6DB29', '#005f00',  231, 29, ''],
        \ 'airline_tabsel':  ['#404042', '#A6DB29',  231, 36, ''],
        \ 'airline_tabtype':  ['#afd700', '#005f00',  231, 36, ''],
        \ 'airline_tabfill':  ['#ffffff', '#000000',  231, 23, ''],
        \ 'airline_tabhid':  ['#c5c5c5', '#404042',  231, 88, ''],
        \ 'airline_tabmod':  ['#ffffff', '#F1266F',  231, 88, ''],
        \ }

  let g:airline#themes#kalisi#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
  let g:airline#themes#kalisi#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
  let g:airline#themes#kalisi#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
  let g:airline#themes#kalisi#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

  " Inactive Mode
  " let s:IA = [ '#c5c5c5' , '#505052' , 239 , 234 , '' ]
  let s:IA = airline#themes#get_highlight('StatusLineNC')
  let g:airline#themes#kalisi#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
  let g:airline#themes#kalisi#palette.inactive_modified = {
        \ 'statusline': [ '#F1266F' , '' , '53' , '' , '' ] ,
        \ }

endfunction

call airline#themes#kalisi#refresh()

