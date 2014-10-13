"
" Colorscheme: Kalisi for airline. Inspired by powerline.
" Arthur Jaron
" hifreeo@gmail.com
" 30.07.2014 
 

" Insert mode                                    
let s:I1 = [ '#ffffff' , '#e80000','','']
let s:I2 = [ '#c5c5c5' , '#901010','','']
let s:I3 = [ '#c5c5c5' , '#500000','','']

" Visual mode                                   
let s:V1 = [ '#2a5d8e' , '#ffffff','','']
let s:V2 = [ '#87e7ff' , '#4077df','','']
let s:V3 = [ '#87e7ff' , '#2a5d8e','','']

" Replace mode
let s:R1 = [ '#6e00ba' , '#ffffff','','']
let s:R2 = [ '#6e00ba' , '#d358ff','','']
let s:R3 = [ '#ce99ff' , '#6e00ba','','']

let g:airline#themes#kalisi#palette = {}
let g:airline#themes#kalisi#palette.accents = {'red': ['#FF0000', '', 88, '']}


function! airline#themes#kalisi#refresh()

  let s:StatusLine = airline#themes#get_highlight('StatusLine')
  let s:StatusLineNC = airline#themes#get_highlight('StatusLineNC')

  " Normal mode
  let s:N1 = [ '#005f00' , '#afd700','',''] 
  let s:N2 = [ '#afd700' , '#005f00','',''] 
  let s:N3 = s:StatusLine


  " Tabline Plugin
  let g:airline#themes#kalisi#palette.tabline = {
        \ 'airline_tab':  ['#A6DB29', '#005f00','',''],
        \ 'airline_tabsel':  ['#404042', '#A6DB29','',''],
        \ 'airline_tabtype':  ['#afd700', '#204d20','',''],
        \ 'airline_tabfill':  s:StatusLine,
        \ 'airline_tabhid':  ['#c5c5c5', '#404042','',''],
        \ 'airline_tabmod':  ['#ffffff', '#F1266F','','']
        \ }

        " \ 'airline_tabfill':  ['#ffffff', '#2b2b2b','',''],
        
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

if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#kalisi#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ s:StatusLineNC,
      \ s:StatusLine,
      \ [ '#005f00' , '#afd700' , '','', ''] )

