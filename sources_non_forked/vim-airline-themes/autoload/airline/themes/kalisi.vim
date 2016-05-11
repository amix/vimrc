"
" Colorscheme: Kalisi for airline. Inspired by powerline.
" Arthur Jaron
" hifreeo@gmail.com
" 24.10.2014 
 
" Visual mode                                   
let s:V1 = [ '#0087ff' , '#ffffff','33','231']
let s:V2 = [ '#005faf' , '#5fafff','25','75']
let s:V3 = [ '#87d7ff' , '#005faf','117','25']

" Replace mode
let s:R1 = [ '#d75fff' , '#ffffff','171','231']
let s:R2 = [ '#5f005f' , '#d75fff','53','171']
let s:R3 = [ '#ff87ff' , '#8700af','213','91']

let g:airline#themes#kalisi#palette = {}


function! airline#themes#kalisi#refresh()

  let s:StatusLine = airline#themes#get_highlight('StatusLine')
  let s:StatusLineNC = airline#themes#get_highlight('StatusLineNC')

  " Insert mode                                    
  let s:I1 = [ '#ffffff' , '#e80000','231','160']
  let s:I2 = [ '#ff0000' , '#5f0000','196','52']
  let s:I3 = s:StatusLine

  " Normal mode
  let s:N1 = [ '#005f00' , '#afd700','22','148'] 
  let s:N2 = [ '#afd700' , '#005f00','148','22'] 
  let s:N3 = s:StatusLine

  " Tabline Plugin
  let g:airline#themes#kalisi#palette.tabline = {
        \ 'airline_tab':    ['#bcbcbc', '#005f00','250','22'],
        \ 'airline_tabsel': ['#404042', '#A6DB29','238','148'],
        \ 'airline_tabtype':['#afd700', '#204d20','148','22'],
        \ 'airline_tabfill': s:StatusLine,
        \ 'airline_tabhid': ['#c5c5c5', '#404042','251','238'],
        \ 'airline_tabmod': ['#d7ff00', '#afd700','190','148'],
        \ 'airline_tabmod_unsel':  ['#d7ff00', '#005f00','190','22']
        \ }

  let g:airline#themes#kalisi#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
  let g:airline#themes#kalisi#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
  let g:airline#themes#kalisi#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
  let g:airline#themes#kalisi#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)

  " Inactive Mode
  let s:IA = airline#themes#get_highlight('StatusLineNC')
  let g:airline#themes#kalisi#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)
  let g:airline#themes#kalisi#palette.inactive_modified = {
        \ 'airline_c':  ['#d7ff00', s:IA[1],'190',s:IA[3]],
        \ }

endfunction

call airline#themes#kalisi#refresh()

if !get(g:, 'loaded_ctrlp', 0)
  finish
endif
let g:airline#themes#kalisi#palette.ctrlp = airline#extensions#ctrlp#generate_color_map(
      \ s:StatusLine,
      \ ['#afd700', '#005f00','148','22'],
      \ [ '#005f00' , '#afd700' , '22','148']
      \)

