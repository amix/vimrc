set cmdheight=1
nmap <leader>q :q<cr>

set gfn=Source\ Code\ Pro\ for\ Powerline\ Medium\ 11
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerd Tree Config
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Syntastic
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_asm_checkers = ['nasm']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Airline Theme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_theme='molokai'
let g:airline_powerline_fonts = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-color-solarised
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("mac") || has("macunix")
    let g:solarized_termcolors=256
endif

syntax enable
set background=dark
colorscheme solarized

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"LightLine
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"let g:lightline = {
      "\ 'active': {
      "\   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['ctrlpmark'] ],
      "\   'right': [ [ 'syntastic', 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
      "\ },
      "\ 'component_function': {
      "\   'fugitive': 'LightLineFugitive',
      "\   'filename': 'LightLineFilename',
      "\   'fileformat': 'LightLineFileformat',
      "\   'filetype': 'LightLineFiletype',
      "\   'fileencoding': 'LightLineFileencoding',
      "\   'mode': 'LightLineMode',
      "\   'ctrlpmark': 'CtrlPMark',
      "\ },
      "\ 'component_expand': {
      "\   'syntastic': 'SyntasticStatuslineFlag',
      "\ },
      "\ 'component_type': {
      "\   'syntastic': 'error',
      "\ },
      "\ 'separator': { 'left': '', 'right': '' },
      "\ 'subseparator': { 'left': '\ue0b1', 'right': '\ueb03' }
      "\ }

"function! LightLineModified()
    "return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
"endfunction

"function! LightLineReadonly()
  "return &ft !~? 'help' && &readonly ? '' : ''
"endfunction

"function! LightLineFilename()
  "let fname = expand('%:t')
    "return fname == 'ControlP' && has_key(g:lightline, 'ctrlp_item') ? g:lightline.ctrlp_item :
            "\ fname == '__Tagbar__' ? g:lightline.fname :
            "\ fname =~ '__Gundo\|NERD_tree' ? '' :
            "\ &ft == 'vimfiler' ? vimfiler#get_status_string() :
            "\ &ft == 'unite' ? unite#get_status_string() :
            "\ &ft == 'vimshell' ? vimshell#get_status_string() :
            "\ ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
            "\ ('' != fname ? fname : '[No Name]') .
            "\ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
"endfunction

"function! LightLineFugitive()
      "try
              "if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
                    "let mark = ''
                    "let branch = fugitive#head()
                    "return branch !=# '' ? mark.branch : ''
              "endif
      "catch
      "endtry
      "return ''
 "endfunction

 "function! LightLineFileformat()
       "return winwidth(0) > 70 ? &fileformat . ' ' . WebDevIconsGetFileFormatSymbol() : ''
 "endfunction

 "function! LightLineFiletype()
       "return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype. ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
 "endfunction

 "function! LightLineFileencoding()
       "return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
 "endfunction

 "function! LightLineMode()
       "let fname = expand('%:t')
       "return fname == '__Tagbar__' ? 'Tagbar' :
            "\ fname == 'ControlP' ? 'CtrlP' :
            "\ fname == '__Gundo__' ? 'Gundo' :
            "\ fname == '__Gundo_Preview__' ? 'Gundo Preview' :
            "\ fname =~ 'NERD_tree' ? 'NERDTree' :
            "\ &ft == 'unite' ? 'Unite' :
            "\ &ft == 'vimfiler' ? 'VimFiler' :
            "\ &ft == 'vimshell' ? 'VimShell' :
            "\ winwidth(0) > 60 ? lightline#mode() : ''
 "endfunction
 "function! CtrlPMark()
        "if expand('%:t') =~ 'ControlP' && has_key(g:lightline, 'ctrlp_item')
            "call lightline#link('iR'[g:lightline.ctrlp_regex])
            "return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
                "\ , g:lightline.ctrlp_next], 0)
        "else
            "return ''
        "endif
"endfunction

"let g:ctrlp_status_func = {
"\ 'main': 'CtrlPStatusFunc_1',
"\ 'prog': 'CtrlPStatusFunc_2',
"\ }

"function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
"let g:lightline.ctrlp_regex = a:regex
"let g:lightline.ctrlp_prev = a:prev
"let g:lightline.ctrlp_item = a:item
"let g:lightline.ctrlp_next = a:next
"return lightline#statusline(0)
"endfunction

"function! CtrlPStatusFunc_2(str)
"return lightline#statusline(0)
"endfunction
"let g:tagbar_status_func = 'TagbarStatusFunc'

"function! TagbarStatusFunc(current, sort, fname, ...) abort
    "let g:lightline.fname = a:fname
    "return lightline#statusline(0)
"endfunction

"augroup AutoSyntastic
"autocmd!
"autocmd BufWritePost *.c,*.cpp call s:syntastic()
"augroup END
"function! s:syntastic()
"SyntasticCheck
"call lightline#update()
"endfunction

"let g:unite_force_overwrite_statusline = 0
"let g:vimfiler_force_overwrite_statusline = 0
"let g:vimshell_force_overwrite_statusline = 0

let g:lightline = {
            \ 'colorscheme': 'landscape',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['ctrlpmark'] ],
            \   'right': [ [ 'syntastic', 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
            \ },
            \ 'component_function': {
            \   'fugitive': 'LightlineFugitive',
            \   'filename': 'LightlineFilename',
            \   'fileformat': 'LightlineFileformat',
            \   'filetype': 'LightlineFiletype',
            \   'fileencoding': 'LightlineFileencoding',
            \   'mode': 'LightlineMode',
            \   'ctrlpmark': 'CtrlPMark',
            \ },
            \ 'component_expand': {
            \   'syntastic': 'SyntasticStatuslineFlag',
            \ },
            \ 'component_type': {
            \   'syntastic': 'error',
            \ },
            \ 'separator': { 'left': '', 'right': '' },
            \ 'subseparator': { 'left': '\ue0b1', 'right': '\ue0b3' }
            \ }

function! LightlineModified()
	return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help' && &readonly ? '' : ''
endfunction

function! LightlineFilename()
    let fname = expand('%:t')
    return fname == 'ControlP' && has_key(g:lightline, 'ctrlp_item') ? g:lightline.ctrlp_item :
                \ fname == '__Tagbar__' ? g:lightline.fname :
                \ fname =~ '__Gundo\|NERD_tree' ? '' :
                \ &ft == 'vimfiler' ? vimfiler#get_status_string() :
                \ &ft == 'unite' ? unite#get_status_string() :
                \ &ft == 'vimshell' ? vimshell#get_status_string() :
                \ ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
                \ ('' != fname ? fname : '[No Name]') .
                \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
    try
        if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
            let mark = ''  " edit here for cool mark
            let branch = fugitive#head()
            return branch !=# '' ? mark.branch : ''
        endif
    catch
    endtry
    return ''
endfunction

function! LightlineFileformat()
    return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
    return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileencoding()
    return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
    let fname = expand('%:t')
    return fname == '__Tagbar__' ? 'Tagbar' :
                \ fname == 'ControlP' ? 'CtrlP' :
                \ fname == '__Gundo__' ? 'Gundo' :
                \ fname == '__Gundo_Preview__' ? 'Gundo Preview' :
                \ fname =~ 'NERD_tree' ? 'NERDTree' :
                \ &ft == 'unite' ? 'Unite' :
                \ &ft == 'vimfiler' ? 'VimFiler' :
                \ &ft == 'vimshell' ? 'VimShell' :
                \ winwidth(0) > 60 ? lightline#mode() : ''
endfunction

function! CtrlPMark()
    if expand('%:t') =~ 'ControlP' && has_key(g:lightline, 'ctrlp_item')
        call lightline#link('iR'[g:lightline.ctrlp_regex])
        return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
                    \ , g:lightline.ctrlp_next], 0)
    else
        return ''
    endif
endfunction

let g:ctrlp_status_func = {
            \ 'main': 'CtrlPStatusFunc_1',
            \ 'prog': 'CtrlPStatusFunc_2',
            \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
    let g:lightline.ctrlp_regex = a:regex
    let g:lightline.ctrlp_prev = a:prev
    let g:lightline.ctrlp_item = a:item
    let g:lightline.ctrlp_next = a:next
    return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
    return lightline#statusline(0)
endfunction

let g:tagbar_status_func = 'TagbarStatusFunc'

function! TagbarStatusFunc(current, sort, fname, ...) abort
    let g:lightline.fname = a:fname
    return lightline#statusline(0)
endfunction

augroup AutoSyntastic
    autocmd!
    autocmd BufWritePost *.c,*.cpp call s:syntastic()
augroup END
function! s:syntastic()
    SyntasticCheck
    call lightline#update()
endfunction

let g:unite_force_overwrite_statusline = 0
let g:vimfiler_force_overwrite_statusline = 0
let g:vimshell_force_overwrite_statusline = 0
