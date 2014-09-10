try
    colorscheme zenburn
catch
endtry

set t_Co=256
" UltiSnips configuration
function! g:UltiSnips_Complete()
    call UltiSnips#ExpandSnippet()
    if g:ulti_expand_res == 0
        if pumvisible()
            return "\<C-n>"
        else
            call UltiSnips#JumpForwards()
            if g:ulti_jump_forwards_res == 0
               return "\<TAB>"
            endif
        endif
    endif
    return ""
endfunction

au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsListSnippets="<c-e>"
" this mapping Enter key to <C-y> to chose the current highlight item
" and close the selection list, same as other IDEs.
" CONFLICT with some plugins like tpope/Endwise
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
" Trigger configuration. Do not use <tab> if you use
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" relative numbers and absolute numbers on current line
set relativenumber
set number

" Highlight current line
set cursorline


" IndentGuides options
let g:indent_guides_auto_colors = 0
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']

set ts=4 sw=4 et
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1

autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=238
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=238


" Syntastic configuration
execute pathogen#infect()
let g:syntastic_enable_signs=1
" let g:syntastic_auto_jump=1
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_check_on_open = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_python_checkers = ['flake8', 'frosted']
let g:syntastic_javascript_checkers = ['jshint']

" Ropevim configuration
let g:ropevim_autoimport_modules = ["os", "shutil", 'django', 'rest_framework', 'moody365']

" Mappings to access buffers (don't use "\p" because a
" delay before pressing "p" would accidentally paste).
" \g : go last-used
nnoremap <Leader>/ :e#<CR>

" No wordwrap
set nowrap

" NERDTree ignore *.pyc
let NERDTreeIgnore = ['\.pyc$']

" Enable TagBar
nmap <F8> :TagbarToggle<CR>
nmap <Leader>f :TagbarOpen fj<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Ag searching and cope displaying
"    requires ag.vim - it's much better than vimgrep/grep
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ackprg = 'ag --nogroup --nocolor --column'

map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
map <leader>n :cn<cr>
map <leader>p :cp<cr>


" Pymode vim settings

let g:pymode_trim_whitespaces = 0
let g:pymode_run = 0
let g:pymode_lint_checkers = ['mccabe']
let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 0
let g:pymode_rope_autoimport_modules = ["os.*","traceback","django.*","lxml.etree","lxml.*", "rest_framework.*"]
let g:pymode_rope_autoimport_import_after_complete = 1


" Remove trailing space on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

"Set tabs to spaces
set tabstop=4
set shiftwidth=4
set expandtab

"UltiSnipets in Tagbar
let g:tagbar_type_snippets = {
    \ 'ctagstype' : 'snippets',
    \ 'kinds' : [
        \ 's:snippets',
    \ ]
\ }

" " Mark the max length line limit
set textwidth=80
set cc=80
hi ColorColumn ctermbg=238
