"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mantenimiento: 
"       Facundo Laffont
"       facundolaffont@gmail.com
"
" Secciones:
"    -> general
"    -> interfaz de usuario de Vim
"    -> colores y fuentes tipográficas
"    -> archivos, backups y deshacer
"    -> texto, tabulación e indentación
"    -> relacionado con el modo visual
"    -> sobre movimiento, pestañas, ventanas y buffers
"    -> línea de estado
"    -> mapeos para edición
"    -> vimgrep searching and cope displaying <¡>falta traducir y modificar sección<!>
"    -> chequeo de gramática
"    -> misceláneo
"    -> sección de definición de funciones
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => general
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Cuántas líneas de historia debe recordar Vim
set history=500

" Habilita la carga de archivos de plugins/indent según el tipo de archivo que se está utilizando
filetype plugin on
filetype indent on

" Cuando el archivo es modificado desde afuera de Vim, se vuelve a cargar
set autoread

" Se va a utilizar el mapleader para generar combinaciones de tecla extra
let mapleader = ","
let g:mapleader = ","

" guardado rápido
nmap <leader>w :w!<cr>

" volver al modo normal casi sin esfuerzo ;)
" <¡>falta que idenfitique si se encuentra en EOF y que vuelva atrás, porque cuando salta una línea
" es muy molesto<!>
ino ñjaf <esc> 

" <¿?>
" :W sudo saves the file (useful for handling the permission-denied error)
command W w !sudo tee % > /dev/null


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => interfaz de usuario de Vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" deja un espacio de 5 líneas entre el curso y el borde superior o inferior
set so=5

" activa el menú Wild
set wildmenu

" ignora los archivos de compilación (útil para git)
set wildignore=*.o,*~,*.pyc,*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store

" siempre muestra la posición del cursor
set ruler

" altura de la línea de comandos
set cmdheight=2

" <?>
" A buffer becomes hidden when it is abandoned
set hid

" configura backspace, los comandos h y l, y los movimientos con las flechas para que el cursor
" responda como normalmente lo hace fuera de Vim, cuando se está al principio o final de la línea
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" ignora la sensibilidad de las mayúsculas cuando se está buscando
set ignorecase

" realiza búsquedas inteligentes en cuanto a la sensibilidad de las mayúsculas
set smartcase

" resalta los resultados de las búsquedas
set hlsearch

" hace que la búsqueda actúe como en los navegadores web modernos
set incsearch 

" no redibuja cuando se ejecutan macros (buena performance)
set lazyredraw 

" activa magic para las expresiones regulares
set magic

" muestra el paréntesis opuesto cuando el cursor está sobre uno
set showmatch 

" cuántas décimas de segundos tarda en parpadear el cursor cuando se indica el paréntesis opuesto
set mat=2

" quita el sonido y el flash en los errores
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" agrega un poco de margen a la izquierda
set foldcolumn=3

" configura el plegado basado en marcas
set foldmethod=marker
set foldmarker={{{,}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => colores y fuentes tipográficas
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" habilita el resaltado según sintaxis
syntax enable 

" intenta utilizar el esquema de color desert
try
    colorscheme desert
catch
endtry

" si el color de fondo es un color oscuro, Vim utiliza colores que contrasten mejor con ese tipo de
" color de fondo
set background=dark

" establece utf8 como la codificación estándar
set encoding=utf8

" utiliza Unix como tipo de archivo estándar
set ffs=unix,mac,dos


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => archivos, backups y deshacer
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => texto, tabulación e indentación
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" usa espacios en vez de tabulación
set expandtab

" usa las tabulaciones de manera inteligente
set smarttab

" 1 tabulación == 4 espacios
set shiftwidth=4
set tabstop=4

" el quiebre de línea se realiza a los 100 caracteres
set lbr
set tw=100

" establece autoindentación, indentación inteligente y quiebre de líneas
set ai
set si
set wrap


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => relacionado con el modo visual
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" en modo Visual, presionando * o ¿, busca la selección actual
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> ¿ :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => sobre movimiento, pestañas, ventanas y buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" mapeos de búsqueda
map <space> /
" <¿>no funciona<?>
map <C-Space> ?

" deshabilita el resaltado de las búsquedas
map <silent> <leader><cr> :noh<cr>

" movimiento entre ventanas
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" cierra el buffer actual
map <leader>bd :Bclose<cr>:tabclose<cr>gT

" cierra todos los buffers
map <leader>ba :bufdo bd<cr>

" navegación entre buffers
map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>

" navegación entre pestañas
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove 
map <leader>t<leader> :tabnext

" 'tl' salta entre la pestaña actual y la última a la que se accedió
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" abre una nueva pestaña con la ruta del buffer actual
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/<cr>

" <¿?>
" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" especifica el comportamiento cuando se cambia de buffer
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry

" al abrir un archivo, vuelve al mismo punto donde se dejó cuando se cerró
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => línea de estado
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" siempre muestra la línea de estado
set laststatus=2

" formateo de la línea de estado
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => mapeos para edición
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" remapea '0' para que posicione el cursor en el primer carácter no blanco
map 0 ^

" colocación de espacios sin tener que entrar en modo INSERT
nn <space>k<space> O<esc>j
nn <space>kk O<esc>
nn <space><space>k O<esc>k
nn <space>kj O<esc>jo<esc>k
nn <space>j<space> o<esc>k
nn <space>jj o<esc>
nn <space><space>j o<esc>j
nn <space>h<space> i<space><esc>l
nn <space>hh i<space><esc>
nn <space><space>h i<space><esc>h
nn <space>hl i<space><esc>la<space><esc>h
nn <space>l<space> a<space><esc>h
nn <space>ll a<space><esc>
nn <space><space>l a<space><esc>l

" <¡>no funciona correctamente<!>
" mueve una línea de texto hacia arriba o hacia abajo
" nmap <D-j> mz:m+<cr>`z
" nmap <D-k> mz:m-2<cr>`z
" vmap <D-j> :m'>+<cr>`<my`>mzgv`yo`z
" vmap <D-k> :m'<-2<cr>`>my`<mzgv`yo`z

" borra los espacios sobrantes al final de la línea, cuando se guarda el buffer
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()

" <¿>falta traducir y modificar toda esta sección<?>
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Ag searching and cope displaying
"    requires ag.vim - it's much better than vimgrep/grep
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" When you press gv you Ag after the selected text
vnoremap <silent> gv :call VisualSelection('gv', '')<CR>

" Open Ag and put the cursor in the right position
map <leader>g :Ag 

" When you press <leader>r you can search and replace the selected text
vnoremap <silent> <leader>r :call VisualSelection('replace', '')<CR>

" Do :help cope if you are unsure what cope is. It's super useful!
"
" When you search with Ag, display your results in cope by doing:
"   <leader>cc
"
" To go to the next search result do:
"   <leader>n
"
" To go to the previous search results do:
"   <leader>p
"
map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
map <leader>n :cn<cr>
map <leader>p :cp<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => chequeo de gramática
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" activa o desactiva el chequeo de gramática
map <leader>ss :setlocal spell!<cr>

" atajos para herramientas de la gramática
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => misceláneo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" quita los caracteres ^M de windows cuando las codificaciones de texto se mezclan
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" abre un buffer sin nombre
map <leader>q :e ~/buffer<cr>

" abre un buffer sin nombre, de tipo Markdown
map <leader>x :e ~/buffer.md<cr>

" activa o desactiva el modo paste
map <leader>pp :setlocal paste!<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => sección de definición de funciones
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ejecuta la acción 'str' como dentro de un menú gráfico
function! CmdLine(str)
    " crea un menu 'Foo' con un submenú 'Bar' que ejecuta el comando str
    exe "menu Foo.Bar :" . a:str

    " ejecuta el menú Foo.Bar
    emenu Foo.Bar

    " borra el menú Foo y todo su contenido
    unmenu Foo
endfunction 

function! VisualSelection(direction, extra_filter) range
    " definición
    let l:saved_reg = @"

    " 
    execute "normal! vgvy"

    
    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'gv'
        call CmdLine("Ag \"" . l:pattern . "\" " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" devuelve true si el modo paste está activado
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction

" redefine el comando Bclose
command! Bclose call <SID>BufcloseCloseIt()
" no cierra la ventana cuando se borra un buffer
function! <SID>BufcloseCloseIt()
   let l:currentBufNum = bufnr("%")
   let l:alternateBufNum = bufnr("#")

   if buflisted(l:alternateBufNum)
     buffer #
   else
     bnext
   endif

   if bufnr("%") == l:currentBufNum
     new
   endif

   if buflisted(l:currentBufNum)
     execute("bdelete! ".l:currentBufNum)
   endif
endfunction

" <?>
" Make VIM remember position in file after reopen
" if has("autocmd")
"   au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
"endif
