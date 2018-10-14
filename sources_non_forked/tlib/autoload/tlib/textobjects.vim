" textobjects.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-01-09.
" @Last Change: 2010-01-10.
" @Revision:    0.0.29

let s:save_cpo = &cpo
set cpo&vim


" :tag: standard-paragraph
" Select a "Standard Paragraph", i.e. a text block followed by blank 
" lines. Other than |ap|, the last paragraph in a document is handled 
" just the same.
"
" The |text-object| can be accessed as "sp". Example: >
"
"   vsp ... select the current standard paragraph
"
" Return 1, if the paragraph is the last one in the document.
function! tlib#textobjects#StandardParagraph() "{{{3
    if line("'}") == line('$')
        norm! vip
        return 1
    else
        norm! vap
        return 0
    endif
endf


function! tlib#textobjects#Init() "{{{3
    if !exists('s:tlib_done_textobjects')
        " sp ... Standard paragraph (for use as |text-objects|).
        vnoremap <silent> sp <Esc>:call tlib#textobjects#StandardParagraph()<CR>
        onoremap <silent> sp :<C-u>normal Vsp<CR>
        let s:tlib_done_textobjects = 1
    endif
endf


let &cpo = s:save_cpo
unlet s:save_cpo
