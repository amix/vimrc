" org.vim - VimOrganizer plugin for Vim
" -------------------------------------------------------------
" Version: 0.30
" Maintainer: Herbert Sitz <hesitz@gmail.com>
" Last Change: 2011 Nov 02
"
" Script: http://www.vim.org/scripts/script.php?script_id=3342
" Github page: http://github.com/hsitz/VimOrganizer 
" Copyright: (c) 2010, 2011 by Herbert Sitz
" The VIM LICENSE applies to all files in the
" VimOrganizer plugin.  
" (See the Vim copyright except read "VimOrganizer"
" in places where that copyright refers to "Vim".)
" http://vimdoc.sourceforge.net/htmldoc/uganda.html#license
" No warranty, express or implied.
" *** *** Use At-Your-Own-Risk *** ***

    nnoremap <silent> <buffer> <localleader>ag :call OrgAgendaDashboard()<cr>
    nnoremap <silent> <buffer> <localleader>et :call OrgTagsEdit()<cr>
    nnoremap <silent> <buffer> <localleader>ci :call OrgClockIn()<cr>
    nnoremap <silent> <buffer> <localleader>co :call OrgClockOut()<cr>
    nnoremap <silent> <buffer> <localleader>d  :call OrgDateDashboard()<cr>
    nnoremap <silent> <buffer> <localleader>t  :call OrgTodoDashboard()<cr>
    "nnoremap <silent> <buffer> q  :sign unplace * | quit<cr>
    nnoremap <silent> <buffer> q  :call OrgQuitAgenda()<cr>
function! OrgQuitAgenda()
    sign unplace *
    bw
    call clearmatches()
    let b:v.chosen_agenda_heading = 0
    if bufnr('ColHeadBuffer') > -1
	"main window has column headings window that
	"is now showing a blank buffer line, push back up . . .
	resize 100
    endif
    "quit
endfunction
   
    nnoremap <silent> <buffer> <c-tab>  :wincmd k<cr>
