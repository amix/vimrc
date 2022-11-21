" para_move.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2012-08-28.
" @Last Change: 2012-08-29.
" @Revision:    3

" Move paragraphs
call tinykeymap#EnterMap("para_move", "gp", {'name': 'move paragraph'})
call tinykeymap#Map("para_move", "j", "silent call tlib#paragraph#Move('Down', '<count>')")
call tinykeymap#Map("para_move", "k", "silent call tlib#paragraph#Move('Up', '<count>')")

