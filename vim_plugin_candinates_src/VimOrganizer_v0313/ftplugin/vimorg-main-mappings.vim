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
"
"Section Mappings and Endstuff
" below block of 10 or 15 maps are ones collected
" from body of doc that weren't getting assigned for docs
" oepened after initial org filetype doc
nnoremap <silent> <buffer> <tab> :call OrgCycle()<cr>
nnoremap <silent> <buffer> <s-tab> :call OrgGlobalCycle()<cr>
nnoremap <silent> <buffer> <localleader>ci :call OrgClockIn(line("."))<cr>
nnoremap <silent> <buffer> <localleader>co :call OrgClockOut()<cr>
"cnoremap <space> <C-\>e(<SID>OrgDateEdit())<CR>
" dl is for the date on the current line
noremap <buffer> <localleader>x :call OrgExportDashboard()<cr>
noremap <buffer> <localleader>d :call OrgDateDashboard()<cr>
noremap <buffer> <localleader>b :call OrgTableDashboard()<cr>
"noremap <silent> <buffer> <localleader>dg :call OrgGenericDateEdit()<cr>
"noremap <silent> <buffer> <localleader>dt :call OrgDateEdit('TIMESTAMP')<cr>
"noremap <silent> <buffer> <localleader>dd :call OrgDateEdit('DEADLINE')<cr>
"noremap <silent> <buffer> <localleader>dc :call OrgDateEdit('CLOSED')<cr>
"noremap <silent> <buffer> <localleader>ds :call OrgDateEdit('SCHEDULED')<cr>
noremap <silent> <buffer> <localleader>a* :call OrgRunAgenda(strftime("%Y-%m-%d"),'w,'')<cr>
noremap <silent> <buffer> <localleader>aa :call OrgRunAgenda(strftime("%Y-%m-%d"),'w,'+ANY_TODO')<cr>
noremap <silent> <buffer> <localleader>at :call OrgRunAgenda(strftime("%Y-%m-%d"),'w,'+UNFINISHED_TODOS')<cr>
noremap <silent> <buffer> <localleader>ad :call OrgRunAgenda(strftime("%Y-%m-%d"),'w,'+FINISHED_TODOS')<cr>
noremap <silent> <buffer> <localleader>ag :call OrgAgendaDashboard()<cr>
noremap <silent> <buffer> <localleader>ac :call OrgCustomSearchMenu()<cr>
"command! -nargs=0 Agenda :call OrgAgendaDashboard()
nnoremap <silent> <buffer> <s-up> :call OrgDateInc(1)<CR>
nnoremap <silent> <buffer> <s-down> :call OrgDateInc(-1)<CR>
nnoremap <silent> <buffer> <2-LeftMouse> :call OrgMouseDate()<CR>
nnoremap <localleader>pl :call s:MyPopup()<cr>
inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<PageDown>"
inoremap <expr> <PageUp>   pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<PageUp>"
"map <silent> <localleader>b  :call ShowBottomCal()<cr> 

nnoremap <silent> <buffer> <localleader>et :call OrgTagsEdit()<cr>

" clear search matching
nnoremap <silent> <buffer> <localleader>cs :let @/=''<cr>

noremap <buffer> <localleader>r :call OrgRefileDashboard()<cr>
"noremap <silent> <buffer> <localleader>rh :call OrgRefile(line('.'))<cr>
"noremap <silent> <buffer> <localleader>rj :call OrgJumpToRefilePoint()<cr>
"noremap <silent> <buffer> <localleader>rx :call OrgJumpToRefilePointPersistent()<cr>
"noremap <silent> <buffer> <localleader>rs :call OrgSetRefilePoint()<cr>
"noremap <silent> <buffer> <localleader>rp :call OrgRefileToPermPoint(line('.'))<cr>
noremap <silent> <buffer> <localleader>v :silent call OrgEval()<cr>

noremap <buffer>   <C-K>         <C-]>
noremap <buffer>   <C-N>         <C-T>
noremap <silent> <buffer>   <localleader>0  :call OrgExpandWithoutText(99999)<CR>
noremap <silent> <buffer>   <localleader>9  :call OrgExpandWithoutText(9)<CR>
noremap <silent> <buffer>   <localleader>8  :call OrgExpandWithoutText(8)<CR>
noremap <silent> <buffer>   <localleader>7  :call OrgExpandWithoutText(7)<CR>
noremap <silent> <buffer>   <localleader>6  :call OrgExpandWithoutText(6)<CR>
noremap <silent> <buffer>   <localleader>5  :call OrgExpandWithoutText(5)<CR>
noremap <silent> <buffer>   <localleader>4  :call OrgExpandWithoutText(4)<CR>
noremap <silent> <buffer>   <localleader>3  :call OrgExpandWithoutText(3)<CR>
noremap <silent> <buffer>   <localleader>2  :call OrgExpandWithoutText(2)<CR>
noremap <silent> <buffer>   <localleader>1  :call OrgExpandWithoutText(1)<CR>
noremap <silent> <buffer>   <localleader><space>  :call OrgExpandWithoutText(1)<CR>
"noremap <silent> <buffer>   <localleader>/ :let @/='exec call OrgExpandWithoutText(1)<CR>
"noremap <silent> <buffer>   <localleader>/ :let @a='/^\*\{1,' . &foldlevel . '\} .*'|call LevSearch()<cr>
nnoremap <buffer> <expr> <localleader>/ '/^\*\{1,' . &foldlevel . '\} .*'
nnoremap <buffer> <expr> <localleader>? '?^\*\{1,' . &foldlevel . '\} .*'

" set reasonable max limit of 12 for '0' command below, because it iterates
" each for each level, just assume 12 is max. . .
noremap <silent> <buffer>   <localleader>,0   :call OrgShowSubs(12,0)<CR>
noremap <silent> <buffer>   <localleader>,9   :call OrgShowSubs(9,0)<CR>
noremap <silent> <buffer>   <localleader>,8   :call OrgShowSubs(8,0)<CR>
noremap <silent> <buffer>   <localleader>,7   :call OrgShowSubs(7,0)<CR>
noremap <silent> <buffer>   <localleader>,6   :call OrgShowSubs(6,0)<CR>
noremap <silent> <buffer>   <localleader>,5   :call OrgShowSubs(5,0)<CR>
noremap <silent> <buffer>   <localleader>,4   :call OrgShowSubs(4,0)<CR>
noremap <silent> <buffer>   <localleader>,3   :call OrgShowSubs(3,0)<CR>
noremap <silent> <buffer>   <localleader>,2   :call OrgShowSubs(2,0)<CR>
noremap <silent> <buffer>   <localleader>,1   :call OrgShowSubs(1,0)<CR>
noremap <silent> <buffer>   <localleader>,;   :call OrgShowSubs(1,0)<CR>


"nnoremap <silent> <buffer> <localleader>no :call NarrowOutline(line('.'))<cr>
"nnoremap <silent> <buffer> <localleader>ns :call NarrowOutline(line('.'))<cr>
"nnoremap <silent> <buffer> <localleader>nc :call NarrowCodeBlock(line('.'))<cr>
nnoremap <silent> <buffer> <localleader>na :call NarrowCodeBlock(line('.'))<cr>
nnoremap <silent> <buffer> <localleader>m :call OrgColumnsDashboard()<cr>
" ----------------------------------------
" table commands
au InsertEnter *.org :call org#tbl#reset_tw(line("."))
au InsertLeave *.org :call org#tbl#format(line("."))
command! -buffer -nargs=* OrgTable call org#tbl#create(<f-args>)
"nnoremap <silent> <buffer> <localleader>bc :call org#tbl#create()<cr>
command! -buffer OrgTableAlignQ call org#tbl#align_or_cmd('gqq')
command! -buffer OrgTableAlignW call org#tbl#align_or_cmd('gww')
command! -buffer OrgTableMoveColumnLeft call org#tbl#move_column_left()
"nnoremap <silent> <buffer> <localleader>bl :call org#tbl#move_column_left()<cr>
command! -buffer OrgTableMoveColumnRight call org#tbl#move_column_right()
"nnoremap <silent> <buffer> <localleader>br :call org#tbl#move_column_right()<cr>

" table function mappings
inoremap <buffer> <expr> <CR> org#tbl#kbd_cr()
inoremap <expr> <buffer> <Tab> org#tbl#kbd_tab()
inoremap <expr> <buffer> <S-Tab> org#tbl#kbd_shift_tab()
nnoremap <buffer> gqq :OrgTableAlignQ<CR>
nnoremap <buffer> gww :OrgTableAlignW<CR>
  "nnoremap <silent><buffer> <A-Left> <Plug>OrgTableMoveColumnLeft
nnoremap <silent><script><buffer>
      \ <Plug>OrgTableMoveColumnLeft :OrgTableMoveColumnLeft<CR>
  "nnoremap <silent><buffer> <A-Right> <Plug>OrgTableMoveColumnRight
nnoremap <silent><script><buffer>
      \ <Plug>OrgTableMoveColumnRight :OrgTableMoveColumnRight<CR>
" -------------------------------------

imap <silent> <buffer>   <s-c-CR>        <c-r>=OrgNewHead('levelup',1)<CR>
imap <silent> <buffer>   <c-CR>          <c-r>=OrgNewHead('leveldown',1)<CR>
imap <silent> <buffer>   <s-CR>          <c-r>=OrgNewHead('same',1)<CR>
nnoremap <silent> <buffer>   <s-c-CR>        :call OrgNewHead('levelup')<CR>
nnoremap <silent> <buffer>   <c-CR>          :call OrgNewHead('leveldown')<CR>
nnoremap <silent> <buffer>   <CR>            :call OrgEnterFunc()<CR>
nnoremap <silent> <buffer> <c-left>           :call OrgShowLess(line("."))<CR>
nnoremap <silent> <buffer> <c-right>          :call OrgShowMore(line("."))<CR>
nnoremap <silent> <buffer> <c-a-left>         :call OrgMoveLevel(line("."),'left')<CR>
nnoremap <silent> <buffer> <c-a-right>        :call OrgMoveLevel(line("."),'right')<CR>
nnoremap <silent> <buffer> <c-a-up>           :<C-U>call OrgMoveLevel(line("."),'up',v:count1)<CR>
nnoremap <silent> <buffer> <c-a-down>         :<C-U>call OrgMoveLevel(line("."),'down',v:count1)<CR>
nnoremap <silent> <buffer> <a-end>            :call OrgNavigateLevels("end")<CR>
nnoremap <silent> <buffer> <a-home>           :call OrgNavigateLevels("home")<CR>
nnoremap <silent> <buffer> <a-up>             :call OrgNavigateLevels("up")<CR>
nnoremap <silent> <buffer> <a-down>           :call OrgNavigateLevels("down")<CR>
nnoremap <silent> <buffer> <a-left>           :call OrgNavigateLevels("left")<CR>
nnoremap <silent> <buffer> <a-right>          :call OrgNavigateLevels("right")<CR>
nnoremap <silent> <buffer> <localleader>le    :call EditLink()<cr>
nnoremap <silent> <buffer> <localleader>lf    :call FollowLink(OrgGetLink())<cr>
nnoremap <silent> <buffer> <localleader>ln    :/]]<cr>
nnoremap <silent> <buffer> <localleader>lp    :?]]<cr>
nnoremap <silent> <buffer> <localleader>lc    :set conceallevel=3\|set concealcursor=nc<cr>
nnoremap <silent> <buffer> <localleader>la    :set conceallevel=3\|set concealcursor=c<cr>
nnoremap <silent> <buffer> <localleader>lx    :set conceallevel=0<cr>
"nnoremap <silent> <buffer>  <localleader>,e  :call OrgSingleHeadingText("expand")<CR>
"nnoremap <silent> <buffer>  <localleader>,E  :call OrgBodyTextOperation(1,line("$"),"expand")<CR>
"nnoremap <silent> <buffer>  <localleader>,C  :call OrgBodyTextOperation(1,line("$"),"collapse")<CR>
"nnoremap <silent> <buffer>  <localleader>,c  :call OrgSingleHeadingText("collapse")<CR>
nnoremap <silent> <buffer>   zc              :call OrgDoSingleFold(line("."))<CR>

" below are alternate mappings for terminals, which
" don't support some of the above key combinations
nnoremap <silent> <buffer> ,<tab>            :call OrgGlobalCycle()<cr>
nnoremap <silent> <buffer> <localleader>zu    :call OrgNavigateLevels("up")<CR>
nnoremap <silent> <buffer> <localleader>zd    :call OrgNavigateLevels("down")<CR>
nnoremap <silent> <buffer> <localleader>zl    :call OrgNavigateLevels("left")<CR>
nnoremap <silent> <buffer> <localleader>zr    :call OrgNavigateLevels("right")<CR>
nnoremap <silent> <buffer> <localleader>zL    :call OrgMoveLevel(line("."),'left')<CR>
nnoremap <silent> <buffer> <localleader>zR    :call OrgMoveLevel(line("."),'right')<CR>
nnoremap <silent> <buffer> <localleader>k    :<c-u>call OrgMoveLevel(line("."),'up',v:count1)<CR>
nnoremap <silent> <buffer> <localleader>j    :<c-u>call OrgMoveLevel(line("."),'down',v:count1)<CR>
nnoremap <silent> <buffer>  <localleader>np  :call OrgNewHead('levelup')<CR>
nnoremap <silent> <buffer>  <localleader>ns  :call OrgNewHead('leveldown')<CR>
