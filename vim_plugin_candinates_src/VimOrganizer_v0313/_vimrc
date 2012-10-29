" This is an example vimrc that should work for testing purposes.
" Integrate the VimOrganizer specific sections into your own
" vimrc if you wish to use VimOrganizer on a regular basis. . .

"===================================================================
" THE NECESSARY STUFF"
" THe three lines below are necessary for VimOrganizer to work right
" =================================================================
filetype plugin indent on
" and then put these lines in vimrc somewhere after the line above
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org 
au BufEnter *.org            call org#SetOrgFileType()

"==============================================================
" THE UNNECESSARY STUFF"
"=============================================================
"  Everything below here is a customization.  None are needed.
"============================================================

" vars below are used to define default Todo list and
" default Tag list.  Both of these can also be defined 
" on a document-specific basis by config lines in a file.
" See :h vimorg-todo-metadata and/or :h vimorg-tag-metadata
" 'TODO | DONE' is the default,so not really necessary to define it at all
let g:org_todo_setup='TODO | DONE'
" OR, e.g.,:
"let g:org_todo_setup='TODO NEXT STARTED | DONE CANCELED'

" include a tags setup string if you want:
let g:org_tags_alist='{@home(h) @work(w) @tennisclub(t)} {easy(e) hard(d)} {computer(c) phone(p)}'
"
" g:org_agenda_dirs specify directories that, along with 
" their subtrees, are searched for list of .org files when
" accessing EditAgendaFiles().  Specify your own here, otherwise
" default will be for g:org_agenda_dirs to hold single
" directory which is directory of the first .org file opened
" in current Vim instance:
" Below is line I use in my Windows install:
" NOTE:  case sensitive even on windows.
let g:org_agenda_select_dirs=["~/desktop/org_files"]
let g:agenda_files = split(glob("~/desktop/org_files/org-mod*.org"),"\n")

" ---------------------
" Emacs setup
" --------------------
" To use Emacs you will need to define the client.  On
" Linux/OSX this is typically simple, just:
"let g:org_command_for_emacsclient = 'emacsclient'
"
"On Windows it is more complicated, and probably involves creating
" a 'soft link' to the emacsclient executable (which is 'emacsclientw')
" See :h vimorg-emacs-setup
"let g:org_command_for_emacsclient = 'c:\users\herbert\emacsclientw.exe'

" ---------------------
" Custom Agenda Searches
" --------------------
" the assignment to g:org_custom_searches below defines searches that a 
" a user can then easily access from the Org menu or the Agenda Dashboard.
" (Still need to add help on how to define them, assignment below
" is hopefully illustrative for now. . . . )
let g:org_custom_searches = [
	    \  { 'name':"Next week's agenda", 'type':'agenda', 
	    \              'agenda_date':'+1w','agenda_duration':'w'}
            \, { 'name':"Next week's TODOS", 'type':'agenda', 
            \    'agenda_date':'+1w','agenda_duration':'w','spec':'+UNFINISHED_TODOS'}
	    \, { 'name':'Home tags', 'type':'heading_list', 'spec':'+HOME'}
	    \, { 'name':'Home tags', 'type':'sparse_tree', 'spec':'+HOME'}
	    \           ]

" --------------------------------
" Custom colors
" --------------------------------"
" OrgCustomColors() allows a user to set highlighting for particular items
function! OrgCustomColors()
    " various text item "highlightings" are below
    " these are the defaults.  Uncomment and change a line if you
    " want different highlighting for the element
    "
    " below are defaults for any TODOS you define.  TODOS that
    " come before the | in a definition will use  'NOTDONETODO'
    " and those that come after are DONETODO
    "hi! DONETODO guifg=green ctermfg=green
    "hi! NOTDONETODO guifg=red ctermfg=lightred

    " heading level highlighting is done in pairs, one for the
    " heading when unfoled and one for folded.  Default is to make
    " them the same except for the folded version being bold:
    " assign OL1 pair for level 1, OL2 pair for level 2, etc.
    "hi! OL1 guifg=somecolor guibg=somecolor 
    "hi! OL1Folded guifg=somecolor guibg=somecolor gui=bold


    " tags are lines below headings that have :colon:separated:tags:
    "hi! Org_Tag guifg=lightgreen ctermfg=blue

    "  lines that begin with '#+' in column 0 are config lines
    "hi! Org_Config_Line guifg=darkgray ctermfg=magenta

    "drawers are :PROPERTIES: and :LOGBOOK: lines and their associated
    " :END: lines
    "hi! Org_Drawer guifg=pink ctermfg=magenta
    "hi! Org_Drawer_Folded guifg=pink ctermfg=magenta gui=bold cterm=bold

    " this applies to value names in :PROPERTIES: blocks 
    "hi! Org_Property_Value guifg=pink ctermfg=magenta

    " three lines below apply to different kinds of blocks
    "hi! Org_Block guifg=#555555 ctermfg=magenta
    "hi! Org_Src_Block guifg=#555555 ctermfg=magenta
    "hi! Org_Table guifg=#888888 guibg=#333333 ctermfg=magenta

    " dates are date specs between angle brackets (<>) or square brackets ([])
    "hi! Org_Date guifg=magenta ctermfg=magenta gui=underline cterm=underline

    " Org_Star is used to "hide" initial asterisks in a heading
    "hi! Org_Star guifg=#444444 ctermfg=darkgray

    "hi! Props guifg=#ffa0a0 ctermfg=gray

    " bold, itals, underline, and code are highlights applied
    " to character formatting
    "hi! Org_Code guifg=darkgray gui=bold ctermfg=14
    "hi! Org_Itals gui=italic guifg=#aaaaaa ctermfg=lightgray
    "hi! Org_Bold gui=bold guifg=#aaaaaa ctermfg=lightgray
    "hi! Org_Underline gui=underline guifg=#aaaaaa ctermfg=lightgray
    "hi! Org_Lnumber guifg=#999999 ctermfg=gray

    " these lines apply to links: [[link]], and [[link][link desc]]
    "if has("conceal")
    "    hi! default linkends guifg=blue ctermfg=blue
    "endif
    "hi! Org_Full_Link guifg=cyan gui=underline ctermfg=lightblue cterm=underline
    "hi! Org_Half_Link guifg=cyan gui=underline ctermfg=lightblue cterm=underline

    "  applies to the Heading line that can be displayed in column view
    "highlight OrgColumnHeadings guibg=#444444 guifg=#aaaaaa gui=underline

    " Use g:org_todo_custom_highlights to set up highlighting for individual
    " TODO items.  Without this all todos that designate an uninished state 
    " will be highlighted using NOTDONETODO highlight (see above) 
    " and all todos that designate a finished state will be highlighted using
    " the DONETODO highlight (see above).
    let g:org_todo_custom_highlights = 
               \     { 'NEXT': { 'guifg':'#888888', 'guibg':'#222222',
               \              'ctermfg':'gray', 'ctermbg':'darkgray'},
               \      'WAITING': { 'guifg':'#aa3388', 
               \                 'ctermfg':'red' } }

endfunction

" below are two examples of Org-mode "hook" functions
" These present opportunities for end-user customization
" of how VimOrganizer works.  For more info see the 
" documentation for hooks in Emacs' Org-mode documentation:
" http://orgmode.org/worg/org-configs/org-hooks.php#sec-1_40
"
" These two hooks are currently the only ones enabled in 
" the VimOrganizer codebase, but they are easy to add so if
" there's a particular hook you want go ahead and request it
" or look for where these hooks are implemented in 
" /ftplugin/org.vim and use them as example for placing your
" own hooks in VimOrganizer:
function! Org_property_changed_functions(line,key, val)
        "call confirm("prop changed: ".a:line."--key:".a:key." val:".a:val)
endfunction
function! Org_after_todo_state_change_hook(line,state1, state2)
        "call confirm("changed: ".a:line."--key:".a:state1." val:".a:state2)
        "call OrgConfirmDrawer("LOGBOOK")
        "let str = ": - State: " . org#Pad(a:state2,10) . "   from: " . Pad(a:state1,10) .
        "            \ '    [' . org#Timestamp() . ']'
        "call append(line("."), repeat(' ',len(matchstr(getline(line(".")),'^\s*'))) . str)
endfunction


