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
"Section Setup
"if !exists('g:v')
"    let g:v={}
"endif
" Calendar plugin at:
"http://www.vim.org/scripts/script.php?script_id=52
let b:v={}
let w:v={}
let b:v.prevlev = 0
let b:v.org_loaded=0
let b:v.lasttext_lev=''
let maplocalleader = ","        " Org key mappings prepend single comma

let s:sfile = expand("<sfile>:p:h")
let b:v.dateMatch = '\(\d\d\d\d-\d\d-\d\d\)'
let b:v.headMatch = '^\*\+\s'
let b:v.tableMatch = '^\(\s*|.*|\s*$\|#+TBLFM\)'
let b:v.taglineMatch = '^\s*:\S\+:\s*$'
let b:v.headMatchLevel = '^\(\*\)\{level}\s'
let b:v.propMatch = '^\s*:\s*\(PROPERTIES\)'
let b:v.propvalMatch = '^\s*:\s*\(\S*\)\s*:\s*\(\S.*\)\s*$'
let b:v.drawerMatch = '^\s*:\(PROPERTIES\|LOGBOOK\)'
let b:v.levelstars = 1
let b:v.effort=['0:05','0:10','0:15','0:30','0:45','1:00','1:30','2:00','4:00']
let b:v.tagMatch = '\(:\S*:\)\s*$'
let b:v.mytags = ['buy','home','work','URGENT']
let b:v.foldhi = ''
let b:v.org_inherited_properties = ['COLUMNS']
let b:v.org_inherited_defaults = {'CATEGORY':expand('%:t:r'),'COLUMNS':'%40ITEM %30TAGS'}
let w:v.total_columns_width = 30
let w:v.columnview = 0
let w:v.org_item_len = 100 
let w:v.org_colview_list = [] 
let w:v.org_current_columns = ''
let w:v.org_column_item_head = ''
let b:v.chosen_agenda_heading = 0

let b:v.buf_tags_static_spec = ''
let b:v.buffer_category = ''
if !exists('g:org_agenda_default_search_spec')
    let g:org_agenda_default_search_spec = 'ANY_TODO'
endif
if exists('g:global_column_defaults') 
    let b:v.buffer_columns = g:global_column_defaults' 
else
    let b:v.buffer_columns = '%40ITEM %30TAGS'
endif
let w:sparse_on = 0
"if exists('g:global_column_view') && g:global_column_view==1
"    let w:v.columnview = 1
"else
"    let w:v.columnview = 0
"endif

let b:v.clock_to_logbook = 1
let b:v.messages = []
let b:v.global_cycle_levels_to_show=4
let b:v.src_fold=0
let b:v.foldhilines = []
let b:v.cycle_with_text=1
let b:v.foldcolors=['Normal','SparseSkip','Folded','WarningMsg','WildMenu','DiffAdd','DiffChange','Normal','Normal','Normal','Normal']
let b:v.cols = []
setlocal cfu=Mycfu
set noswapfile
hi MatchGroup guibg=yellow guifg=black

setlocal ignorecase         " searches ignore case
setlocal smartcase          " searches use smart case
setlocal autoindent 
setlocal backspace=2
setlocal nowrap
setlocal tw=78
setlocal expandtab
setlocal nosmarttab
setlocal softtabstop=0 
setlocal foldcolumn=1 
setlocal tabstop=4   
setlocal shiftwidth=4
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*\\-\\s\\+
if has("conceal")
    set conceallevel=3
    set concealcursor=nc
endif
setlocal indentexpr=
setlocal foldexpr=OrgFoldLevel(v:lnum)
"setlocal iskeyword+=<
setlocal nocindent
setlocal iskeyword=@,39,45,48-57,_,129-255

let b:v.basedate = strftime("%Y-%m-%d %a")
let b:v.sparse_list = []
let b:v.fold_list = []
let b:v.suppress_indent=0
let b:v.suppress_list_indent=0

" LINE BELOW IS MAJOR IF THAT ENCOMPASSES MOST OF org.vim
" endif is near bottom of document
" everything in between is executed only the first time an
" org file is opened
if !exists('g:org_loaded')

if !exists('g:org_custom_column_options')
    let g:org_custom_column_options = ['%ITEM %15DEADLINE %35TAGS', '%ITEM %35TAGS'] 
endif

if !exists('g:org_command_for_emacsclient') && (has('unix') || has('macunix'))
    let g:org_command_for_emacsclient = 'emacsclient'
endif
if !exists('g:org_custom_colors')
    let g:org_custom_colors=[]
endif
if !exists('g:org_tags_persistent_alist')
    let g:org_tags_persistent_alist = ''
endif
if !exists('g:org_tags_alist')
    let g:org_tags_alist = ''
endif
if !exists('g:org_confirm_babel_evaluate')
    let g:org_confirm_babel_evaluate = 0
endif
if has('win32') || has('win64')
    let s:cmd_line_quote_fix = '^'
else
    let s:cmd_line_quote_fix = ''
endif
let g:org_todos_done_dict = {}
let g:org_todos_notdone_dict = {}
let g:org_agenda_todos_done_pattern = ''
let g:org_agenda_todos_notdone_pattern = ''
let g:org_clock_history=[]
let g:org_reverse_note_order = 0
let g:org_html_app=''
let g:org_pdf_app=''
let s:org_headMatch = '^\*\+\s'
let s:org_cal_date = '2000-01-01'
let g:org_export_babel_evaluate = 1
let g:org_tag_group_arrange = 0
let g:org_first_sparse=1
let g:org_clocks_in_agenda = 0
let s:remstring = '^\s*:\S'
let s:block_line = '^\s*\(:\|DEADLINE\|SCHEDULED\|CLOSED\|<\d\d\d\d-\|[\d\d\d\d-\)'
"let s:remstring = '^\s*\(:\|DEADLINE:\|SCHEDULED:\|CLOSED:\|<\d\d\d\d-\)'
let g:org_use_calendar = 1
let g:org_todoitems=[]
let s:headline = ''
let g:org_ColumnHead = 'Lines'
let g:org_gray_agenda = 0
let g:org_sparse_lines_after = 10
let g:org_capture_file=''
let g:org_log_todos=0
let g:org_timegrid=[8,17,1]
let w:v.org_colview_list = []
let s:firsttext = ''
let g:org_supported_link_types = '\(http\|file\|mailto\)'
let g:org_unsupported_link_types = '\(vm\|wl\|mhe\|rmail\|gnus\|bbdb\|irc\|info\|shell\|elisp\)'


let w:v.org_item_len=100
let w:sparse_on = 0
let g:org_folds = 1
let g:org_show_fold_lines = 1
let g:org_columns_default_width = 15
let s:org_columns_master_heading = 0
let w:v.org_colview_list=[]
let g:org_show_fold_dots = 0
let g:org_show_matches_folded=1
let g:org_indent_from_head = 0
let g:org_agenda_skip_gap = 2
let g:org_agenda_days=7
let g:org_agenda_minforskip = 8

let g:org_show_balloon_tips=1
let g:org_datelist = []
let g:org_search_spec = ''
let g:org_deadline_warning_days = 3
let s:org_weekdays = ['mon','tue','wed','thu','fri','sat','sun']
let s:org_weekdaystring = '\cmon\|tue\|wed\|thu\|fri\|sat\|sun'
let s:org_months = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
let s:org_monthstring = '\cjan\|feb\|mar\|apr\|may\|jun\|jul\|aug\|sep\|oct\|nov\|dec'
let s:include_inherited_props=0
let s:AgendaBufferName = "__Agenda__"
let s:sparse_lines = {}

"testing stuff
function! CustomSearchesSetup()
    let g:org_custom_searches = [
                \    { 'name':"Next week's agenda", 'type':'agenda', 
                \                'agenda_date':'+1w','agenda_duration':'w'}
                \    ,{ 'name':"Next week's TODOS", 'type':'agenda', 
                \                'agenda_date':'+1w','agenda_duration':'w','spec'='+UNFINISHED_TODOS'}
                \    , { 'name':'Home tags', 'type':'heading_list', 'spec':'+HOME'}
                \    , { 'name':'Home tags', 'type':'sparse_tree', 'spec':'+HOME'}
                \           ]
endfunction
function! RunCustom(searchnum)
    let mydict = g:org_custom_searches[a:searchnum]
    if mydict.type ==? 'agenda'
        call OrgRunAgenda( DateCueResult( mydict.agenda_date, s:Today()), 
                        \  get(mydict, 'agenda_duration', 'w'),
                        \  get(mydict, 'spec','')  )
    elseif mydict.type ==? 'sparse_tree'
        call OrgRunSearch( mydict.spec, 1 )
    elseif mydict.type ==? 'sparse_tree_regex'
        silent call s:SparseTreeRun(mydict.spec)
    elseif mydict.type ==? 'heading_list'
        call OrgRunSearch( mydict.spec )
    endif
endfunction
"Section Tag and Todo Funcs
function! OrgProcessConfigLines()
    let b:v.org_config_lines = []
    let b:v.todoitems = []
    silent g/^#+/call add( b:v.org_config_lines, getline(line('.')) )
    
    " clear out for new tag settings
    let b:v.tagdict = {}
    let b:v.buf_tags_static_spec = ''
    let b:v.tagchars=''
    let b:v.tags_order = []
    if g:org_tags_alist ==# ''
        let b:v.dynamic_tags=1
    else
        let b:v.dynamic_tags=0
    endif
    
    for line in b:v.org_config_lines
        if line =~ '^#+CATEGORY'
            let b:v.buffer_category = matchstr( line ,'^#+CATEGORY:\s*\zs.*')
            let b:v.org_inherited_defaults['CATEGORY'] = b:v.buffer_category
        elseif line =~ '^#+COLUMNS'
            let b:v.buffer_columns = matchstr( line ,'^#+COLUMNS:\s*\zs.*')
            let b:v.org_inherited_defaults['COLUMNS'] = b:v.buffer_columns
            "let w:v.org_current_columns = b:v.buffer_columns
        elseif line =~ '#+STARTUP:'
            let startup_list = split(matchstr( line, '#+STARTUP:\s*\zs.*') )
            for item in startup_list
               silent! exec "let b:v." . item . "=1"
            endfor
        elseif line =~ '#+TAGS:'
            let newtags = matchstr( line, '#+TAGS:\s*\zs.*') 
            if newtags ==# ''
                let b:v.dynamic_tags = 1
            else
                let b:v.buf_tags_static_spec .= newtags . ' \n '
            endif
        elseif line =~ '\(#+TODO:\|#+SEQ_TODO:\)'
            call OrgTodoSetup(matchstr(line,'\(#+TODO:\|#+SEQ_TODO:\)\s*\zs.*'))
        endif
    endfor
    if empty(b:v.todoitems)
        call OrgTodoSetup(g:org_todo_setup)
    endif

    call OrgTagSetup( b:v.buf_tags_static_spec )

    " get rid of b:v.buffer_category (and columns also) and just use o_i_d var???
    if b:v.buffer_category ==# ''
        let b:v.buffer_category = expand("%:t:r")
    endif

    normal gg
endfunction

function! OrgTodoConvert(orgtodo)
    let todolist = []
    let sublist = []
    let b:v.tododict = {}
   " let templist = []
    let temp_list = split(a:orgtodo,'\s\+')

    for item in temp_list
        if item == '|' 
            continue
        endif
        let b:v.tododict[item] = {}
        if matchstr(item,'.*(.)') ==# ''
            let b:v.tododict[item].todotext = item
            let b:v.tododict[item].todochar = ''
        else
            let b:v.tododict[item].todotext = matchstr(item,'.*\ze(.)')
            let b:v.tododict[item].todochar = matchstr(item,'.*(\zs.\ze)')
        endif
    endfor
    " count '|' chars in list, if 0 or 1 then
    " it is like Org-mode format, otherwise
    " sublists are used in non-done slot"
    let bar_count = count(split(a:orgtodo,'\zs'),'|')
    let after_bar = 0
    if bar_count >= 2
        for item in temp_list
           if item != '|'
                call add(sublist,item)
           elseif (item ==? '|') 
               call add(todolist,sublist)
               let sublist = []
           endif
        endfor
    else
        for item in temp_list
           if (item != '|') && (after_bar == 1)
                call add(sublist,item)
            elseif (item != '|') && (after_bar == 0)
                call add(todolist,item)
           elseif (item ==? '|') 
               let sublist = []
               let after_bar = 1
            endif
        endfor
    endif
    if sublist != []
        call add(todolist,sublist)
    endif
    return todolist
endfunction
        
function! OrgTodoSetup(todolist_str)
    let todolist = OrgTodoConvert(a:todolist_str)
    let b:v.todo_setup = a:todolist_str
    "set up list and patterns for use throughout
    let b:v.todoitems=[]
    let b:v.todo_first_letters = ''
    "let b:v.fulltodos=todolist
    let b:v.todocycle=[]
    let b:v.todoMatch=''
    let b:v.todoNotDoneMatch=''
    let b:v.todoDoneMatch=''
    let i = 0
    while i < len(todolist) 
        if type(todolist[i]) == type('abc')
            let thistodo = matchstr(todolist[i],'.*ze(.)')
            let thistodo = b:v.tododict[todolist[i]].todotext
            let todolist[i] = substitute(todolist[i],'(.)','','')
            call add(b:v.todoitems,thistodo)
            call add(b:v.todocycle,thistodo)
            " add to patterns
            "let newtodo = b:v.todoitems[len(b:v.todoitems)-1]
            let newtodo = thistodo
            let b:v.todoMatch .= newtodo . '\|'
            if i < len(todolist) - 1
                let b:v.todoNotDoneMatch .= newtodo . '\|'
                let g:org_todos_notdone_dict[newtodo] = 1
            else
                let b:v.todoDoneMatch .= newtodo . '\|'
                let g:org_todos_done_dict[newtodo] = 1
            endif
        else
            "item is itself a list
            let j = 0
            while j < len(todolist[i])
                let thisitem = b:v.tododict[todolist[i][j]].todotext
                let todolist[i][j] = substitute(todolist[i][j],'(.)','','')
                call add(b:v.todoitems,thisitem )
                if j == 0
                    call add(b:v.todocycle,thisitem)
                endif
                " add to patterns
                let b:v.todoMatch .= thisitem . '\|'
                if i < len(todolist) - 1
                    let b:v.todoNotDoneMatch .= thisitem . '\|'
                    let g:org_todos_notdone_dict[thisitem] = 1
                else
                    let b:v.todoDoneMatch .= thisitem . '\|'
                    let g:org_todos_done_dict[thisitem] = 1
                endif
                let j += 1
            endwhile
        endif
        let i += 1
    endwhile
    let b:v.todoMatch = '^\*\+\s*\zs\('.b:v.todoMatch[:-2] . ')'
    let b:v.todoDoneMatch = '^\*\+\s*\zs\('.b:v.todoDoneMatch[:-2] . ')'
    let b:v.todoNotDoneMatch = '^\*\+\s*\zs\('.b:v.todoNotDoneMatch[:-2] . ')'
    let b:v.fulltodos = todolist

    for item in keys( b:v.tododict )
        let item_char = tolower( b:v.tododict[item].todochar)
        if item_char ==# ''
            let item_char = tolower(item[0])
        endif
        "execute 'map <silent> <buffer> <localleader>t' . item_char . 
        "        \  ' :call OrgSequenceTodo(line(''.''),''' . item_char . ''')<cr>'
    endfor
"    map <silent> <buffer> <localleader>tx :call OrgSequenceTodo(line('.'),'x')<cr>
"    map <silent> <buffer> <localleader><space> :call OrgSequenceTodo(line('.'))<cr>

endfunction
function! s:CurfileAgenda()
    exec "let g:agenda_files=['".expand("%:p")."']"
endfunction

function! OrgTagSetup(tagspec)
       let b:v.tags = split(tr(a:tagspec,'{}','  '),'\s\+') 
       for item in b:v.tags
            if item =~ '('
                let char = matchstr(item,'(\zs.\ze)')
                let tag = matchstr(item,'.*\ze(')
            else
                "find an unused character
                let char = ''
                let tag = item   
                let i = 0
                while i < len(item)
                    "if !has_key(chardict, item[i])
                    " find char that isn't in tagchars yet
                    if b:v.tagchars !~ item[i]
                        let char = item[i]
                        "let chardict[item[i]] = 1
                        break
                    endif
                    let i += 1
                endwhile
                if char ==# ''
                    for i in range(65,90)
                        if b:v.tagchars !~ nr2char(i)
                            let char = nr2char(i)
                            break
                        endif
                    endfor
                endif
            endif
            let b:v.tagdict[item] = {'char':char, 'tag':tag, 'exclude':'', 'exgroup':0}
            call add(b:v.tags_order,item)
            if char != ''
                let b:v.tagchars .= char
            endif
        endfor

       let templist = a:tagspec
       let i = 1
        while templist =~ '{.\{}}'
            "cycle through groups and add exclude chars for any group members
            let strikeout = matchstr(templist,'{.\{-}}')
            let exclusive = matchstr(templist,'{\zs.\{-}\ze}')
            let templist = substitute(templist,strikeout,'','')
            let xlist = split(exclusive,'\s\+')
            for item in xlist
                let b:v.tagdict[item].exgroup = i
                for x in xlist
                    if x != item
                           let b:v.tagdict[item].exclude .= b:v.tagdict[x].char
                    endif
                endfor
            endfor
            let i += 1
        endwhile
endfunction


function! OrgTagsEdit(...)
    let line_file_str = ''
    let lineno=line('.')
    let file = expand("%")
    if bufname("%") ==? ('__Agenda__')
        " new file and lineno below to test with new line marker in agenda
        let file = s:filedict[str2nr(matchstr(getline(line('.')), '^\d\d\d'))]
        let lineno = str2nr(matchstr(getline(line('.')),'^\d\d\d\zs\d*'))

        call s:OrgSaveLocation()
        call s:LocateFile(file)
        call s:SetDynamicTags()
        call s:OrgRestoreLocation()

        let b:v.tagdict = getbufvar(file,'v').tagdict
        let b:v.tags_order = getbufvar(file,'v').tags_order
    else
        call s:SetDynamicTags()
    endif
    
    let heading_tags = get(s:GetProperties(lineno,0,file),'TAGS','')
    
    let new_heading_tags = s:TagMenu(heading_tags)
    if new_heading_tags != heading_tags
            silent call s:SetProp('tags',new_heading_tags,lineno, file)
    endif
endfunction

function! s:TagMenu(heading_tags)
    let heading_tags = a:heading_tags
    
    let tagstring = ''
    let tagchars = ''
    for item in b:v.tags_order
        let tagchars .= b:v.tagdict[item].char
        if match(heading_tags,':'.b:v.tagdict[item].tag .':') >= 0
            let tagstring .= b:v.tagdict[item].char
        endif
    endfor

    hi Cursor guibg=black
    let cue = ''
    set nomore
    while 1
        echo repeat('-',winwidth(0)-1)
        echohl Title | echo 'Choose tags:   ' | echohl None | echon '( <enter> to accept, <esc> to cancel )'
        echo '------------'
        let oldgroup = 0
        let items_in_row = 1
        for item in b:v.tags_order
            if item ==? '\n'
                continue
            endif
            let curindex = index(b:v.tags_order,item)
            let newgroup = b:v.tagdict[item].exgroup
            let select = ' '
            if match(tagstring,b:v.tagdict[item].char) >= 0
                let select = 'X'
                echohl Question
            else
                echohl None
            endif
            "if (g:org_tag_group_arrange == 0) || (newgroup != oldgroup) || (newgroup == 0 ) || (b:v.tags_order[curindex+1] ==? '\n')
            if (curindex == 0) || (b:v.tags_order[curindex-1] ==? '\n') || (winwidth(0) - (items_in_row*20) < 20)
                echo repeat(' ',3) . '[' | echohl Question | echon select | echohl None | echon '] ' 
                echohl None | echon b:v.tagdict[item].tag | echohl Title | echon '('.b:v.tagdict[item].char.')' | echohl None
                let nextindent = repeat(' ',12-len(b:v.tagdict[item].tag))
                let items_in_row = 1
            else    
                "echon repeat(' ',3) . 
                echon nextindent
                echon '[' | echohl Question | echon select | echohl None | echon '] ' 
                echohl None | echon b:v.tagdict[item].tag | echohl Title | echon '('.b:v.tagdict[item].char.')' | echohl None
                let nextindent = repeat(' ',12-len(b:v.tagdict[item].tag))
                let items_in_row += 1
                "echon repeat(' ', 12-len(b:v.tagdict[item]))
            endif
            let oldgroup = b:v.tagdict[item].exgroup
        endfor
        echo ""
            "echohl LineNr | echon 'Date+time ['.basedate . ' '.basetime.']: ' 
            "echohl None | echon cue.'_   =>' | echohl WildMenu | echon ' '.newdate.' '.newtime
            let nchar = getchar()
            let newchar = nr2char(nchar)
            if (nchar ==? "\<BS>") && (len(cue)>0)
                let cue = cue[:-2]
            elseif nchar ==? "\<s-c-up>"
                let cue = ((curdif-365>=0) ?'+':'').(curdif-365).'d'
            elseif newchar ==? "\<s-cr>"
                " add new tag . . . todo . . .
            elseif newchar ==? "\<cr>"
                break
            elseif newchar ==? "\<Esc>"
                hi Cursor guibg=gray
                redraw
                return a:heading_tags
            elseif (match(tagchars,newchar) >= 0) 
                if (match(tagstring,newchar) == -1) 
                    let tagstring .= newchar
                    " check for mutually exclusve tags
                    for item in keys(b:v.tagdict)
                        if b:v.tagdict[item].char ==? newchar
                            let exclude_str = b:v.tagdict[item].exclude
                            let tagstring = tr(tagstring,exclude_str,repeat(' ',len(exclude_str)))
                            break
                        endif
                    endfor
                else
                    let tagstring = tr(tagstring,newchar,' ')
                endif
            endif
            call substitute(tagstring,' ','','')
            echon repeat(' ',72)
            redraw
    endwhile

    hi Cursor guibg=gray
    redraw
    echo 
    set more

    let heading_tags = ''
    for item in keys(b:v.tagdict)
        if (item!='\n') && (match(tagstring, b:v.tagdict[item].char) >= 0)
            let heading_tags .= b:v.tagdict[item].tag . ':'
        endif
    endfor
    if heading_tags ># '' | let heading_tags = ':' . heading_tags | endif
    return heading_tags
endfunction

function! s:SetDynamicTags()
    let taglist = s:GetBufferTags()
    let chardict = {}
    let b:v.tagdict = {}
    let b:v.tagchars = ''
    let b:v.tags_order = []

    if b:v.buf_tags_static_spec ==# ''
        let static_tags = g:org_tags_alist . ' ' . g:org_tags_persistent_alist
        if static_tags ==# ''
            let b:v.dynamic_tags_only = 1
        endif
    elseif exists('b:v.noptags')
        let static_tags = b:v.buf_tags_static_spec
    else
        let static_tags = b:v.buf_tags_static_spec . ' ' . g:org_tags_persistent_alist
    endif

    if exists('b:v.dynamic_tags_only') && (b:v.dynamic_tags_only == 1)
        let setup_string = join(taglist)
    elseif exists('b:v.dynamic_tags') && (b:v.dynamic_tags == 1)
        "first need to remove dups in dynamic taglist
        let temp_list = split(static_tags)
        for i in range(0,len(temp_list)-1)
            if temp_list[i] =~ '(.)'
               let temp_list[i] = matchstr(temp_list[i],'^.*\ze(')
            endif
        endfor
        let dup_list = s:Intersect( temp_list, taglist )
        for item in dup_list
            call remove( taglist, index(taglist, item) )
        endfor
        let setup_string =  static_tags . ' ' . join(taglist) 
    else
        let setup_string = static_tags
    endif

    call OrgTagSetup( setup_string )

endfunction
        
function! s:GetBufferTags()
    let save_cursor = getpos(".") 
    let b:v.buftagdict = {}
    " call addtags for each headline in buffer
    g/^\*/call s:AddTagsToDict(line("."))
    call setpos('.',save_cursor)
    return sort(keys(b:v.buftagdict))
endfunction
inoremap <F5> <C-R>=OrgEffort()<CR>
noremap <F5> A<C-R>=OrgEffort()<CR>
function! OrgEffort()
    if getline(line('.'))=~':Effort:'
        call setline(line('.'), substitute(getline(line('.')),'ort:\zs.*','',''))
        normal A  
        call complete(col('.'),b:v.effort)
    endif
    return ''
endfunction
function! s:AddTagsToDict(line)
    let taglist = s:GetTagList(a:line)
    if !empty(taglist)
        for item in taglist
            execute "let b:v.buftagdict['" . item . "'] = 1"
        endfor
    endif
endfunction

function! s:GetTagList(line)
    let text = getline(a:line+1)
    if (text !~ b:v.drawerMatch) && (text !~ b:v.dateMatch) && (text =~ s:remstring)
        let tags = matchlist(text,':\(\S*\):\s*$')
        if !empty(tags)
            return split(tags[1],':')
        else
            return []
        endif
    else
        return []
    endif
endfunction
function! s:IsTagLine(line)
    let text = getline(a:line)
    return (text !~ b:v.drawerMatch) && (text !~ b:v.dateMatch) && (text =~ s:remstring)
endfunction
function! s:GetTags(line)
    if s:IsTagLine(a:line+1)
        return matchstr(getline(a:line+1),':.*$')
    else
        return ''
    endif
endfunction
function! s:AddTag(tag,line)
    if s:IsTagLine(a:line + 1)
        if matchstr(getline(a:line+1),':'.a:tag.':') ==# ''
            call setline(a:line+1,getline(a:line+1) . ':' .a:tag. ':')
        endif
    else
        call append(a:line, '     :' . a:tag . ':')
    endif
endfunction
function! s:TagInput(line)
    let linetags = s:GetTagList(a:line)
    if empty(linetags)
        call append(a:line,':')
    endif   
    let buftags = s:GetBufferTags()
    let displaytags = deepcopy(buftags)
    call insert(displaytags,'  Exit Menu')
    while 1
        let curstatus = []
        call add(curstatus,0)
        let i = 1
        let linetags = s:GetTagList(a:line)
        while i < len(buftags) + 1 
            if index(linetags, buftags[i-1]) >= 0 
                let cbox = '[ X ]'
                call add(curstatus,1)
            else
                let cbox = '     '
                call add(curstatus,0)
            endif

            let displaytags[i] = cbox . s:PrePad('&'.buftags[i-1],28)
            let i += 1
        endwhile

        let @/=''
        if foldclosed(a:line) > 0
            let b:v.sparse_list = [a:line]
        else
            normal V
        endif
        redraw
        if foldclosed(a:line) > 0
            let b:v.sparse_list = []
        else
            normal V
        endif
        "call insert(displaytags,'Choose tags below:')
        "let key = inputlist(displaytags) - 1 
        let taglist = join(displaytags,"\n") 
        set guioptions+=v
        let key = confirm('Choose tags:',taglist)-1
        set guioptions-=v
        "call remove(displaytags,0)
        if (key == 0)   " || (key == 1)
            " need setline for final redraw
            call setline(a:line+1,getline(a:line+1))
            redraw
            break
        endif
        let curstatus[key] = 1 - curstatus[key]
        let newtags = ''
        let i = 1
        while i < len(curstatus)
            if curstatus[i] == 1
                let newtags .= ':' . buftags[i-1] . ':'
            endif
            let i += 1
        endwhile
        let newtags = substitute(newtags, '::',':','g')
        call setline(a:line+1, repeat(' ',s:Starcount(a:line)+1) . newtags)

    endwhile
    if empty(s:GetTagList(a:line))
        execute a:line+1 .'d'
        execute a:line
    endif   
endfunction

function! s:UnconvertTags(line)
    if s:IsTagLine(a:line+1)
        normal J
    endif
endfunction
function! <SID>GlobalUnconvertTags(state)
    if exists('g:org_emacs_autoconvert') && (g:org_emacs_autoconvert != 0)
        let s:save_cursor = getpos(".")
        let s:last_changenr = a:state
        mkview
        normal A 
        g/^\*\+\s/call s:UnconvertTags(line("."))
        silent! %s/^\(\s*\):\(DEADLINE\|SCHEDULED\|CLOSED\|CLOCK\|<\d\d\d\d-\d\d-\d\d\)/\1\2/
    endif
endfunction
function! <SID>UndoUnconvertTags()
    if exists('g:org_emacs_autoconvert') && (g:org_emacs_autoconvert != 0)
        silent exec 'undo ' . s:last_changenr 
        silent undo
        loadview
        call setpos(".",s:save_cursor)
    endif
endfunction

function! s:ConvertTags(line)
    let tags = matchstr(getline(a:line), '\(:\S*:\)\s*$')
    if tags ># ''
        s/\s\+:.*:\s*$//
        call append(a:line, repeat(' ',s:Starcount(a:line)+1) . tags)
    endif
endfunction
function! <SID>GlobalConvertTags()
    "if exists('g:org_emacs_autoconvert') && (g:org_emacs_autoconvert != 0)
        let save_cursor = getpos(".")
        g/^\*\+\s/call s:ConvertTags(line("."))
        silent! %s/^\(\s*\)\(DEADLINE:\|SCHEDULED:\|CLOSED:\|CLOCK:\|<\d\d\d\d-\d\d-\d\d\)/\1:\2/
        call setpos(".",save_cursor)
    "endif
endfunction
function! s:GlobalFormatTags()
    let save_cursor = getpos(".")
    g/^\*\+\s/call s:FormatTags(line("."))
    call setpos(".",save_cursor)
endfunction
function! s:FormatTags(line)
    let tagmatch = matchlist(getline(a:line),'\(:\S*:\)\s*$')
    if !empty(tagmatch)
        let linetags = tagmatch[1]
        s/\s\+:.*:\s*$//
        " add newtags back in, including new tag
        call setline(a:line,getline(a:line) . '    ' 
                    \ . repeat(' ', winwidth(0) - len(getline(a:line)) - len(linetags) - 15) 
                    \ . linetags)
    endif
endfunction

function! s:FCTest(line)
    if foldclosed(a:line) != a:line
        return a:line . ' ---  ' . foldclosed(a:line)
    endif
endfunction

function! OrgSequenceTodo(line,...)
    if a:0 == 1
        if a:1 ==? 'x'
            let newtodo = ''
        else
            for item in b:v.todoitems
                if item[0] ==? a:1
                    let newtodo = item
                endif
            endfor
        endif
    endif
    let linetext = getline(a:line)
    if (linetext =~ s:org_headMatch) 
        " get first word in line and its index in todoitems
        let tword = matchstr(linetext,'\*\+\s\+\zs\S\+\ze')
        if a:0 == 1
            call s:ReplaceTodo(newtodo)
        else
            call s:ReplaceTodo()
        endif
    endif
endfunction
function! s:NextTodo(curtodo)
    let curtodo = a:curtodo
    " check whether word is in todoitems and make appropriate
    " substitution
    let j = -1
    let newi = -1
    let i = index(b:v.fulltodos,curtodo)
    if i == -1 
        let i = 0
        while i < len(b:v.fulltodos)
            if type(b:v.fulltodos[i]) == type([])
                let j = index(b:v.fulltodos[i],curtodo)
                if j > -1
                    break
                endif
            endif
            let i += 1
        endwhile
    endif

    if i == len(b:v.fulltodos)-1
        let newtodo = ''
    else
        if (i == len(b:v.fulltodos))
            " not found, newtodo is index 0
            let newi = 0
        elseif (i >= 0) 
            let newi = i+1
        endif

        if type(b:v.fulltodos[newi]) == type([])
            let newtodo = b:v.fulltodos[newi][0]
        else
            let newtodo = b:v.fulltodos[newi]
        endif
    endif
    return newtodo
endfunction
function! s:PreviousTodo(curtodo)
    let curtodo = a:curtodo
    " check whether word is in todoitems and make appropriate
    " substitution
    let j = -1
    let newi = -1
    let i = index(b:v.fulltodos,curtodo)
    if i == -1 
        let i = 0
        while i < len(b:v.fulltodos)
            if type(b:v.fulltodos[i]) == type([])
                let j = index(b:v.fulltodos[i],curtodo)
                if j > -1
                    break
                endif
            endif
            let i += 1
        endwhile
    endif

    "if i == len(b:v.fulltodos)-1
    if i == 0
        let newtodo = ''
    else
        if (i == len(b:v.fulltodos))
            " not found, newtodo is index 0
            let newi = len(b:v.fulltodos) - 1
        elseif (i > 0) 
            let newi = i-1
        endif

        if type(b:v.fulltodos[newi]) == type([])
            let newtodo = b:v.fulltodos[newi][0]
        else
            let newtodo = b:v.fulltodos[newi]
        endif
    endif
    return newtodo
endfunction

function! OrgTodoDashboard()
    let save_cursor = getpos('.')
    let save_window = winnr()
    if bufname("%") ==? ('__Agenda__')
        let file = s:filedict[str2nr(matchstr(getline(line('.')), '^\d\d\d'))]
        let lineno = str2nr(matchstr(getline(line('.')),'^\d\d\d\zs\d*'))
        let buffer_lineno = s:ActualBufferLine(lineno,bufnr(file))
        let b:v.todoitems = getbufvar(file,'v').todoitems
        let b:v.todo_setup = getbufvar(file,'v').todo_setup
        let props = s:GetProperties(buffer_lineno, 0, file)
        let Replace_func = function('s:AgendaReplaceTodo')
    else
        exec s:OrgGetHead()
        let props = s:GetProperties(line('.'),0)
        let Replace_func = function('s:ReplaceTodo')
    endif
    echohl MoreMsg
    echo " ================================="
    echo " Todos defined in this document are:"
    echo "    " . b:v.todo_setup
    echo " ================================="
    echo " Press key for a todo command:"
    echo " ---------------------------------"
    echo " f (or n)  cycle current heading's todo Forward/Next"
    echo " b (or p)  cycle current heading's todo Backward/Previous"
    echo " t         mark current heading with initial 'unfinished' state"
    echo " d         mark current heading with main 'finished' state"
    "if bufname("%") !=? ('__Agenda__')
        let i = 1
        for item in b:v.todoitems 
            echo ' ' . i . '   mark current heading as ' . item
            let i += 1
        endfor
    "endif
    echo " "
    echohl Question
    let key = nr2char(getchar())
    redraw
    "let thisline = getline(line('.'))
    "let curTodo = matchstr(thisline, '\*\+ \zs\S\+')
    
    if key =~? 'f\|n'
        call Replace_func()
    elseif key =~? 'b\|p'
        call Replace_func('todo-bkwd')
    elseif key ==? 't'
        call Replace_func(b:v.todoitems[0])
    elseif key ==? 'd'
        let done_state = (type(b:v.fulltodos[-1])==type([])) ? b:v.fulltodos[-1][0] : b:v.fulltodos[-1]
        call Replace_func(done_state)
    elseif key =~ '[1-9]'
        call Replace_func(b:v.todoitems[key-1])
    else
        echo "No todo action selected."
    endif
    echohl None
    exe save_window . 'wincmd w'
    call setpos('.',save_cursor)
endfunction
function! s:AgendaReplaceTodo(...)
    " wrapper to call OrgAgendaGetText to do todo operation
    "  OrgAgendaGetText does double duty (needs to be
    "  refactored) and both retrieves text from main buffer
    "  and handles todo replacements
    if bufname('%') != '__Agenda__'
        echo "Not in agenda, can't use AgendaReplaceTodo"
        return
    endif

    let file = s:filedict[str2nr(matchstr(getline(line('.')), '^\d\d\d'))]
    let b:v.fulltodos = getbufvar(file,'v').fulltodos
    let b:v.todoitems = getbufvar(file,'v').todoitems
    let todoword = matchstr(getline(line('.')), '.* \*\+ \zs\S\+')
    if a:0 == 0
        let newtodo = 'todo-fwd'
    else
        let newtodo = a:1
    endif
    if newtodo == 'todo-fwd'
        let newtodo = s:NextTodo(todoword)
    elseif newtodo == 'todo-bkwd'
        let newtodo = s:PreviousTodo(todoword)
    else
        let newtodo = a:1
    endif
    call OrgAgendaGetText(1,newtodo)

endfunction
function! s:ReplaceTodo(...)
    "a:1 would be newtodo word
    let save_cursor = getpos('.')
    if getline(line('.'))[0] == '*'
        exec s:OrgGetHead()
    endif
    let thisline = getline(line('.'))
    if bufname("%") !=? '__Agenda__'
        let todoword = matchstr(thisline, '\*\+ \zs\S\+')
    else
        let file = s:filedict[str2nr(matchstr(getline(line('.')), '^\d\d\d'))]
        " fulltodos needed for s:NewTodo()
        let b:v.fulltodos = getbufvar(file,'v').fulltodos
        let b:v.todoitems = getbufvar(file,'v').todoitems
        let todoword = matchstr(thisline, '.* \*\+ \zs\S\+')
    endif

    if a:0 == 0
        let newtodo = 'todo-fwd'
    else
        let newtodo = a:1
    endif
    if newtodo == 'todo-fwd'
        let newtodo = s:NextTodo(todoword)
    elseif newtodo == 'todo-bkwd'
        let newtodo = s:PreviousTodo(todoword)
    else
        let newtodo = a:1
    endif

    " if going to main done state check for repeater and change date if necessary
    if  (bufnr("%") != bufnr('Agenda')) && (newtodo =~ b:v.todoDoneMatch[11:])
        let newtodo = s:CheckDateRepeaterDone(todoword, newtodo)
    endif
    let s:last_newtodo = newtodo    " used to set agenda line in next pass from agenda

    if newtodo ># ''
        let newtodo .= ' '
    endif
    if (index(b:v.todoitems,todoword) >= 0) 
        if newtodo ># ''
            let newline = substitute(getline(line(".")),
                        \ '\* ' . todoword.' ',
                        \ '\* ' . newtodo,'g')
        else
            let newline = substitute(getline(line(".")),
                        \ '\* ' . todoword.' ',
                        \ '\* ' . '','g')
        endif
    else
        let newline = substitute(getline(line(".")),
                    \ '\zs\* \ze\S\+', 
                    \ '\* ' . newtodo ,'g')
    endif


    call setline(line("."),newline)
    if exists("*Org_after_todo_state_change_hook") && (bufnr("%") != bufnr('Agenda'))
        let Hook = function("Org_after_todo_state_change_hook")
        call Hook(line('.'),todoword,newtodo)
    endif

    call setpos('.',save_cursor)
endfunction
function! s:CheckDateRepeaterDone(state1,state2)
    "check for date repeater on change of todo to done state
    " and handle logging and resetting of date"
    let newtodo = a:state2
    let props = s:GetProperties(line('.'),0)
    let repeat_pattern = '\d\d\d\d-\d\d-\d\d.*[ +.]+\d\+\S\+.*'
    for dateprop in ['DEADLINE','SCHEDULED','TIMESTAMP']
        let thisdate = get(props,dateprop)
        if thisdate =~ repeat_pattern
            "put in log note
            call OrgConfirmDrawer("LOGBOOK")
            let str = ":- State: " . printf('%.10s','"'.a:state2.'"') . "   from: " . printf('%.10s','"'.a:state1.'"') .
                        \ '    [' . org#Timestamp() . ']'
            call append(line("."), repeat(' ',len(matchstr(getline(line(".")),'^\s*'))) . str)
            exec s:OrgGetHead()
            let newtodo = b:v.todocycle[0]
            "change date as appropriate
            let basedate = matchstr(thisdate,'\d\d\d\d-\d\d-\d\d')
            let cue = '+' . matchstr(thisdate,'+\d*[dwmy]')
            if     thisdate =~ ' +\d*[dwmy]'
                let newdate = DateCueResult(cue,basedate)
            elseif thisdate =~ '\.+\d*[dwmy]'
                let newdate = DateCueResult(cue,org#Timestamp()[0:9])
            elseif thisdate =~ '++\d*[dwmy]'
                let newdate = DateCueResult(cue,basedate)
                let i = 0
                while newdate <= org#Timestamp()[0:9]
                    if i == 9
                        call confirm('Ten adjustments failed to bring to future date.')
                        break
                    endif
                    let newdate = DateCueResult(cue,newdate)
                    let i += 1
                endwhile
            endif
            let mydow = calutil#dayname(newdate)
            call s:SetProp(dateprop,'<' . newdate . ' ' . mydow . thisdate[14:] . '>')
            " break as soon as one repeater is found
            call confirm('Repeater date: entering log and resetting date.')
            break
        endif
    endfor
    return newtodo
endfunction

"Section Navigation Funcs
"
function! s:OrgSubtreeLastLine()
    " Return the line number of the next head at same level, 0 for none
    return s:OrgSubtreeLastLine_l(line("."))
endfunction

function! s:OrgSubtreeLastLine_l(line)
    if a:line == 0
        return line("$")
    endif
    let l:starthead = s:OrgGetHead_l(a:line)
    let l:stars = s:Starcount(l:starthead) 
    let l:mypattern = substitute(b:v.headMatchLevel,'level', '1,'.l:stars, "")    
    let l:lastline = s:Range_Search(l:mypattern,'nW', line("$"), l:starthead) 
    " lastline now has NextHead on abs basis so return end of subtree
    if l:lastline != 0 
        let l:lastline -= 1
    else
        let l:lastline = line("$")
    endif
    return l:lastline

endfunction

function! s:HasAncestorHeadOf(line,ancestor)
    let ultimate = s:OrgUltimateParentHead_l(a:line)
    if (a:line < a:ancestor) || (a:ancestor < ultimate)
        let result = 0
    elseif (a:line == a:ancestor)
        let result = 1
    else
        let test_ancestor = s:OrgParentHead_l(a:line) 
        while 1
            if (test_ancestor == a:ancestor)
               let result = 1
               break
            elseif test_ancestor < ultimate 
               let result = 0
               break
            endif
            let test_ancestor = s:OrgParentHead_l(test_ancestor)
        endwhile
    endif

    return result
endfunction
function! s:OrgUltimateParentHead()
    " Return the line number of the parent heading, 0 for none
    return s:OrgUltimateParentHead_l(line("."))
endfunction

function! s:OrgUltimateParentHead_l(line)
    " returns 0 for main headings, main heading otherwise
    let l:starthead = s:OrgGetHead_l(a:line)

    if s:Ind(l:starthead) >  1
        return s:Range_Search('^* ','bnW',1,l:starthead)
    else
        return 0
    endif
endfunction

function! s:OrgParentHead()
    " Return the line number of the parent heading, 0 for none
    return s:OrgParentHead_l(line("."))
endfunction

function! s:OrgParentHead_l(line)
    " todo -- get b:v.levelstars in here
    let l:starthead = s:OrgGetHead_l(a:line)
    let l:parentheadlevel = s:Starcount(l:starthead) - b:v.levelstars
    if l:parentheadlevel <= 0 
        return 0
    else
        let l:mypattern = substitute(b:v.headMatchLevel,'level',l:parentheadlevel,'')
        return s:Range_Search(l:mypattern,'bnW',1,l:starthead)
    endif
endfunction


function! s:Range_Search(stext, flags, ...)
    " searches range, restores cursor to 
    " beginning position, and returns
    " first occurrence of pattern
    let save_cursor = getpos(".")
    " a:1 and a:2 are stopline and startline
    if a:0 == 2
        let l:stopline = a:1
        " go to startline
        execute a:2 
        normal! $
    elseif a:0 == 1
        let l:stopline = a:1
    else
        let l:stopline = line("$")
    endif
    let l:result =  search(a:stext, a:flags, l:stopline)
    call setpos('.',save_cursor)
    return l:result
endfunction

function! s:OrgGetHead()
    return s:OrgGetHead_l(line("."))
endfunction

function! s:OrgGetHead_l(line)
    if s:IsText(a:line)   
        return s:Range_Search(b:v.headMatch,'nb', 1, a:line)
    else
        return a:line
    endif
endfunction

function! s:OrgPrevSiblingHead()
    return s:OrgPrevSiblingHead_l(line("."))
endfunction
function! s:OrgPrevSiblingHead_l(line)
    if s:Ind(a:line) > 0
        let upperline = s:OrgParentHead_l(a:line)
    else
        let upperline = 0
    endif
    let sibline = s:OrgPrevHeadSameLevel_l(a:line)
    if (sibline <= upperline) 
        let sibline = 0
    endif
    return sibline
endfunction

function! s:OrgNextSiblingHead()
    return s:OrgNextSiblingHead_l(line("."))
endfunction
function! s:OrgNextSiblingHead_l(line)
    if s:Ind(a:line) > 0
        let lastline = s:OrgSubtreeLastLine_l(s:OrgParentHead_l(a:line))
    else
        let lastline = line("$")
    endif
    let sibline = s:OrgNextHeadSameLevel_l(a:line)
    if (sibline > lastline) 
        let sibline = 0
    endif
    return sibline
endfunction

function! s:OrgNextHead()
    " Return the line number of the next heading, 0 for none
    return s:OrgNextHead_l(line("."))
endfunction
function! s:OrgNextHead_l(line)
    return s:Range_Search(b:v.headMatch,'n', line("$"),a:line)
endfunction

function! s:OrgPrevHead()
    " Return the line number of the previous heading, 0 for none

    return s:OrgPrevHead_l(line("."))

endfunction

function! s:OrgPrevHead_l(line)

    return s:Range_Search(b:v.headMatch,'nb', 1, a:line-1)

endfunction

function! s:OrgNextHeadSameLevel()
    " Return the line number of the next head at same level, 0 for none
    return s:OrgNextHeadSameLevel_l(line("."))
endfunction

function! s:OrgNextHeadSameLevel_l(line)
    let level = s:Starcount(a:line) 
    let mypattern = substitute(b:v.headMatchLevel,'level', level, "") 
    let foundline = s:Range_Search(mypattern,'nW', line("$"), a:line)
    if foundline < line ("$")
        return foundline
    else
        if s:Starcount(foundline) > 0
            return foundline
        else
            return 0
        endif
    endif       
endfunction

function! s:OrgPrevHeadSameLevel()
    " Return the line number of the previous heading, 0 for none
    return s:OrgPrevHeadSameLevel_l(line("."))
endfunction
function! s:OrgPrevHeadSameLevel_l(line)
    let l:level = s:Starcount(a:line)
    let l:mypattern = substitute(b:v.headMatchLevel,'level', l:level, "") 
    let foundline = s:Range_Search(mypattern,'nbW', 1, a:line-1)
    if foundline > 1
        return foundline
    else
        if (s:Starcount(foundline) > 0) && (a:line != 1)
            return 1
        else
            return 0
        endif
    endif       

endfunction

function! s:OrgFirstChildHead()
    " Return the line number of first child, 0 for none
    return s:OrgFirstChildHead_l(line("."))
endfunction
function! s:OrgFirstChildHead_l(line)
    let l:starthead = s:OrgGetHead_l(a:line)

    let l:level = s:Starcount(l:starthead) + 1
    let l:nexthead = s:OrgNextHeadSameLevel_l(l:starthead)
    if l:nexthead == 0 
        let l:nexthead = line("$") 
    endif
    let l:mypattern = substitute(b:v.headMatchLevel,'level', l:level, "") 
    return s:Range_Search(l:mypattern,'nW',l:nexthead, l:starthead)
endfunction

function! s:OrgLastChildHead()
    " Return the line number of the last child, 0 for none
    return s:OrgLastChildHead_l(line("."))
endfunction

function! s:OrgLastChildHead_l(line)
    " returns line number of last immediate child, 0 if none
    let l:starthead = s:OrgGetHead_l(a:line)

    let l:level = s:Starcount(l:starthead) + 1

    let l:nexthead = s:OrgNextHeadSameLevel_l(l:starthead)
    if l:nexthead == 0 
        let l:nexthead = line("$") 
    endif

    let l:mypattern = substitute(b:v.headMatchLevel,'level', l:level, "") 
    return s:Range_Search(l:mypattern,'nbW',l:starthead, l:nexthead)

endfunction

function! s:MyLastChild(line)
    " Return the line number of the last decendent of parent line
    let l:parentindent = s:Ind(a:line)
    if s:IsText(a:line+1)
        let l:searchline = s:NextLevelLine(a:line+1)
    else    
        let l:searchline = a:line+1
    endif
    while s:Ind(l:searchline) > l:parentindent
        let l:searchline = l:searchline+1
    endwhile
    return l:searchline-1
endfunction

function! s:NextVisibleHead(line)
    " Return line of next visible heanding, 0 if none
    let save_cursor = getpos(".")

    while 1
        let nh = s:OrgNextHead()
        if (nh == 0) || s:IsVisibleHeading(nh)
            break
        endif
        execute nh
    endwhile

    call setpos('.',save_cursor)
    return nh

endfunction

function! s:FoldStatus(line)
    " adds new heading or text level depending on type
    let l:fc = foldclosed(a:line)
    if l:fc == -1
        let l:status = 'unfolded'
    elseif l:fc > 0 && l:fc < a:line
        let l:status = 'infold'
    elseif l:fc == a:line
        let l:status = 'foldhead'
    endif   
    return l:status
endfunction 

function! OrgEnterFunc()
    let syn_items = synstack(line('.'),col('.'))
    call map(syn_items, "synIDattr(v:val,'name')")
    if (index(syn_items,'Org_Full_Link') >= 0) || ( index(syn_items,'Org_Half_Link') >= 0)
        call FollowLink( OrgGetLink() )
    else
        call OrgNewHead('same')
    endif
endfunction
        
function! OrgNewHead(type,...)
    " adds new heading or text level depending on type
    if a:0 == 1
        normal 
    endif
    execute s:OrgGetHead()
    let l:org_line = line(".")
    let l:linebegin = matchlist(getline(line(".")),'^\(\**\s*\)')[1]
    if s:IsText(line(".")) == 0

        let l:lastline  = s:OrgSubtreeLastLine()  
        if a:type ==? 'levelup'
            let l:linebegin = substitute(l:linebegin,'^\*\{'.b:v.levelstars.'}','','')
        elseif a:type ==? 'leveldown'
            let l:linebegin = substitute(l:linebegin,'^\*',repeat('*',b:v.levelstars+1),'')
        endif   
        call append( l:lastline ,l:linebegin)
        execute l:lastline + 1
        startinsert!

    endif
    return ''
endfunction

function! s:IsText(line)
    " checks for whether line is any kind of text block
    " test if line matches all-inclusive text block pattern
    return (getline(a:line) !~ b:v.headMatch) && (a:line <= line('$')) 
endfunction 

function! s:NextLevelAbs(line)
    " Return line of next heading
    " in absolute terms, not just visible headings
    let l:i = 1
    " go down to next non-text line
    while s:IsText(a:line + l:i)
        let l:i = l:i + 1
        "if (a:line + l:i) == line("$")
        :"  return 0
        "endif  
    endwhile    
    return a:line + l:i
endfunction

function! s:NextLevelLine(line)
    " Return line of next heading
    let l:fend = foldclosedend(a:line)
    if l:fend == -1
        let l:i = 1
        " go down to next non-text line
        while s:IsText(a:line + l:i) 
            let l:i = l:i + 1
        endwhile    
        return a:line + l:i
    else
        return l:fend+1
    endif
endfunction

function! s:HasChild(line)
    " checks for whether heading line has
    " a sublevel
    " checks to see if heading has a non-text sublevel 
    let nh = s:OrgNextHead_l(a:line)
    if nh == 0 
        return 0
    else
        return (s:Ind(nh) > s:Ind(a:line))
    endif
    
"    if s:IsText(a:line + 1) && 
"                \   (s:Ind(s:NextLevelLine(a:line+1)) > s:Ind(a:line))
"        return 1
"    elseif s:IsText(a:line + 1) == 0 && 
"                \   (s:Ind(s:NextLevelLine(a:line)) > s:Ind(a:line))
"        return 1
"    else
"        return 0    
"    endif   
endfunction

function! s:DoFullCollapse(line) 
    let lastline = s:OrgSubtreeLastLine_l(a:line)
    if lastline == a:line 
        return
    else
        while foldclosedend(a:line) < lastline
            normal! zc
        endwhile
    endif
    " make sure headline is not just 
    " text collapse
    " test if line matches all-inclusive text block pattern
 "   while foldclosed(a:line) == -1 && (s:HasChild(a:line) || s:IsText(a:line+1))
 "       normal! zc
 "   endwhile       
 "   if s:IsTextOnlyFold(a:line) && s:HasChild(a:line)
 "       normal! zc
 "       if s:IsTextOnlyFold(a:line) && s:HasChild(a:line)
 "           normal! zc
 "           if s:IsTextOnlyFold(a:line) && s:HasChild(a:line)
 "               normal! zc
 "           endif
 "       endif   
 "   endif   
endfunction

function! s:IsTextOnlyFold(line)
    " checks for whether heading line has full fold
    " or merely a text fold
    "if s:IsText(a:line + 1) && (foldclosed(a:line + 1) == a:line) 
    if s:IsText(a:line + 1) && (foldclosedend(a:line) > 0)
                \    && (s:Ind(foldclosedend(a:line)) <= s:Ind(a:line))
        return 1
    else
        return 0
    endif   
endfunction

function! s:MaxVisIndent(headingline)
    " returns max indent for 
    " visible lines in a heading's subtree
    " used by ShowSubs
    let l:line = a:headingline
    let l:endline = s:OrgSubtreeLastLine()
    "let l:endline = s:MyLastChild(l:line)
    let l:maxi = s:Ind(l:line)
    let l:textflag = 0
    while l:line <= l:endline
        if (s:Ind(l:line) > l:maxi) && 
                    \   ( foldclosed(l:line) == l:line 
                    \  || foldclosed(l:line) == -1  )
            let l:maxi = s:Ind(l:line)
            if s:IsText(l:line)
                let l:textflag = 1
            endif   
        endif
        let l:line = l:line + 1
    endwhile    
    return l:maxi + l:textflag
endfunction

function! OrgShowLess(headingline)
    " collapses headings at farthest out visible level
    let l:maxi = s:MaxVisIndent(a:headingline)
    let l:offset = l:maxi - s:Ind(a:headingline)
    echo 'offset:  ' . l:offset
    if l:offset > 1 
        call s:ShowSubs(l:offset - 1,0)
    elseif l:offset == 1
        normal zc
        "normal! zc
    endif   
endfunction


function! OrgShowMore(headingline)
    " expands headings at furthest out 
    " visible level in a heading's subtree
    let l:maxi = s:MaxVisIndent(a:headingline)
    let l:offset = l:maxi - s:Ind(a:headingline)
    if l:offset >= 0 
        call s:ShowSubs(l:offset + 1,0)
        if l:maxi == s:MaxVisIndent(a:headingline)
            "call OrgSingleHeadingText('expand')
        endif
    endif
endfunction

function! OrgShowSubs(number,withtext)
    " used by comma-num mapping
    " expands/collapses individual heading to level visibility equal to a:number
    if getline(line('.'))[0] != '*'
        exec s:OrgPrevHead()
    endif
    let cur_level = s:Ind(line('.')) - 1
    if a:number > cur_level
        let rel_level = a:number - cur_level 
        if rel_level >= 1
             call s:ShowSubs(rel_level  ,0)
         endif
    else
        call s:DoFullCollapse(line('.'))
    endif
    normal ztkj
endfunction

function! s:ShowSubs(number,withtext)
    " shows specif number of levels down from current 
    " heading, includes text
    " or merely a text fold
    let save_cursor = getpos(".")

    call s:DoFullCollapse(line("."))
    let l:start = foldclosed(line("."))
    if l:start != -1
        let l:end = foldclosedend(line("."))
        exec "" . l:start . "," . l:end . "foldc!"
        exec "normal! zv"
        let to_level = 2
        for to_level in range( 2 , a:number )
            exec "" . l:start . "," . l:end . "foldo"
        endfor
    endif
    if a:withtext == 0
        call OrgSingleHeadingText('collapse')
    endif   

    call setpos(".",save_cursor)
endfunction

" 2 args of start line num and direction ('up' or 'down')
"command -nargs=* OrgMoveLevel :call OrgMoveLevel(<f-args>,v:count1)
nmap <buffer> <localleader>,q :<C-U>call OrgMoveLevel(line('.'),'up',v:count1)<cr>

function! OrgMoveLevel(line, direction,...)
    if a:0>=1
        let mycount = a:1
    else
        let mycount = 1
    endif
    " move a heading tree up, down, left, or right
    let lastline = s:OrgSubtreeLastLine_l(a:line)
    if a:direction ==? 'up'
        let l:headabove = a:line
        let count_message = ''
        for i in range( 1, mycount)
            let lasthead = l:headabove
            let l:headabove = s:OrgPrevSiblingHead_l(l:headabove)
            if l:headabove  > 0
                let count_message = 'Moved up ' . i . ' levels.' 
            elseif i == 1
                " break with no message here
                break
            else
                let l:headabove = lasthead
                if i <= mycount | let count_message .= '  No more siblings above.' | endif
                break
            endif
        endfor
        if l:headabove > 0 
            let l:lines = getline(line("."), lastline)
            call s:DoFullCollapse(a:line)
            silent normal! dd
            call append(l:headabove-1,l:lines)
            execute l:headabove
            call s:ShowSubs(1,0)
            echo count_message
        else
            echo "No sibling heading above in this subtree."
        endif
    elseif a:direction ==? 'down'
        let l:headbelow = a:line
        let count_message = ''
        for i in range(1, mycount)
            let lasthead = l:headbelow
            let l:headbelow = s:OrgNextSiblingHead_l(l:headbelow)
            if l:headbelow  > 0
                let count_message = 'Moved down ' . i . ' levels.' 
            elseif i == 1
                " break with no message here
                break
            else
                let l:headbelow = lasthead
                if i <= mycount | let count_message .= '  No more siblings below.' | endif
                break
            endif
        endfor
        if l:headbelow > 0 
            let endofnext = s:OrgSubtreeLastLine_l(l:headbelow)
            let lines = getline(line("."),lastline)
            silent call append(endofnext,lines)
            execute endofnext + 1
            " set mark and go back to delete original subtree
            normal ma
            execute a:line
            call s:DoFullCollapse(a:line)
            silent normal! dd
            normal g'a
            call s:ShowSubs(1,0)
            echo count_message
        else 
            echo "No sibling below in this subtree."
        endif
    elseif a:direction ==? 'left'
        if s:Ind(a:line) > 2 
            " first move to be last sibling
            let movetoline = s:OrgSubtreeLastLine_l(s:OrgParentHead_l(a:line))
            let lines = getline(line("."),lastline)
            call append(movetoline,lines)
            execute movetoline + 1
            " set mark and go back to delete original subtree
            normal ma
            execute a:line
            call s:DoFullCollapse(a:line)
            silent exe 'normal! dd'
            normal g'a
            " now move tree to the left
            normal ma
            silent execute line(".") ',' . s:OrgSubtreeLastLine() . 's/^' . repeat('\*',b:v.levelstars) .'//'
            call s:DoFullCollapse(a:line)
            normal g'a
            call s:ShowSubs(1,0)
            execute line(".")
        else 
            echo "You're already at main heading level."
        endif       
    elseif a:direction ==? 'right'
        if s:Ind(s:OrgPrevHead_l(a:line)) >= s:Ind(a:line)
            execute a:line . ',' . lastline . 's/^\*/'.repeat('\*',b:v.levelstars+1).'/'
            call s:DoFullCollapse(a:line)
            execute a:line
            call s:ShowSubs(1,0)
        else
            echo "Already at lowest level of this subtree."
        endif   
    endif
endfunction

function! OrgNavigateLevels(direction)
    " Move among headings 
    " direction: "up", "down", "right", "left","end", or 'home'
    if s:IsText(line("."))
        exec s:OrgGetHead()
        return  
    endif

    if s:Ind(line(".")) > 0 
        let lowerlimit = s:OrgParentHead()
        let upperlimit = s:OrgSubtreeLastLine_l(lowerlimit)
    else
        let lowerlimit = 0
        let upperlimit = line("$")
    endif       

    if a:direction ==? "left"
        let dest = s:OrgParentHead()
        let msg = "At highest level."
    elseif a:direction ==? "home"
        let dest = s:OrgParentHead()
        let msg = "At highest level."
    elseif a:direction ==? "right"
        let dest = s:OrgFirstChildHead()
        let msg = (dest > 0 ? "Has subheadings, but none visible."
                    \  : "No more subheadings.")
    elseif a:direction ==? 'end'
        let dest = s:OrgLastChildHead()
        let msg = (dest > 0 ? "Has subheadings, but none visible."
                    \  : "No more subheadings.")
    elseif a:direction ==? 'up'
        let dest = s:OrgPrevHeadSameLevel()
        let msg = "Can't go up more here."
    elseif a:direction ==? 'down'
        let dest = s:OrgNextHeadSameLevel()
        let msg = "Can't go down more."
    endif

    let visible = s:IsVisibleHeading(dest) 
    if (dest > 0) && visible && (dest >= lowerlimit) && (dest <= upperlimit) 
        execute dest
    else 
        echo msg
    endif   
endfunction

function! OrgHeadingFirstText(headline)
    exec a:headline + 1
    let found = 0
    while 1
        let thisline = getline(line('.'))
        if thisline =~ b:v.headMatch
            break
        else
            if (thisline !~ s:remstring) && (thisline !~ b:v.dateMatch)
                \ && (thisline !~ b:v.drawerMatch)
                let found = line('.')
                break
            elseif line('.') == line('$')
                break
            endif
        endif
        exec line('.') + 1
    endwhile
    return found
endfunction

function! OrgUnfoldBodyText(headline)
    if OrgHeadingFirstText(a:headline) > 0
        normal zv
    endif
endfunction

function! OrgExpandWithoutText(tolevel)
    " expand all headings but leave Body Text collapsed 
    " tolevel: number, 0 to 9, of level to expand to
    "  expand levels to 'tolevel' with all body text collapsed
    let l:startline = 1 
    let l:endline = line("$")
    let l:execstr = "set foldlevel=" . string(a:tolevel  )
    "let l:execstr = "set foldlevel=" . (a:tolevel - 1)
    exec l:execstr  
    call OrgBodyTextOperation(l:startline,l:endline,"collapse")
endfunction
function! s:OrgExpandSubtree(headline,...)
    if a:0 > 0
        let withtext = a:1
    endif
    let save_cursor = getpos(".")
    call s:DoFullFold(a:headline)
    "let end = foldclosedend(a:headline)
    "normal! zO
    "call OrgBodyTextOperation(a:headline, end, 'collapse')
    call s:ShowSubs(3,withtext)
    call setpos(".",save_cursor)
endfunction
function! s:OrgExpandHead(headline)
    let save_cursor = getpos(".")
    call s:DoFullFold(a:headline)
    "let end = foldclosedend(a:headline)
    "normal! zO
    "call OrgBodyTextOperation(a:headline, end, 'collapse')
    call s:ShowSubs(1,0)
    while foldclosed(a:headline) !=  -1
        normal! zo
    endwhile
    call setpos(".",save_cursor)
endfunction
function! s:DoFullFold(headline)
    let save_cursor = getpos(".")
    "normal! zo
    call s:DoAllTextFold(a:headline)
    let fend = foldclosedend(a:headline)
    if ((fend > a:headline) && (s:Ind(fend+1) > s:Ind(a:headline)))
                \ || (s:Ind(a:headline+1) > s:Ind(a:headline))
        normal zc
    endif
    call setpos(".",save_cursor)
endfunction
function! s:OrgCycle(headline)
    let save_cursor = getpos(".")
    let end = foldclosedend(a:headline)
    if (end>0) && (s:Ind(end+1) <= s:Ind(a:headline))
        call s:OrgExpandHead(a:headline)
    elseif ((end == -1) && (s:Ind(s:OrgNextHead_l(a:headline)) > s:Ind(a:headline))          
                \ && (foldclosed(s:OrgNextHead_l(a:headline)) > 0))
        let nextsamelevel = s:OrgNextHeadSameLevel_l(a:headline)
        let nextuplevel = s:OrgNextHeadSameLevel_l(s:OrgParentHead_l(a:headline)) 
        if (nextsamelevel > 0) && (nextsamelevel > nextuplevel)
            let endline = nextsamelevel
        elseif nextuplevel > a:headline
            let endline = nextuplevel
        else 
            let endline = line('$')
        endif
        if b:v.cycle_with_text
            call OrgBodyTextOperation(a:headline+1,endline,'expand')
        else
            call s:OrgExpandSubtree(a:headline,0)
        endif
    else
        call s:DoFullFold(a:headline)
    endif
    call setpos(".",save_cursor)
endfunction
function! OrgCycle()
    if getline(line(".")) =~ b:v.headMatch
        call s:OrgCycle(line("."))
    elseif getline(line(".")) =~ b:v.drawerMatch
        normal! za
    elseif getline(line('.')) =~ '^\s*|.*|\s*$'
        " we're in a table, do tab and short circuit
        exec "normal i\tl"
        return
    endif
    " position to top of screen with cursor in col 0
    "normal! z.
    normal! ztkj
endfunction
let s:orgskipthirdcycle = 0
function! OrgGlobalCycle()
    if getline(line('.')) =~ '^\s*|.*|\s*$'
        "short circuit if we're in table
        exec "normal i\<s-tab>l"
        return
    endif
    if exists('w:sparse_on') && w:sparse_on
        call s:ClearSparseTree()
    endif
    if (&foldlevel > 1) && (&foldlevel != b:v.global_cycle_levels_to_show)
        call OrgExpandWithoutText(1)
    elseif &foldlevel == 1
        call OrgExpandWithoutText(b:v.global_cycle_levels_to_show)
    "elseif (&foldlevel > 1) && ( s:orgskipthirdcycle == 0 ) 
    "    let s = getpos('.')
    "    g/^\*\+ /call OrgUnfoldBodyText(line('.'))
    "    call setpos('.',s)
    "    let s:orgskipthirdcycle = 1
    else
        let save_cursor = getpos('.')
        set foldlevel=9999
        silent exec 'g/' . b:v.drawerMatch . '/normal! zc'
        let s:orgskipthirdcycle = 0
        call setpos('.',save_cursor)
    endif
endfunction
function! s:LastTextLine(headingline)
    " returns last text line of text under
    " a heading, or 0 if no text
    let l:retval = 0
    if s:IsText(a:line + 1) 
        let l:i = a:line + 1
        while s:IsText(l:i)
            let l:i = l:i + 1
        endwhile
        let l:retval = l:i - 1
    endif
    return l:retval
endfunction

function! s:ShowSynStack()
    for id in synstack(line("."),col("."))
        echo synIDattr(id,"name")
    endfor  
endfunction
function! s:SignList()
    let signlist = ''
    redir => signlist
    silent execute "sign list"
    redir END
    return split(signlist,'\n')
endfunction
function! s:DeleteSigns()
    " first delete all placed signs
    sign unplace *
    let signs = s:SignList()
    for item in signs
        silent execute "sign undefine " . matchstr(item,'\S\+ \zs\S\+\ze ') 
    endfor
    sign define piet text=>>
    sign define fbegin text=>
    sign define fend text=<
endfunction

function! s:GetPlacedSignsString(buffer)
    let placedstr = ''
    redir => placedstr
        silent execute "sign place buffer=".a:buffer
    redir END
    return placedstr

endfunction
function! s:GetProperties(hl,withtextinfo,...)
    let save_cursor = getpos(".")
    if a:0 >=1
        let curtab = tabpagenr()
        let curwin = winnr()
    " optional args are: a:1 - lineno, a:2 - file
        call s:LocateFile(a:1)
    endif
    let datesdone = 0
    let result1 = {}
    let result = {}

    let linetext = getline(a:hl)
    if linetext[0] == '*'
        let hl = a:hl
    else
        let hl = s:OrgGetHead_l(a:hl)
        let linetext = getline(hl)
    endif

    let result1['LINE'] = hl
    let result1['LEVEL'] = s:Ind(hl) - 1
    "let linetext = getline(hl)
    let result1['ITEM'] = linetext
    let result1['FILE'] = expand("%:t")
    " get date on headline, if any
    if linetext =~ b:v.dateMatch
        let result1['ld'] = matchlist(linetext,b:v.dateMatch)[1]
    endif
    if (getline(hl+1) =~ b:v.tagMatch) && (getline(hl+1) !~ b:v.drawerMatch)
        let result1['TAGS'] = matchstr(getline(hl+1),b:v.tagMatch)
    endif
    if linetext =~ b:v.todoMatch
        let result1['TODO'] = matchstr(linetext,b:v.todoMatch)
    else
        let result1['TODO'] = ''
    endif

    let line = hl + 1
    "let firsttext=0
    while 1
        let ltext = getline(line)
        if ltext =~ b:v.propMatch
            let result = s:GetPropVals(line+1)        
        elseif (ltext =~ '^\s*:\s*CLOCK')
            " do nothing
        elseif  (ltext !~ s:block_line) || (ltext =~ b:v.headMatch)
            call extend(result, result1)
            if datesdone
                call extend(result, dateresult)
            endif
            let result['BLOCK_END'] = line - 1
            break
        elseif (ltext =~ b:v.dateMatch) && !datesdone
            let dateresult = s:GetDateVals(line)
            let datesdone = 1
            " no break, go back around to check for props
        "elseif  (ltext =~ '^\s*$') || (ltext =~ '^\s*:\s*CLOCK')
        endif
        let line += 1
    endwhile
    " *****************************************
    " get inherited properties
    if s:include_inherited_props == 1
        for item in b:v.org_inherited_properties
            if index(keys(result), item) == -1
                let result[item] = s:IProp(hl , item)
            endif
        endfor
    endif
    " *****************************************
    " get last line
    if a:withtextinfo
        "let result['tbegin'] = line 
        let result['TEND'] = s:OrgNextHead_l(hl) - 1
    endif
    if a:0 >= 1
        execute "tabnext ".curtab
        execute curwin . "wincmd w"
    endif
    call setpos(".",save_cursor)
    "debugging
    let g:org_result = result
    return result
endfunction

function! s:GetDateVals(line)
    "result is dict with all date vals 
    let myline = a:line
    let result = {}
    while 1
        let ltext = getline(myline)
        let mtest1 = '<\zs'.b:v.dateMatch.'.*\ze>'
        let mtest2 = '\[\zs'.b:v.dateMatch.'.*\ze\]'
        if ltext =~ mtest1
            "let mymatch = matchlist(ltext, '.\{-}\(<\d\d\d\d-\d\d-\d\d\) \S\S\S\( \d\d:\d\d\)*')
            "let mydate = mymatch[1] . mymatch[2] . '>'
            let mymatch = '^\s*\(:DEADLINE:\|:SCHEDULED:\|:CLOSED:\|:<\)\s*\zs.*'
            let mydate = matchstr(ltext,mymatch)
            let mydate = (mydate[0]=='<') ? mydate[1:-2] : mydate[:-2]
            if ltext =~ 'DEADLINE'
                let dtype = 'DEADLINE'
            elseif ltext =~ 'SCHEDULED'
                let dtype = 'SCHEDULED'
            elseif ltext =~ 'CLOSED'
                let dtype = 'CLOSED'
            else
                let dtype = 'TIMESTAMP'
            endif
        elseif ltext =~ mtest2
            let mydate = matchstr(ltext, mtest2)
            "let mydate = substitute(ltext, '\(\[\d\d\d\d-\d\d-\d\d\) \S\S\S\( \d\d:\d\d\)*','\1\2','')
            let dtype = 'TIMESTAMP_IA'
        else
            break
        endif

        try
            "only add if first of dtype encountered
            if get(result,dtype) == 0
                let result[dtype] = mydate  
            endif
        catch /^Vim\%((\a\+)\)\=:E/ 
        endtry
        let myline += 1
    endwhile
    return result
endfunction

function! s:GetPropVals(line)
    "result is dict with all prop vals 
    let myline = a:line
    let result = {}
    while 1
        let ltext = getline(myline)
        if ltext =~ b:v.propvalMatch
            let mtch = matchlist(ltext, b:v.propvalMatch)
            " mtch[1] is now property, mtch[2] is its value
            try
                let result[toupper(mtch[1])] = mtch[2]   
            catch /^Vim\%((\a\+)\)\=:E/ 
            endtry
        else
            break
        endif
        let myline += 1
    endwhile
    return result
endfunction


function! s:RedoTextIndent()
    set fdm=manual
    set foldlevel=9999
    exec 1
    let myindent = 0
    while line(".") < line("$")
        let line = getline(line("."))
        if matchstr(line,'^\*\+') ># ''
            let myindent = len(matchstr(line,'^\*\+')) + g:org_indent_from_head 
            normal j
        else 
            let text = matchstr(line,'^ *\zs.*')
            let spaces = len(matchstr(line,'^ *'))
            if (spaces != (myindent + 1)) && (text != '')
                call setline(line("."),repeat(' ',myindent+1) . text)
            endif
            normal j
        endif
    endwhile
    exec 1
    set fdm=expr
endfunction

function! s:LoremIpsum()
    let lines = ["Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.", "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?","At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat."]
    return split(lines[s:Random(3)-1],'\%70c\S*\zs \ze')
endfunction

"Section A New Section Here

function! s:Random(range)
    "returns random integer from 1 to range
    return (Rndm() % a:range) + 1
endfunction

function! s:RandomDate()
    let date = string((2009 + s:Random(3) - 1)).'-'.s:Pre0(s:Random(12)).'-'.s:Pre0(s:Random(28))
    let dstring = ''
    if s:Random(3) == 3
        let dstring = date. ' ' . calutil#dayname(date)
    else
        let dstring = date. ' ' . calutil#dayname(date).' '.s:Pre0(s:Random(23)).':'.s:Pre0((s:Random(12)-1)*5)
    endif
    if s:Random(6) == 6
        let dstring .= ' +'.s:Random(4).['d','w','m'][s:Random(3)-1]
    endif
    return '<'.dstring.'>'
    "if a:date_type != ''
    "    call s:SetProp(a:date_type,date)
    "else
    "    silent execute "normal A".date
    "endif
endfunction
function! s:SetRandomDate(...)
    call s:OrgGetHead()
    if a:0 == 1
        let date_type = a:1
    else
        let date_type = ['DEADLINE','TIMESTAMP','SCHEDULED'][s:Random(3)-1]
    endif
    if date_type != ''
        call s:SetProp(date_type,s:RandomDate())
    else
        let hl = line('.')
        let dmatch = match(getline(hl),'\s*<\d\d\d\d-\d\d-\d\d')
        if dmatch > 0
            let dmatch = dmatch - 1
            call setline(hl,getline(hl)[:dmatch])
        endif
        let newd = s:RandomDate()
        execute hl
        execute "normal A ". newd
    endif
endfunction
function! s:SetRandomTodo()
    let newtodo = b:v.todoitems[s:Random(3)-1]
    if index(b:v.todoitems,matchstr(getline(line('.')),'^\*\+ \zs\S*\ze ')) >= 0
        call setline(line('.'),matchstr(getline(line('.')),'^\*\+ ') . newtodo . 
                    \    ' '. matchstr(getline(line('.')),'^\*\+ \S* \zs.*')) 
    else
        call setline(line('.'),matchstr(getline(line('.')),'^\*\+ ') . newtodo . 
                    \    ' '.  matchstr(getline(line('.')),'^\*\+ \zs.*')) 
    endif

endfunction

function! s:UpdateHeadlineSums()
    g/^\s*:TOTALCLOCKTIME/d
    call OrgMakeDict()
    let g:tempdict = {}
    g/^\*\+ /let g:tempdict[line('.')] = b:v.org_dict.SumTime(line('.'),'ITEMCLOCKTIME')
    let items = sort(map(copy(keys(g:tempdict)),"str2nr(v:val)"),'s:NumCompare')
    let i = len(items) - 1
    while i >= 0
        if g:tempdict[items[i]] != '0:00'
            call s:SetProp('TOTALCLOCKTIME',g:tempdict[items[i]],items[i])
        endif
        let i = i-1
    endwhile
endfunction

function! s:IProp(headline,property)
    let prop = a:property
    let parent = s:OrgParentHead_l(a:headline)
    if parent == 0 
        return get(b:v.org_inherited_defaults,prop)
    else
        return s:GetProperties(parent,0)[prop]
    endif
endfunction 

function! OrgMakeDictInherited()
    call OrgProcessConfigLines()
    let b:v.org_dict =  {'0':{'c':[],'CATEGORY':b:v.org_inherited_defaults['CATEGORY'] }}
    function! b:v.org_dict.iprop(ndx,property) dict
        let prop = a:property
        let ndx = a:ndx
        let result = get(self[ndx] , prop,'')
        if (result ==# '') && (ndx != 0)
            "recurse up through parents in tree
            let result = b:v.org_dict.iprop(self[ndx].parent,prop)
        endif
        return result
    endfunction 
    execute 1
   let next = 1
   if s:IsText(line('.'))
      let next = s:OrgNextHead()
   endif
   while next > 0
      execute next
      if getline(line('.'))[1] ==? ' '
          let parent = 0
      else
          let parent = s:OrgParentHead()
      endif
      let b:v.org_dict[line('.')] = {'parent': parent}
      let next = s:OrgNextHead()
   endwhile 
   " parent properties assigned above, now explicity record CATEGORY for 
   " any headlines where CATEGORY won't be inherited
   silent execute 'g/^\s*:CATEGORY:/let b:v.org_dict[s:OrgGetHead()].CATEGORY = matchstr(getline(line(".")),":CATEGORY:\\s*\\zs.*")'
endfunction

function! OrgMakeDict()
    let b:v.org_dict = {}
    call OrgMakeDictInherited()
    function! b:v.org_dict.SumTime(ndx,property) dict
        let prop = a:property
        let result = get(self[a:ndx].props , prop,'0:00')
        " now recursion down the subtree of children in c
        for item in self[a:ndx].c
            let result = s:AddTime(result,b:v.org_dict.SumTime(item,prop))
        endfor
        return result
    endfunction 
    function! b:v.org_dict.Sum(ndx,property) dict
        let prop = a:property
        let result = get(self[a:ndx].props , prop)
        " now recursion down the subtree of children in c
        for item in self[a:ndx].c
            let result += b:v.org_dict.Sum(item,prop)
        endfor
        return result
    endfunction 
    execute 1
   let next = 1
   if s:IsText(line('.'))
      let next = s:OrgNextHead()
   endif
   while next > 0
      execute next
      let b:v.org_dict[line('.')].c = []
      let b:v.org_dict[line('.')].props = s:GetProperties(line('.'),1)
      let parent = b:v.org_dict[line('.')].parent
      call add(b:v.org_dict[parent].c ,line('.'))
      let next = s:OrgNextHead()
   endwhile 
endfunction

function! s:ClearSparseTreeOld()
    set fdm=manual
    silent exe '%s/^*x//'
    silent exe 'undojoin | %s/^*o//'
    "g/^*x/call substitute(getline(line(".")),'^*x',''))
    "g/^*o/call substitute(getline(line(".")),'^*o',''))

    call clearmatches()
    set fdm=expr
    echo "sparse tree view cleared"
endfunction

function! s:SparseTreeRun(term)

    call s:ClearSparseLists()
    let w:sparse_on = 1
    execute 'g/' . a:term . '/call add(b:v.sparse_list,line("."))'
    call s:SparseTreeDoFolds()
    call clearmatches()
    let g:org_first_sparse=1
    let b:v.signstring= s:GetPlacedSignsString(bufnr("%")) 
    set fdm=expr
    set foldlevel=0
    let g:org_first_sparse=1
    execute 'let @/ ="' . a:term .'"'
    execute 'g/' . a:term . '/normal zv'
    set hlsearch
    execute 1
endfunction

function! s:SparseTreeDoFolds()
    let i = len(b:v.sparse_list) - 1
    while i >= 0
        "if b:v.sparse_list[i] + g:org_sparse_lines_after > line("$")
        if b:v.sparse_list[i] + 10 > line("$")
            call remove(b:v.sparse_list, i) "insert(b:v.fold_list,0)
            let i -= 1
            continue
        "elseif (i>0) && (b:v.sparse_list[i] < b:v.sparse_list[i-1] + g:org_sparse_lines_after)
        elseif (i>0) && (b:v.sparse_list[i] < b:v.sparse_list[i-1] + 10)
            call remove(b:v.sparse_list, i) "insert(b:v.fold_list,0)
            let i -= 1
            continue
        else
            let phead = s:OrgUltimateParentHead_l(b:v.sparse_list[i])
            if phead >= 1 
                call insert(b:v.fold_list,phead-1)
            else
                " match is already on level 1 head
                call insert(b:v.fold_list,b:v.sparse_list[i]-1)
            endif
        endif

        let i -= 1
    endwhile        
    "call map(b:v.sparse_list,"v:val + g:org_sparse_lines_after")
    call map(b:v.sparse_list,"v:val + 10")
    call insert(b:v.sparse_list, 1)
    call add(b:v.fold_list, line("$"))

    " sign method to potentially supersede list based method above
    call s:DeleteSigns()
    for item in b:v.sparse_list
        execute "sign place " . item ." line=".item." name=fbegin buffer=".bufnr("%")
    endfor
    for item in b:v.fold_list
        execute "sign place " . item ." line=".item." name=fend buffer=".bufnr("%")
    endfor
    let s:sparse_lines = {}
    for item in b:v.sparse_list
        let s:sparse_lines[item] = 1
        let s:sparse_lines[item-1] = 1
    endfor
    for item in b:v.fold_list
        let s:sparse_lines[item] = 1
    endfor
    " FoldTouch below instead of fdm line above to save time
    " updating folds for just newly changed foldlevel lines
    "call s:FoldTouch()

endfunction

function! s:ClearSparseLists()
    " mylist with lines of matches
    let b:v.sparse_list = []
    " foldlist with line before previous level 1 parent
    let b:v.fold_list = []
    let b:v.sparse_heads = []
endfunction
function! s:ClearSparseTree()
    " mylist with lines of matches
    let w:sparse_on = 0
    let b:v.sparse_list = []
    " foldlist with line before previous level 1 parent
    let b:v.fold_list = []
    set fdm=expr
    set foldlevel=1
    execute 1
endfunction

function! s:FoldTouch()
    " not used right now, since speed increase over 
    " set fdm=expr is uncertain, and was having problems
    " in cleanly undoing it.
    "
    " touch each line in lists to update their fold levels  
    let i = 0
    while i < len(b:v.sparse_list)
        execute b:v.sparse_list[i]
        " insert letter 'b' to  force level update and then undo
        silent execute "normal! ib"
        silent execute "normal! u"
        execute b:v.fold_list[i]
        silent execute "normal! ib"
        silent execute "normal! u"
        let i += 1
    endwhile
endfunction

function! s:OrgIfExpr()
    let mypattern = ''
    " two wrapper subst statements around middle 
    " subst are to make dates work properly with substitute/split
    " operation
    let ifstring_list = [[]]
    let test_str = g:org_search_spec
    if test_str[0] !~ '[+-]'
        let test_str = '+' . test_str
    endif

    let ndx=0
    let result_if_list = []
"try
    while 1
" text string
" curly bracket reg ex string
" numeric comparison
" single operand -- TAG or TODO

        let m = matchlist(test_str,'^\(|'
                    \ .  '\|[+-]\w\{-}[!<>\=]=*".\{-}"'         
                    \ .  '\|[+-]\w\{-}[!\=]=*{.\{-}}'           
                    \ .  '\|[+-]\w\{-}[=<>!]=*[0-9+-.][0-9.]*'  
                    \ .  '\|[+-]\w*\)'                          
                    \ .  '\(.*\)')
        if m[1] == '|'
            call add(ifstring_list,[])
            let ndx += 1
            let test_str = m[2]
            if test_str !~ '+\|-'
                let test_str = '+' . test_str
            endif
        elseif m[1] ># ''
            call add(ifstring_list[ndx],m[1])
            let test_str = m[2]
            if test_str == ''
                break
            endif
        else
            break
        endif
    endwhile
        
    for ifstr in ifstring_list

        let b:v.my_if_list = ifstr
        let ifexpr = ''
        " okay, right now we have split list with each item prepended by + or -
        " now change each item to be a pattern match equation in parens
        " e.g.,'( prop1 =~ propval) && (prop2 =~ propval) && (thisline =~tag)
        let i = 0
        "using while structure because for structure doesn't allow changing
        " items?
        while i < len(b:v.my_if_list)
            let item = b:v.my_if_list[i]
            if item[0] !~ '+\|-'
                let item = '+' . item
            endif
            " Propmatch has '=' sign and something before and after
            if item =~ 'TEXT=\S.*'
                let mtch = matchlist(item[1:],'\(\S.*\)=\(\S.*\)')
                let b:v.my_if_list[i] = "(s:Range_Search('" . mtch[2] . "','nbW'," 
                let b:v.my_if_list[i] .= 'tbegin,tend)> 0)'
                let i += 1
                " loop to next item
                continue
            endif
            if item =~ '\S.*[=><]\S.*'
                if item =~ '[^<>!]=\\('
                    let item = substitute(item,'=','=~','')
                elseif item =~ '[^!]={'
                    let item = substitute(item,'[^!]\zs=','=~','')
                    let item = substitute(item,'{','"','')
                    let item = substitute(item,'}','"','')
                elseif item =~ '!={'
                    let item = substitute(item,'!=','!~','')
                    let item = substitute(item,'{','"','')
                    let item = substitute(item,'}','"','')
                elseif item =~ '[^<>!]=[^=]'
                    let item = substitute(item,'=','==','')
                endif
                let pat = '\(\S\{-}\)\(==\|=\~\|!\~\|>=\|<=\|!=\|<\|>\)\(\S.*\)'
                let mtch = matchlist(item[1:],pat)
                let mtch[1] = toupper(mtch[1])
                if mtch[3] =~ '^[+\-0-9.][0-9.]*$'
                    " numeric comparison
                    let b:v.my_if_list[i] = (item[0]=='-' ? '!' : '') . '(get(lineprops,"' . mtch[1] . '") ' . mtch[2]. mtch[3] . ')'
                else
                    " string comparison
                    let rightside="'".mtch[3][1:-2]."'"
                    let b:v.my_if_list[i] = (item[0]=='-' ? '!' : '') . '(get(lineprops,"' . mtch[1] . '","") ' . mtch[2]. rightside. ')'
                             " line below is addd on to exclude headings not
                             " having an entry at all from the comparison
                             "   \ . '&& (get(lineprops,"' . mtch[1] . '","") != "")'
                endif
                let i += 1
                " loop to next item
                continue
            endif

            " it must be a todo or tag item
            if item[0] ==? '+'
                let op = '=~'
            elseif item[0] ==? '-'
                let op = '!~'
            endif
            if index(b:v.todoitems,item[1:]) >= 0
                let item = '(thisline ' . op . " '^\\*\\+\\s*" . item[1:] . "')"
                let b:v.my_if_list[i] = item
            elseif item[1:] =~? 'UNFINISHED_TODO\|UNDONE_TODO'
                let item = '(thisline ' . op . " '" . b:v.todoNotDoneMatch . "')"
                let b:v.my_if_list[i] = item
            elseif item[1:] =~? 'FINISHED_TODO\|DONE_TODO'
                let item = '(thisline ' . op . " '" . b:v.todoDoneMatch . "')"
                let b:v.my_if_list[i] = item
            elseif item[1:] ==? 'ANY_TODO'
                let item = '(thisline ' . op . " '" . b:v.todoMatch . "')"
                let b:v.my_if_list[i] = item
            else
                "not a todo so we treat it as a tag item
                let item = '(thisline ' . op . " ':" . item[1:] . ":')"
                let b:v.my_if_list[i] = item
            endif
            let i += 1 
        endwhile    
        let i = 0
        let b:v.check1 = b:v.my_if_list
        let ifexpr = ''
        while i < len(b:v.my_if_list) 
            let ifexpr .= b:v.my_if_list[i]
            if i < len(b:v.my_if_list) - 1
                let ifexpr .= ' && '
            endif
            let i += 1
        endwhile

        "return ifexpr
        call add(result_if_list, ifexpr)
    endfor
"    let succeeded = 1
"finally
"    if !exists('succeeded')
"        return []
"    else
        return result_if_list
"    endif
endtry
endfunction

function! s:CheckIfExpr(line,ifexpr,...)
    " this is 'ifexpr' eval func used for agenda date searches
    let headline = s:OrgGetHead_l(a:line)
    " 0 arg is to not get start and end line numbers
    let lineprops=s:GetProperties(headline,0)
    " _thisline_ is variable evaluated in myifexpr
    let thisline = getline(headline)
    if s:IsTagLine(headline + 1)
        let thisline .= ' ' . getline(headline+1)
    endif
    let result = 0
    for item in a:ifexpr
        if eval(item) == 1
            let result = 1
            break
        endif
    endfor
    return result

endfunction

function! FileDict()
    return s:filedict
endfunction

function! s:OrgIfExprResults(ifexpr,...)
    " ifexpr has single compound expression that will evaluate
    " as true only for desired lines
    let sparse_search = 0
    if a:0 > 0
        let sparse_search = a:1
    endif

    "let myifexpr = a:ifexpr
    
    execute 1
    if getline(line('.'))!~ '^\*\+ '
        let headline = s:OrgNextHead()
    else
        let headline = 1
    endif
    let g:checkexpr = a:ifexpr
    while 1
        if headline > 0 
            execute headline
            " _thisline_ is variable evaluated in myifexpr
            let thisline = getline(headline)
            if s:IsTagLine(headline + 1)
                let thisline .= ' ' . getline(headline+1)
            endif
            " lineprops is main variable tested in 'ifexpr' 
            " expression that gets evaluated
            "let lineprops = s:GetProperties(headline,1)
            let lineprops = b:v.org_dict[headline].props

            for if_item in a:ifexpr
                " next line is to fix for text area search
                " now that we can reference tbegin and tend
                let myifexpr = substitute(if_item,'TBEGIN,TEND',get(lineprops,'TBEGIN') .','. get(lineprops,'TEND'),"")
                "
                "********  eval() is what does it all ***************
                if eval(myifexpr)
                    if sparse_search
                        let keyval = headline
                    else
                        "let keyval = s:PrePad(index(s:agenda_files_copy, lineprops.file . '.org'),3,'0') . s:PrePad(headline,5,'0')
                        "let keyval = s:PrePad(lineprops.file,3,'0') . s:PrePad(headline,5,'0')
                        let keyval = s:PrePad(s:filenum,3,'0') . s:PrePad(headline,5,'0')
                    endif

                    let g:adict[keyval]=lineprops
                    if !exists('g:adict[keyval].CATEGORY')
                        let g:adict[keyval].CATEGORY = b:v.org_dict.iprop(headline,'CATEGORY')
                    endif
                    break
                endif
            endfor
            normal l
            let headline = s:OrgNextHead() 
        else
            break
        endif
    endwhile
endfunction

function! s:MakeResults(search_spec,...)
    let s:filedict = copy(g:agenda_files)
    let sparse_search = 0
    if a:0 > 0
        let sparse_search = a:1
    endif
    let save_cursor = getpos(".")
    let curfile = substitute(expand("%"),' ','\\ ','g')

    let g:org_search_spec = a:search_spec
    let g:org_todoitems=[]
    let g:adict = {}
    let g:datedict = {}
    let s:agenda_files_copy = copy(g:agenda_files)
    " fix so copy doesn't have full path. .  .
    "call map(s:agenda_files_copy, 'matchstr(v:val,"[\\/]") > "" ? matchstr(v:val,"[^/\\\\]*$") : v:val')
    if sparse_search 
        call OrgMakeDict()
        let ifexpr = s:OrgIfExpr()
        call s:OrgIfExprResults(ifexpr,sparse_search)
    else
        let g:in_agenda_search = 1
        for file in g:agenda_files
            "execute 'tab drop ' . file
            call s:LocateFile(file)
            let s:filenum = index(g:agenda_files,file)
            call OrgMakeDict()
            let ifexpr = s:OrgIfExpr()
            let g:org_todoitems = extend(g:org_todoitems,b:v.todoitems)
            call s:OrgIfExprResults(ifexpr,sparse_search)
        endfor
        unlet g:in_agenda_search
        call s:LocateFile(curfile)
    endif
    call setpos(".",save_cursor)
endfunction
function! s:OrgSaveLocation()
    let file_loc = bufname('%') ==? '__Agenda__' ? '__Agenda__' : expand('%:p')
    let g:location = [ file_loc , getpos('.') ]
endfunction
function! s:OrgRestoreLocation()
    if expand('%:p') != g:location[0]
        call s:LocateFile( g:location[0] )
    endif
    call setpos( '.', g:location[1] )
endfunction
function! s:DaysInMonth(date)
        let month = str2nr(a:date[5:6])
        let year = str2nr(a:date[0:3])
        if (month == 2) && (year % 4) 
            let days = 28 
        elseif month == 2
            let days = 29
        elseif index([1,3,5,7,8,10,12],month) >= 0 
            let days = 31
        else
            let days = 30
        endif
        return days
endfunction

function! s:MakeAgenda(date,count,...)
    if a:0 >= 1
        let g:org_search_spec = a:1
    else
        let g:org_search_spec = ''
    endif
    let as_today = ''
    if a:0 >= 2
        let as_today = a:2
    endif
    
    call s:OrgSaveLocation()

    let l:count = a:count
    if l:count ==? 'd' | let l:count = 1 | endif
    if l:count ==? 'w'
        let g:agenda_startdate = calutil#cal(calutil#jul(a:date) - calutil#dow(a:date))
        let g:org_agenda_days=7
    elseif l:count ==? 'm'
        let g:agenda_startdate = a:date[0:7].'01'
        let g:org_agenda_days = s:DaysInMonth(a:date)
    elseif l:count ==? 'y'
        let g:agenda_startdate = a:date[0:3].'-01-01'
        let g:org_agenda_days = ( a:date[0:3] % 4 == 0 ) ? 366 : 365
    else
        let g:agenda_startdate = a:date
        let g:org_agenda_days = l:count
    endif
    if l:count == 1 | let as_today = g:agenda_startdate | endif
    let g:adict = {}
    let s:filedict = copy(g:agenda_files)
    let s:agenda_files_copy = copy(g:agenda_files)
    let g:datedict = {}
    call s:MakeCalendar(g:agenda_startdate,g:org_agenda_days)
    let g:in_agenda_search=1
    for file in g:agenda_files
        call s:LocateFile(file)
        let b:v.org_dict = {}
        " only do CATEGORIES for dict if no search spec
        if g:org_search_spec ==# ''
            call OrgMakeDictInherited()
        else
            call OrgMakeDict()
        endif
        let s:filenum = index(g:agenda_files,file)
        let t:agenda_date=a:date
        if as_today ># ''
            call s:GetDateHeads(g:agenda_startdate,g:org_agenda_days,as_today)
        else 
            call s:GetDateHeads(g:agenda_startdate,g:org_agenda_days)
        endif
    endfor
    unlet g:in_agenda_search

    call s:OrgRestoreLocation()

endfunction

function! s:NumCompare(i1, i2)
    return a:i1 == a:i2 ? 0 : a:i1 > a:i2 ? 1 : -1
endfunc

function! OrgRunSearch(search_spec,...)
        "set mouseshape-=n:busy,v:busy,i:busy
    if bufnr('Calendar') > 0 
        execute 'bw!' . bufnr('Calendar')
    endif   

    try
    "if bufname('%') ==? '__Agenda__'
        "wincmd k
        "bwipeout __Agenda__
    "endif
    if bufnr('__Agenda__') >= 0
        bwipeout __Agenda__
    endif

    let g:agenda_head_lookup={}
    let sparse_search = 0
    let search_type = ''
    if a:0 > 0
        if a:1 == 1
            let sparse_search = a:1
        else
            let search_type=a:1
        endif
    endif
    let g:adict={}
    let g:agenda_date_dict={}
    if !exists("g:agenda_files") || (g:agenda_files == [])
        if has('dialog_con') || has('dialog_gui')
            unsilent call confirm("No agenda files defined.  Will add current file to agenda files.")
        endif
        call s:CurfileAgenda()
    endif
    if exists('b:v.sparse_list') && (len(b:v.sparse_list) > 0)
        call s:ClearSparseTree()
    endif
    call s:MakeResults(a:search_spec,sparse_search)

    if sparse_search
        call s:ResultsToSparseTree()
    else
        call s:ResultsToAgenda( search_type )
    endif

    finally
        "set mouseshape-=n:busy,v:busy,i:busy
    endtry
endfunction
function! s:ResultsToAgenda( search_type )
    " make agenda buf have its own todoitems, need
    " to get rid of g:... so each agenda_file can have
    " its own todoitems defined. . . "
    let todos = b:v.todoitems
    let todoNotDoneMatch = b:v.todoNotDoneMatch
    let todoDoneMatch = b:v.todoDoneMatch
    let todoMatch = b:v.todoMatch
    let fulltodos = b:v.fulltodos
    if bufnr('__Agenda__') >= 0
        bwipeout __Agenda__
    endif
    :AAgenda
    let b:v={}
    let b:v.todoitems = todos
    let b:v.todoNotDoneMatch = todoNotDoneMatch
    let b:v.todoDoneMatch = todoDoneMatch
    let b:v.todoMatch = todoMatch
    let b:v.fulltodos = fulltodos
    %d
    set nowrap
    map <buffer> <silent> <tab> :call OrgAgendaGetText()<CR>
    map <buffer> <silent> <s-CR> :call OrgAgendaGetText(1)<CR>
    map <silent> <buffer> <c-CR> :MyAgendaToBuf<CR>
    map <silent> <buffer> <CR> :AgendaMoveToBuf<CR>
    nmap <silent> <buffer> ,r :call OrgRunSearch(matchstr(getline(1),'spec: \zs.*$'))<CR>
    nmap <silent> <buffer> <s-up> :call OrgDateInc(1)<CR>
    nmap <silent> <buffer> <s-down> :call OrgDateInc(-1)<CR>
    nmap <silent> <buffer> <localleader>t    :call OrgTodoDashboard()<CR>
    "call matchadd( 'OL1', '\s\+\*\{1}.*$' )
    "call matchadd( 'OL2', '\s\+\*\{2}.*$') 
    "call matchadd( 'OL3', '\s\+\*\{3}.*$' )
    "call matchadd( 'OL4', '\s\+\*\{4}.*$' )
    call s:AgendaBufHighlight()
    "wincmd J
    let i = 0
    call s:ADictPlaceSigns()
    call setline(1, ["Headlines matching search spec: ".g:org_search_spec,''])
    if a:search_type ==? 'agenda_todo'
        let msg = "Press num to redo search: "
        let numstr= ''
        nmap <buffer> r :call OrgRunSearch(g:org_search_spec,'agenda_todo')<cr>
        let tlist = ['ANY_TODO','UNFINISHED_TODOS', 'FINISHED_TODOS'] + s:Union(g:org_todoitems,[])
        for item in tlist
            let num = index(tlist,item)
            let numstr .= '('.num.')'.item.'  '
            execute "nmap <buffer> ".num."  :call OrgRunSearch('+".tlist[num]."','agenda_todo')<CR>"
        endfor
        call append(1,split(msg.numstr,'\%72c\S*\zs '))
    endif
    for key in sort(keys(g:adict))
        call setline(line("$")+1, key . ' ' . 
                    \ printf("%-12.12s",g:adict[key].CATEGORY ) . ' ' .
                    \ s:PrePad(matchstr(g:adict[key].ITEM,'^\*\+ '),8) .
                    \ matchstr(g:adict[key].ITEM,'\* \zs.*$'))
                    "\ org#Pad(g:adict[key].file,13)  . 
        let i += 1
    endfor
endfunction

function! s:ResultsToSparseTree()
        "call s:ClearSparseTree()
        let w:sparse_on = 1
        let temp = []
        for key in keys(g:adict)
            call add(temp,g:adict[key].LINE)
        endfor
        let b:v.sparse_list = sort(temp,'s:NumCompare')
        "for key in keys(g:adict)
        "   call add(b:v.sparse_heads,str2nr(key))
        "endfor
        "for item in sort(b:v.sparse_heads,'NumCompare')
        call sort(b:v.fold_list,"s:NumCompare")
        call s:SparseTreeDoFolds()
        "for item in sort(b:v.fold_list,'NumCompare')
        set fdm=expr
        set foldlevel=0
        call clearmatches()
        for item in b:v.sparse_list
            if item > 11
                execute item - g:org_sparse_lines_after
                normal! zv
                call matchadd('Search','\%' . (item - g:org_sparse_lines_after) . 'l')
            endif
        endfor
        execute 1
endfunction

function! s:TestTime()
    let g:timestart=join(reltime(), ' ') 
    let g:start = strftime("%")
    let i = 0
    set fdm=expr
    let g:timefinish=join(reltime(), ' ')
    echo g:timestart . ' --- ' . g:timefinish
endfunction
function! s:TestTime2(fu)
    let g:timestart=join(reltime(), ' ') 
    let g:start = strftime("%")
    let i = 0
    execute a:fu
    let g:timefinish=join(reltime(), ' ')
    echo g:timestart . ' --- ' . g:timefinish
endfunction

function! s:ADictPlaceSigns()
    let myl=[]
    call s:DeleteSigns()  " signs were placed during search
    for key in keys(g:adict)
        let headline = matchstr(key,'^\d\d\d\zs\d\+')
        let filenum = str2nr(key[0:2])
        let buf = bufnr(s:agenda_files_copy[filenum])
        try
            silent execute "sign place " . headline . " line=" 
                        \ . headline . " name=piet buffer=" . buf  
        catch 
            echo "ERROR: headline " . headline . ' and buf ' .buf 
            echo key .', '. matchstr(key,'^.*\ze_\d\+$')
        finally
        endtry
    endfor
endfunction
function! s:DateDictPlaceSigns()
    let myl=[]
    call s:DeleteSigns()  " signs were placed in GetDateHeads
    for key in keys(g:agenda_date_dict)
        let myl = get(g:agenda_date_dict[key], 'l')
        if len(myl) > 0
            for item in myl
                let dateline = matchstr(item,'^\d\d\d\zs\d\+')
                let filenum = str2nr(item[0:2])
                let buf = bufnr(s:agenda_files_copy[filenum])
                try
                    silent execute "sign place " . dateline . " line=" 
                                \ . dateline . " name=piet buffer=" . buf  
                catch 
                    echo "ERROR: headline " . headline . ' and buf ' . buf . ' and dateline ' . dateline

                    echo (matchstr(item,'^\d\+\s\+\zs\S\+') . '.org')
                finally
                endtry
            endfor
        endif
    endfor
endfunction

function! s:DateDictToScreen()
    let message = ["Press <f> or <b> for next or previous period, q to close agenda," ,
                \ "<Enter> on a heading to synch main file, <ctl-Enter> to goto line," ,
                \ "<tab> to cycle heading text, <shift-Enter> to cycle Todos.",'']
    let search_spec = g:org_search_spec ># '' ? g:org_search_spec : 'None - include all heads'
    call add(message,"Agenda view for " . g:agenda_startdate 
                \ . " to ". calutil#cal(calutil#jul(g:agenda_startdate)+g:org_agenda_days-1)
                \ . ' matching FILTER: ' . search_spec  )
                "\ . ' with SearchSpec=' . search_spec  )
    call add(message,'')
    call setline(1,message)
    call s:DateDictPlaceSigns()
    let gap = 0
    let mycount = len(keys(g:agenda_date_dict)) 
    for key in sort(keys(g:agenda_date_dict))
        if empty(g:agenda_date_dict[key].l)
            let gap +=1
            call setline(line('$')+ 1,g:agenda_date_dict[key].marker)
        else
            if (gap > g:org_agenda_skip_gap) && (g:org_agenda_minforskip <= mycount)
                silent execute line("$")-gap+2 . ',$d'
                call setline(line("$"), ['','  [. . . ' .gap. ' empty days omitted ]',''])
            endif
            let gap = 0
            call setline(line('$')+ 1,g:agenda_date_dict[key].marker)
            call setline(line('$')+ 1,g:agenda_date_dict[key].l)
            if ((g:org_agenda_days == 1) || (key == strftime("%Y-%m-%d"))) && exists('g:org_timegrid') && (g:org_timegrid != [])
                call s:PlaceTimeGrid(g:agenda_date_dict[key].marker)
            endif
        endif
    endfor
    if (gap > g:org_agenda_skip_gap) && (g:org_agenda_minforskip <= mycount)
        silent execute line("$")-gap+2 . ',$d'
        call setline(line("$"), ['','  [. . . ' .gap. ' empty days omitted ]',''])
    endif
endfunction
function! s:PlaceTimeGrid(marker)
    let grid = s:TimeGrid(g:org_timegrid[0],g:org_timegrid[1],g:org_timegrid[2])
    call search(a:marker)
    exec line('.')+1
    if getline(line('.'))=~'\%24c\d\d:\d\d'
        "if, at least one time item put grid lines in and then sort with other time items
        let start = line('.')
        call append(line('.'),grid)
        while (matchstr(getline(line('.')),'\%24c\d\d:\d\d'))
            if line('.') != line('$')
                exec line('.')+1
                let end = line('.')-1
            else
                let end = line('.')
                break
            endif
        endwhile
        exec start.','.end.'sort /.*\%23c/'
        " now delete duplicates where grid is same as actual entry
        exec end
        while line('.') >= start
            let match1 = matchstr(getline(line('.')),'\%24c.*\%29c')
            let match2 = matchstr(getline(line('.')-1),'\%24c.*\%29c')
            if match1 ==? match2
                if match1[0] ==? ' '
                    normal ddk
                else
                    normal kdd
                endif
            endif
            exec line('.')-1
        endwhile
    endif
endfunction
function! OrgRunAgenda(date,count,...)
    try

    if bufname('%') ==? '__Agenda__'
        " vsplit agenda ***************
        "wincmd h
        " *******************
        wincmd k
    endif
    let g:agenda_head_lookup={}
    let win = bufwinnr('Calendar')
    if win >= 0 
        execute win . 'wincmd w'
        normal ggjjj
        wincmd l
        execute 'bw!' . bufnr('Calendar')

    endif   
    if !exists("g:agenda_files") || (g:agenda_files == [])
        unsilent call confirm("No agenda files defined.  Will add current file to agenda files.")
        call s:CurfileAgenda()
    endif
    if exists('b:v.sparse_list') && (len(b:v.sparse_list) > 0)
        call s:ClearSparseTree()
    endif
    " a:1 is search_spec, a:2 is "today" for search
    if a:0 == 1
        call s:MakeAgenda(a:date,a:count,a:1)
    elseif a:0 == 2
        call s:MakeAgenda(a:date,a:count,a:1,a:2)
    else
        call s:MakeAgenda(a:date,a:count)
    endif
    let todos = b:v.todoitems
    let todoNotDoneMatch = b:v.todoNotDoneMatch
    let todoDoneMatch = b:v.todoDoneMatch
    let todoMatch = b:v.todoMatch
    let fulltodos = b:v.fulltodos
    if bufnr('__Agenda__') >= 0
        bwipeout __Agenda__
    endif
    :AAgenda
    let b:v={}
    let b:v.todoitems = todos
    let b:v.todoNotDoneMatch = todoNotDoneMatch
    let b:v.todoDoneMatch = todoDoneMatch
    let b:v.todoMatch = todoMatch
    let b:v.fulltodos = fulltodos
    silent exe '%d'
    set nowrap
    nmap <silent> <buffer> <c-CR> :MyAgendaToBuf<CR>
    nmap <silent> <buffer> <CR> :AgendaMoveToBuf<CR>
    nmap <silent> <buffer> vt :call OrgRunAgenda(strftime("%Y-%m-%d"), 'd',g:org_search_spec)<CR>
    nmap <silent> <buffer> vd :call OrgRunAgenda(g:agenda_startdate, 'd',g:org_search_spec,g:agenda_startdate)<CR>
    nmap <silent> <buffer> vw :call OrgRunAgenda(g:agenda_startdate, 'w',g:org_search_spec)<CR>
    nmap <silent> <buffer> vm :call OrgRunAgenda(g:agenda_startdate, 'm',g:org_search_spec)<CR>
    nmap <silent> <buffer> vy :call OrgRunAgenda(g:agenda_startdate, 'y',g:org_search_spec)<CR>
    nmap <silent> <buffer> f :<C-U>call OrgAgendaMove('forward',v:count1)<cr>
    nmap <silent> <buffer> b :<C-U>call OrgAgendaMove('backward',v:count1)<cr>
    nmap <silent> <buffer> <tab> :call OrgAgendaGetText()<CR>
    nmap <silent> <buffer> <s-CR> :call OrgAgendaGetText(1)<CR>
    "nmap <silent> <buffer> r :call OrgRunAgenda(g:agenda_startdate, g:org_agenda_days,g:org_search_spec)<CR>
    nmap <silent> <buffer> r :call OrgRefreshCalendarAgenda()<CR>
    nmap <silent> <buffer> <s-up> :call OrgDateInc(1)<CR>
    nmap <silent> <buffer> <s-down> :call OrgDateInc(-1)<CR>
    command! -buffer -nargs=* Agenda :call OrgAgendaCommand(<f-args>)

    "wincmd J
    for key in keys(g:agenda_date_dict)
        call sort(g:agenda_date_dict[key].l, 's:AgendaCompare')
    endfor
    call s:DateDictToScreen()
    if win >= 0
        let year = matchstr(t:agenda_date,'\d\d\d\d')
        let month = matchstr(t:agenda_date,'\d-\zs\d\d\ze-')
        execute 'Calendar ' . year .' '. str2nr(month) 
        execute bufwinnr('Agenda').'wincmd w'
    endif
    " rigamarole to get status line window, if any, back to zero height
    let curheight=winheight(0)
    wincmd k
    resize
    wincmd j
    execute 1
    execute 'resize ' . curheight   

    finally
        "set mouseshape-=n:busy,v:busy,i:busy
    endtry

endfunction
function! OrgRefreshCalendarAgenda()
    let g:org_search_spec = matchstr(getline(5),'FILTER:\s*\zs.*$')
    if g:org_search_spec =~ '\c^None'
       let g:org_search_spec = ''
    endif
    call OrgRunAgenda(g:agenda_startdate, g:org_agenda_days,g:org_search_spec)
endfunction
function! s:Resize()
    let cur = winheight(0)
    resize 
    resize cur
endfunction

function! s:GetDateHeads(date1,count,...)
    let save_cursor=getpos(".")
    if g:org_search_spec ># ''
        let b:v.agenda_ifexpr = s:OrgIfExpr()
    endif
    let g:date1 = a:date1
    let date1 = a:date1
    let date2 = calutil#Jul2Cal(calutil#Cal2Jul(split(date1,'-')[0],split(date1,'-')[1],split(date1,'-')[2]) + a:count)
    execute 1
    "while search('\(\|[^-]\)[[<]\d\d\d\d-\d\d-\d\d','W') > 0
    while search('[^-][[<]\d\d\d\d-\d\d-\d\d','W') > 0
        let repeatlist = []
        let line = getline(line("."))
        let datematch = matchstr(line,'[[<]\d\d\d\d-\d\d-\d\d\ze')
        let repeatmatch = matchstr(line, '<\d\d\d\d-\d\d-\d\d.*[ +.]+\d\+\S\+.*>\ze')
        if repeatmatch != ''
            " if date has repeater then call once for each repeat in period
            let repeatlist = s:RepeatMatch(repeatmatch[1:],date1,date2)
            for dateitem in repeatlist
                if a:0 == 1
                    call s:ProcessDateMatch(dateitem,date1,date2,a:1)
                else
                    call s:ProcessDateMatch(dateitem,date1,date2)
                endif
            endfor
        else
            if (datematch[0]!='[') || g:org_clocks_in_agenda
                if a:0 == 1
                    call s:ProcessDateMatch(datematch[1:],date1,date2,a:1)
                else
                    call s:ProcessDateMatch(datematch[1:],date1,date2)
                endif
            endif
        endif
        "endif
    endwhile
    call setpos(".",save_cursor)
endfunction

function! s:ProcessDateMatch(datematch,date1,date2,...)
    if a:0 > 0
        let today = a:1
    else
        let today = strftime("%Y-%m-%d")
    endif
    let datematch = a:datematch
    let rangedate = matchstr(getline(line(".")),'--<\zs\d\d\d\d-\d\d-\d\d')

    let locator = s:PrePad(s:filenum,3,'0') . s:PrePad(line('.'),5,'0') . '  '
    
    let g:myline = s:OrgGetHead_l(line('.'))
    "let g:myline = s:OrgParentHead_l(line('.'))
    "if g:myline == 0
    "    let filename = org#Pad(b:v.org_dict[g:myline].CATEGORY,13)
    "else
        "let filename = org#Pad(b:v.org_dict.iprop(g:myline,'CATEGORY'),12) . ' '
        let filename = printf("%-12.12s",b:v.org_dict.iprop(g:myline,'CATEGORY')) . ' '
    "endif
    let line = getline(line("."))
    let date1 = a:date1
    let date2 = a:date2
    let s:headline=0
    if (datematch >= date1) && (datematch < date2)
                \ && ((g:org_search_spec ==# '') || (s:CheckIfExpr(line("."),b:v.agenda_ifexpr)))
        let mlist = matchlist(line,'\(DEADLINE\|SCHEDULED\|CLOSED\)')
        call s:SetHeadInfo()
        if empty(mlist)
            " it's a regular date, first check for time parts
            let tmatch = matchstr(line,' \zs\d\d:\d\d\ze.*[[>]')
            if tmatch ># ''
                let tmatch2 = matchstr(line,'<.\{-}-\zs\d\d:\d\d\ze.*>')
                if tmatch2 ># ''
                    let tmatch .= '-' . tmatch2
                else
                    if match(line,':\s*CLOCK\s*:') >= 0
                        let tmatch .= '-'.matchstr(line,'--\[.\{-}\zs\d\d:\d\d\ze\]')
                        let s:headtext = s:headtext[0:6] . 'Clocked: ('.matchstr(line,'->\s*\zs.*$') .') '.s:headtext[7:]
                    else
                        let tmatch .= '......'
                    endif
                endif
            endif
            call add(g:agenda_date_dict[datematch].l,  locator . filename . org#Pad(tmatch,11) . s:headtext)
            "call add(g:agenda_date_dict[datematch].l,  line(".") . repeat(' ',6-len(line("."))) . filename . org#Pad(tmatch,11) . s:headtext)
            if rangedate != ''
                "let startdate = matchstr(line,'<\zs\d\d\d\d-\d\d-\d\d\ze')
                "let thisday = calutil#jul(datematch) - calutil#jul(startdate) + 1
                let days_in_range = calutil#jul(rangedate) - calutil#jul(datematch) + 1
                "let rangestr = '('.thisday.'/'.days_in_range.')'
                let i = days_in_range
                "while (rangedate < date2) && (rangedate > datematch)
                while (rangedate > datematch)
                    let rangestr = '('.i.'/'.days_in_range.')'
                    if exists("g:agenda_date_dict['".rangedate."']")
                        "call add(g:agenda_date_dict[rangedate].l,  line(".") . repeat(' ',6-len(line("."))) . 
                        call add(g:agenda_date_dict[rangedate].l,  locator . 
                                    \ filename . org#Pad(rangestr,11) . s:headtext)
                    endif
                    let rangedate = calutil#cal(calutil#jul(rangedate) - 1)
                    let i = i - 1
                endwhile
                " to end of line to avoid double
                " treatment
                normal $
            endif
        else
            " it's a deadline/scheduled/closed date
            let type = org#Pad(mlist[1][0] . tolower(mlist[1][1:]) . ':' , 11)
            call add(g:agenda_date_dict[datematch].l,  locator . filename . type  . s:headtext)
        endif
    endif
    " Now test for late and upcoming warnings if 'today' is in range
    if (today >= date1) && (today < date2)
        if (datematch < today) && (match(line,'\(DEADLINE\|SCHEDULED\)')>-1)
                    \ && ((g:org_search_spec ==# '') || (s:CheckIfExpr(line("."),b:v.agenda_ifexpr)))
            let mlist = matchlist(line,'\(DEADLINE\|SCHEDULED\)')
            call s:SetHeadInfo()
            if !empty(mlist)
                let dayspast = calutil#jul(today) - calutil#jul(datematch)
                if mlist[1] ==? 'DEADLINE'
                    let newpart = org#Pad('In',6-len(dayspast)) . '-' . dayspast . ' d.:' 
                else
                    let newpart = org#Pad('Sched:',9-len(dayspast)) . dayspast . 'X:'
                endif
                call add(g:agenda_date_dict[today].l,  locator . filename . newpart . s:headtext)
            endif
            " also put in warning entry for deadlines when appropriate
        elseif (datematch > today) && (match(line,'DEADLINE')>-1)
                    \ && ((g:org_search_spec ==# '') || (s:CheckIfExpr(line("."),b:v.agenda_ifexpr)))
            let mlist = matchlist(line,'DEADLINE')
            call s:SetHeadInfo()
            if !empty(mlist)
                let daysahead = calutil#jul(datematch) - calutil#jul(today)
                let g:specific_warning = str2nr(matchstr(line,'<\S*\d\d.*-\zs\d\+\zed.*>'))
                if (daysahead <= g:org_deadline_warning_days) || (daysahead <= g:specific_warning)
                    let newpart = org#Pad('In',7-len(daysahead)) . daysahead . ' d.:' 
                    call add(g:agenda_date_dict[today].l,  locator . filename . newpart . s:headtext)
                endif
            endif
        endif
    endif
    " finally handle things for a range that began before date1
    if (rangedate != '')  && (datematch < date1)
                \ && ((g:org_search_spec ==# '') || (s:CheckIfExpr(line("."),b:v.agenda_ifexpr)))
        let days_in_range = calutil#jul(rangedate) - calutil#jul(datematch) + 1
        if rangedate >= date2
            let last_day_to_add = calutil#jul(date2) - calutil#jul(datematch) 
            let rangedate = calutil#cal(calutil#jul(date2)-1)
        else
            let last_day_to_add = days_in_range
        endif

        call s:SetHeadInfo()
        let i = last_day_to_add
        while (rangedate >= date1)
            let rangestr = '('.i.'/'.days_in_range.')'
            call add(g:agenda_date_dict[rangedate].l,  locator . 
                        \ filename . org#Pad(rangestr,11) . s:headtext)
            let rangedate = calutil#cal(calutil#jul(rangedate) - 1)
            let i = i - 1
        endwhile
        " go past match to avoid double treatment
        normal $
    endif
    if s:headline > 0
        let g:agenda_head_lookup[line(".")]=s:headline
    endif
endfunction

function! s:SetHeadInfo()
    let s:headline = s:OrgGetHead_l(line("."))
    let s:headtext = getline(s:headline)
    let s:mystars = matchstr(s:headtext,'^\*\+')
    let s:headstars = s:PrePad(s:mystars,6)
    let s:headtext = s:headstars . ' ' . s:headtext[len(s:mystars)+1:]
endfunction

function! s:RepeatMatch(rptdate, date1, date2)
    let yearflag = 0
    let basedate = matchstr(a:rptdate,'\d\d\d\d-\d\d-\d\d')
    if basedate >= a:date2
        " no need for repeat, rturn to check fo deadlien warnings
        return [basedate]
    endif
    let date1 = a:date1
    if basedate > date1
        let date1 = basedate
    endif
    let baserpt = matchstr(a:rptdate, ' \S\S\S [.+ ]\{0,1}+\zs\S\+\ze.*>')
    let rptnum = matchstr(baserpt, '^\d\+')
    let rpttype = matchstr(baserpt, '^\d\+\zs.')
    let g:rptlist = []
    let date1jul = calutil#jul(date1)
    let date2jul = calutil#jul(a:date2)
    if rpttype ==? 'w'
        let rpttype = 'd'
        let rptnum = str2nr(rptnum)*7
    endif
    if rpttype ==? 'y'
        let rpttype = 'm'
        let rptnum = str2nr(rptnum)*12
        let yearflag = 1
    endif
    if rpttype ==? 'd'
        let dmod = (date1jul - calutil#jul(basedate)) % rptnum
        let i = 0
        while 1
            let testjul = date1jul - dmod + (i*rptnum)
            if testjul < date2jul
                call add(g:rptlist, calutil#cal(testjul))
            else
                break
            endif
            let i += 1
        endwhile
    elseif rpttype ==? 'm'
        let g:special = baserpt[-1:]
        let monthday = str2nr(basedate[8:]) 
        let baseclone = basedate
        " this if-structure assigns begin test month as 
        " first repeat month _before_ date1
        if yearflag
            if (date1[:6]) >= (date1[:3] . baseclone[4:6]) 
                let baseclone = date1[:3] . baseclone[4:]
            else
                let baseclone = string(str2nr(date1[:3]) - 1) . baseclone[4:]
            endif
            let first_of_month_jul = calutil#jul(baseclone[:7]. '01')
        else
            let first_of_month_jul = calutil#jul(date1[:4] .
                        \ s:Pre0( date1[5:6] - 1) . '-01')
        endif

        if g:special ==? '*'
            let specialnum = (monthday / 7) + 1
            let specialdaynum = calutil#dow(basedate)
        endif
        while 1
            if g:special != '*'
                let testjul = first_of_month_jul - 1 + monthday
            else
                " process for 'xth weekday of month' type
                let fdow = calutil#dow(calutil#cal(first_of_month_jul))
                if fdow == specialdaynum
                    let testjul = first_of_month_jul + (specialnum-1)*7
                elseif fdow < specialdaynum
                    let testjul = first_of_month_jul + (specialnum-1)*7
                                \ + (specialdaynum - fdow)
                elseif fdow > specialdaynum
                    let testjul = first_of_month_jul + (specialnum*7)
                                \ - (fdow - specialdaynum)
                endif
            endif

            if (testjul < date2jul) && (testjul >= first_of_month_jul)
                call add(g:rptlist, calutil#cal(testjul))
            else
                "put in this one to check for deadlien warnings
                "if len(g:rptlist)>0
                call add(g:rptlist, calutil#cal(testjul))
                "endif
                break
            endif
            let first_cal = calutil#cal(first_of_month_jul)
            let nextmonth = str2nr(first_cal[5:6]) + rptnum
            let year = str2nr(first_cal[0:3]) 
            if nextmonth >= 13
                let nextmonth = (nextmonth-12)
                let year += 1 
            endif
            let first_of_month_jul = calutil#jul(string(year) . '-' . s:Pre0(nextmonth) . '-01')
        endwhile
    endif

    return g:rptlist

endfunction

function! s:BufMinMaxDate()
    let b:v.MinMaxDate=['2099-12-31','1900-01-01']
    g/<\d\d\d\d-\d\d-\d\d/call s:CheckMinMax()

endfunction
function! s:CheckMinMax()
    let date = matchstr(getline(line(".")),'<\zs\d\d\d\d-\d\d-\d\d')
    if (date < b:v.MinMaxDate[0])
        let b:v.MinMaxDate[0] = date
    endif
    if (date > b:v.MinMaxDate[1])
        let b:v.MinMaxDate[1] = date
    endif
endfunction        
function! s:Timeline(...)
    if a:0 > 0
        let spec = a:1
    else
        let spec = ''
    endif
    if bufname("%") ==? '__Agenda__'
        "go back up to main org buffer
        wincmd k
    endif
    if exists('g:org_search_spec')
        let prev_spec = g:org_search_spec
    endif
    if exists('g:agenda_files')
        let prev_files = g:agenda_files
    endif
    exec "let g:agenda_files=['".substitute(expand("%"),' ','\\ ','g')."']"
    call s:BufMinMaxDate()
    let num_days = 1 + calutil#jul(b:v.MinMaxDate[1]) - calutil#jul(b:v.MinMaxDate[0])
    try
        call OrgRunAgenda(b:v.MinMaxDate[0], num_days,spec)
    finally
        if exists('prev_spec')
            let g:org_search_spec = prev_spec
        endif
        if exists('prev_files')
            let g:agenda_files = prev_files
        endif
    endtry
endfunction

function! s:Pre0(s)
    return repeat('0',2 - len(a:s)) . a:s
endfunction

function! s:PrePad(s,amt,...)
    if a:0 > 0
        let char = a:1
    else
        let char = ' '
    endif
    return repeat(char,a:amt - len(a:s)) . a:s
endfunction
function! s:AgendaCompare(i0, i1)
    let mymstr = '^\(\d\+\)\s\+\(\S\+\)\s\+\(\%24c.\{11}\).*\(\*\+\)\s\(.*$\)'
    " mymstr below would be better match string regex, but generic dates
    " have no text at position 24 to match \S . . . "
    "let mymstr = '^\(\d\+\)\s\+\(\S\+\)\s\+\(\S.\{10}\).*\(\*\+\)\s\(.*$\)'
    " [1] is lineno, [2] is file, [3] is scheduling, [4] is levelstarts, 
    " [5] is headtext
    let cp0 = matchlist(a:i0,mymstr)
    let cp1 = matchlist(a:i1,mymstr)
    let myitems = [cp0, cp1]
    let sched_comp = []
    let i = 0
    while i < 2
        let item = myitems[i]
        if item[3][0] ==? 'S'
            if item[3][5] ==? ':'
                "let str_ord = 'a' . substitute(item[3][6:8],' ', '0','')
                let str_ord = 'aa' . s:PrePad(1000-str2nr(item[3][6:8]),' ', '0')
            else
                let str_ord = 'ab000'
            endif
        elseif item[3][0] ==? 'I' 
            if matchstr(item[3],'-') ># ''
                let str_ord = 'd-'.s:PrePad(1000-str2nr(matchstr(item[3],'\d\+')),3,'0')
            else
                let str_ord = 'da'.s:PrePad(matchstr(item[3],'\d\+'),3,'0')
            endif
        elseif item[3][0] ==? 'D'
            let str_ord = 'd0000'
        elseif item[3][0] ==? ' '
            let str_ord = 'zzzzz'
        else
            let str_ord = item[3][:4]
        endif
        call add(sched_comp,str_ord.item[2].s:PrePad(item[1],5,'0'))
        let i += 1
    endwhile 

    return sched_comp[0] ==? sched_comp[1] ? 0 : sched_comp[0] > sched_comp[1] ? 1 : -1

"    let num1 = str2nr(matchstr(a:i1,'In *\zs[ -]\d\+\ze d.:'))
"    let num2 = str2nr(matchstr(a:i2,'In *\zs[ -]\d\+\ze d.:'))
"    if num1 == 0 
"        let num1 = str2nr(matchstr(a:i1,'Sched: *\zs\d\+\zeX:'))
"        if num1 !=0 
"            let num1 = -num1 - 10000
"        endif
"    endif
"    if num2 == 0 
"        let num2 = str2nr(matchstr(a:i2,'Sched: *\zs\d\+\zeX:'))
"        if num2 !=0 
"            let num2 = -num2 - 10000
"        endif
"    endif
"    if (a:i1 =~ '^\d\+\s\+\S\+\s\+\d') 
"        let num1=num1-20000
"    endif
"    if (a:i2 =~ '^\d\+\s\+\S\+\s\+\d') 
"        let num2=num2-20000
"    endif

"    return num1 == num2 ? 0 : num1 > num2 ? 1 : -1

endfunc

function! s:DateListAdd(valdict)
    let namelist = [' GENERAL','SCHEDULED','CLOSED','DEADLINE']
    let templist = []
    call add(templist, get(a:valdict,'ud',0))
    call add(templist, get(a:valdict,'sd',0))
    call add(templist, get(a:valdict,'cd',0))
    call add(templist, get(a:valdict,'dd',0))
    let i = 0
    while i < 4
        if templist[i] != 0
            call add(g:org_datelist, templist[i] . ' ' . namelist[i] . ' ' . a:valdict.l )
        endif
        let i += 1
    endwhile
    return a:valdict
endfunction 

function! OrgAgendaMove(direction,count)
    " need to add functionaity for count, refactor
    let cnt = (a:count == 0) ? 1 : a:count
    let cnt = (a:direction ==? 'forward') ? cnt : -cnt
    let date_jul = calutil#jul(g:agenda_startdate)
    let ymd_list = split(g:agenda_startdate,'-')

    if g:org_agenda_days == 1
        let g:agenda_startdate = calutil#cal(date_jul + (cnt * 1))
    elseif g:org_agenda_days == 7
        let g:agenda_startdate = calutil#cal(date_jul + (cnt * 7))
    elseif g:org_agenda_days >= 360
        let g:agenda_startdate = string(ymd_list[0] + (cnt * 1)) .'-01-01'
    else
        let ymd_list[1] = ymd_list[1] + (cnt * 1)
        if ymd_list[1] > 0
            let ymd_list[0] += ymd_list[1] / 12
            let ymd_list[1] = ymd_list[1] % 12
        else
            let ymd_list[0] += (ymd_list[1] / 12) - 1
            let ymd_list[1] = 12 + (ymd_list[1] % 12)
        endif
        let g:agenda_startdate = ymd_list[0] . '-' .
                    \ s:Pre0(string(ymd_list[1])) .'-01'
        let g:org_agenda_days = s:DaysInMonth(g:agenda_startdate)
    endif

    if g:org_agenda_days == 1
        call OrgRunAgenda(g:agenda_startdate,g:org_agenda_days,g:org_search_spec,g:agenda_startdate)
    else
        call OrgRunAgenda(g:agenda_startdate,g:org_agenda_days,g:org_search_spec)
    endif
endfunction

function! s:TimeGrid(starthour,endhour,inc)
    let result = []
    for i in range(a:starthour, a:endhour,a:inc)
        call add(result,repeat(' ',23).s:Pre0(i).':00......       ------------')
    endfor
    return result
endfunction

function! s:MakeCalendar(date, daycount)
    "function! s:MakeCalendar(year, month, day, daycount)
    " this function is taken from vim tip by Siegfried Bublitz
    " at: http://vim.wikia.com/wiki/Generate_calendar_file
    " with many mods to 1.output to list rather than to buffer
    " and 2. get weekday and weekno from calutils
    let g:agenda_date_dict = {}
    let g:agenda_head_lookup = {}
    "let startdate = calutil#Jul2Cal((calutil#Cal2Jul(a:year,a:month,a:day) - calutil#DayOfWeek(a:year,a:month,a:day)))
    let year = split(a:date,'-')[0]
    let month = split(a:date,'-')[1]
    let day = split(a:date,'-')[2]
    let day = str2nr(day)

    if a:daycount == 7
        let wd = 1
    elseif (a:daycount>=28) && (a:daycount <=31)
        let wd = calutil#dow(a:date[0:7].'01') + 1
    else
        let wd = calutil#dow(a:date) + 1
    endif
    let week = 1 + (calutil#Cal2Jul(year,month,day) - calutil#Cal2Jul(year,1,1)) / 7
    let index = 0
    let datetext = ''
    let diy = 777 " day in year, wrong before next year
    while (index < a:daycount) " no of days to output
        let diy = diy + 1
        if (wd > 7)
            let wd = 1
            let week = week + 1
            if (week >= 53)
                if (week >= 54)
                    let week = 1
                elseif (day >= 28 || day <= 3)
                    let week = 1
                endif
            endif
        endif
        let monthnames=['January','February','March','April','May','June','July',
                    \ 'August','September','October','November','December']
        "let daynames = ['Mon','Tue','Wed','Thu','Fri','Sat','Sun']
        let daynames = ['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday']
        let dn = daynames[wd-1]
        if ((day > 31) || (month == 2 && (day > 29 || day > 28 && year % 4))
                    \ || (month == 4 && day > 30) || (month == 6 && day > 30)
                    \ || (month == 9 && day > 30) || (month == 11 && day > 30))
            let day = 1
            let month = month + 1
            if (month > 12)
                let month = 1
                let diy = 1
                let year = year + 1
                if (wd <= 3)
                    let week = 1
                endif
            endif
        endif

        let datetext = dn . repeat(' ',10-len(dn)) . (day<10?'   ':'  ') . 
                    \ day . ' ' . monthnames[month-1] . ' ' . year . (wd == 1 ? ' Wk' . week : '' )

        let g:agenda_date_dict[year . '-' . s:Pre0(month) .  '-' .  (day<10 ? '0'.day : day) ]
                    \ = {'marker': datetext, 'l': [] }
        let index = index + 1
        let day = day + 1
        let wd = wd + 1
    endwhile

endfunction

function! s:ActualBufferLine(lineref_in_agenda,bufnumber)
    let actual_line = matchstr(s:GetPlacedSignsString(a:bufnumber),'line=\zs\d\+\ze\s\+id='.a:lineref_in_agenda)
    return actual_line
endfunction

function! s:AgendaPutText(...)
    let save_cursor = getpos(".")
    let thisline = getline(line("."))
    if thisline =~ '^\d\+\s\+'
        if (getline(line(".") + 1) =~ '^\*\+ ')
            "let file = matchstr(thisline,'^\d\+\s\+\zs\S\+\ze')
            "let file = s:filedict[str2nr(matchstr(thisline, '^\d\d\d'))]
            let file = s:agenda_files_copy[str2nr(matchstr(thisline, '^\d\d\d'))]
            "let lineno = matchstr(thisline,'^\d\+\ze\s\+')
            let lineno = str2nr(matchstr(thisline,'^\d\d\d\zs\d*'))
            let starttab = tabpagenr() 

            "call s:LocateFile(file.'.org')
            call s:LocateFile(file)
            if g:agenda_date_dict != {}
                "let confirmhead = g:agenda_head_lookup[lineno]
                let confirmhead = lineno
            elseif g:adict != {}
                let confirmhead = lineno
            endif
            let newhead = matchstr(s:GetPlacedSignsString(bufnr("%")),'line=\zs\d\+\ze\s\+id='.confirmhead)
            let newhead = s:OrgGetHead_l(newhead)
            execute newhead
            let lastline = s:OrgNextHead_l(newhead) - 1
            if lastline > newhead
                let g:text = getline(newhead,lastline)
            elseif lastline == -1
                let g:text = getline(newhead,line('$'))
            else    
                let g:text = []
            endif

            execute 'tabnext ' . starttab
            execute bufwinnr('Agenda').'wincmd w'

            call setpos(".",save_cursor)
            " okay, we're back in agenda and have main buffer's
            " text in g:text, now need to compare it

            normal j
            let firstline = line(".")
            let daytextpat = '^\S\+\s\+\d\{1,2}\s\S\+\s\d\d\d\d'
            while (getline(line(".")) !~ '^\d\+\s\+') && (line(".") != line("$"))
                        \ && (getline(line(".")) !~ daytextpat)
                        \ && (getline(line(".")) !~ '\d empty days omitted')
                normal j
            endwhile
            let lastline = line(".")
            if (lastline < line("$"))  ||
                        \ ( (getline(line(".")) =~ '^\d\+\s\+')
                        \ || (getline(line(".")) =~ daytextpat) 
                        \ || (getline(line(".")) =~ '\d empty days omitted') )
                let lastline = line(".") - 1
            endif
            "execute firstline . ', ' . lastline . 'd'
            if g:text ==? getline(firstline, lastline)
                echo "headings are identical"
            else
                if g:adict != {}
                    let resp = confirm("Heading's text has changed, save changes?","&Save\n&Cancel",1)
                    if resp == 1
                        call s:SaveHeadline(file, newhead,getline(firstline,lastline))
                        "call s:SaveHeadline(file, confirmhead,getline(firstline,lastline))
                    else
                        echo "Changes were _not_ saved."
                    endif
                else
                    call confirm("Heading's text has changed, but saving is\n"
                        \ . "temporarily disabled for date-based agenda views.\n"
                        \ . "No changes are being saved, original buffer text remains as it was.")
                endif
            endif
        endif
    else
        echo "You're not on a headline line."
    endif
    call setpos(".",save_cursor)
endfunction
function! s:SaveHeadline(file, headline, lines)
    let file = a:file
    let headline = a:headline
    let lines=a:lines
    let starttab = tabpagenr() 

    "call s:LocateFile(file.'.org')
    call s:LocateFile(file)
    "let newhead = matchstr(s:GetPlacedSignsString(bufnr("%")),'line=\zs\d\+\ze\s\+id='.headline)
    let newhead = a:headline
    execute newhead

    let lastline = s:OrgNextHead_l(newhead) - 1
    execute newhead+1.','.lastline.'d'
    " don't delete orig headline b/c that's where sign is placed
    call setline(newhead,lines[0])
    call append(newhead,lines[1:])

    execute 'tabnext ' . starttab
    execute bufwinnr('Agenda').'wincmd w'

endfunction
function! OrgAgendaGetText(...)
    "type: 'datedict' for date agenda, 'adict' for regular search
    let cycle_todo = 0
    if a:0 >= 1 
        let cycle_todo = 1
        if a:0 == 2
            let newtodo = a:2
        endif
    endif
    " called by <TAB> map to toggle view of heading's body text in agenda
    " view
    let save_cursor = getpos(".")
    let thisline = getline(line("."))
    let curTodo = matchstr(thisline, '\*\+ \zs\S\+')
    if thisline =~ '^\d\+\s\+'
        if (getline(line(".") + 1) =~ '^\d\+\s\+') || (line(".") == line("$")) ||
                    \ (getline(line(".") + 1 ) =~ '^\S\+\s\+\d\{1,2}\s\S\+\s\d\d\d\d')
                    \ || (getline(line(".") + 1 ) =~ '\d empty days omitted')
            let file = s:agenda_files_copy[str2nr(matchstr(thisline, '^\d\d\d'))]
            let lineno = str2nr(matchstr(thisline,'^\d\d\d\zs\d*'))
            let starttab = tabpagenr() 

            call s:LocateFile(file)
            let save_cursor2 = getpos(".")
            "let confirmhead = s:OrgGetHead_l(headline)
            if g:agenda_date_dict != {}
                "let confirmhead = g:agenda_head_lookup[lineno]
                let confirmhead = lineno
            elseif g:adict != {}
                let confirmhead = lineno
            endif
            let newhead = matchstr(s:GetPlacedSignsString(bufnr("%")),'line=\zs\d\+\ze\s\+id='.confirmhead)
            let newhead = s:OrgGetHead_l(newhead)
            " HIGHLIGHt the headline ***************************
            "set foldlevel=1
            execute newhead
            "normal! zv
            "if getline(line('.')) =~ b:v.headMatch
            "    "restrict to headings only
            "    call s:OrgExpandSubtree(newhead,0)
            "endif
            "normal! z.
            "normal V
            "redraw
            ""sleep 100m
            "normal V
            "let b:v.chosen_agenda_heading = s:OrgGetHead()
            "call clearmatches()
            "let headlevel = s:Ind(b:v.chosen_agenda_heading)
            "let headlevel = (headlevel > 6) ? '' : headlevel-1
            "call matchadd('Org_Chosen_Agenda_Heading' . headlevel,'\%' . b:v.chosen_agenda_heading .'l')
            " ****************************

            if cycle_todo
                if a:0 >= 2
                    call s:ReplaceTodo(newtodo)
                else
                    call s:ReplaceTodo()
                endif
                normal V
                redraw
                sleep 100m
                normal V
            else
                let lastline = s:OrgNextHead_l(newhead) - 1
                if lastline > newhead
                    let g:text = getline(newhead,lastline)
                elseif lastline == -1 
                    let g:text = getline(newhead,line('$'))
                else    
                    let g:text = []
                endif
            endif
            call setpos(".",save_cursor2)
            execute 'tabnext ' . starttab
            execute bufwinnr('Agenda').'wincmd w'
            if !cycle_todo
                call append(line("."),g:text)
            endif
        else
            normal j
            let firstline = line(".")
            let daytextpat = '^\S\+\s\+\d\{1,2}\s\S\+\s\d\d\d\d'
            while (getline(line(".")) !~ '^\d\+\s\+') && (line(".") != line("$"))
                        \ && (getline(line(".")) !~ daytextpat)
                        \ && (getline(line(".")) !~ '\d empty days omitted')
                normal j
            endwhile
            let lastline = line(".")
            if (lastline < line("$"))  ||
                        \ ( (getline(line(".")) =~ '^\d\+\s\+')
                        \ || (getline(line(".")) =~ daytextpat) 
                        \ || (getline(line(".")) =~ '\d empty days omitted')) 
                let lastline = line(".") - 1
            endif
            call setpos(".",save_cursor)
            call s:AgendaPutText()
            silent execute firstline . ', ' . lastline . 'd'
        endif
    else
        echo "You're not on a headline line."
    endif
    call setpos(".",save_cursor)
    if cycle_todo
        if a:0 >= 2
            call s:ReplaceTodo(s:last_newtodo)
        else
            call s:ReplaceTodo(s:last_newtodo)
        endif
        echo "Todo cycled."
    endif
    " now switch back quickly and highlight in main buffer
    :AgendaMoveToBuf
endfunction

function! s:IsVisibleHeading(line)
    " returns 1 if line is of a visible heading,
    " 0 if not
    " a heading is visible if foldclosed = -1
    " (i.e., it's not in a fold) 
    " OR if it's not in an earlier-started fold
    " (i.e. start of fold heading is in is 
    " same as line of heading)
    " ***************************   the second and third lines of if 
    " statement are necessary because of bug where foldclosed is less 
    " than a head even though it is the fold head ***************
    let fc = foldclosed(a:line)
    if ((a:line > 0) && (fc == -1)) || (fc == a:line)
                \ || ((fc < a:line) &&  s:IsText(fc) )
                \ || ((fc < a:line) &&  (foldclosedend(fc) < a:line) )
        "   \ || (s:Ind(a:line) == 2)
        return 1
    else    
        return 0
    endif
endfunction

function! OrgSingleHeadingText(operation)
    " expand or collapse all visible Body Text
    " under Heading fold that cursor is in
    " operation:  "collapse" or "expand"
    " expand or collapse all Body Text 
    " currently visible under current heading
    let l:startline = line(".")
    let l:endline = s:OrgSubtreeLastLine_l(l:startline) - 1
    call OrgBodyTextOperation(l:startline,l:endline,a:operation)
endfunction

function! s:StarLevelFromTo(from, to)
    let save_cursor = getpos(".")
    set fdm=manual
    let b:v.levelstars = a:to
    ChangeSyn
    g/^\*\+/call setline(line("."),substitute(getline(line(".")),'^\*\+','*' . 
                \ repeat('*',(len(matchstr(getline(line(".")),'^\*\+')) - 1) * a:to / a:from),''))
    set fdm=expr
    call setpos(".",save_cursor)
endfunction

function! s:StarsForLevel(level)
    return 1 + (a:level - 1) * b:v.levelstars
endfunction

function! s:OrgExpandLevelText(startlevel, endlevel)
    " expand regular text for headings by level
    let save_cursor = getpos(".")

    normal gg
    let startlevel = s:StarsForLevel(a:startlevel)
    let endlevel = s:StarsForLevel(a:endlevel)
    let l:mypattern = substitute(b:v.headMatchLevel,'level', startlevel . ',' . endlevel, "") 
    while search(l:mypattern, 'cW') > 0
        execute line(".") + 1
        while getline(line(".")) =~ b:v.drawerMatch
           execute line(".") + 1
           normal! j
        endwhile
        if s:IsText(line(".")) 
            normal zv
        endif
        "normal l
    endwhile

    call setpos('.',save_cursor)

endfunction

" just an idea using 'global' not used anywhere yet
" visible is problem, must operate only on visible, doesn't do ths now
function! s:BodyTextOperation3(startline,endline, operation)
    let l:oldcursor = line(".")
    let nh = 0
    call cursor(a:startline,0)
    g/\*\{4,}/s:DoAllTextFold(line("."))
    call cursor(l:oldcursor,0)

endfunction


function! OrgBodyTextOperation(startline,endline, operation)
    " expand or collapse all Body Text from startline to endline
    " operation:  "collapse" or "expand"
    " save original line 
    let l:oldcursor = line(".")
    let nh = 0
    " go to startline
    call cursor(a:startline,0)
    " travel from start to end operating on any
    while 1
        if getline(line(".")) =~ b:v.headMatch
            if a:operation ==? "collapse"
                call s:DoAllTextFold(line("."))
            elseif a:operation ==? 'expand'
                normal zv
            endif
            "elseif s:IsText(line(".")+1) && foldclosed(line(".")) == line(".")
            "elseif foldclosed(line(".")) == line(".")
            "   "echo 'in expand area'
            "   if a:operation ==? 'expand'
            "       normal zv
            "   endif   
        endif
        let lastnh = nh
        let nh = s:NextVisibleHead(line("."))
        "echo 'last ' . lastnh . '    now ' . nh
        if (nh == 0) || (nh >= a:endline) || (nh == lastnh) 
            "echo "hit break"
            break
        elseif lastnh == nh
            break
            echo "bad exit from BodyTextOp"
        else
            "echo "hit ex"
            execute nh 
        endif

    endwhile
    " now go back to original line position in buffer
    call cursor(l:oldcursor,0)
endfunction

let g:calendar_sign = 'OrgCalSign'
function! OrgCalSign(day, month, year)
  if a:year .'-'.s:Pre0(a:month).'-'.s:Pre0(a:day) ==? s:org_cal_date
      return 1
  else
      return 0
  endif
endfunction
function! OrgSetLine(line, file, newtext)
    let save_cursor = getpos(".")
    let curfile = expand("%:t")

    call s:LocateFile(a:file)
    call setline(a:line,a:newtext)
    
    call s:LocateFile(curfile)
    call setpos('.',save_cursor)
endfunction
function! OrgGetLine(line, file)
    let save_cursor = getpos(".")
    let curfile = expand("%:t")

    call s:LocateFile(a:file)
    let result = getline(a:line)
    
    call s:LocateFile(curfile)
    call setpos('.',save_cursor)
    return result
endfunction
function! OrgAgendaDateType()
    " return type of date line in Agenda
    let text = getline(line('.'))[19:29]
    if text =~ 'Sched'
        let result = 'Scheduled'
    elseif text =~ '\(In \|DEADLINE\)'
       let result = 'Deadline'
    elseif text =~ 'Closed'
        let result = 'Closed'
    elseif text =~ '('
        let result = 'Range'
    else
        let result = 'Regular'
    endif
    return result
endfunction

function! GetDateAtCursor()

    return matchstr( GetDateSpecAtCursor() , '^[[<]\zs\d\d\d\d-\d\d-\d\d' )

endfunction

function! GetDateSpecAtCursor()
    let savecursor = getpos(".")
    " save visual bell settings so no bell
    " when not found"
    let orig_vb = &vb
    let orig_t_vb = &t_vb
    set vb t_vb=

    "  check for date string within brackets
    normal! va<
    silent! normal! "xy
    call setpos('.',savecursor)
    if len(@x) < 7 
        "normal! vi["xy
        normal! va[
        silent! normal! "xy
    endif

    if (len(@x)>=15) && (len(@x)<41)
        let date = matchstr(@x,'^[[<]\d\d\d\d-\d\d-\d\d')
    else
        let date = ''
    endif

    " restore visual bell settings
    let &vb = orig_vb
    let &t_vb = orig_t_vb

    if date ># ''
        " return with only opening '<' or '['
        return @x
    else
        return ''
    endif

    call setpos(".", savecursor)
        
endfunction

function! CalEdit( sdate, stime )
        " bring up calendar to edit and return a date value
        let basedate = a:sdate ==# '' ? s:Today() : a:sdate 
        let basetime = a:stime
        let newdate = '<' . basedate[0:9] . ' ' . calutil#dayname(basedate[0:9]) . (basetime ># '' ? ' ' . b:v.basetime : '') . '>'
        let newtime = basetime

        hi Cursor guibg=black
        let s:org_cal_date = newdate[1:10]
        call Calendar(1,newdate[1:4],str2nr(newdate[6:7]))
        " highlight chosen dates in calendar
        hi Ag_Date guifg=red
        call matchadd('Ag_Date','+\s\{0,1}\d\+')
        redraw
        let g:calendar_action='<SNR>'.s:SID().'_CalendarInsertDate'
        let cue = ''
        while 1
            echohl LineNr | echon 'Date+time ['.basedate . ' '.basetime.']: ' 
            "echohl None | echon cue.'_   =>' | echohl WildMenu | echon ' '.newdate[:-2] . ' ' . newtime 
            echohl None | echon cue.'_   =>' | echohl WildMenu | echon ' '.newdate[:-2] . '>' 
            let nchar = getchar()
            let newchar = nr2char(nchar)
            if newdate !~ 'interpret'
                let curdif = calutil#jul(newdate[1:10])-calutil#jul(s:Today())
            endif
            if (nchar ==? "\<BS>") && (len(cue)>0)
                let cue = cue[:-2]
            elseif nchar ==? "\<s-right>"
                let cue = ((curdif+1>=0) ?'+':'').(curdif+1).'d'
            elseif nchar ==? "\<s-left>"
                let cue = ((curdif-1>=0) ?'+':'').(curdif-1).'d'
            elseif nchar ==? "\<s-down>"
                let cue = ((curdif+7>=0) ?'+':'').(curdif+7).'d'
            elseif nchar ==? "\<s-up>"
                let cue = ((curdif-7>=0) ?'+':'').(curdif-7).'d'
            elseif nchar ==? "\<c-down>"
                let cue = ((curdif+30>=0) ?'+':'').(curdif+30).'d'
            elseif nchar ==? "\<c-up>"
                let cue = ((curdif-30>=0) ?'+':'').(curdif-30).'d'
            elseif nchar ==? "\<s-c-down>"
                let cue = ((curdif+365>=0) ?'+':'').(curdif+365).'d'
            elseif nchar ==? "\<s-c-up>"
                let cue = ((curdif-365>=0) ?'+':'').(curdif-365).'d'
            elseif newchar ==? "\<cr>"
                break
            elseif newchar ==? "\<Esc>"
                hi Cursor guibg=gray
                if bufwinnr('__Calendar') > 0
                    bdelete Calendar
                endif
                redraw
                return ''
            elseif (nchar ==? "\<LeftMouse>") && (v:mouse_win > 0) && (bufwinnr('__Calendar') == v:mouse_win)
                let g:cal_list=[]
                exe v:mouse_win . "wincmd w"
                exe v:mouse_lnum
                exe "normal " . v:mouse_col."|"
                normal 
                if g:cal_list != []
                    if newtime ># ''
                        let timespec = newtime
                    else
                        let timespec = matchstr(newdate,'\S\+:.*>')
                    endif
                    let newdate = '<'.g:cal_list[0].'-'.s:Pre0(g:cal_list[1]).'-'.s:Pre0(g:cal_list[2]) . ' '
                    let newdate .= calutil#dayname( g:cal_list[0].'-'.g:cal_list[1].'-'.g:cal_list[2])
                    let newdate .=  timespec ># '' ? ' ' . timespec : ''.'>'
                    break
                endif
            else
                let cue .= newchar
            endif
            try
                let newdate = '<' . s:GetNewDate(cue,basedate,basetime)  . '>'
            catch
                " don't raise error if user mistypes cue. . . 
                " or if last char makes cue uninterpretable
                let newdate = "can't interpret date cue  "
            endtry
            if g:org_use_calendar && (match(newdate,'\d\d\d\d-\d\d')>=0)
                let s:org_cal_date = newdate[1:10]
                call Calendar(1,newdate[1:4],str2nr(newdate[6:7]))
            endif
            echon repeat(' ',72)
            redraw
        endwhile
        hi Cursor guibg=gray
        bdelete __Calendar
        return newdate 
endfunction

command! OrgColumns :call OrgColumnsDashboard()
function! OrgColumnsDashboard()
    let save_cursor = getpos('.')
    if !exists('w:v.columnview')
        let w:v={'columnview':0}
        let w:v.org_item_len=100
        let w:v.org_colview_list = []
        let w:v.org_current_columns = ''
        let w:v.org_column_item_head = ''
    endif
    if !exists('b:v.org_columns_show_headings')
        let b:v.org_columns_show_headings = 0
    endif
    echohl WarningMsg
    let save_more = &more
    set nomore
    let force_all = 0
    while 1
        echohl MoreMsg
        echo "=========================================="
        echo " Buffer default columns:           " . b:v.buffer_columns
        echo " Current default columns:          " . w:v.org_current_columns
        echo " Column view is currently:         " . (w:v.columnview==1 ? 'ON' : 'OFF')
        echo " Show column headers is currently: " . (b:v.org_columns_show_headings ? 'ON' : 'OFF')
        echo " Heading line count is currently:  " . (g:org_show_fold_lines==1 ? 'ON' : 'OFF')
        if (w:v.columnview == 0) && (force_all == 1)
                echo " NEXT CHOICE WILL BE APPLIED TO ENTIRE BUFFER"
        endif
        echo " "
        echo " Press key to enter a columns command"
        echo " ------------------------------------"
        if (w:v.columnview == 0) && (force_all == 0)
                echo " f   force all of buffer to use chosen columns"
        endif
        if w:v.org_current_columns != b:v.buffer_columns
            echo " r   revert to buffer default columns"
        endif
        echo " t   toggle column view on/off"
        echo " h   toggle show headings on/off"
        echo " l   line count on/off"
        if len(g:org_custom_column_options) > 0 
            echo " Custom columns settings:"
        endif
        let i = 0
        while i < len(g:org_custom_column_options) 
            echo " " . i . "   " . g:org_custom_column_options[i]
            let i += 1
        endwhile
        echo " ------------------------------------"
        echo " "
        echohl Question
        let key = nr2char(getchar())
        redraw
        
        if key ==? 'f'
            let force_all = 1
            redraw
            continue
        endif

        let master_head = (force_all == 1 ) ? 0 : line('.')
       
        if key ==? 'r'
            let w:v.org_current_columns = b:v.buffer_columns
            if w:v.columnview == 1
                "turn off col view
                call ToggleColumnView(master_head, w:v.org_current_columns)
            endif
            call ToggleColumnView(master_head, w:v.org_current_columns)
        elseif key ==? 't'
            " current columns will get set in SetColumnHeads()
            call ToggleColumnView(master_head,'')
        elseif key ==? 'h'
            let b:v.org_columns_show_headings = 1 - b:v.org_columns_show_headings
        elseif key ==? 'l'
            let g:org_show_fold_lines = 1 - g:org_show_fold_lines
        elseif key =~ '[0-9]'
            let w:v.org_current_columns = g:org_custom_column_options[key]
            if w:v.columnview == 1
                " turn off
                call ToggleColumnView(master_head, w:v.org_current_columns)
            endif
            call ToggleColumnView(master_head, w:v.org_current_columns)
        else
            echo "No column option selected."
        endif
        break
    endwhile

    if b:v.org_columns_show_headings == 0
        call s:ColHeadWindow('',0)
    elseif (w:v.columnview == 1) && (bufnr('ColHeadBuffer') == -1) 
        call s:ColHeadWindow(w:v.org_column_item_head)
    endif
    echohl None
    let &more = save_more
    call s:AdjustItemLen()
    " redraw folded headings
    setlocal foldtext=OrgFoldText()
    call setpos('.',save_cursor)
endfunction
function! OrgDateDashboard()
    let save_cursor = getpos('.')
    let save_window = winnr()
    if bufname("%") ==? ('__Agenda__')
        let file = s:filedict[str2nr(matchstr(getline(line('.')), '^\d\d\d'))]
        let lineno = str2nr(matchstr(getline(line('.')),'^\d\d\d\zs\d*'))
        let buffer_lineno = s:ActualBufferLine(lineno,bufnr(file))
        let props = s:GetProperties(buffer_lineno, 0, file)
    else
        let props = s:GetProperties(line('.'),0)
    endif
    echohl MoreMsg
    echo " ================================="
    echo " Press key, for a date command:"
    echo " ---------------------------------"
    echo " d   set DEADLINE for current heading (currently: " . get(props,'DEADLINE','NONE') . ')'
    echo " s   set SCHEDULED for current heading (currently: " . get(props,'SCHEDULED','NONE') . ')'
    echo " c   set CLOSED for current heading (currently: " . get(props,'CLOSED','NONE') . ')'
    echo " t   set TIMESTAMP for current heading (currently: " . get(props,'TIMESTAMP','NONE') . ')'
    echo " g   set date at cursor"
    echo " "        
    echo " "
    echohl Question
    let key = nr2char(getchar())
    redraw
    if key ==? 'd'
        call OrgDateEdit('DEADLINE')
    elseif key ==? 's'
        call OrgDateEdit('SCHEDULED')
    elseif key ==? 'c'
        call OrgDateEdit('CLOSED')
    elseif key ==? 't'
        call OrgDateEdit('TIMESTAMP')
    elseif key ==? 'g'
        call OrgGenericDateEdit()
    else
        echo "No date command selected."
    endif
    echohl None
    exe save_window . 'wincmd w'
    call setpos('.',save_cursor)
endfunction

function! OrgGenericDateEdit()
    " edit date at cursor, not necessarily any specific type
    let save_cursor = getpos('.')
    let old_cal_navi = g:calendar_navi
    unlet g:calendar_navi
    try
        let my_date = GetDateSpecAtCursor()
        let orig_date = matchstr( my_date, '[[<]\zs\d\d\d\d-\d\d-\d\d' )
        let orig_time = matchstr( my_date, '[[<]\d\d\d\d-\d\d-\d\d ... \zs\d\d:\d\d' )

        if matchstr(my_date,'[[<]\d\d\d\d-\d\d-\d\d.\{-}+\d\+') != ''
           call confirm("Date has a repeater.  Please edit by hand.")
           return
        endif

        let cal_result = CalEdit(orig_date, orig_time)

        " put new date into text
        call setpos('.', save_cursor)
        if cal_result =~ '^.\d\d\d\d-\d\d'
            let @x = cal_result[1:-2]
            if my_date ># ''
                "replace existing date within delimiters
                exec 'normal vi' . my_date[0] . 'd'
                normal h"xpll
            else
                "paste in brand new date
                exec 'normal i <> '
                normal hh"xpll
            endif
        endif
        redraw
        echo 
        redraw
    finally
        let g:calendar_navi = old_cal_navi
    endtry
endfunction

function! OrgDateEdit(type)
    " type can equal DEADLINE/CLOSED/SCHEDULED/TIMESTAMP 
    let save_cursor = getpos('.')
    let old_cal_navi = g:calendar_navi
    unlet g:calendar_navi
    try
        let dtype = a:type
        if bufname("%") ==? ('__Agenda__')
            "get file, lineno, and other data if in Agenda
            let from_agenda=1
            let file = s:filedict[str2nr(matchstr(getline(line('.')), '^\d\d\d'))]
            let lineno = str2nr(matchstr(getline(line('.')),'^\d\d\d\zs\d*'))
            let buffer_lineno = s:ActualBufferLine(lineno,bufnr(file))
            let bufline = OrgGetLine(buffer_lineno,file)
        else
            let from_agenda=0
            let buffer_lineno = line('.')
            let bufline = getline(buffer_lineno)
            let file = expand("%:t")
        endif
        if dtype =~ '\(DEADLINE\|SCHEDULED\|CLOSED\|TIMESTAMP\)'
            let my_date = s:GetProp(dtype,buffer_lineno, file)
        
            let orig_date = matchstr( my_date, '[[<]\zs\d\d\d\d-\d\d-\d\d' )
            let orig_time = matchstr( my_date, '[[<]\d\d\d\d-\d\d-\d\d ... \zs\d\d:\d\d' )

            if matchstr(bufline,'[[<]\d\d\d\d-\d\d-\d\d.\{-}+\d\+') != ''
               call confirm("Date has a repeater.  Please edit by hand.")
               return
            endif

            let cal_result = CalEdit(orig_date, orig_time)

            " back to main window if agenda is above calendar after close
            if (from_agenda == 0) && bufname("%") ==? '__Agenda__'
               wincmd k 
            endif

            " set buffer text with new date . . . 
            call s:SetProp(dtype,cal_result,buffer_lineno, file)

            redraw
            echo 
            redraw
        else
            echo "Date type must be one of: DEADLINE, SCHEDULED, CLOSED, or TIMESTAMP."
        endif
    finally
        let g:calendar_navi = old_cal_navi
        call setpos('.',save_cursor)
    endtry
endfunction

function! s:GetNewTime(cue, basetime)
    " called from caledit()
    let timecue = a:cue
    if timecue =~ '\d\d:\d\d'
        let mytime = ' '.timecue
    else
        let mytime = ''
    endif
    return mytime

endfunction

function! s:GetNewDate(cue,basedate,basetime)
    " called from caledit()
    if match(a:cue,':') >= 0
        let cue = matchstr(a:cue,'^\S\+\ze \S\+:')
        let timecue = matchstr(a:cue,'\S\+:\S\+')
    else
        let cue = a:cue
        let timecue = ''
    endif
    let basedate = a:basedate
    let newdate = DateCueResult( cue , basedate )
    if timecue =~ '\d\d:\d\d'
        let mytime = ' '.timecue
    else
        let mytime = a:basetime ># '' ? ' ' . a:basetime : ''
    endif
    let mydow = calutil#dayname(newdate)
    return newdate . ' ' . mydow . mytime
endfunction
function! DateCueResult( cue, basedate)
        let cue = a:cue
        let basedate = a:basedate
        if cue =~ '^\(+\|++\|-\|--\)$'
            let cue = cue . '1d'
        elseif cue =~ '^\(+\|++\|-\|--\)\d\+$'
            let cue = cue .'d'
        endif
        if cue ==? '.'
            let newdate = strftime('%Y-%m-%d')
        elseif cue ==# ''
            let newdate = a:basedate
        elseif (cue =~ '^\d\+$') && (str2nr(cue) <= 31)
            " day of month string
            if str2nr(cue) > str2nr(basedate[8:9])
                let newdate = calutil#cal(calutil#jul(basedate[0:7].s:Pre0(cue)))
            else 
                let newmonth = s:Pre0(basedate[5:6]+1)
                let newdate = calutil#cal(calutil#jul(basedate[0:4].newmonth.'-'.s:Pre0(cue)))
            endif
        elseif cue =~ '^\d\+[-/]\d\+$'
            " month/day string
            let month = matchstr(cue,'^\d\+')
            let day = matchstr(cue,'\d\+$')
            let year = basedate[0:3]
            if basedate[0:4] . s:Pre0(month) . '-' . s:Pre0(day) < basedate
                let year = year + 1
            endif
            let newdate = calutil#cal(calutil#Cal2Jul(year,month,day))
        elseif cue =~ '\d\+/\d\+/\d\+'
            " m/d/y string
            let month = matchstr(cue,'^\d\+\ze/.*/')
            let day = matchstr(cue,'/\zs\d\+\ze/')
            let year = matchstr(cue,'/\zs\d\+\ze$')
            if len(year) < 3
                let year +=2000
            endif
            let newdate = calutil#cal(calutil#Cal2Jul(year,month,day))
        elseif cue =~ '\d\+-\d\+-\d\+'
            " y-m-d string
            let year = matchstr(cue,'^\d\+\ze-.*-')
            if year < 100
                let year +=2000
            endif
            let month = matchstr(cue,'-\zs\d\+\ze-')
            let day = matchstr(cue,'-\zs\d\+\ze$')
            let newdate = calutil#cal(calutil#Cal2Jul(year,month,day))

            "       elseif cue =~ s:org_monthstring
            "           let mycount = matchstr(cue,'^\d\+')
            "           let mymonth = 
            "           let newday = index(s:org_weekdays,cue)
            "           let oldday = calutil#dow(basedate)
            "           if newday > oldday
            "               let amt=newday-oldday
            "           elseif newday < oldday
            "               let amt =7-oldday+newday
            "           else
            "               let amt = 7
            "           endif
            "           let newdate=calutil#cal(calutil#jul(basedate)+amt)
        elseif cue =~ s:org_weekdaystring
            " wed, 3tue, 5fri, i.e., dow string
            let mycount = matchstr(cue,'^\d\+')
            let myday = matchstr(cue,s:org_weekdaystring) 
            let newday = index(s:org_weekdays,myday)
            let oldday = calutil#dow(matchstr(basedate,'\d\d\d\d-\d\d-\d\d'))
            if newday > oldday
                let amt=newday-oldday
            elseif newday < oldday
                let amt =7-oldday+newday
            else
                let amt = 7
            endif
            let amt = amt + (mycount*7)
            let newdate=calutil#cal(calutil#jul(basedate)+amt)
        elseif cue =~ '\c\([-+]\|[-+][-+]\)\d\+[ dwmy]'
            " plus minus count of dwmy
            let mlist =  matchlist(cue,'\c\([-+]\|[-+][-+]\)\(\d\+\)\([ wdmy]\)')
            let op = mlist[1]
            let mycount = mlist[2]
            let type = mlist[3]
            if len(op) == 1
                let mydate = strftime('%Y-%m-%d')
            else
                let mydate = basedate
            endif
            let op = op[0]
            let year = mydate[0:3]
            let month = mydate[5:6]
            let day = mydate[8:9]
            if type ==? 'y'
                let type = 'm'
                let mycount = mycount * 12
            elseif type ==? 'w'
                let type='d'
                let mycount = mycount * 7
            endif
            if type ==? 'm'
                if (op ==? '+')
                    let yplus = mycount / 12
                    let mplus = mycount % 12
                    let year +=   yplus
                    let month += mplus
                    if month > 12
                        let month = month - 12
                        let year = year + 1
                    endif
                elseif ((mycount % 12) >= month) && (op ==? '-')
                    let yminus = mycount/12
                    let year = year - yminus - 1
                    let month = (month + 12 - (mycount % 12))   
                else " '-' with month greater
                    let month = month - (mycount % 12)
                    let year = year - (mycount / 12)
                endif
                " correct for bad dates
                while calutil#cal(calutil#jul(year.'-'.s:Pre0(month).'-'.s:Pre0(day)))[5:6] != month
                    let day = day - 1
                endwhile
            elseif (type ==? 'd') || (type ==? ' ')
                let newjul = calutil#jul(mydate)
                if op ==? '+'
                    let newjul = newjul + mycount
                else
                    let newjul = newjul - mycount
                endif
                "execute 'let newjul = newjul ' . op . mycount
                let mydate = calutil#cal(newjul)
                let year = mydate[0:3]
                let month = mydate[5:6]
                let day = mydate[8:9]
            endif

            let newdate = year . '-' . s:Pre0(month) . '-' . s:Pre0(day)
        else
            return " ?? can't interpret your spec"
        endif
        return newdate
endfunction
function! s:TimeInc(direction)
    let save_cursor = getpos(".")
    let i = 0
    let col = save_cursor[2] - 1
    let line = getline(line("."))
    if line[col] =~ '\d'
        let i = 1
        while i < 6
            let start = col - i
            let end = col - i + 6
            silent execute 'let timetest = line[' . start . ':' . end .']'
            if timetest =~ ' \d\d:\d\d[>\]]'
                break
            endif          
            let i += 1
        endwhile
    else
        let i = 6
    endif
    if i == 6
        execute "normal! \<s-up>"
        return
    else
        let start = col - i + 1
        let end = col - i + 5
        execute 'let time = line[' . start . ':' . end .']'
        if i > 3
            let newminutes = (time[3:4] + (a:direction *5)) 
            let newminutes = newminutes - (newminutes % 5)
            if (newminutes >= 60) 
                let newminutes = 0
                let newhours = time[0:1] + 1
            elseif (newminutes == -5) && (a:direction == -1)
                let newminutes = 55
                let newhours = time[0:1] - 1
            else
                let newhours = time[0:1]
            endif
        else
            let newhours = time[0:1] + (1 * a:direction)
            let newminutes = time[3:4]
        endif
        if newhours >= 24
            let newhours = 0
            "let tempsave = getpos(".")
        elseif newhours < 0
            let newhours = 23
            "execute "normal ".start-6."|"
            "call OrgDateInc(a:direction)
            "call setpos(".",tempsave)         
        endif
        let matchcol = col-i+2
        execute 's/\%'.matchcol.'c\zs\d\d:\d\d/' . s:Pre0(newhours) . ':' . s:Pre0(newminutes).'/'
    endif
    call setpos(".",save_cursor)
endfunction
function! OrgDateInc(direction)
    "       <dddd-dd-dd
    "       01234567890
    "       09876543210
    let save_cursor = getpos(".")
    let i = 0
    let col = save_cursor[2] - 1
    let line = getline(line("."))
    if line[col] =~ '\d'
        let i = 1
        while i < 21
            let start = col - i
            let end = col - i + 11
            silent execute 'let datetest = line[' . start . ':' . end .']'
            if datetest =~ '[<[]\d\d\d\d-\d\d-\d\d'
                break
            endif          
            let i += 1
        endwhile
    else
        let i = 21
    endif
    if i == 21
        execute "normal! \<s-up>"
        return
    else
        if i > 12
            call setpos(".",save_cursor)
            call s:TimeInc(a:direction)
            return
        endif
        let start = col - i + 1
        let end = col - i + 11
        execute 'let date = line[' . start . ':' . end .']'
        if i > 7
            let newdate = calutil#cal(calutil#jul(date) + a:direction)
            let newyear = newdate[0:3]
            let newmonth = newdate[5:6]
            let newday = newdate[8:9]
        elseif i < 5
            let spot = 'year'
            let newyear = date[0:3] + a:direction
            let newmonth = date[5:6]
            let newday = date[8:9]
            "execute 's/\d\d\d\d/' . newyear . '/'
        else
            let spot = 'month'
            let newmonth = date[5:6] + a:direction  
            let newday = date[8:9]
            if newmonth > 12
                let newyear = date[0:3] + 1
                let newmonth = '01'
                let newday = '01'
            elseif newmonth < 1
                let newyear = date[0:3] - 1
                let newmonth = '12'
                let newday = '31'
            else
                let newyear = date[0:3]
                let newday = date[8:9]
            endif
        endif
        " correct for bad dates
        while calutil#cal(calutil#jul(newyear.'-'.newmonth.'-'.newday))[5:6] != newmonth
            let newday = newday - 1
        endwhile
        let matchcol = col-i+2
        execute 's/\%'.matchcol.'c\zs\d\d\d\d-\d\d-\d\d/' . newyear . '-' . s:Pre0(newmonth) . '-' . s:Pre0(newday).'/'
        " update dow if there is one
        let end +=5
        silent execute 'let datetest = line[' . start . ':' . end .']'
        if datetest =~ '\d\d\d\d-\d\d-\d\d \S\S\S'
            let dow = calutil#DayOfWeek(newyear,newmonth,newday,2)
            silent execute 's/\%'.matchcol.'c\(\d\d\d\d-\d\d-\d\d \)\S\S\S/\1' . dow.'/'
        endif          
    endif
    call setpos(".",save_cursor)
endfunction

function! s:GetClock()
    return '['.strftime("%Y-%m-%d %a %H:%M").']'
endfunction 
function! OrgClockIn(...)
    let save_cursor=getpos(".")
    let lineno=line('.')
    if bufname("%") ==? ('__Agenda__')
        let lineno = matchstr(getline(line('.')),'^\d\+')
        let file = matchstr(getline(line('.')),'^\d\+\s*\zs\S\+').'.org'
        let str = ','.lineno.',"'.file.'"'
        call s:SetProp('CLOCKIN','',lineno,file)
    else
   
        if a:0 > 1
            execute a:1
        endif
        execute s:OrgGetHead()
        if s:IsTagLine(line(".")+1)
            execute line('.')+1
        endif
        "exe 'normal o:CLOCK: ' . s:GetClock()
        call append(line('.'),'  :CLOCK: '.s:GetClock())
        let dict={'file':expand("%"),'line':line('.'),'Timestamp':org#Timestamp()}
        call add(g:org_clock_history,dict)
    endif


    call setpos(".",save_cursor)
endfunction
function! s:GetOpenClock()
    let found_line = 0
    let file = ''
    if !exists('g:agenda_files') || (g:agenda_files == [])
        unsilent call confirm("No agenda files defined, will search only this buffer for open clocks.")
        let found = search('CLOCK: \[\d\d\d\d-\d\d-\d\d \S\S\S \d\d:\d\d\]\($\|\s\)','w')
    else
        let g:in_agenda_search = 1
        for file in g:agenda_files
            call s:LocateFile(file)
            let found_line = search('CLOCK: \[\d\d\d\d-\d\d-\d\d \S\S\S \d\d:\d\d\]\($\|\s\)','w')
            let file = expand("%")
            if found_line > 0
                break
            endif
        endfor
        unlet g:in_agenda_search
    endif
    return [file,found_line]
endfunction
function! OrgClockOut(...)
    let cur_file=expand("%")
    let save_cursor= getpos('.')
    if a:0 > 1
        execute a:1
    else
        let oc = s:GetOpenClock()
        if oc[0] ># '' 
           call s:LocateFile(oc[0])
           execute oc[1]
        endif
    endif
    execute s:OrgGetHead()
    let bottom = s:OrgNextHead() > 0 ? s:OrgNextHead() - 1 : line("$")
    let str = 'CLOCK: \[\d\d\d\d-\d\d-\d\d \S\S\S \d\d:\d\d\]\($\|\s\)'
    let found = s:Range_Search(str,'n',bottom,line("."))
    if found
        execute found
        execute 'normal A--' . s:GetClock() 
        if b:v.clock_to_logbook 
            let headline = s:OrgGetHead()
            let clockline = getline(line(".")) . ' -> ' . s:ClockTime(line("."))
            normal! dd
            call OrgConfirmDrawer("LOGBOOK",headline)
            let clockline = matchstr(getline(line(".")),'^\s*') . matchstr(clockline,'\S.*')
            call append(line("."),clockline )
        endif
        let msg = "Open clock found and clocked out in \n"
        let msg .= "file: ".expand("%")."\n"
        let msg .= "in headline at line number: ".headline
        call confirm(msg)
    else
        echo 'No open clock found. . . .'
    endif
    call s:LocateFile(cur_file)
    call setpos(".",save_cursor)
endfunction
function! s:UpdateAllClocks()
    %g/^\s*:CLOCK:/call s:AddClockTime(line("."))
endfunction
function! s:AddClockTime(line)
    call setline(a:line,matchstr(getline(a:line),'.*\]') . ' -> ' . s:ClockTime(a:line))
endfunction

function! s:UpdateClockSums()
    let save_cursor = getpos(".")
    g/^\s*:ItemClockTime/d
    call s:UpdateAllClocks()
    g/^\s*:CLOCK:/call s:SetProp('ItemClockTime', s:SumClockLines(line(".")))
    call setpos(".",save_cursor)
endfunction

function! s:SumClockLines(line)
    let save_cursor = getpos(".")
    execute s:OrgGetHead_l(a:line) + 1
    "execute a:line + 1
    let hours = 0
    let minutes = 0
    while 1
        let text = getline(line("."))
        if text !~ s:remstring
            break
        endif
        let time = matchstr(text,'CLOCK.*->\s*\zs\d\+:\d\+')
        if time ># ''
            let hours   += str2nr(split(time,':')[0])
            let minutes += str2nr(split(time,':')[1])
        endif

        if line('.') == line('$')
            break
        else
            execute line('.') + 1
        endif
        
    endwhile
    let totalminutes = (60 * hours) + minutes
    call setpos(".",save_cursor)
    return (totalminutes/60) . ':' . s:Pre0(totalminutes % 60)

endfunction
function! s:UpdateBlock()
    normal j
   ?^#+BEGIN:
    let block_type = matchstr(getline(line('.')),'\S\+\s\+\zs\S\+')
   if matchstr(getline(line('.')+1),'^#+END') ==# ''
        normal jV/^#+END/-1dk
    endif
    if block_type ==? 'clocktable'
        let block_type='ClockTable'
    endif
    let mycommand = block_type.'()'
    execute "call append(line('.'),".mycommand.")"
endfunction
function! ClockTable()
    let save_cursor = getpos(".")

    call s:UpdateClockSums()
    call s:UpdateHeadlineSums()
    call OrgMakeDict()
    let g:ctable_dict = {}
    let mycommand = "let g:ctable_dict[line('.')] = "
                \ . "{'text':s:GetProperties(line('.'),0)['ITEM']"
                \ . " , 'time':s:GetProperties(line('.'),0)['TOTALCLOCKTIME']}"
    g/:TOTALCLOCKTIME/execute mycommand
    let total='00:00'
    for item in keys(g:ctable_dict)
        "let test = g:ctable_dict[item].text
        if g:ctable_dict[item].text[0:1] ==? '* '
        "if test[0:1] ==? '* '
            let total = s:AddTime(total,g:ctable_dict[item].time)
        endif
    endfor
    let result = ['Clock summary at ['.org#Timestamp().']','',
                \ '|Lev| Heading                      |  ClockTime',
                \ '|---+------------------------------+-------+--------' ,
                \ '|   |                      *TOTAL* | '.total ]
    for item in sort(keys(g:ctable_dict),'s:NumCompare')
        let level = len(matchstr(g:ctable_dict[item].text,'^\*\+')) 
        let treesym = repeat('   ',level-2) . (level > 1 ? '\_ ' : '')
        let str = '| '.level.' | ' 
                    \ . org#Pad(treesym . matchstr(g:ctable_dict[item].text,'^\*\+ \zs.*')[:20],28) . ' | '
                    \ . repeat('      | ',level-1)
                    \ . s:PrePad(g:ctable_dict[item].time,5) . ' |'
        if g:ctable_dict[item].text[0:1] ==? '* '
            call add(result, '|---+------------------------------+-------+-------+' )
        endif
        call add(result, str)
    endfor
    call setpos(".",save_cursor)
    
    unlet b:v.org_dict
    return result

endfunction

function! s:NumCompare(i1,i2)
    let i1 = str2nr(a:i1)
    let i2 = str2nr(a:i2)
    return i1 == i2 ? 0 : i1>i2 ? 1 : -1
endfunction

function! s:ClockTime(line)
    let ctext = getline(a:line)
    let start = matchstr(ctext,'CLOCK:\s*\[\zs\S\+\s\S\+\s\S\+\ze\]')
    let end = matchstr(ctext,'--\[\zs.*\ze\]')
    let daydifference = calutil#jul(end[0:9])-calutil#jul(start[0:9])
    let startmin = 60*start[15:16] + start[18:19]
    let endmin = 60*end[15:16] + end[18:19]
    let totalmin = (daydifference * 1440) + (endmin - startmin)
    return string(totalmin/60) . ':' . s:Pre0(totalmin % 60)
endfunction
function! s:AddTime(time1, time2)
    let time1 = a:time1
    let time2 = a:time2
    if match(time1,':') == -1 | let time1 = '00:00' | endif
    if match(time2,':') == -1 | let time2 = '00:00' | endif
    let hours = str2nr(matchstr(time1,'^.*\ze:')) + str2nr(matchstr(time2,'^.*\ze:'))
    let minutes = (60*hours) + time1[-2:] + time2[-2:]
    return (minutes/60) . ':' . s:Pre0(minutes % 60)
endfunction
function! s:GetProp(key,...)
    let save_cursor = getpos(".")
    if a:0 >=2
        let curtab = tabpagenr()
        let curwin = winnr()
    " optional args are: a:1 - lineno, a:2 - file
        call s:LocateFile(a:2)
    endif
    if (a:0 >= 1) && (a:1 > 0)
        execute a:1 
    endif
    execute s:OrgGetHead() + 1
    let myval = ''
    while 1
        let text = getline(line("."))
        if text !~ s:remstring
            break
        endif
        let mymatch = matchstr(text,':\s*'.a:key.'\s*:')
        if mymatch ># ''
            let myval = matchstr(text,':\s*'.a:key.'\s*:\s*\zs.*$')
            break
        endif
        execute line(".") + 1
        if line(".") == line("$")
            break
        endif
    endwhile
    if a:0 >= 2
        execute "tabnext ".curtab
        execute curwin . "wincmd w"
    endif
    call setpos(".",save_cursor)
    return myval

endfunction
function! s:SetDateProp(type,newdate,...)
    " almost identical to s:GetProp() above, need to refactor
    let save_cursor = getpos(".")
    if a:0 == 1
        execute a:1 + 1
    else
        execute line(".") + 1
    endif
    let myval = ''
    while 1
        let text = getline(line("."))
        if text !~ s:remstring
            break
        endif
        let mymatch = matchstr(text,'\s*'.a:type.'\s*:')
        if mymatch ># ''
            execute 's/'.a:type.'.*$/'.a:type.':<'.a:newdate.'>/'
            break
        endif
        execute line(".") + 1
    endwhile
    call setpos(".",save_cursor)
    return myval
endfunction
function! s:SetProp(key, val,...)
    let save_cursor = getpos(".")
    " optional args are: a:1 - lineno, a:2 - file
    if a:0 >=2
        let curtab = tabpagenr()
        let curwin = winnr()
        call s:LocateFile(a:2)
    endif
    if (a:0 >= 1) && (a:1 > 0)
        execute a:1 
    endif
    let key = a:key
    let val = a:val
    execute s:OrgGetHead() 
    " block_end was end of properties block, but getting that 
    " from GetProperties(line('.'),0) creates problems with 
    " line numbers having changed from previous run of OrgMakeDict
    " So, just use next head as end of block for now.
    let block_end = s:OrgNextHead()
    let block_end = (block_end == 0) ? line('$') : block_end
    if key =~ 'DEADLINE\|SCHEDULED\|CLOSED\|TIMESTAMP'
        " it's one of the five date props
        " find existing date line if there is one
        if key ==? 'TIMESTAMP' 
            let key = ''
            let foundline = s:Range_Search('^\s*:\s*<\d\d\d\d-\d\d-\d\d','n',block_end,line("."))
        elseif key ==? 'TIMESTAMP_IA' 
            let key = ''
            let foundline = s:Range_Search('^\s*:\s*[\d\d\d\d-\d\d-\d\d','n',block_end,line("."))
        else
            let foundline = s:Range_Search('^\s*\(:\)\{}'.key.'\s*:','n',block_end,line("."))
        endif
        if foundline > 0
            exec foundline
            exec 's/:\s*<\d\d\d\d.*$/'.':'.a:val
        else
            let line_ind = len(matchstr(getline(line(".")),'^\**'))+1 + g:org_indent_from_head
            if s:IsTagLine(line('.')+1)
                execute line('.') + 1
            endif
            call append(line("."),org#Pad(' ',line_ind)
                        \ .':'.key.(key ==# ''?'':':').a:val)
        endif
    elseif key ==? 'tags'
        if s:IsTagLine(line('.') + 1)
            call setline(line('.') + 1, a:val)
        else
            call append(line('.'), a:val)
        endif
        execute line('.') + 1
        normal =$
        execute line('.') - 1
    elseif key ==? 'CLOCKIN'
        call OrgClockIn()
    elseif key ==? 'CLOCKOUT'
        call OrgClockOut(a:val)
    else
        " it's a regular key/val pair in properties drawer
        call OrgConfirmDrawer("PROPERTIES")
        while (getline(line(".")) !~ '^\s*:\s*' . key) && 
                    \ (getline(line(".")) =~ s:remstring) &&
                    \ (line('.') != line('$'))
            execute line(".") + 1
        endwhile

        if (getline(line(".")) =~ s:remstring) && (getline(line('.')) !~ '^\s*:END:')
            call setline(line("."), matchstr(getline(line(".")),'^\s*:') .
                        \ key . ': ' . val)
        else
            execute line(".") - 1
            call OrgConfirmDrawer("PROPERTIES")
            let curindent = matchstr(getline(line(".")),'^\s*')
            let newline = curindent . ':' . key . ': ' . val
            call append(line("."),newline)
        endif
    endif

    "if exists("*Org_property_changed_functions") && (bufnr("%") != bufnr('Agenda'))
    "    let Hook = function("Org_property_changed_functions")
    "    silent execute "call Hook(line('.'),a:key, a:val)"
    "endif
    if a:0 >=2
        "back to tab/window where setprop call was made
        execute "tabnext ".curtab
        execute curwin . "wincmd w"
    endif
    call setpos(".",save_cursor)
endfunction
function! OrgCycleAgendaFiles(direction)
    if !empty('g:agenda_files')
        let cur_file = expand("%:p")
        let ndx = index(g:agenda_files,cur_file) 
        if ndx > -1
            let ndx += (a:direction ==? 'backward') ? -1 : 1
            let ndx = (ndx == len(g:agenda_files)) ? 0 : ndx 
            let ndx = (ndx == -1) ? (len(g:agenda_files) - 1) : ndx 
            let filename = g:agenda_files[ndx]
        else
            let filename = g:agenda_files[0]
        endif
        call s:LocateFile(filename)
    else
        echo "No agenda files defined."
    endif
    
endfunction
function! s:LocateFile(filename)
    let filename = a:filename

    "if !exists("g:agenda_files") || (g:agenda_files == [])
    "    call confirm('You have no agenda files defined right now.\n'
    "                \ . 'Will assign current file to agenda files.')
    "    call s:CurfileAgenda()
    "endif
    "let myvar = ''
    "" set filename

    "if filename != '__Agenda__'
    "    " but change to be full name if appropriate
    "    for item in g:agenda_files
    "        " match fullpathname or just filename w/o path
    "        if (item ==? a:filename) || (item =~ matchstr(a:filename,'.*[/\\]\zs.*'))
    "            let filename = item
    "            break
    "        endif
    "    endfor
    "endif

    if bufwinnr(filename) >= 0
        silent execute bufwinnr(filename)."wincmd w"
    else
        execute 'tab drop ' . filename
        if &ft != 'org'
            call org#SetOrgFileType()
        endif
    endif

endfunction

function! OrgConfirmDrawer(type,...)
    let line = s:OrgGetHead()
    if a:0 == 1
        let line = a:1
    endif
    execute line
    let bottom = s:OrgNextHead() > 0 ? s:OrgNextHead() - 1 : line("$")
    let found = s:Range_Search(':\s*'. a:type . '\s*:','n',bottom,line)
    if !found
        while getline(line(".") + 1) =~ s:remstring
            execute line('.')+1
        endwhile
        "if line == line(".")-1
        "    "back to headline in this case
        "    execute line
        "endif
        execute 'normal o:'. a:type . ':'
        execute 'normal o:END:'
        normal k
    else
        execute found
    endif
endfunction

function! OrgGetLink()
    let savecursor = getpos('.')

    let linkdict = {'link':'','desc':''}
    let curpos = getpos('.')[2]
    call search('\[\[','bc',line('.'))
    let startpos = getpos('.')[2] - 1
    call search(']]','ce',line('.'))
    let endpos = getpos('.')[2] - 1
    if (curpos >= startpos) && (curpos <= endpos)
        let linktext = getline(line("."))[ startpos : endpos ]
        if linktext =~ ']\['
            let linkdict.link = matchstr(linktext,'\[\[\zs.*\ze]\[')
            let linkdict.desc = matchstr(linktext,']\[\zs.*\ze]]')
        else
            let linkdict.link = matchstr(linktext,'\[\[\zs.*\ze]]')
        endif
    endif
    call setpos('.',savecursor)
    return linkdict
endfunction
function! FollowLink(ldict)
    let ld = a:ldict
    let ld.suffix = ''
    "process things so org-compatible while still calling Utl
    let prefix = matchstr(ld.link,'\S\{-1,}\ze:')
    if exists(":Utl") == 0
       echo "The Vim plugin Utl.vim must be installed to follow links."
        echo "You can find a copy at:"
        echo 'http://www.vim.org/scripts/script.php?script_id=293'
        return
    endif
    if prefix =~ g:org_unsupported_link_types
        echo 'Link type "' . prefix '" not supported in VimOrganizer.'
        return
    endif
    " now have to translate from org format to Utl format
    if ld.link[0] ==# '#'
        let ld.link = '#tn=:CUSTOM_ID:\s\*' . ld.link[1:]
        let prefix = '#'
    elseif prefix ==? 'file' && (ld.link[5] ==# '.')  
        " || ld.link[5:6] ==# '\S:') 
        " take file prefix out b/c Utl can't handle relative file paths
        let ld.link = ld.link[5:]
    endif

    if prefix ==? 'file' && (ld.link =~ '::')
        let mylist = split(ld.link,'::')
        let ld.link = mylist[0]
        let ld.suffix = mylist[1]
    endif

    if (prefix ==# '') || ((prefix !~ g:org_supported_link_types) && (prefix != '#'))
        " we have org text search that needs different treatment
        call FollowTextLink(a:ldict.link)
        "for search_type in ['dedicated', 'headline1', 'headline2', 'general']
        "    let savecursor = getpos('.')
        "    if search_type is 'dedicated'
        "        let newlink = '#tn=<<' . a:ldict.link . '>>'
        "    elseif search_type is 'headline1'
        "        let newlink = '#tn=' . b:v.todoMatch . a:ldict.link 
        "    elseif search_type is 'headline2'
        "        let newlink = '#tn=^*\+ ' . a:ldict.link
        "    else
        "        let newlink = '#tn=' . a:ldict.link
        "    endif

        "    let newlink = substitute(newlink,' ','\\ ','g')
        "    let g:newlink = newlink
        "    silent! exec 'Utl o '. newlink . ' split'

        "    if line('.') != savecursor[1] 
        "        break
        "    endif
        "endfor
    else
        exec 'Utl o ' . ld.link . ' split'
        if ld.suffix ># ''
            call FollowTextLink(ld.suffix)
        endif
    end
endfunction
function! FollowTextLink(link)
    for search_type in ['dedicated', 'headline1', 'headline2', 'general']
        let savecursor = getpos('.')
        if search_type is 'dedicated'
            let newlink = '#tn=<<' . a:link . '>>'
        elseif search_type is 'headline1'
            let newlink = '#tn=' . b:v.todoMatch . a:link 
        elseif search_type is 'headline2'
            let newlink = '#tn=^*\+ ' . a:link
        else
            let newlink = '#tn=' . a:link
        endif

        let newlink = substitute(newlink,' ','\\ ','g')
        let g:newlink = newlink
        silent! exec 'Utl o '. newlink . ' split'

        if line('.') != savecursor[1] 
            break
        endif
    endfor
endfunction
function! EditLink()
    "is this here: and is this there:
    let thislink = OrgGetLink()

    let link = input('Link: ', thislink.link)
    let desc = input('Description: ', thislink.desc)
    
    if thislink.link !=# ''
        "delete existing hyperlink
        call search('\[\[','b',line('.'))
        normal v/]]/exx
    endif 

    silent exec 'normal i[[' . link . ']' . (desc ># '' ? '[' . desc . ']' : '') . ']'
    echo ''
endfunction
function! OrgMouseDate()
    let @x=''
    let date=''
    let save_cursor = getpos(".")
    let found = ''
    silent! let date = GetDateAtCursor()
    call setpos('.',save_cursor)
    let linkdict = OrgGetLink()
    if date ># ''
        let found='date'
        let date = date[0:9]
    elseif linkdict.link ># ''
        let found= 'link'
    else
        call setpos(".",save_cursor)
        " get area between colons, if any, in @x
        normal T:vt:"xy
        if (matchstr(@x,'\S\+') ># '') && (len(@x)<25)
            let found = 'tag'
        endif
    endif
    call setpos(".",save_cursor)
    if found ==? 'date'
        call OrgRunAgenda(date,1,'',date)
        " go to 8th line in agenda buf
        execute 8
    elseif found ==? 'link'
        call FollowLink(linkdict)
    elseif found ==? 'tag'
        call OrgRunSearch('+'.@x)
    else
        echo 'Nothing found to search for.'
    endif

endfunction
function! s:SetColumnHead()
" NOT USED NOW, NEEDS to be redone since switch to using orgmode-style col
" specs
    "let i = 0
    "while i < len(w:v.org_colview_list)
    "    let result .= '|' . s:PrePad(w:v.org_colview_list[i] , w:v.org_colview_list[i+1]) . ' ' 
    "    let i += 2
    "endwhile
    "let g:org_ColumnHead = result[:-2]
endfunction

function! s:OrgSetColumnList(line_for_cols,...)
    " call GetProperties making sure it gets inherited props (viz. COLUMNS)
    let save_inherit_setting = s:include_inherited_props
    let s:include_inherited_props = 1
    try
        let column_prop = s:GetProperties(a:line_for_cols,0)['COLUMNS']
    finally
        let s:include_inherited_props = save_inherit_setting
    endtry

    if (a:0 >= 1) && (a:1==0)
        " use 0 for master head, i.e., columns for entire doc
        let w:v.org_columns_master_heading = a:1
    else
        let w:v.org_columns_master_heading = s:OrgGetHead_l(a:line_for_cols)
    endif
    if (a:0 >= 2) && (a:2 ># '')
        " use column spec that was passed in
        let column_prop = a:2
    else   
        let w:v.org_current_columns = column_prop
    endif
    
    let result = ''
    let g:org_column_headers = ''
    let i = 0
    
    if column_prop ># ''
        let w:v.org_colview_list=split(column_prop,' ')
    else
        let w:v.org_colview_list=[]
    endif
    
    call s:SetColumnHeaders()

endfunction
function! s:SetColumnHeaders()
    " build g:org_column_headers
    let g:org_column_headers = ''
    let w:v.org_column_item_head = ''
    for item in (w:v.org_colview_list)
        let [ fmt, field, hdr ] = matchlist(item,'%\(\d*\)\(\S\{-}[^({]*\)(*\([^ )]*\)')[1:3]
        let fmt = (fmt ==# '') ? '%-' . g:org_columns_default_width . 's' : ('%-' . fmt . 's')
        if field ==# 'ITEM' 
           let w:v.org_column_item_head = (hdr=='') ? 'ITEM' : hdr
           continue 
        endif
        let g:org_column_headers .= printf('|' . fmt, (hdr ==# '') ? field : hdr )  
    endfor

endfunction
function! s:GetFoldColumns(line)
    let save_inherit_setting = s:include_inherited_props
    let s:include_inherited_props = 1
    try
        let props = s:GetProperties(a:line,0)
    finally
        let s:include_inherited_props = save_inherit_setting
    endtry
    " build text string with column values
    let result = ''
    for item in (w:v.org_colview_list)
        let [ fmt, field, hdr ] = matchlist(item,'%\(\d*\)\(\S\{-}[^({]*\)(*\(\S*\))*')[1:3]
        if field ==# 'ITEM' | continue | endif
        let fldtext = get(props,field,'')
        let fmt = (fmt ==# '') ? g:org_columns_default_width :  fmt 
        " truncate text if too long
        let fldtext = (len(fldtext)<=fmt) ? fldtext : (fldtext[:fmt-3] . '..')
        "let fmt = '%-' . g:org_columns_default_width . 's' : ('%-' . fmt . 's')
        let result .= printf( '|%-'.fmt.'s', fldtext,'') 
    endfor

    return result

endfunction
function! ToggleColumnView(master_head,col_spec)

    if w:v.columnview
        let winnum = bufwinnr('ColHeadBuffer')
        if winnum > 0 
            execute "bw!" . bufnr('ColHeadBuffer')
        endif
        let w:v.columnview = 0
    else
        call s:OrgSetColumnList(line('.'),a:master_head,a:col_spec)
        call s:ColHeadWindow(w:v.org_column_item_head)
        let w:v.columnview = 1
    endif   
endfunction
function! <SID>ColumnStatusLine()
    if exists('g:org_column_headers')
        let part2 = s:PrePad(g:org_column_headers, winwidth(0)-13) 

        return '   ' . w:v.org_column_item_head .  part2
    endif
endfunction
function! s:AdjustItemLen()
    " called on VimResized event, adjusts length of heading when folded
    if &filetype != 'org'
        return
    endif

    if !exists('w:v.columnview')
        let w:v={'columnview':0, 'org_item_len':100, 'org_colview_list':[],'org_current_columns':'','org_column_item_head':''}
    endif
    let i = 1
    let w:v.total_columns_width = 3
    let colspec = split(w:v.org_current_columns, ' ')
    
    for item in colspec
        let [ flen, field ] = matchlist(item,'%\(\d*\)\(\S\{-}[^({]*\)')[1:2]
        if field == 'ITEM' | continue | endif
        let w:v.total_columns_width += (flen > 0) ? flen : g:org_columns_default_width
    endfor
    
    let w:v.org_item_len = winwidth(0) - 10 - ((w:v.columnview==1) ? w:v.total_columns_width : 0)
endfunction
au VimResized * :call s:ResizedWin()
function! s:ResizedWin()
    let curwin = winnr()
    ""avoid using 'windo' b/c it screws up colheadbuffer's 0 height
    for i in range(1,winnr('$'))
        if getbufvar(winbufnr(i),'&filetype') == 'org'
             exec i . 'wincmd w'
             call s:AdjustItemLen()
        endif
    endfor
    exec curwin . 'wincmd w'
endfunction

function! <SID>CalendarChoice(day, month, year, week, dir)
    let g:agenda_startdate = a:year.'-' . s:Pre0(a:month).'-'.s:Pre0(a:day) 
    call OrgRunAgenda(g:agenda_startdate, g:org_agenda_days,g:org_search_spec)
endfunction
function! <SID>CalendarInsertDate(day, month, year, week, dir)
    if (a:year > 0) && (a:month>0) && (a:day>0)
        let g:cal_list=[a:year,a:month,a:day] 
    endif
    
    "call confirm('got here')
endfunction
function! s:SID()
    return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun
let g:org_sid = s:SID()
function! OrgSID(func)
    execute 'call <SNR>'.s:SID().'_'.a:func
endfunction
function! OrgFunc(func,...)
    "not working, itnended to be general way to 
    " call script-local functions
    let myfunc = function('<SNR>'.s:SID().'_'.a:func)
    if a:000 > 0
        let myargs = split(a:000,',')
    else
        let myargs = ''
    endif
endfunction
    
function! s:MyPopup()
    call feedkeys("i\<c-x>\<c-u>")
endfunction

let g:calendar_action = '<SNR>' . s:SID() .'_CalendarChoice'
let b:v.ColorList=['purple', 'green', 'white', 'black','blue','red','orange','green']
function! s:CompleteOrg(findstart, base)
    if a:findstart
        " locate the start of the word
        let line = getline('.')
        let start = col('.') - 1
        while (start > 0) && (line[start - 1] =~ '\a')
            let start -= 1
        endwhile
        return start
    else
        let prop = matchstr(getline(line(".")),'^\s*:\zs\s*\S\+\s*\ze:')
        " find months matching with "a:base"
        let res = []
        execute "let proplist = b:v." . prop . 'List' 
        "for m in split("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec")
        for m in proplist
            if m =~ '^' . a:base
                call add(res, m)
            endif
        endfor
        return res
    endif
endfunction
"set completefunc=CompleteOrg


function! OrgFoldText(...)
    " Create string used for folded text blocks
    if a:0 == 1
        let l:line = getline(line("."))
        let foldstart = line(".")
    else
        let l:line = getline(v:foldstart)
        let foldstart = v:foldstart
    endif
    let origline = l:line
    let l:nextline = getline(foldstart + 1)
    let myind = s:Ind(foldstart)

    "let level_highlight = hlID(b:v.foldcolors[myind])
    let level_highlight = hlID('OL' . (myind-1) . 'Folded')

    " get rid of header prefix
    let l:line = substitute(l:line,'^\*\+\s*','','g')
    let l:line = repeat(' ', s:Starcount(foldstart)+1) . l:line 
    let line_count = v:foldend - v:foldstart

    if l:line =~ b:v.drawerMatch
        "let level_highlight = hlID('Title')
        let level_highlight = hlID('Org_Drawer_Folded')
        let l:line = repeat(' ', len(matchstr(l:line,'^ *'))-1)
                    \ . matchstr(l:line,'\S.*$') 
        let line_count = line_count - 1
    elseif l:line[0] ==? '#'
        let level_highlight = hlID('VisualNOS')
    elseif w:v.columnview==1
        let mytrim = w:v.org_item_len
        let line = line[:mytrim]
    else
        let mytrim = w:v.org_item_len
        let line = line[:mytrim]
    endif
    if exists('w:sparse_on') && w:sparse_on && (a:0 == 0) 
        let b:v.signstring= s:GetPlacedSignsString(bufnr("%")) 
        if match(b:v.signstring,'line='.v:foldstart.'\s\sid=\d\+\s\sname=fbegin') >=0
        "if index(b:v.sparse_list,v:foldstart) > -1            "v:foldstart == 10
            let l:line = '* * * * * * * * * * * ' . (v:foldend - v:foldstart) . ' lines skipped here * * * * * * *'
            let l:line .= repeat(' ', winwidth(0)-len(l:line)-28) . 'SPARSETREE SKIP >>'
            let level_highlight = hlID('TabLineFill')
        endif
    endif
    if g:org_show_fold_dots 
        let l:line .= '...'
    endif
    let offset = &fdc + 5*(&number) + (w:v.columnview ? 7 : 1)
    if w:v.columnview && (origline =~ b:v.headMatch) 
        if (w:v.org_columns_master_heading == 0) || s:HasAncestorHeadOf(foldstart,w:v.org_columns_master_heading)
            let l:line .= s:PrePad(s:GetFoldColumns(foldstart), winwidth(0)-len(l:line) - offset)
        else
            let offset -= 6
        endif
    endif
    if a:0 && (foldclosed(line('.')) > 0)
        let l:line .= s:PrePad("(" 
            \  . s:PrePad( (foldclosedend(line('.'))-foldclosed(line('.'))) . ")",5),
            \ winwidth(0)-len(l:line) - offset) 
    elseif (g:org_show_fold_lines ) || (l:line =~ b:v.drawerMatch) 
        let offset = (w:v.columnview && l:line =~ b:v.drawerMatch) ? offset - 6 : offset 
        let l:line .= s:PrePad("|" . s:PrePad( line_count . "|",5),
                    \ winwidth(0)-len(l:line) - offset) 
    endif
    if exists('v:foldhighlight')
        if foldstart == b:v.chosen_agenda_heading
            let v:foldhighlight = hlID('Org_Chosen_Agenda_Heading' . (myind>6 ? '' : myind-1))
        else
            let v:foldhighlight = level_highlight
        endif
        if exists('v:todohighlight')
            if matchstr(origline, b:v.todoMatch) ># ''
                let this_todo = matchstr(origline, '^\*\+ \zs\S*')
                if hlID(this_todo) > 55      " > 55 avoids built-in todo group
                   let v:todohighlight = hlID(this_todo) 
                else
                    let v:todohighlight = ('* ' . this_todo =~ b:v.todoDoneMatch) ? hlID('DONETODO') : hlID('NOTDONETODO')
                endif
            else
                let v:todohighlight=0
            endif
        endif
    endif
    return l:line
endfunction

function! s:MySort(comppattern) range
    let b:v.sortcompare = a:comppattern
    let b:v.complist = ['\s*\S\+','\s*\S\+\s\+\zs\S\+','\s*\(\S\+\s\+\)\{2}\zs\S\+'
                \ , '\s*\(\S\+\s\+\)\{3}\zs\S\+'
                \ , '\s*\(\S\+\s\+\)\{4}\zs\S\+'
                \ , '\s*\(\S\+\s\+\)\{5}\zs\S\+'
                \ , '\s*\(\S\+\s\+\)\{6}\zs\S\+']
    let mylines = getline(a:firstline, a:lastline)
    let mylines = sort(mylines,"s:BCompare")
    call setline(a:firstline, mylines)
    unlet b:v.sortcompare
    unlet b:v.complist
endfunction

function! s:BCompare(i1,i2)
    if !exists('b:v.sortcompare')
        echo 'b:v.sortcompare is not defined'
        return
    endif
    let i = 0

    while i < len(b:v.sortcompare)
        " prefix an item by 'n' if you want numeric sorting
        if (i < len(b:v.sortcompare) - 1) && (b:v.sortcompare[i] ==? 'n')
            let i = i + 1
            let m1 = str2nr(matchstr(a:i1,b:v.complist[b:v.sortcompare[i]-1])) 
            let m2 = str2nr(matchstr(a:i2,b:v.complist[b:v.sortcompare[i]-1]))
        else
            let m1 = matchstr(a:i1,b:v.complist[b:v.sortcompare[i]-1]) 
            let m2 = matchstr(a:i2,b:v.complist[b:v.sortcompare[i]-1])
        endif
        if m1 == m2
            if i == len(b:v.sortcompare) - 1
                return 0
            else
                let i += 1
                continue
            endif
        elseif m1 > m2 
            return 1
        else 
            return -1
        endif
    endwhile
endfunction

function! s:OrgShowMatch(cycleflag)
    "wincmd k
    " first, make sure agenda buffer has same heading pattern
    " and todo list as main buffer
    call s:GotoMainWindow()
    let l:headMatch = b:v.headMatch
    let l:todoitems = b:v.todoitems
    "wincmd j
    call s:GotoAgendaWindow()
    let b:v.headMatch = l:headMatch
    let b:v.todoitems = l:todoitems
    if a:cycleflag
        call OrgSequenceTodo(line("."))
    endif
    "let g:showndx = line(".")-1
    if getline(line(".")) =~ '^\d\+'
        let g:showndx = matchlist(getline(line(".")),'^\d\+')[0]
        execute "let b:v.sparse_list = [" . g:showndx . ']'
    endif
    "wincmd k
    call s:GotoMainWindow()
    call OrgExpandWithoutText(1)
    execute g:showndx
    "execute g:alines[g:showndx]
    normal zv
    if a:cycleflag
        call OrgSequenceTodo(line("."))
    endif
    if getline(line(".")) =~ b:v.headMatch
        call OrgBodyTextOperation(line("."),s:OrgNextHead(),'collapse')
    endif
    "wincmd j
    call s:GotoAgendaWindow()
endfunction
command! MySynch call <SID>OrgShowMatch(0)
command! MySynchCycle call <SID>OrgShowMatch(1)
command! MyAgendaToBuf call <SID>OrgAgendaToBufTest()
command! AgendaMoveToBuf call s:OrgAgendaToBuf()

command! -range CodeEval :call <SID>CodeEval
command! -buffer -nargs=* Agenda :call OrgAgendaCommand(<f-args>)
function! CodeEval() range
    
endfunction

function! OrgAgendaCommand(...)
    if exists('a:1')
        let mydate = a:1
    else
        let mydate = s:Today()
    endif
    if exists('a:2')
        let viewdays = a:2
    else
        let viewdays = 'w'
    endif
    if exists('a:3')
        let search_spec = a:3
    else
        let search_spec = ''
    endif
    if mydate =~ '\d\d\d\d-\d\d-\d\d'
        call OrgRunAgenda(mydate,viewdays,search_spec)
    else
        call OrgRunAgenda(DateCueResult(mydate,s:Today()),viewdays,search_spec)
    endif
endfunction

function! s:OrgAgendaToBufTest()
    " this loads unfolded buffer into same window as Agenda
    if getline(line(".")) =~ '^\d\+'
        let thisline = getline(line('.'))
        let g:tofile = s:filedict[str2nr(matchstr(thisline, '^\d\d\d'))]
        let g:showndx = str2nr(matchstr(thisline,'^\d\d\d\zs\d*'))
        "let g:showndx = matchlist(getline(line(".")),'^\d\+')[0]
        "let g:tofile = matchlist(getline(line(".")),'^\d\+\s*\(\S\+\)')[1]
    endif
    let cur_buf = bufnr("%")
    let g:org_folds=0
    let newbuf = bufnr(g:tofile)
    execute "b"newbuf
    execute g:showndx
    let g:org_folds=1
endfunction
function! s:OrgAgendaToBuf()
    let win = bufwinnr('Calendar')
    if win >= 0 
        execute win . 'wincmd w'
        wincmd c
        execute bufwinnr('Agenda').'wincmd w'
    endif   

    if getline(line(".")) =~ '^\d\+'
        let thisline = getline(line('.'))
        let g:tofile = s:filedict[str2nr(matchstr(thisline, '^\d\d\d'))]
        let g:showndx = str2nr(matchstr(thisline,'^\d\d\d\zs\d*'))
    endif
    let ag_line = line(".")
    let ag_height = winheight(0)
    let cur_buf = bufnr("%")  " should be Agenda
    close!
    call s:LocateFile(g:tofile )
    "call s:LocateFile(g:tofile . '.org')
    if &fdm != 'expr'
        set fdm=expr
    endif
    " vsplit agenda *********************
    "vsplit
    " *********************
    split
    "wincmd J
    execute "b"cur_buf
    "call s:LocateFile(g:tofile . '.org')
    wincmd x
    "let new_buf=bufnr("%")
    "setlocal cursorline
    set foldlevel=1
    execute g:showndx
    normal! zv
    if getline(line('.')) =~ b:v.headMatch
        "restrict to headings only
        call s:OrgExpandSubtree(g:showndx,0)
    endif
    "normal! z.
    "normal V
    "redraw
    "sleep 100m
    "normal V
    let b:v.chosen_agenda_heading = s:OrgGetHead()
    call clearmatches()
    let headlevel = s:Ind(b:v.chosen_agenda_heading)
    let headlevel = (headlevel > 6) ? '' : headlevel-1
    call matchadd('Org_Chosen_Agenda_Heading' . headlevel,'\%' . b:v.chosen_agenda_heading .'l')
    "wincmd j
    execute bufnr('Agenda').'wincmd w'
    "wincmd c
    "split
    "wincmd j
    "execute "b" . bufnr('Agenda')
    execute ag_line
    resize
    execute "resize " . ag_height 
    "set foldlevel=9999
    "execute g:showndx
    "normal! z.
    if win >= 0
        Calendar
        execute 1
        wincmd l
        wincmd j
    endif
endfunction

function! s:OrgSource()
    unlet g:org_loaded
    source $VIM/vimfiles/ftplugin/org.vim
endfunction

function! s:OrgSetLevel(startlevel, endlevel)
    "call OrgExpandWithoutText(a:endlevel)
    call s:OrgExpandLevelText(a:startlevel, a:endlevel)
endfunction

function! s:Starcount(line)
    " used to get number of stars for a heading
    return (len(matchstr(getline(a:line),'^\**\s'))-1)
endfunction

function! s:GotoAgendaWindow()
    "wincmd b
    silent execute "b __Agenda__"
endfunction

function! s:GotoMainWindow()
    wincmd t
endfunction

function! s:Ind(line) 
    " used to get level of a heading (todo : rename this function)
    "return 1 + (len(matchstr(getline(a:line),'^\**\s'))-1)/b:v.levelstars  
    return 2 + (len(matchstr(getline(a:line),'^\**\s'))-2)/b:v.levelstars  

endfunction

function! s:DoAllTextFold(line)
    "let d = inputdialog('in fullfold')
    if s:IsText(a:line+1) == 0
        return 
    endif
    while ((s:NextVisibleHead(a:line) != foldclosedend(a:line) + 1) 
                \ && (foldclosedend(a:line) <= line("$"))
                \ && (s:NextVisibleHead(a:line) != 0)
                \ && (OrgFoldLevel(a:line) =~ '>')) 
                \ || (foldclosedend(a:line) < 0)  
                \ || ((s:NextVisibleHead(a:line) == 0) && (s:OrgSubtreeLastLine() == line('$')) && (foldclosedend(a:line)!=line('$')))
        call OrgDoSingleFold(a:line)
    endwhile
endfunction

function! OrgDoSingleFold(line)
    if (foldclosed(a:line) == -1) "&& (getline(a:line+1) !~ b:v.headMatch)
        if (getline(a:line+1) !~ b:v.headMatch) || (s:Ind(a:line+1) > s:Ind(a:line))
            while (foldclosed(a:line) == -1) && (a:line != line('$'))
                normal! zc
            endwhile
        endif
        "elseif (foldclosed(a:line) < a:line)
        " do nothing, line is not visible
    else
        let cur_end = foldclosedend(a:line)
        " I know runaway can happen if at last heading in document,
        " not sure where else
        let runaway_count = 0
        if (cur_end >= line("$")) "|| (OrgFoldLevel(cur_end+1) ==? '<0')
            return
        endif
        if getline(cur_end+1) =~ b:v.drawerMatch
            "while (foldclosedend(a:line) == cur_end) && (runaway_count < 10)
            while (foldclosedend(a:line) == cur_end) && (cur_end != line("$"))
                let runaway_count += 1
                normal! zc
            endwhile
        elseif getline(cur_end+1) !~ b:v.headMatch
            "while (foldclosedend(a:line) == cur_end) && (runaway_count < 10)
            while (foldclosedend(a:line) == cur_end) && (cur_end <= line("$"))
                let runaway_count += 1
                normal! zc
            endwhile
        elseif (getline(cur_end+1) =~ b:v.headMatch) && (s:Ind(cur_end+1) > s:Ind(a:line))
            while (foldclosedend(a:line) == cur_end) && (cur_end != line("$"))
                "   let runaway_count += 1
                normal! zc
            endwhile
        endif
    endif
endfunction

function! OrgFoldLevel(line)
    " called as foldexpr to determine the fold level of a line.
    "if exists('g:flist')
    "    call add(g:flist,a:line)
    "endif
    "if g:org_folds == 0
    "    return 0
    "endif
    " STUFF to short-circuit FOR SPARSE TREE LEVELS
    if exists('w:sparse_on') && w:sparse_on && (get(s:sparse_lines,a:line) == 1)
        if index(b:v.sparse_list,a:line+1) >= 0
            return '<0'
        endif
        let sparse = index(b:v.sparse_list,a:line)
        if sparse >= 0
            return '>99'
        endif
        let sparse = index(b:v.fold_list,a:line)
        if sparse >= 0
            return '<0' 
        endif
    endif

    "let l:text = getline(a:line)
    "let l:nexttext = getline(a:line + 1)
    let [l:text, l:nexttext] = getline(a:line,a:line+1)
    "if l:text =~ b:v.headMatch
    if l:text =~ '^\*\+\s'
        let b:v.myAbsLevel = s:Ind(a:line)
    elseif (b:v.lasttext_lev ># '') && (l:text !~ s:remstring) && (l:nexttext !~ '^\*\+\s') && (b:v.lastline == a:line - 1)
        let b:v.lastline = a:line
        return b:v.lasttext_lev
    endif
    let l:nextAbsLevel = s:Ind(a:line + 1)


    "if l:text[0] ==? '*'
    if l:text =~ '^\*\+\s'
        " we're on a heading line
        let b:v.lasttext_lev = ''
        
        if l:nexttext =~ b:v.drawerMatch
            let b:v.lev = '>' . string(b:v.myAbsLevel + 4)
        elseif l:nexttext =~ s:remstring
            let b:v.lev = '>' . string(b:v.myAbsLevel + 6)
        elseif (l:nexttext !~ b:v.headMatch) && (a:line != line('$'))
            let b:v.lev = '>' . string(b:v.myAbsLevel + 3)
        elseif l:nextAbsLevel > b:v.myAbsLevel
            let b:v.lev = '>' . string(b:v.myAbsLevel)
        elseif l:nextAbsLevel < b:v.myAbsLevel
            let b:v.lev = '<' . string(l:nextAbsLevel)
        else
            let b:v.lev = '<' . b:v.myAbsLevel
        endif
        let b:v.prevlev = b:v.myAbsLevel

    else    
        "we have a text line 
        if b:v.lastline != a:line - 1    " backup to headline to get bearings
            if l:text =~ b:v.drawerMatch
                let b:v.prevlev = s:Ind(s:OrgPrevHead_l(a:line))
            else
                "don't just back up, recalc previous lines
                " to set variables correctly
                let prevhead = s:OrgPrevHead_l(a:line)
                if prevhead == 0
                    " shortcircuit here, it's blank line prior to any head
                    return -1
                endif
                let i = prevhead
                "for item in range(prevhead,a:line-1)
                "    call OrgFoldLevel(item)
                "endfor
            endif
            "let b:v.prevlev = s:Ind(s:OrgPrevHead_l(a:line))
        endif

        if l:text =~ b:v.drawerMatch
            let b:v.lev = '>' . string(b:v.prevlev + 4)
        elseif l:text =~ s:remstring
            if (getline(a:line - 1) =~ b:v.headMatch) && (l:nexttext =~ s:remstring)
                let b:v.lev =  string(b:v.prevlev + 5)
            elseif (l:nexttext !~ s:remstring) || 
                        \ (l:nexttext =~ b:v.drawerMatch) 
                let b:v.lev = '<' . string(b:v.prevlev + 4)
            else
                let b:v.lev = b:v.prevlev + 4
            endif
        elseif l:text[0] != '#'
            let b:v.lev = (b:v.prevlev + 2)
            let b:v.lasttext_lev = b:v.lev
        elseif b:v.src_fold  
            if l:text =~ '^#+begin_src'
                let b:v.lev = '>' . (b:v.prevlev + 2)
            elseif l:text =~ '^#+end_src'
                let b:v.lev = '<' . (b:v.prevlev + 2)
            endif
        else 
            let b:v.lev = (b:v.prevlev + 2)
        endif   

        if l:nexttext =~ '^\* '
            " this is for perf reasons, closing fold
            " back to zero avoids foldlevel calls sometimes
            let b:v.lev = '<0'
        elseif l:nexttext =~ '^\*\+\s'
            let b:v.lev = '<' . string(l:nextAbsLevel)
        endif

    endif   
    let b:v.lastline = a:line
    return b:v.lev    

endfunction

function! s:AlignSection(regex,skip,extra) range
    " skip is first part of regex, 'regex' is part to match
    " they must work together so that 'skip.regex' is matched
    " and the point where they connect is where space is inserted
    let extra = a:extra
    let sep = empty(a:regex) ? '=' : a:regex
    let minst = 999
    let maxst = 0
    let b:v.stposd = {}
    let section = getline(a:firstline, a:lastline)
    for line in section
        let stpos = matchend(line,a:skip)   
        let b:v.stposd[index(section,line)]=stpos
        if maxst < stpos
            let maxst = stpos
        endif
        let stpos = len(matchstr(matchstr(line,a:skip),'\s*$'))
        if minst > stpos
            let minst = stpos
        endif
    endfor
    call map(section, 's:AlignLine(v:val, sep, a:skip, minst, maxst - matchend(v:val,a:skip), extra)')
    call setline(a:firstline, section)
endfunction

function! s:AlignLine(line, sep, skip, maxpos, offset, extra)
    let b:v.m = matchlist(a:line, '\(' .a:skip . '\)\('.a:sep.'.*\)')
    if empty(b:v.m)
        return a:line
    endif
    let spaces = repeat(' ',  a:offset + a:extra)
    exec 'return b:v.m[1][:-' . a:maxpos .'] . spaces . b:v.m[3]'
endfunction
function! s:AlignSectionR(regex,skip,extra) range
    let extra = a:extra
    let sep = empty(a:regex) ? '=' : a:regex
    let minst = 999
    let maxpos = 0
    let maxst = 0
    let b:v.stposd = {}
    let section = getline(a:firstline, a:lastline)
    for line in section
        execute 'let pos = matchend(line, a:skip ." *".sep)'
        if maxpos < pos
            let maxpos = pos
        endif
        let stpos = len(matchstr(matchstr(line,a:skip),'\s*$')) 
        if minst > stpos
            let minst = stpos
        endif
    endfor
    call map(section, 's:AlignLine(v:val, sep, a:skip, minst, maxpos - matchend(v:val,a:skip.sep) , extra)')
    call setline(a:firstline, section)
endfunction
function! s:ColHeadWindow(itemhead,...)
    if (a:0 >= 1) && (a:1 == 0) 
       if bufnr('ColHeadBuffer') > -1
           bw ColHeadBuffer
       endif
       return
    endif

    au! BufEnter ColHeadBuffer
    "let s:AgendaBufferName = 'ColHeadBuffer'
    "call s:AgendaBufferOpen(1)
    "let s:AgendaBufferName = '__Agenda__'
    1split ColHeadBuffer
    call s:ScratchBufSetup()
    
    execute "setlocal statusline=%#OrgColumnHeadings#%{<SNR>" . s:SID() . '_ColumnStatusLine()}'
    set winfixheight
    set winminheight=0
    let w:v = {'org_column_item_head': a:itemhead}
    
    wincmd j
    " make lower window as big as possible to shrink 
    " ColHeadWindow to zero height
    let curheight = winheight(0)
    resize 100
    if bufwinnr('Agenda') > 0
        execute "resize " . curheight 
    endif
    au BufEnter ColHeadBuffer call s:ColHeadBufferEnter()
endfunction

function! s:ColHeadBufferEnter()
    "prevents user from entering this buffer
    "wincmd j
endfunction
" AgendaBufferOpen
" Open the scratch buffer
function! s:AgendaBufferOpen(new_win)
    let split_win = a:new_win

    " If the current buffer is modified then open the scratch buffer in a new
    " window
    if !split_win && &modified
        let split_win = 1
    endif

    " Check whether the scratch buffer is already created
    let scr_bufnum = bufnr(s:AgendaBufferName)
    if scr_bufnum == -1
        " open a new scratch buffer
        if split_win
            " vsplit agenda ***************
            "exe "vnew " . s:AgendaBufferName
            " ***************************
            exe "new " . s:AgendaBufferName
        else
            exe "edit " . s:AgendaBufferName
        endif
        " vsplit agenda *******************
        "wincmd L
        " ******************
        wincmd J
    else
        " Agenda buffer is already created. Check whether it is open
        " in one of the windows
        let scr_winnum = bufwinnr(scr_bufnum)
        if scr_winnum != -1
            " Jump to the window which has the scratch buffer if we are not
            " already in that window
            if winnr() != scr_winnum
                exe scr_winnum . "wincmd w"
            endif
        else
            " open a window and put existing Agenda in
            if split_win
                " vspli agenda ********************
                "exe "vsplit +buffer" . scr_bufnum
                " **************************
                exe "split +buffer" . scr_bufnum
            else
                exe "buffer " . scr_bufnum
            endif
        endif
    endif
endfunction

function! s:CaptureBuffer()
    let w:prevbuf=bufnr("%")
    sp _Capture_
    normal ggVGd
    normal i** 
    silent exec "normal o<".org#Timestamp().">"
    call s:ScratchBufSetup()
    command! -buffer W :call s:ProcessCapture()
    normal gg$a
    
endfunction
function! s:ProcessCapture()
    normal ggVG"xy
    execute "tab drop ".g:org_capture_file
    normal gg
    call search('^\* Agenda')
    execute s:OrgSubtreeLastLine()
    normal p
    normal gg
    silent write
    redo
    call s:LocateFile('_Capture_')
    execute "bd"
endfunction

command! EditAgendaFiles :call EditAgendaFiles()
function! EditAgendaFiles()
    if !exists("g:agenda_files") || (g:agenda_files == [])
        call s:CurfileAgenda()
    endif
    tabnew
    call s:ScratchBufSetup()
    command! W :call s:SaveAgendaFiles()
    let msg = "These are your current agenda files:"
    let msg2 = "Org files in your 'g:org_agenda_select_dirs' are below."
    call setline(1,[msg])
    call append(1, repeat('-',winwidth(0)-5))
    call append("$",g:agenda_files + ['',''])
    " change '\ ' to plain ' ' for current text in buffer
    silent! execute '%s/\\ / /g'
    let line = repeat('-',winwidth(0)-5)
    call append("$",[line] + [msg2,"To add files to 'g:agenda_files' copy or move them ","to between the preceding lines and press :W to save (or :q to cancel):","",""])
    for item in g:org_agenda_select_dirs
        call append("$",split(globpath(item,"*.org"),"\n"))
    endfor
endfunction
function! s:SaveAgendaFiles()
    " yank files into @a
   normal gg/^--jV/^--?^\S"ay 
   let @a = substitute(@a,' ','\\ ','g')
   if g:agenda_files[0][1] != '-'
        let g:agenda_files = split(@a,"\n")
    else
        let g:agenda_files=[]
    endif
    :bw
    delcommand W
endfunction

function! s:ScratchBufSetup()
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal buflisted
    setlocal fdc=1
endfunction
function! s:Emacs2PDF()
    silent !"c:program files (x86)\emacs\emacs\bin\emacs.exe" -batch --visit=newtest3.org --funcall org-export-as-pdf
    "silent !c:\sumatra.exe newtest3.org
endfunction
function! s:Today()
    return strftime("%Y-%m-%d")
endfunction

function! OrgCustomSearchMenu()
    if !exists('g:org_custom_searches') || empty(g:org_custom_searches)
        echo "No custom searches defined."
    else
        echo " Press number to run custom search:"
        echo " ----------------------------------"
        let i = 1
        for item in g:org_custom_searches
            "echo '   (' . i . ') ' . item.name . '  ' . item.type 
            echo printf(" (%d) %-25s %10s", i, item.name, item.type )
            let i += 1
        endfor
        echo " "
        let key = nr2char(getchar())
        let itemnum = str2nr(key)
        if itemnum > 0 && itemnum <= len(g:org_custom_searches)
            call RunCustom( itemnum - 1 )
        else
            echo 'No search was chosen.'
        endif
    endif
endfunction

function! OrgAgendaDashboard()
    if (bufnr('__Agenda__') >= 0) && (bufwinnr('__Agenda__') == -1)
        " move agenda to cur tab if it exists and is on a different tab
        let curtab = tabpagenr()
        call s:LocateFile('__Agenda__')
        wincmd c
        execute "tabnext ".curtab
        split
        winc j
        buffer __Agenda__
    else
        " show dashboard if there is no agenda buffer or it's 
        " already on this tab page
        let restrict = 0
        let saved_afiles = []
        while 1
            echohl MoreMsg
            echo ""
            echo " ================================"
            echo " Press key for an agenda command:"
            echo " --------------------------------"
            echo " a   Agenda for current week"
            echo " t   List of all TODO entries"
            echo " m   Match a TAGS/PROP/TODO query"
            echo " L   Timeline for current buffer"
            "echo ' s   Freeform regex search, not heading-metadata'
            echo " "
            echo " c   Show custom search menu"
            echo " "
            echo " h   Headline-metadata-based sparse tree search"
            echo " f   Freeform (i.e., regex) sparse tree search" 
            echo " <   restrict to current buffer"
            if restrict == 1
                echo "     Will restrict to current buffer.  Press a key to choose search..."
            endif
            echo ""
            echohl None
            let key = nr2char(getchar())
            redraw
            if key == '<'
                let restrict = 1
                continue
            else
                break
            endif
        endwhile
        if restrict == 1
            let save_win = winnr()
            for winnum in range(1,winnr('$'))
                exec winnum . 'wincmd w'
                if expand('%') =~ '\.org$'
                    let saved_afiles = copy(g:agenda_files)
                    let g:agenda_files = [expand('%:p')]
                    break
                endif
            endfor
            exec save_win . 'wincmd w'
        endif
        try
            if key ==? 't'
                silent execute "call OrgRunSearch('+ANY_TODO','agenda_todo')"
            elseif key ==? 'a'
                "if (g:org_search_spec ==# '') 
                    "let g:org_search_spec = g:agenda_default_search_spec
                "endif
                silent execute "call OrgRunAgenda(s:Today(),'w', g:org_agenda_default_search_spec)"
            elseif key ==? 'L'
                silent execute "call s:Timeline()"
            elseif key ==? 'c'
                execute "call OrgCustomSearchMenu()"
            elseif key ==? 'm'
                let mysearch = input("Enter search string: ")
                silent execute "call OrgRunSearch(mysearch)"
            elseif key ==? 'h'
                let g:org_sparse_spec = input("Enter search string: ")
                if bufname("%") ==? '__Agenda__'
                    :bd
                endif
                silent execute "call OrgRunSearch(g:org_sparse_spec,1)"
            elseif key ==? 'f'
                let g:org_sparse_spec = input("Enter search string: ")
                if bufname("%") ==? '__Agenda__'
                    :bd
                endif
                silent call s:SparseTreeRun(g:org_sparse_spec)
            endif
        finally
            if len(saved_afiles) > 0
                let g:agenda_files = copy(saved_afiles)
            endif
        endtry
    endif
endfunction

function! s:AgendaBufHighlight()
    hi Overdue guifg=red
    hi Upcoming guifg=yellow
    hi DateType guifg=#dd66bb
    hi Locator guifg=#333333

    hi Dayline guifg=#44aa44 gui=underline
    hi Weekendline guifg=#55ee55 gui=underline
  
   call s:AgendaHighlight()
    let daytextpat = '^[^S]\S\+\s\+\d\{1,2}\s\S\+\s\d\d\d\d.*'
    let wkendtextpat = '^S\S\+\s\+\d\{1,2}\s\S\+\s\d\d\d\d.*'
    syntax match AOL1 ' \*\{1} .*$'
    syntax match AOL2 ' \*\{2} .*$'
    syntax match AOL3 ' \*\{3} .*$'
    syntax match AOL4 ' \*\{4} .*$'
    syntax match AOL5 ' \*\{5} .*$'
    
    call matchadd( 'Overdue', '^\S*\s*\S*\s*\(In\s*\zs-\S* d.\ze:\|Sched.\zs.*X\ze:\)')
    call matchadd( 'Upcoming', '^\S*\s*\S*\s*In\s*\zs[^-]* d.\ze:')
    syntax match Locator '^\d\+' conceal
    syntax match TimeGridSpace '^ \{8}\ze *\d\d:\d\d' conceal
    call matchadd( 'Dayline', daytextpat )
    call matchadd( 'Weekendline', wkendtextpat)
    call matchadd( 'DateType','DEADLINE\|SCHEDULED\|CLOSED')
    "
    let donepat = ' \*\+ \zs\(' . join(keys(g:org_todos_done_dict),'\|') . '\) '
    exec "syntax match DONETODO /" . donepat . '/ containedin=AOL1,AOL2,AOL3,AOL4,AOL5'
    let notdonepat = ' \*\+ \zs\(' . join(keys(g:org_todos_notdone_dict),'\|') . '\) '
    exec "syntax match NOTDONETODO /" . notdonepat . '/ containedin=AOL1,AOL2,AOL3,AOL4,AOL5'

    call s:OrgCustomTodoHighlights()
    
    execute "source " . s:sfile . '/vimorg-agenda-mappings.vim'

endfunction
function! s:AgendaHighlight()
    if g:org_gray_agenda
        hi link AOL1 NONE 
        hi link AOL2 NONE
        hi link AOL3 NONE
        hi link AOL4 NONE
        hi link AOL5 NONE
        hi Deadline guifg=lightred
        hi Scheduled guifg=lightyellow
        
    else
        hi link AOL1 OL1
        hi link AOL2 OL2
        hi link AOL3 OL3
        hi link AOL4 OL4
        hi link AOL5 OL5
        hi Deadline guifg=NONE
        hi Scheduled guifg=NONE
    endif
endfunction

function! OrgScreenLines() range
    " returns lines as
    " seen on screen, including folded text overlays
    " Call with visual selection set, or will
    " use last selection
    let save_cursor = getpos('.')
    let newline=0
    let oldline=1
    let mylines=[]
    normal '>
    let endline = line('.')
    " go to first line of selection
    normal '<
    while (line('.') <= endline) && (newline != oldline)
        let oldline=line('.')
        let newline=oldline
        call add(mylines,OrgFoldText(line('.')))
        normal j
        let newline=line('.')
    endwhile
    call setpos('.',save_cursor)
    return mylines
endfunction

function! s:CurTodo(line)
    let result = matchstr(getline(a:line),'.*\* \zs\S\+\ze ')`
    if index(b:v.todoitems,curtodo) == -1
        let result = ''
    endif
    return result
endfunction

"autocmd CursorHold * call s:Timer()
function! s:Timer()
    call feedkeys("f\e")
    " K_IGNORE keycode does not work after version 7.2.025)
    echo strftime("%c")
    " there are numerous other keysequences that you can use
endfunction

autocmd BufNewFile __Agenda__ call s:ScratchBufSetup()
autocmd BufWinEnter __Agenda__ call s:AgendaBufHighlight()
" Command to edit the scratch buffer in the current window
"command! -nargs=0 Agenda call s:AgendaBufferOpen(0)
" Command to open the scratch buffer in a new split window
command! -nargs=0 AAgenda call s:AgendaBufferOpen(1)

command! -nargs=0 OrgToPDF :call s:ExportToPDF()
command! -nargs=0 OrgToHTML :call s:ExportToHTML()
command! -nargs=0 OrgToAscii :call s:ExportToAscii()
command! -nargs=0 OrgToDocBook :call s:ExportToDocBook()
function! s:OrgHasEmacsVar()
    let result = 1
    if !exists('g:org_command_for_emacsclient')
        let msg = "=============================================== \n"
                \ . "You're trying to call out to Emacs but \n"
                \ . "you haven't set an Emacs command variable. \n"
                \ . "You should set this in your vimrc by including \n"
               \ . "a line like: \n\n"
               \ . "    let g:org_command_for_emacsclient=[put command to start emacs here] \n\n"
               \ . "See :h vimorg-emacs-setup for more info. \n\n"
               \ . "The call you attempted to Emacs will now be aborted.  \n"
               \ . "Revise your vimrc and restart Vim to use this feature.\n"
               \ . "==============================================\n"
               \ . "Press <enter> to continue."
        call input(msg)
        let result = 0
    endif
    return result
endfunction
function! OrgEvalBlock()
    let savecursor = getpos('.')
    let save_showcmd = &showcmd | set noshowcmd
    
    let block_name = matchstr(getline(line('.')),'\c^#+BEGIN:\s*\zs\S\+')

    if block_name ==# ''
        echo "You aren't on BEGIN line of dynamic block."
        return
    endif
    let end = search('\c^#+END','n','') 
    let start=line('.')
    exec (start+1) . ',' . (end-1) . 'delete'
    exec start
    let line_mark = '@@@@@' . start . '@e@f@g@h'
    exec 'normal o' . line_mark 
    
    silent write!
    let this_file = substitute(expand("%:p"),'\','/','g')
    let this_file = substitute(this_file,' ','\ ','g')

    let part1 = '(let ((org-confirm-babel-evaluate nil)(buf (find-file \' . s:cmd_line_quote_fix . '"' . this_file . '\' . s:cmd_line_quote_fix . '"' . '))) (progn (search-forward \^"' . line_mark . '\^" )(forward-line -1)(org-dblock-update)(beginning-of-line)(set-mark (point))(re-search-forward \^"^#\\+END\^")(end-of-line)(write-region (mark) (point) \' . s:cmd_line_quote_fix . '"~/org-block.org\' . s:cmd_line_quote_fix . '")(set-buffer buf) (not-modified) (kill-this-buffer)))' 
    " line below was using org-narrow-to-block, which may use again
        "let part1 = '(let ((org-confirm-babel-evaluate nil)(buf (find-file \' . s:cmd_line_quote_fix . '"' . this_file . '\' . s:cmd_line_quote_fix . '"' . '))) (progn (search-forward \^"' . line_mark . '\^" )(forward-line -1)(org-dblock-update)(org-narrow-to-block)(write-region (point-min) (point-max) \' . s:cmd_line_quote_fix . '"~/org-block.org\' . s:cmd_line_quote_fix . '")(set-buffer buf) (not-modified) (kill-this-buffer)))' 
        let orgcmd = g:org_command_for_emacsclient . ' --eval ' . s:cmd_line_quote_fix . '"' . part1 . s:cmd_line_quote_fix . '"'
        redraw
        unsilent echo "Calculating in Emacs. . . "
        if exists('*xolox#shell#execute')
            silent call xolox#shell#execute(orgcmd, 1)
        else
          silent  exe '!' . orgcmd
        endif
        let g:orgcmd = orgcmd
        exec start
        normal 3ddk
        silent exe 'read ~/org-block.org'
        redraw
        unsilent echo "Block is being evaluated in Emacs. . .   Evaluation complete."

        let &showcmd = save_showcmd
    call setpos('.',savecursor)
endfunction

function! s:OrgTableOptionList(A,L,P)
    return keys(s:OrgTableEvalOptions())
endfunction
command! -buffer -nargs=? -complete=customlist,s:OrgTableOptionList OrgTblEval :call OrgEvalTable(<f-args>)
function! s:OrgTableEvalOptions()
    return  { 'col_right':'org-table-move-column-right',
                            \ 'col_left': 'org-table-move-column-left',
                            \ 'col_delete': 'org-table-delete-column',
                            \ 'col_insert': 'org-table-insert-column',
                            \ 'row_down': 'org-table-move-row-down',
                            \ 'row_up': 'org-table-move-row-up',
                            \ 'row_delete': 'org-table-kill-row',
                            \ 'row_insert': 'org-table-insert-row',
                            \ 'row_sort_region_alpha': 'org-table-sort-lines nil ?a',
                            \ 'row_sort_region_numeric': 'org-table-sort-lines nil ?n',
                            \ 'row_sort_region_alpha_reverse': 'org-table-sort-lines nil ?A',
                            \ 'row_sort_region_numeric_reverse': 'org-table-sort-lines nil ?N',
                            \ 'row_hline_insert': 'org-table-insert-hline',
                           \  'convert_region_to_table':'org-table-convert-region (point-min) (point-max)'  }
endfunction

command! -buffer -nargs=0 OrgTableDashboard :call OrgTableDashboard()
function! OrgTableDashboard()
    if s:OrgHasEmacsVar() == 0
       return
    endif
    let save_more = &more | set nomore
    let save_showcmd = &showcmd | set noshowcmd
    " different dashboard for "in table" and "not in table"
    " show export dashboard
    if getline(line('.')) =~ '^\s*$'
        let rows_cols = input("Create new table (enter rows, columns): ")
        if rows_cols =~ '^\d\+\s*,\s*\d\+$'
            let [rows,cols] = split(rows_cols,',')
            call org#tbl#create(cols,rows)
        elseif rows_cols =~ '^\d\+\s\+\d\+$'
            let [rows,cols] = split(rows_cols,' ')
            call org#tbl#create(cols,rows)
        endif
        return
    endif
    echohl MoreMsg
    echo " --------------------------------"
    echo " Press key for table  operation:"
    echo " --------------------------------"
    if getline(line('.')) !~ b:v.tableMatch
        let mydict = {  't' : 'convert_region_to_table'}  
        echo " [t]  Create (t)able from current block" 
    else
        let mydict = { 'l':'col_left', 'r':'col_right', 'e':'col_delete', 'o':'col_insert',
                \     'd':'row_down', 'u':'row_up', 'x':'row_delete', 
                \     'i':'row_insert', 'a':'row_sort_region_alpha', 'A':'row_sort_region_alpha_reverse',
                \     'n':'row_sort_region_numeric', 'N':'row_sort_region_numeric', 'h':'row_hline_insert'
                \      } 
        echo " COLUMN:  [l] Move left  [r] Move right  [e] Delete  [o] Insert"
        echo " ROW:     [d] Move down  [u] Move up     [x] Delete  [i] Insert"
        echo " "
        echo " RowSort: [a] alpha(a-z)     [A] alpha(z-a)"
        echo "          [n] numeric(1..9)  [N] numeric(9-1)"
        echo ""
        echo "          [h] insert horizontal line"
    endif
    echo " "
    echohl None
    let key = nr2char(getchar())
    for item in keys(mydict)
        if key == 't'
            let thisline = getline(line('.'))
            if thisline !~ '^\s*$'
                let firstline = search('^\s*$','nb','') + 1
                let lastline = search('^\s*$','n','') - 1
                exec firstline . ',' . lastline . 'call OrgEvalTable(mydict[item])'
            else
                echo "You aren't in a block of text."
            endif
            break
        elseif (key =~# item) 
            exec 'OrgTblEval ' . mydict[item]
            break
        endif
    endfor
    let &more = save_more
    let &showcmd = save_showcmd

endfunction

function! OrgEvalTable(...) range
    let options = s:OrgTableEvalOptions()
    if a:0 == 1
        let opt = a:1
        let opt_cmd = '(' . options[opt] . ')'
    else
        let opt = 'just_eval'
        let opt_cmd = ''
    endif
    let savecursor = getpos('.')
    if a:firstline == a:lastline
        " get start, end for whole table
        call search('^\s*[^|]','b','')
        let start=line('.')
        call search('^\(\s*|\)\@!','','')
        let end=line('.')
    else
        let start=a:firstline
        let end  =a:lastline
    endif
    let line_offset = savecursor[1] - start + 1
    "let line_offset = savecursor[1] - start 
    " find first line after table block and check for formulas
        exe start . ',' . end . 'w! ~/org-tbl-block.org'
        if opt != 'convert_region_to_table'
            let part1 = '(let ((org-confirm-babel-evaluate nil)'
                       \  . '(buf (find-file \' . s:cmd_line_quote_fix . '"~/org-tbl-block.org\' . s:cmd_line_quote_fix . '"' . ')))'
                       \  . '(progn (beginning-of-line ' . line_offset . ')(forward-char ' . savecursor[2] .')'
                       \  . '(org-table-maybe-eval-formula)' 
                       \  . ((opt=='just_eval') ? '' : '(unwind-protect ') . opt_cmd 
                       \  . '(org-table-recalculate-buffer-tables)(save-buffer buf)(kill-buffer buf))))' 
        else
            let part1 = '(let ((org-confirm-babel-evaluate nil)'
                       \  . '(buf (find-file \' . s:cmd_line_quote_fix . '"~/org-tbl-block.org\' . s:cmd_line_quote_fix . '"' . ')))'
                       \  . '(progn (beginning-of-line ' . line_offset . ')(forward-char ' . savecursor[2] .')'
                       \  . '(goto-char (point-min))(set-mark (point))(goto-char (point-max))' . opt_cmd
                       \  . '(save-buffer buf)(kill-buffer buf)))' 
        endif
        let orgcmd = g:org_command_for_emacsclient . ' --eval ' . s:cmd_line_quote_fix . '"' . part1 . s:cmd_line_quote_fix . '"'
        redraw
        unsilent echo "Calculating in Emacs. . . "

        let g:orgcmd = orgcmd

        if exists('*xolox#shell#execute')
            silent let myx = xolox#shell#execute(orgcmd . '| cat', 1)
        else
            silent exe '!' . orgcmd
        endif
        exe start .',' . end . 'read ~/org-tbl-block.org'
        exe start . ',' . end . 'd'
        redraw
        unsilent echo "Calculating in Emacs. . .   Calculations complete. " 
    "else
    "    unsilent echo "No #+TBLFM line at end of table, so no calculations necessary."
    "endif
    call setpos('.',savecursor)
endfunction
function! OrgEval()
    if s:OrgHasEmacsVar() == 0
        call confirm('VimOrganizer has not been configured to make calls to Emacs.'
                  \ . "\nPlease see :h vimorg-emacs-setup.") 
       return
    endif
    let line = getline(line('.'))
    if line =~ '\c^#+BEGIN:'
        call OrgEvalBlock()
    elseif line =~ '^\s*|.*|\s*$'
        call OrgEvalTable()
    elseif line =~ '\c^#+BEGIN_'
        call OrgEvalSource()
    else    
        unsilent echo "No evaluation done.  You must be in a table, or on an initial "
             \ . "\nblock line that begins in col 0 with #+BEGIN . . ."
    endif
endfunction

function! OrgEvalSource()
    let savecursor = getpos('.')
    let start = search('^#+begin_src','bn','') - 1
    let prev_end = search('^#+end_src','bn','') 
    let end = search('^#+end_src','n','') 
    if (start == -1) || (end == 0) || ( ( prev_end > start ) && (prev_end < line('.') ) )
        echo "You aren't in a code block."
        return
    endif
    exec end
    " include results if there is result block w/in a couple of lines
    if ( search('^\s*#+results','nW','') - end ) <= 3
        call search('^\s*#+results','','')
        if getline(line('.')+1) =~ '#+BEGIN_RESULT'
            call search('^#+END_RESULT')
            normal j
        else
            " :'s used as linebegins w/first blank line as end of result block
            call search('^\s*$','','')
        endif
        normal k
        let end = line('.')
    endif
    exe start . ',' . end . 'w! ~/org-src-block.org'
    let part1 = '(let ((org-confirm-babel-evaluate nil)) (progn (find-file \' . s:cmd_line_quote_fix . '"~/org-src-block.org\' . s:cmd_line_quote_fix . '"' . ')(org-babel-next-src-block)(org-babel-execute-src-block)(save-buffer)(kill-buffer)))' 
    let orgcmd = g:org_command_for_emacsclient . ' --eval ' . s:cmd_line_quote_fix . '"' . part1 . s:cmd_line_quote_fix . '"'
    if exists('*xolox#shell#execute')
        silent call xolox#shell#execute(orgcmd, 1)
    else
        silent exe "!" . orgcmd
    endif
    exe start .',' . end . 'read ~/org-src-block.org'
    exe start . ',' . end . 'd'
    call setpos('.',savecursor)
endfunction
function! MyExpTest()
    let g:orgpath='c:\users\herbert\emacsclientw.exe --eval '
    let g:myfilename = substitute(expand("%:p"),'\','/','g')
    let g:myfilename = substitute(g:myfilename, '/ ','\ ','g')
    let g:myvar = '(let ((org-export-babel-evaluate nil)) (progn (find-file \^' . '"' . g:myfilename . '\^' . '"' . ') (org-export-as-html-and-open 3) (kill-buffer) ))'
    let g:myc =  '!' . g:orgpath . '^"' . g:myvar . '^"' 
    silent exec g:myc
endfunction
function! OrgExportDashboard()
    if s:OrgHasEmacsVar() == 0
       return
    endif
    let save_more = &more | set nomore
    let save_showcmd = &showcmd | set noshowcmd
    " show export dashboard
    "let mydict = { 't':'template', 'a':'ascii', 'n':'latin1', 'u':'utf8',
    let mydict = { 't':'template', 'a':'ascii', 'A':'ascii', 'o':'odt', 'O':'odt-and-open',
            \     'n':'latin1', 'N':'latin1', 'u':'utf8','U':'utf8',
            \     'h':'html', 'b':'html-and-open', 'l':'latex', 
            \     'f':'freemind', 'j':'taskjuggler', 'k':'taskjuggler-and-open',
            \     'p':'pdf', 'd':'pdf-and-open', 'D':'docbook', 'g':'tangle',  
            \     'F':'current-file', 'P':'current-project', 'E':'all' } 
    echohl MoreMsg
    echo " Press key for export operation:"
    echo " --------------------------------"
    echo " [t]   insert the export options template block"
    echo " "
    echo " [a/n/u]  export as ASCII/Latin1/utf8  [A/N/U] ...and open in buffer"
    echo " "
    echo " [h] export as HTML"
    echo " [b] export as HTML and open in browser"
    echo " "
    echo " [l] export as LaTeX"
    echo " [p] export as LaTeX and process to PDF"
    echo " [d] . . . and open PDF file"
    echo " "
    echo " [o] export as ODT        [O] as ODT and open"
    echo " [D] export as DocBook"
    echo " [V] export as DocBook, process to PDF, and open"
    echo " [x] export as XOXO       [j] export as TaskJuggler"
    echo " [m] export as Freemind   [k] export as TaskJuggler and open"

    echo " [g] tangle file"
    echo " "
    echo " [F] publish current file"
    echo " [P] publish current project"
    echo " [E] publish all projects"
    echo " "
    echohl None
    let key = nr2char(getchar())
    for item in keys(mydict)
        if (item ==# key) && (item !=# 't')
            "let g:org_emacs_autoconvert = 1
            "call s:GlobalUnconvertTags(changenr())
            let exportfile = expand('%:t') 
            silent exec 'write'

            let orgpath = g:org_command_for_emacsclient . ' -n --eval '
            let g:myfilename = substitute(expand("%:p"),'\','/','g')
            let g:myfilename = substitute(g:myfilename, '/ ','\ ','g')
            " set org-mode to either auto-evaluate all exec blocks or evaluate none w/o
            " confirming each with yes/no
            if g:org_export_babel_evaluate == 1
                let g:mypart1 = '(let ((org-export-babel-evaluate t)(org-confirm-babel-evaluate nil)'
            else
                let g:mypart1 = '(let ((org-export-babel-evaluate nil)'
            endif
            let g:mypart1 .= '(buf (find-file \' . s:cmd_line_quote_fix . '"' . g:myfilename . '\' . s:cmd_line_quote_fix . '"))) (progn  (' 

            if item =~? 'g' 
                let g:mypart3 = ' ) (set-buffer buf) (not-modified) (kill-this-buffer) ))'
            else  
                let g:mypart3 = ' nil ) (set-buffer buf) (not-modified) (kill-this-buffer) ))'
            endif
            
            if item =~# 'F\|P\|E'
                let command_part2 = ' org-publish-' . mydict[key]
            elseif item == 'g'
                let command_part2 = ' org-babel-tangle'
            else
                let command_part2 = ' org-export-as-' . mydict[key]
            endif

            let orgcmd =  orgpath . s:cmd_line_quote_fix . '"' . g:mypart1 . command_part2 . g:mypart3 . s:cmd_line_quote_fix . '"'
            let g:orgcmd = orgcmd
            " execute the call out to emacs
            redraw
            echo "Export in progress. . . "
            if exists('*xolox#shell#execute')
                "silent! let g:expmsg = xolox#shell#execute(orgcmd . ' | cat ', 1)
                silent! call xolox#shell#execute(orgcmd , 1)
            else
                "execute '!' . orgcmd
                silent! execute '!' . orgcmd
            endif
            redraw
            echo "Export in progress. . . Export complete."
            break
        endif
    endfor
    if key ==# 't' 
        let template = [
                    \ '#+TITLE:     ' . expand("%p")
                    \ ,'#+AUTHOR:   '
                    \ ,'#+EMAIL:    '
                    \ ,'#+DATE:     ' . strftime("%Y %b %d %H:%M")
                    \ ,'#+DESCRIPTION: '
                    \ ,'#+KEYWORDS: '
                    \ ,'#+LANGUAGE:  en'
                    \ ,'#+OPTIONS:   H:3 num:t toc:t \n:nil @:t ::t |:t ^:t -:t f:t *:t <:t'
                    \ ,'#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc'
                    \ ,'#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js'
                    \ ,'#+EXPORT_SELECT_TAGS: export'
                    \ ,'#+EXPORT_EXCLUDE_TAGS: noexport'
                    \ ,'#+LINK_UP:   '
                    \ ,'#+LINK_HOME: '
                    \ ,'#+XSLT: '
                    \ ]
        silent call append(line('.')-1,template)
    elseif key =~# 'A\|N\|U'
        exec 'split ' . expand('%:r') . '.txt'
        normal gg
    endif

    let &more = save_more
    let &showcmd = save_showcmd

endfunction

function! s:MailLookup()
    Utl openlink https://mail.google.com/mail/?hl=en&shva=1#search/after:2010-10-24+before:2010-10-26
    "https://mail.google.com/mail/?hl=en&shva=1#search/after%3A2010-10-24+before%3A2010-10-26
endfunction
function! s:Union(list1, list2)
    " returns the union of two lists
    " (some algo ...)
    let rdict = {}
    for item in a:list1
            let rdict[item] = 1
    endfor
    for item in a:list2
            let rdict[item] = 1
    endfor
    return sort(keys(rdict))
endfunc 
function! s:Intersect(list1, list2)
    " returns the intersection of two lists
    " (some algo ...)
    " fro andy wokula on vim-use mailing list
    let rdict = {}
    for item in a:list1
        if has_key(rdict, item)
            let rdict[item] += 1
        else
            let rdict[item] = 1
        endif
    endfor
    for item in a:list2
        if has_key(rdict, item)
            let rdict[item] += 1
        else
            let rdict[item] = 1
        endif
    endfor
    call filter(rdict, 'v:val == 2')
    return sort(keys(rdict))
endfunc 

function! OrgSetEmphasis( emph_char ) range
    let emph_char = a:emph_char
    let my_mode = mode()
    if my_mode ==? 'v'
        exe 'normal oi' . emph_char 
        exe 'normal gvoi' . emph_char
    else
        exe 'normal i' . emph_char . emph_char
    endif
endfunction

function! s:OrgCustomTodoHighlights()
    if !exists('g:org_todo_custom_highlights')
        return
    endif
    for item in keys(g:org_todo_custom_highlights)
        let d = g:org_todo_custom_highlights
        if has('gui_running')
            let fg = get(d[item], 'guifg')
            let bg = get(d[item], 'guibg')
            exec 'hi! ' . item . ((fg>#'')  ? ' guifg=' . fg : '') . ((bg>#'') ? ' guibg=' . bg : '')
        else
            let fg = get(d[item], 'ctermfg')
            let bg = get(d[item], 'ctermfg')
            exec 'hi! ' . item . ((fg>#'')  ? ' ctermfg=' . fg : '') . ((bg>#'') ? ' ctermbg=' . bg : '')
        endif

        " xxxx todo put back in containedins, do synclears? check order?
        if bufname('%')=='__Agenda__'
            exec 'syntax match ' . item . ' ' .  '+ \*\+ \zs' . item . ' + containedin=AOL1,AOL2,AOL3,AOL4,AOL5,AOL6' 
            " containedin=AOL1'
        else
            exec 'syntax match ' . item . ' ' .  '+^.*\* \zs' . item . ' + containedin=OL1,OL2,OL3,OL4,OL5,OL6' 
        endif
        "call matchadd(item, '^.*\* \zs' . item . ' ')
    endfor
endfunction

function! OrgSetColors()
    " Set highlights for outline headings.  These are set from existing
    " highlights in a colorscheme:
    "  OL1 from Statement
    "  OL2 from Identifier
    "  OL3 from Constant
    "  OL4 from Comment
    "  OL5 from Special
    for pair in [ ['OL1','Statement'], ['OL2','Identifier'], ['OL3','Constant'],
            \     ['OL4','Comment'],   ['OL5','Special'] ]
        execute 'hi clear ' . pair[0]
        execute 'hi clear ' . pair[0] .'Folded'
        execute 'hi ' . pair[0] . ' ' . org#GetGroupHighlight( pair[1] )
        execute 'hi ' . pair[0] . 'Folded ' . org#GetGroupHighlight( pair[1] )
        execute 'hi ' . pair[0] . ' gui=NONE'
        execute 'hi ' . pair[0] . 'Folded gui=bold'
    endfor
    " set up highlights to use for headlines
    " involves create new set of highlights to 
    " correspond to OL1-OL5, but this time with bold flag
    " folded headlines use different highlights:
    " folded OL1 uses Folded
    " folded OL2 uses WarningMsg
    " folded OL3 uses WildMenu
    " folded OL4 uses DiffAdd
    " folded OL5 uses DiffChange
    "for pair in [ ['Folded','Statement'], ['WarningMsg','Identifier'], ['WildMenu','Constant'],
    "        \     ['DiffAdd','Comment'],   ['DiffChange','Special'] ]
    "    execute 'hi clear ' . pair[0]
    "    execute 'hi ' . pair[0] . ' ' . org#GetGroupHighlight( pair[1] )
    "    execute 'hi ' . pair[0] . ' gui=bold'
    "endfor

    "blank out foldcolumn
    if has('gui_running')
        hi! FoldColumn guifg=bg guibg=bg 
    else
        try
            hi! FoldColumn ctermfg=bg ctermbg=bg 
        catch
            hi! FoldColumn ctermfg=0 ctermbg=0
        endtry
    endif
    "ctermfg=bg ctermbg=bg
    "show text on SignColumn
    hi! SignColumn guibg=fg guibg=bg 
    "ctermfg=fg ctermbg=bg

    " various text item "highlightings" are below
    " change to suit your taste and put in OrgCustomColors() (see below)
    hi! Org_Drawer guifg=pink ctermfg=magenta
    hi! Org_Drawer_Folded guifg=pink ctermfg=magenta gui=bold cterm=bold
    hi! Org_Property_Value guifg=pink ctermfg=magenta
    hi! Org_Block guifg=#555555 ctermfg=magenta
    hi! Org_Src_Block guifg=#555555 ctermfg=magenta
    hi! Org_Table guifg=#888888 guibg=#333333 ctermfg=magenta
    hi! Org_Config_Line guifg=darkgray ctermfg=magenta
    hi! Org_Tag guifg=lightgreen ctermfg=blue
    hi! Org_Date guifg=magenta ctermfg=magenta gui=underline cterm=underline
    hi! Org_Star guifg=#444444 ctermfg=darkgray
    hi! Props guifg=#ffa0a0 ctermfg=gray
    hi! Org_Code guifg=darkgray gui=bold ctermfg=14
    hi! Org_Itals gui=italic guifg=#aaaaaa ctermfg=lightgray
    hi! Org_Bold gui=bold guifg=#aaaaaa ctermfg=lightgray
    hi! Org_Underline gui=underline guifg=#aaaaaa ctermfg=lightgray
    hi! Org_Lnumber guifg=#999999 ctermfg=gray

    if has("conceal")
        hi! default linkends guifg=blue ctermfg=blue
    endif
    hi! Org_Full_Link guifg=cyan gui=underline ctermfg=lightblue cterm=underline
    hi! Org_Half_Link guifg=cyan gui=underline ctermfg=lightblue cterm=underline
    highlight OrgColumnHeadings guibg=#444444 guifg=#aaaaaa gui=underline

    "hi! GENERICTODO guifg=pink ctermfg=lightred
    hi! DONETODO guifg=green ctermfg=green
    hi! NOTDONETODO guifg=red ctermfg=lightred


    "hi! default TODO guifg=orange guibg=NONE ctermfg=14 ctermbg=NONE
    "hi! default DONE guifg=green guibg=NONE ctermfg=green ctermbg=NONE

    "user can define OrgCustomColors() in vimrc for above items, these will be executed
    "here and override the defaults above.
    if exists('*OrgCustomColors')
        call OrgCustomColors()
    endif

    call s:OrgCustomTodoHighlights()

    " this for block and line after set highlights for headings in main
    " buffer when they're selected in Agenda.  Used in OrgFoldText().
    for i in range(1,5)
        let hlstring = org#GetGroupHighlight('OL' . i)
        if hlstring =~ 'guibg'
            let hlstring = substitute(hlstring,'guibg=\S+','guibg=#444444','')
        else
            let hlstring = hlstring . ' guibg=#444444'
        endif
        exec 'hi! Org_Chosen_Agenda_Heading' . i . ' ' . hlstring
    endfor
    hi! Org_Chosen_Agenda_Heading guibg=#444444 ctermbg=gray
endfunction
autocmd ColorScheme  * :silent! call OrgSetColors()
call OrgSetColors()

"Section for refile and archive funcs
function! OrgRefileDashboard()
    echohl MoreMsg
    echo " ================================"
    echo " Press key for a refile command:"
    echo " --------------------------------"
    echo " h   refile heading (including subtree) to point"
    echo " p   refile heading (including subtree) to point"
    echo " j   jump to refile point"
    echo " x   jump to persistent refile point"
    echo " s   set persistent refile point"
    echo " "
    echo " "
    echohl Question
    let key = nr2char(getchar())
    redraw
    if key ==? 'h'
        call OrgRefile(line('.'))
    elseif key ==? 'p'
        call OrgRefileToPermPoint(line('.'))
    elseif key ==? 'j'
        call OrgJumpToRefilePoint()
    elseif key ==? 'x'
        call OrgJumpToRefilePointPersistent()
    elseif key ==? 's'
        call OrgSetRefilePoint()
    else
        echo "No refile option selected."
    endif
    echohl None
endfunction

let g:org_heading_temp=['','','','','','','','']
function! OutlineHeads()
    let level = s:Ind(line('.'))
    let g:org_heading_temp[level-1] = matchstr(getline(line('.')),'^\*\+ \zs.*')
    " put level 1 head in result
    let result = expand("%:t") .  g:org_heading_temp[0]
    " now add full tree to level of current heading
    for item in g:org_heading_temp[1: level-1]
        let result .= '/' . item
    endfor
    return result 
endfunction
function! GetMyItems(arghead)
    let arghead = a:arghead
    let result = []
    if a:arghead[-1:] == '*'
        let arghead = arghead[:-2]
    endif
    let ilist = split(arghead,'/')
    call s:OrgSaveLocation()
    if expand("%:t") != ilist[0]
        "call s:LocateFile( '~\Desktop\org_files\' . ilist[0] )
        call s:LocateFile( fnamemodify(s:refile_file,":p:h:") . '/' . ilist[0] )
        if &ft != 'org'
            set ft=org
        endif
    endif
    if a:arghead[-1:] == '*'
        let tolevel = 3
    else
        let tolevel = len(ilist)
    endif
    exec 'g/^\*\{1,' . tolevel . '} /call add(result,OutlineHeads())'
    call s:OrgRestoreLocation()
    return result
endfunction
function! FileList(arghead,sd,gf)
    let arghead = substitute(a:arghead,'\~','\\\~','g')
    let s:myheads  = ['[current file]'] + copy(g:agenda_files)
    let matches = filter( copy( s:myheads ),'v:val =~ arghead')
    redraw!
    return join( matches, "\n" )
endfunction

function! HeadingList(arghead,sd,gf)
    let arghead = a:arghead
    let s:myheads = GetMyItems(arghead)
    let matches = filter( copy( s:myheads ),'v:val =~ a:arghead')
    redraw!
    return join( matches, "\n" )
endfunction

function! GetTarget()
    let orig_wildmode = &wildmode
    set wildmode=list:full
    try
        " need to modify getcmdline to strip back on bs
        cmap <c-BS> <C-\>egetcmdline()[-1:] == '/' ? matchstr(getcmdline()[:-2], '.*\ze/.*' ) . '/' : matchstr(getcmdline(), '.*\ze/.*') . '/'<CR>
        while 1
            let s:refile_file = ''
            let s:refile_file = input("Target file: ","[current file]",'custom,FileList')
            if s:refile_file ==# '[current file]'
                let s:refile_file = expand("%") 
            elseif index(s:myheads,s:refile_file) == -1
                break
            endif
            let heading = input('Outline heading: ', fnamemodify(s:refile_file,':t:') . "\t",'custom,HeadingList')
            if heading ==# ''
                let heading = ''
                continue
            else
                return [ s:refile_file, matchstr(heading,'.\{-}/\zs.*')]
            endif
        endwhile
    finally
        let &wildmode = orig_wildmode
        cunmap <c-BS>
    endtry
endfunction
function! OrgJumpToRefilePointPersistent()
    if exists('s:persistent_refile_point') && (len(s:persistent_refile_point)==2)
        call OrgGotoHeading( s:persistent_refile_point[0], s:persistent_refile_point[1] )
        normal zv
    else
        echo 'No persistent refile point assigned.'
    endif
endfunction
function! OrgJumpToRefilePoint()
    let my_refile_point = GetTarget()
    if len(my_refile_point)==2
        call OrgGotoHeading( my_refile_point[0], my_refile_point[1] )
        normal zv
    else
        echo "Jump aborted."
    endif
endfunction
function! OrgSetRefilePoint()
    let s:persistent_refile_point = GetTarget()
endfunction
function! OrgRefileToPermPoint(headline)
    if s:persistent_refile_point[1] !=# ''
        silent call DoRefile( s:persistent_refile_point, a:headline )
        redraw!
        echo "Heading and its subtree refiled to: \n" . s:persistent_refile_point[0] . '/' . s:persistent_refile_point[1]
    else
        echo 'Refile aborted.'
    endif
endfunction
function! OrgRefile(headline)
   " let head = (a:0 > 0) ? a:1 : line('.')
    let targ_list = GetTarget()
    if targ_list[1] !=# ''
        silent call DoRefile( targ_list, a:headline )
        redraw!
        echo "Heading and its subtree refiled to: \n" . targ_list[0] . '/' . targ_list[1]
    else
        echo 'Refile aborted.'
    endif
endfunction
function! ChangeLevel( text_lines, change_val )
    let mylines = split( a:text_lines, "\n")
    let change_val = a:change_val
    let i = 0
    while i < len(mylines)
        if mylines[i][0] == '*'
            if change_val > 0
                let mylines[i] = repeat('*',change_val) . mylines[i]
            else
                let abs_change = -(change_val)
                let mylines[i] = mylines[i][ abs_change :] 
            endif
        endif
        let i += 1
    endwhile
    return mylines
endfunction
function! DoRefile(targ_list,headline)

    let targ_list = a:targ_list
    let headline = a:headline
    call s:OrgSaveLocation()
    let refile_stars = s:Ind(headline) - 1
    silent execute headline . ',' . s:OrgSubtreeLastLine_l(headline) .  'delete x'
    call OrgGotoHeading(targ_list[0],targ_list[1])
    let target_stars = s:Ind(line('.')) " don't subtract 1 b/c refile will be subhead
    if refile_stars != target_stars
        let x = ChangeLevel( @x, target_stars - refile_stars )
    else
        let x = split( @x, "\n")
    endif
    if g:org_reverse_note_order
        exec (s:OrgNextHead() - 1)
    else
        exec s:OrgSubtreeLastLine()
    endif
    silent call append(line('.') , x)
    call s:OrgRestoreLocation()
endfunction
function! OrgGotoHeading(target_file, target_head, ...)
    call s:LocateFile( a:target_file )
    normal gg
    let head_list = split(a:target_head,'/')
    call search( '^\* ' . head_list[0], 'c', '')
    let heading_line = line('.')
    let last_subline = s:OrgSubtreeLastLine_l(line('.'))
    let i = 1
    while i < len(head_list)
        let stars = repeat('\*', i + 1)
        call search( '^' . stars . ' ' . head_list[i], '', last_subline)
        let i += 1
    endwhile
endfunction 
    

command! PreLoadTags :silent  call <SID>GlobalConvertTags()
command! PreWriteTags :silent call <SID>GlobalUnconvertTags(changenr())
command! PostWriteTags :silent call <SID>UndoUnconvertTags()
au BufRead *.org :PreLoadTags
au BufWrite *.org :PreWriteTags
au BufWritePost *.org :PostWriteTags

setlocal fillchars=|, 

"Section Narrow Region
let g:nrrw_rgn_vert=1
let g:nrrw_custom_options={'wrap':0}
command! -buffer Narrow :call NarrowCodeBlock(line('.'))

function! NarrowCodeBlock(line)
    if exists(":NarrowRegion") == 0
       echo "The Vim plugin NrrwRgn.vim must be installed for"
        echo "narrowing to work.  You can find a copy at:"
        echo 'http://www.vim.org/scripts/script.php?script_id=3075'
        return
    endif
    " function first tests if inside src block, and if so
    " narrows the code block.  If not, then
    " tests for headline and narrows the heading subtree
    " save buf vars to put in new org buffer if subtree narrowing
    let main_buf_vars = b:v
    execute a:line
    call search('^#+begin_src','b','')
    let start=line('.') + 1
    let language = matchstr(getline(line('.')), '^#+begin_src \zs\S\+')
    if language == 'emacs-lisp'
        let language = 'lisp'
    endif
    call search('^#+end_src','','')
    let end=line('.') - 1

    let start_width = winwidth(0)
    let &winwidth = winwidth(0) / 3
    if (start <= a:line) && (end >= a:line)
        execute start ',' . end . 'call nrrwrgn#NrrwRgn()'
        if filereadable($VIMRUNTIME . '/ftplugin/' . language . '.vim')
            execute 'set ft=' . language
        endif
        if filereadable($VIMRUNTIME. '/syntax/' . language . '.vim')
            execute 'set syntax=' . language
        endif

        let &winwidth=start_width*2/3
    else
        let start = s:OrgGetHead_l(a:line)
        if start > 0 
            let end = s:OrgSubtreeLastLine_l(start)
            execute start . ',' . end . 'call nrrwrgn#NrrwRgn()'
            " then set ftype in new buffer
            set ft=org
            let b:v = main_buf_vars
            let &winwidth=start_width*2/3
        else
            execute a:line
            echo "You're not in a source code block or an outline heading."
        endif
    endif
endfunction
" Org Menu Entries
amenu &Org.&View.Entire\ &Document.To\ Level\ &1<tab>,1 :set foldlevel=1<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &2<tab>,2 :set foldlevel=2<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &3<tab>,3 :set foldlevel=3<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &4<tab>,4 :set foldlevel=4<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &5<tab>,5 :set foldlevel=5<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &6<tab>,6 :set foldlevel=6<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &7<tab>,7 :set foldlevel=7<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &8<tab>,8 :set foldlevel=8<cr>
amenu &Org.&View.Entire\ &Document.To\ Level\ &9<tab>,9 :set foldlevel=9<cr>
amenu &Org.&View.Entire\ &Document.Expand\ Level\ &All :set foldlevel=99999<cr>
amenu &Org.&View.&Subtree.To\ Level\ &1<tab>,,1 :silent call OrgShowSubs(1,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &2<tab>,,2 :silent call OrgShowSubs(2,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &3<tab>,,3 :silent call OrgShowSubs(3,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &4<tab>,,4 :silent call OrgShowSubs(4,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &5<tab>,,5 :silent call OrgShowSubs(5,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &6<tab>,,6 :silent call OrgShowSubs(6,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &7<tab>,,7 :silent call OrgShowSubs(7,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &8<tab>,,8 :silent call OrgShowSubs(8,0)<cr>
amenu &Org.&View.&Subtree.To\ Level\ &9\ \ \ \ \ \ <tab>,,9 :silent call OrgShowSubs(9,0)cr>
amenu &Org.-Sep1- :
amenu &Org.&New\ Heading.New\ Head\ Same\ Level<tab><cr>(or\ <s-cr>) :call OrgNewHead('same')<cr>
amenu &Org.&New\ Heading.New\ Subhead<tab><c-cr> :call OrgNewHead('leveldown')<cr>
amenu &Org.&New\ Heading.New\ Head\ Parent\ Level<tab><s-c-cr> :call OrgNewHead('levelup')<cr>
amenu &Org.&Navigate\ Headings.&Up\ to\ Parent\ Heading<tab><a-left> :exec <SID>OrgParentHead()<cr>
amenu &Org.&Navigate\ Headings.&First\ Child\ Heading<tab><a-right> :exec <SID>OrgFirstChildHead()<cr>
amenu &Org.&Navigate\ Headings.&Last\ Child\ Heading :exec <SID>OrgLastChildHead()<cr>
amenu &Org.&Navigate\ Headings.&Next\ Heading :exec <SID>OrgNextHead()<cr>
amenu &Org.&Navigate\ Headings.&Previous\ Heading :exec <SID>OrgPrevHead()<cr>
amenu &Org.&Navigate\ Headings.Next\ &Same\ Level :exec <SID>OrgNextHeadSameLevel()<cr>
amenu &Org.&Navigate\ Headings.Previous\ Same\ Level :exec <SID>OrgPrevHeadSameLevel()<cr>
amenu &Org.&Navigate\ Headings.Next\ &Sibling<tab><a-down> :exec <SID>OrgNextSiblingHead()<cr>
amenu &Org.&Navigate\ Headings.Previous\ Sibling<tab><a-up> :exec <SID>OrgPrevSiblingHead()<cr>
amenu &Org.Edit\ &Structure.Move\ Subtree\ &Up<tab><c-a-up> :call OrgMoveLevel(line('.'),'up')<cr>
amenu &Org.Edit\ &Structure.Move\ Subtree\ &Down<tab><c-a-down> :call OrgMoveLevel(line('.'),'down')<cr>
amenu &Org.Edit\ &Structure.&Promote\ Subtree<tab><c-a-left> :call OrgMoveLevel(line('.'),'left')<cr>
amenu &Org.Edit\ &Structure.&Demote\ Subtree<tab><c-a-right> :call OrgMoveLevel(line('.'),'right')<cr>
vmenu &Org.&Editing.&Bold\ (*)<tab>,cb              "zdi*<C-R>z*<ESC>l
vmenu &Org.&Editing.&Italic\ (/)<tab>,ci            "zdi/<C-R>z/<ESC>l
vmenu &Org.&Editing.&Underline\ (_)<tab>,cu         "zdi_<C-R>z_<ESC>l
vmenu &Org.&Editing.&Code\ (=)<tab>,cc              "zdi=<C-R>z=<ESC>l
amenu &Org.&Editing.-Sep22- :
amenu &Org.&Editing.&Narrow<tab>,na :silent call NarrowCodeBlock(line('.'))<cr>
"amenu &Org.&Editing.Narrow\ &Codeblock<tab>,nc :silent call NarrowCodeBlock(line('.'))<cr>
"amenu &Org.&Editing.Narrow\ Outline\ &Subtree<tab>,ns :silent call NarrowOutline(line('.'))<cr>
amenu &Org.&Refile.&Refile\ to\ Point<tab>,rh :call OrgRefile(line('.'))<cr>
amenu &Org.&Refile.&Jump\ to\ Point<tab>,rj :call OrgJumpToRefilePoint()<cr>
amenu &Org.&Refile.&Jump\ to\ Persistent\ Point<tab>,rx :call OrgJumpToRefilePointPersistent()<cr>
amenu &Org.&Refile.&Jump\ to\ Point<tab>,rj :call OrgJumpToRefilePoint()<cr>
amenu &Org.&Refile.&Set\ Persistent\ Refile\ Point<tab>,rs :call OrgSetRefilePoint()<cr>
amenu &Org.&Refile.Refile\ to\ Persistent\ Point<tab>,rp :call OrgRefileToPermPoint(line('.'))<cr>
amenu &Org.-Sep2- :
amenu &Org.&Columns\ Menu :call OrgColumnsDashboard()<cr>
amenu &Org.&Hyperlinks.Add/&edit\ link<tab>,le :call EditLink()<cr>
amenu &Org.&Hyperlinks.&Follow\ link<tab>,lf :call FollowLink(OrgGetLink())<cr>
amenu &Org.&Hyperlinks.&Next\ link<tab>,ln :/]]<cr>
amenu &Org.&Hyperlinks.&Previous\ link<tab>,lp :?]]<cr>
amenu &Org.&Hyperlinks.Perma-compre&ss\ links<tab>,lc :set conceallevel=3\|set concealcursor=nc<cr>
amenu &Org.&Hyperlinks.&Autocompress\ links<tab>,la :set conceallevel=3\|set concealcursor=c<cr>
amenu &Org.&Hyperlinks.No\ auto&compress\ links<tab>,lx :set conceallevel=0<cr>
amenu &Org.&Table.$Table\ Dashboard<tab>,b :call OrgTableDashboard()<cr>
amenu &Org.&Table.E$valuate\ Table<tab>,v :call OrgTableDashboard()<cr>
amenu &Org.-Sep3- :
amenu <silent> &Org.TODO\ &Dashboard<tab>,t :call OrgTodoDashboard()<CR>
amenu <silent> &Org.TODO\ &Cycle<tab><s-cr> :call <SID>ReplaceTodo()<CR>
"amenu <silent> &Org.TODO\ Cycle\ &Backward<tab><c-s-cr> :call <SID>ReplaceTodo('todo-bkwd')<CR>
amenu &Org.Edit\ TA&GS<tab>,et  :call OrgTagsEdit()<cr>
amenu &Org.&Dates\ and\ Scheduling.Add/Edit\ &Deadline<tab>,dd :call OrgDateEdit('DEADLINE')<cr>
amenu &Org.&Dates\ and\ Scheduling.Add/Edit\ &Scheduled<tab>,ds :call OrgDateEdit('SCHEDULED')<cr>
amenu &Org.&Dates\ and\ Scheduling.Add/Edit\ &Closed<tab>,dc :call OrgDateEdit('CLOSED')<cr>
amenu &Org.&Dates\ and\ Scheduling.Add/Edit\ &Timestamp<tab>,dt :call OrgDateEdit('TIMESTAMP')<cr>
amenu &Org.&Dates\ and\ Scheduling.Add/Edit\ &GenericDate<tab>,dg :call OrgGenericDateEdit()<cr>
amenu &Org.&Logging\ work.Clock\ in<tab>,ci :call OrgClockIn(line('.'))<cr>
amenu &Org.&Logging\ work.Clock\ out<tab>,co :call OrgClockOut()<cr>
amenu &Org.-Sep4- :
amenu &Org.Agenda\ command<tab>,ag :call OrgAgendaDashboard()<cr>
amenu <silent> &Org.&Do\ Emacs\ Eval<tab>,v :call OrgEval()<cr>
amenu &Org.File\ &List\ for\ Agenda :call EditAgendaFiles()<cr>
amenu &Org.Special\ &views\ current\ file :call OrgCustomSearchMenu()<cr>
amenu &Org.-Sep5- :
amenu &Org.Narro&w.Outline\ &Subtree<tab>,ns :call NarrowOutline(line('.'))<cr>
amenu &Org.Narro&w.&Code\ Block<tab>,nc :call NarrowCodeBlock(line('.'))<cr>
amenu &Org.-Sep6- :
amenu &Org.Export/Publish\ w/Emacs :call OrgExportDashboard()<cr>

"*********************************************************************
"*********************************************************************
"  'endif' below is special 'endif' closing the 'if !exists(org_loaded)
"  line near top of file.  Thus the main functions are loaded
"  only once for all buffers, with settngs at begin of file
"  and mappings below this line executed for each buffer 
"  having org filetype
"*********************************************************************
"*********************************************************************
endif
let g:org_loaded=1
let b:v.org_loaded=1
"*********************************************************************
"*********************************************************************
"*********************************************************************
"*********************************************************************
" convert to VimOrganizer tag format and add colon (:) before dates
PreLoadTags
" below is default todo setup, anything different can be done
" in vimrc (or in future using a config line in the org file itself)
if !exists('g:in_agenda_search') && ( &foldmethod!= 'expr') && !exists('b:v.bufloaded')
    setlocal foldmethod=expr
    "setlocal foldexpr=OrgFoldLevel(v:lnum)
    set foldlevel=1
    let b:v.bufloaded=1
else
    setlocal foldmethod=manual
endif
"if !exists('b:v.todoitems')
"    call OrgTodoSetup('TODO | DONE')
"endif
if !exists('g:org_todo_setup')
    let g:org_todo_setup = 'TODO | DONE'
endif
if !exists('g:org_tag_setup')
    let g:org_tag_setup = '{home(h) work(w)}'
endif

call OrgProcessConfigLines()
exec "syntax match DONETODO '" . b:v.todoDoneMatch . "' containedin=OL1,OL2,OL3,OL4,OL5,OL6" 
exec "syntax match NOTDONETODO '" . b:v.todoNotDoneMatch . "' containedin=OL1,OL2,OL3,OL4,OL5,OL6" 

"Menu stuff
function! MenuCycle()
    if foldclosed(line('.')) > -1
        exec foldclosed(line('.'))
    else
        exec s:OrgGetHead()
    endif
    call OrgCycle()
endfunction

nmap <silent> <buffer> <localleader>t    :call OrgTodoDashboard()<CR>
nmap <silent> <buffer> <s-CR>    :call <SID>ReplaceTodo()<CR>
" c-s-cr already taken
"nmap <silent> <buffer> <c-s-CR>    :call <SID>ReplaceTodo('todo-bkwd')<CR>
if !has('gui_running')
    nmap <silent> <buffer> <localleader>nt   :call <SID>ReplaceTodo()<CR>
endif
execute "source " . expand("<sfile>:p:h") . '/vimorg-main-mappings.vim'

" below is autocmd to change tw for lines that have comments on them
" I think this should go in vimrc so i runs for each buffer load
"  :autocmd CursorMoved,CursorMovedI * :if match(getline(line(".")), '^*\*\s') == 0 | :setlocal textwidth=99 | :else | :setlocal textwidth=79 | :endif 
set com=sO::\ -,mO::\ \ ,eO:::,::,sO:>\ -,mO:>\ \ ,eO:>>,:>
set fo=qtcwn
let b:v.current_syntax = "org"
setlocal foldtext=OrgFoldText()


" vim600: set tabstop=4 shiftwidth=4 smarttab expandtab fdm=expr foldexpr=getline(v\:lnum)=~'^"Section'?0\:getline(v\:lnum)=~'^func'?1\:2:
