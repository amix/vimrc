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

if exists("g:org_autoload_funcs")
	finish
endif

let g:org_autoload_funcs=1

function! org#SetOrgFileType()
        "if expand("%:e") == 'org'
                if &filetype != 'org'
                        execute "set filetype=org"
			
"			if !exists('g:org_todo_setup')
"				let g:org_todo_setup = 'TODO | DONE'
"			endif
"			if !exists('g:org_tag_setup')
"				let g:org_tag_setup = '{home(h) work(w)}'
"			endif
"			
"			call OrgProcessConfigLines()
"			exec "syntax match DONETODO '" . b:v.todoDoneMatch . "' containedin=OL1,OL2,OL3,OL4,OL5,OL6" 
"			exec "syntax match NOTDONETODO '" . b:v.todoNotDoneMatch . "' containedin=OL1,OL2,OL3,OL4,OL5,OL6" 
		
                endif
        "endif
endfunction     

function! org#Pad(s,amt)
    return a:s . repeat(' ',a:amt - len(a:s))
endfunction

function! org#Timestamp()
    return strftime("%Y-%m-%d %a %H:%M")
endfunction

function! org#GetGroupHighlight(group)
    " this code was copied and modified from code posted on StackOverflow
    " http://stackoverflow.com/questions/1331213/how-to-modify-existing-highlight-group-in-vim
    " Redirect the output of the "hi" command into a variable
    " and find the highlighting
    redir => GroupDetails
    exe "silent hi " . a:group
    redir END

    " Resolve linked groups to find the root highlighting scheme
    while GroupDetails =~ "links to"
        let index = stridx(GroupDetails, "links to") + len("links to")
        let LinkedGroup =  strpart(GroupDetails, index + 1)
        redir => GroupDetails
        exe "silent hi " . LinkedGroup
        redir END
    endwhile

    " Extract the highlighting details (the bit after "xxx")
    let MatchGroups = matchlist(GroupDetails, '\<xxx\>\s\+\(.*\)')
    let ExistingHighlight = MatchGroups[1]

    return ExistingHighlight

endfunction

