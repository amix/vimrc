" Window opens the location list with the given height up to 10 lines maximum.
" Otherwise g:go_loclist_height is used. If no or zero height is given it
" closes the window
function! go#list#Window(...)
    " we don't use lwindow to close the location list as we need also the
    " ability to resize the window. So, we are going to use lopen and cclose
    " for a better user experience. If the number of errors in a current
    " location list increases/decreases, cwindow will not resize when a new
    " updated height is passed. lopen in the other hand resizes the screen.
    if !a:0 || a:1 == 0
        lclose
        return
    endif

    let height = get(g:, "go_loclist_height", 0)
    if height == 0
        " prevent creating a large location height for a large set of numbers
        if a:1 > 10
            let height = 10
        else
            let height = a:1
        endif
    endif

    exe 'lopen '. height
endfunction


" Get returns the current list of items from the location list
function! go#list#Get()
  return getloclist(0)
endfunction

" Populate populate the location list with the given items
function! go#list#Populate(items)
	call setloclist(0, a:items, 'r')
endfunction

function! go#list#PopulateWin(winnr, items)
	call setloclist(a:winnr, a:items, 'r')
endfunction

" Parse parses the given items based on the specified errorformat nad
" populates the location list.
function! go#list#ParseFormat(errformat, items)
  " backup users errorformat, will be restored once we are finished
  let old_errorformat = &errorformat

	" parse and populate the location list
  let &errorformat = a:errformat
	lgetexpr a:items

	"restore back
  let &errorformat = old_errorformat
endfunction

" Parse parses the given items based on the global errorformat nad
" populates the location list.
function! go#list#Parse(items)
	lgetexpr a:items
endfunction

" JumpToFirst jumps to the first item in the location list
function! go#list#JumpToFirst()
  ll 1 
endfunction

" Clean cleans the location list
function! go#list#Clean()
	lex []
endfunction
