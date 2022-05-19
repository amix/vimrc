"=============================================================================
"    Copyright: Copyright (c) 2001-2014, Jeff Lanzarotta
"               All rights reserved.
"
"               Redistribution and use in source and binary forms, with or
"               without modification, are permitted provided that the
"               following conditions are met:
"
"               * Redistributions of source code must retain the above
"                 copyright notice, this list of conditions and the following
"                 disclaimer.
"
"               * Redistributions in binary form must reproduce the above
"                 copyright notice, this list of conditions and the following
"                 disclaimer in the documentation and/or other materials
"                 provided with the distribution.
"
"               * Neither the name of the {organization} nor the names of its
"                 contributors may be used to endorse or promote products
"                 derived from this software without specific prior written
"                 permission.
"
"               THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
"               CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
"               INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
"               MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
"               DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
"               CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
"               SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
"               NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
"               LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
"               HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
"               CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
"               OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
"               EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" Name Of File: bufexplorer.vim
"  Description: Buffer Explorer Vim Plugin
"   Maintainer: Jeff Lanzarotta (delux256-vim at yahoo dot com)
" Last Changed: Monday, 03 November 2014
"      Version: See g:bufexplorer_version for version number.
"        Usage: This file should reside in the plugin directory and be
"               automatically sourced.
"
"               You may use the default keymappings of
"
"                 <Leader>be  - Opens BufExplorer
"                 <Leader>bs  - Opens horizontally split window BufExplorer
"                 <Leader>bv  - Opens vertically split window BufExplorer
"
"               Or you can override the defaults and define your own mapping
"               in your vimrc file, for example:
"
"                   nnoremap <silent> <F11> :BufExplorer<CR>
"                   nnoremap <silent> <m-F11> :BufExplorerHorizontalSplit<CR>
"                   nnoremap <silent> <c-F11> :BufExplorerVerticalSplit<CR>
"
"               Or you can use
"
"                 ":BufExplorer"                - Opens BufExplorer
"                 ":BufExplorerHorizontalSplit" - Opens horizontally window BufExplorer
"                 ":BufExplorerVerticalSplit"   - Opens vertically split window BufExplorer
"
"               For more help see supplied documentation.
"      History: See supplied documentation.
"=============================================================================

" Plugin Code {{{1
" Exit quickly if already running or when 'compatible' is set. {{{2
if exists("g:bufexplorer_version") || &cp
    finish
endif
"2}}}

" Version number
let g:bufexplorer_version = "7.4.6"

" Check for Vim version {{{2
if v:version < 700
    echohl WarningMsg
    echo "Sorry, bufexplorer ".g:bufexplorer_version." required Vim 7.0 and greater."
    echohl None
    finish
endif

" Create commands {{{2
command! BufExplorer :call BufExplorer()
command! BufExplorerHorizontalSplit :call BufExplorerHorizontalSplit()
command! BufExplorerVerticalSplit :call BufExplorerVerticalSplit()

" Set {{{2
function! s:Set(var, default)
    if !exists(a:var)
        if type(a:default)
            execute "let" a:var "=" string(a:default)
        else
            execute "let" a:var "=" a:default
        endif

        return 1
    endif

    return 0
endfunction

" Script variables {{{2
let s:MRU_Exclude_List = ["[BufExplorer]","__MRU_Files__"]
let s:MRUList = []
let s:name = '[BufExplorer]'
let s:originBuffer = 0
let s:running = 0
let s:sort_by = ["number", "name", "fullpath", "mru", "extension"]
let s:splitMode = ""
let s:tabSpace = []
let s:types = {"fullname": ':p', "path": ':p:h', "relativename": ':~:.', "relativepath": ':~:.:h', "shortname": ':t'}

" Setup the autocommands that handle the MRUList and other stuff. {{{2
autocmd VimEnter * call s:Setup()

" Setup {{{2
function! s:Setup()
    call s:Reset()

    " Now that the MRUList is created, add the other autocmds.
    augroup BufExplorer
        autocmd!
        autocmd BufEnter,BufNew * call s:ActivateBuffer()
        autocmd BufWipeOut * call s:DeactivateBuffer(1)
        autocmd BufDelete * call s:DeactivateBuffer(0)
        autocmd BufWinEnter \[BufExplorer\] call s:Initialize()
        autocmd BufWinLeave \[BufExplorer\] call s:Cleanup()
        autocmd TabEnter * call s:TabEnter()
        autocmd SessionLoadPost * call s:Reset()
    augroup END
endfunction

" Reset {{{2
function! s:Reset()
    " Build initial MRUList. This makes sure all the files specified on the
    " command line are picked up correctly.
    let s:MRUList = range(1, bufnr('$'))

    " Initialize one tab space array, ignore zero-based tabpagenr since all
    " tabpagenr's start at 1.  -1 signifies this is the first time we are
    " referencing this tabpagenr.
    "
    " If Vim has been loaded with mksession, then it is possible for more tabs
    " to exist.  So use tabpagenr() to determine how large to make the array. If
    " there are 4 tabs, there should be 5 elements in this array.
    "
    " Each element will hold a CSV list of buffers viewed in that tab.  So on
    " the 3rd tab, if there user has viewed 4 different buffers in that tab, the
    " value would be:
    "    echo s:tabSpace[3]
    "    [4, 9, 1, 10]
    "    echo s:tabSpace
    "    [[-1], [-1], [-1], [4, 9, 1, 10], [-1]]
    let s:tabSpace = []
    let i = 0

    while(tabpagenr('$') > 0 && i <= tabpagenr('$'))
        call add(s:tabSpace, [-1])
        let i = i + 1
    endwhile
endfunction

" ActivateBuffer {{{2
function! s:ActivateBuffer()
    " Verify the current tabpage exists in the
    " current s:tabSpace array.  This can be missing
    " entries when restoring sessions.
    let i = 0
    while( tabpagenr('$') > 0 && i <= tabpagenr() )
        " Number:     0
        " String:     1
        " Funcref:    2
        " List:       3
        " Dictionary: 4
        " Float:      5
        if type(get(s:tabSpace, i)) == 0
            call add(s:tabSpace, [-1])
        endif

        let i = i + 1
    endwhile

    let _bufnr = bufnr("%")
    let list = get(s:tabSpace, tabpagenr(), [-1])

    if !empty(list) && list[0] == '-1'
        " The first time we add a tab, Vim uses the current buffer
        " as it's starting page.  Even though we are about to
        " edit a new page (BufEnter is triggered after), so
        " remove the -1 entry indicating we have covered this case.
        let list = []
        call add(list, _bufnr)
        let s:tabSpace[tabpagenr()] = list
    elseif empty(list) || index(list, _bufnr) == -1
        " Add new buffer to this tab's buffer list.
        call add(list, _bufnr)
        let s:tabSpace[tabpagenr()] = list

        if g:bufExplorerOnlyOneTab == 1
            " If a buffer can only be available in 1 tab page ensure this
            " buffer is not present in any other tabs
            let tabidx = 1
            while tabidx < len(s:tabSpace)
                if tabidx != tabpagenr()
                    let bufidx = index(s:tabSpace[tabidx], _bufnr)
                    if bufidx != -1
                        call remove(s:tabSpace[tabidx], bufidx)
                    endif
                endif
                let tabidx = tabidx + 1
            endwhile
        endif
    endif

    call s:MRUPush(_bufnr)
endfunction

" DeactivateBuffer {{{2
function! s:DeactivateBuffer(remove)
    let _bufnr = str2nr(expand("<abuf>"))
    call s:MRUPop(_bufnr)
endfunction

" TabEnter {{{2
function! s:TabEnter()
    " Make s:tabSpace 1-based
    if empty(s:tabSpace) || len(s:tabSpace) < (tabpagenr() + 1)
        call add(s:tabSpace, [-1])
    endif
endfunction

" MRUPop {{{2
function! s:MRUPop(bufnr)
    call filter(s:MRUList, 'v:val != '.a:bufnr)
endfunction

" MRUPush {{{2
function! s:MRUPush(buf)
    " Skip temporary buffer with buftype set. Don't add the BufExplorer window
    " to the list.
    if s:ShouldIgnore(a:buf) == 1
        return
    endif

    " Remove the buffer number from the list if it already exists.
    call s:MRUPop(a:buf)

    " Add the buffer number to the head of the list.
    call insert(s:MRUList, a:buf)
endfunction

" ShouldIgnore {{{2
function! s:ShouldIgnore(buf)
    " Ignore temporary buffers with buftype set.
    if empty(getbufvar(a:buf, "&buftype") == 0)
        return 1
    endif

    " Ignore buffers with no name.
    if empty(bufname(a:buf)) == 1
        return 1
    endif

    " Ignore the BufExplorer buffer.
    if fnamemodify(bufname(a:buf), ":t") == s:name
        return 1
    endif

    " Ignore any buffers in the exclude list.
    if index(s:MRU_Exclude_List, bufname(a:buf)) >= 0
        return 1
    endif

    " Else return 0 to indicate that the buffer was not ignored.
    return 0
endfunction

" Initialize {{{2
function! s:Initialize()
    let s:_insertmode = &insertmode
    set noinsertmode

    let s:_showcmd = &showcmd
    set noshowcmd

    let s:_cpo = &cpo
    set cpo&vim

    let s:_report = &report
    let &report = 10000

    setlocal nonumber
    setlocal foldcolumn=0
    setlocal nofoldenable
    setlocal cursorline
    setlocal nospell

    setlocal nobuflisted

    let s:running = 1
endfunction

" Cleanup {{{2
function! s:Cleanup()
    if exists("s:_insertmode")
        let &insertmode = s:_insertmode
    endif

    if exists("s:_showcmd")
        let &showcmd = s:_showcmd
    endif

    if exists("s:_cpo")
        let &cpo = s:_cpo
    endif

    if exists("s:_report")
        let &report = s:_report
    endif

    let s:running = 0
    let s:splitMode = ""

    delmarks!
endfunction

" BufExplorerHorizontalSplit {{{2
function! BufExplorerHorizontalSplit()
    let s:splitMode = "sp"
    execute "BufExplorer"
endfunction

" BufExplorerVerticalSplit {{{2
function! BufExplorerVerticalSplit()
    let s:splitMode = "vsp"
    execute "BufExplorer"
endfunction

" BufExplorer {{{2
function! BufExplorer()
    let name = s:name

    if !has("win32")
        " On non-Windows boxes, escape the name so that is shows up correctly.
        let name = escape(name, "[]")
    endif

    " Make sure there is only one explorer open at a time.
    if s:running == 1
        " Go to the open buffer.
        if has("gui")
            execute "drop" name
        endif

        return
    endif

    " Add zero to ensure the variable is treated as a number.
    let s:originBuffer = bufnr("%") + 0

    silent let s:raw_buffer_listing = s:GetBufferInfo(0)

    " We may have to split the current window.
    if s:splitMode != ""
        " Save off the original settings.
        let [_splitbelow, _splitright] = [&splitbelow, &splitright]

        " Set the setting to ours.
        let [&splitbelow, &splitright] = [g:bufExplorerSplitBelow, g:bufExplorerSplitRight]
        let _size = (s:splitMode == "sp") ? g:bufExplorerSplitHorzSize : g:bufExplorerSplitVertSize

        " Split the window either horizontally or vertically.
        if _size <= 0
            execute 'keepalt ' . s:splitMode
        else
            execute 'keepalt ' . _size . s:splitMode
        endif

        " Restore the original settings.
        let [&splitbelow, &splitright] = [_splitbelow, _splitright]
    endif

    if !exists("b:displayMode") || b:displayMode != "winmanager"
        " Do not use keepalt when opening bufexplorer to allow the buffer that
        " we are leaving to become the new alternate buffer
        execute "silent keepjumps hide edit".name
    endif

    call s:DisplayBufferList()

    " Position the cursor in the newly displayed list on the line representing
    " the active buffer.  The active buffer is the line with the '%' character
    " in it.
    execute search("%")
endfunction

" DisplayBufferList {{{2
function! s:DisplayBufferList()
    " Do not set bufhidden since it wipes out the data if we switch away from
    " the buffer using CTRL-^.
    setlocal buftype=nofile
    setlocal modifiable
    setlocal noswapfile
    setlocal nowrap

    call s:SetupSyntax()
    call s:MapKeys()

    " Wipe out any existing lines in case BufExplorer buffer exists and the
    " user had changed any global settings that might reduce the number of
    " lines needed in the buffer.
    silent keepjumps 1,$d _

    call setline(1, s:CreateHelp())
    call s:BuildBufferList()
    call cursor(s:firstBufferLine, 1)

    if !g:bufExplorerResize
        normal! zz
    endif

    setlocal nomodifiable
endfunction

" MapKeys {{{2
function! s:MapKeys()
    if exists("b:displayMode") && b:displayMode == "winmanager"
        nnoremap <buffer> <silent> <tab> :call <SID>SelectBuffer()<CR>
    endif

    nnoremap <script> <silent> <buffer> <2-leftmouse> :call <SID>SelectBuffer()<CR>
    nnoremap <script> <silent> <buffer> <CR>          :call <SID>SelectBuffer()<CR>
    nnoremap <script> <silent> <buffer> <F1>          :call <SID>ToggleHelp()<CR>
    nnoremap <script> <silent> <buffer> <s-cr>        :call <SID>SelectBuffer("tab")<CR>
    nnoremap <script> <silent> <buffer> B             :call <SID>ToggleOnlyOneTab()<CR>
    nnoremap <script> <silent> <buffer> b             :call <SID>SelectBuffer("ask")<CR>
    nnoremap <script> <silent> <buffer> d             :call <SID>RemoveBuffer("delete")<CR>
    xnoremap <script> <silent> <buffer> d             :call <SID>RemoveBuffer("delete")<CR>
    nnoremap <script> <silent> <buffer> D             :call <SID>RemoveBuffer("wipe")<CR>
    xnoremap <script> <silent> <buffer> D             :call <SID>RemoveBuffer("wipe")<CR>
    nnoremap <script> <silent> <buffer> f             :call <SID>ToggleFindActive()<CR>
    nnoremap <script> <silent> <buffer> m             :call <SID>MRUListShow()<CR>
    nnoremap <script> <silent> <buffer> o             :call <SID>SelectBuffer()<CR>
    nnoremap <script> <silent> <buffer> p             :call <SID>ToggleSplitOutPathName()<CR>
    nnoremap <script> <silent> <buffer> q             :call <SID>Close()<CR>
    nnoremap <script> <silent> <buffer> r             :call <SID>SortReverse()<CR>
    nnoremap <script> <silent> <buffer> R             :call <SID>ToggleShowRelativePath()<CR>
    nnoremap <script> <silent> <buffer> s             :call <SID>SortSelect()<CR>
    nnoremap <script> <silent> <buffer> S             :call <SID>ReverseSortSelect()<CR>
    nnoremap <script> <silent> <buffer> t             :call <SID>SelectBuffer("tab")<CR>
    nnoremap <script> <silent> <buffer> T             :call <SID>ToggleShowTabBuffer()<CR>
    nnoremap <script> <silent> <buffer> u             :call <SID>ToggleShowUnlisted()<CR>

    for k in ["G", "n", "N", "L", "M", "H"]
        execute "nnoremap <buffer> <silent>" k ":keepjumps normal!" k."<CR>"
    endfor
endfunction

" SetupSyntax {{{2
function! s:SetupSyntax()
    if has("syntax")
        syn match bufExplorerHelp     "^\".*" contains=bufExplorerSortBy,bufExplorerMapping,bufExplorerTitle,bufExplorerSortType,bufExplorerToggleSplit,bufExplorerToggleOpen
        syn match bufExplorerOpenIn   "Open in \w\+ window" contained
        syn match bufExplorerSplit    "\w\+ split" contained
        syn match bufExplorerSortBy   "Sorted by .*" contained contains=bufExplorerOpenIn,bufExplorerSplit
        syn match bufExplorerMapping  "\" \zs.\+\ze :" contained
        syn match bufExplorerTitle    "Buffer Explorer.*" contained
        syn match bufExplorerSortType "'\w\{-}'" contained
        syn match bufExplorerBufNbr   /^\s*\d\+/
        syn match bufExplorerToggleSplit  "toggle split type" contained
        syn match bufExplorerToggleOpen   "toggle open mode" contained

        syn match bufExplorerModBuf    /^\s*\d\+.\{4}+.*/
        syn match bufExplorerLockedBuf /^\s*\d\+.\{3}[\-=].*/
        syn match bufExplorerHidBuf    /^\s*\d\+.\{2}h.*/
        syn match bufExplorerActBuf    /^\s*\d\+.\{2}a.*/
        syn match bufExplorerCurBuf    /^\s*\d\+.%.*/
        syn match bufExplorerAltBuf    /^\s*\d\+.#.*/
        syn match bufExplorerUnlBuf    /^\s*\d\+u.*/
        syn match bufExplorerInactBuf  /^\s*\d\+ \{7}.*/

        hi def link bufExplorerBufNbr Number
        hi def link bufExplorerMapping NonText
        hi def link bufExplorerHelp Special
        hi def link bufExplorerOpenIn Identifier
        hi def link bufExplorerSortBy String
        hi def link bufExplorerSplit NonText
        hi def link bufExplorerTitle NonText
        hi def link bufExplorerSortType bufExplorerSortBy
        hi def link bufExplorerToggleSplit bufExplorerSplit
        hi def link bufExplorerToggleOpen bufExplorerOpenIn

        hi def link bufExplorerActBuf Identifier
        hi def link bufExplorerAltBuf String
        hi def link bufExplorerCurBuf Type
        hi def link bufExplorerHidBuf Constant
        hi def link bufExplorerLockedBuf Special
        hi def link bufExplorerModBuf Exception
        hi def link bufExplorerUnlBuf Comment
        hi def link bufExplorerInactBuf Comment
    endif
endfunction

" ToggleHelp {{{2
function! s:ToggleHelp()
    let g:bufExplorerDetailedHelp = !g:bufExplorerDetailedHelp

    setlocal modifiable

    " Save position.
    normal! ma

    " Remove old header.
    if s:firstBufferLine > 1
        execute "keepjumps 1,".(s:firstBufferLine - 1) "d _"
    endif

    call append(0, s:CreateHelp())

    silent! normal! g`a
    delmarks a

    setlocal nomodifiable

    if exists("b:displayMode") && b:displayMode == "winmanager"
        call WinManagerForceReSize("BufExplorer")
    endif
endfunction

" GetHelpStatus {{{2
function! s:GetHelpStatus()
    let ret = '" Sorted by '.((g:bufExplorerReverseSort == 1) ? "reverse " : "").g:bufExplorerSortBy
    let ret .= ' | '.((g:bufExplorerFindActive == 0) ? "Don't " : "")."Locate buffer"
    let ret .= ((g:bufExplorerShowUnlisted == 0) ? "" : " | Show unlisted")
    let ret .= ((g:bufExplorerShowTabBuffer == 0) ? "" : " | Show buffers/tab")
    let ret .= ((g:bufExplorerOnlyOneTab == 0) ? "" : " | One tab/buffer")
    let ret .= ' | '.((g:bufExplorerShowRelativePath == 0) ? "Absolute" : "Relative")
    let ret .= ' '.((g:bufExplorerSplitOutPathName == 0) ? "Full" : "Split")." path"

    return ret
endfunction

" CreateHelp {{{2
function! s:CreateHelp()
    if g:bufExplorerDefaultHelp == 0 && g:bufExplorerDetailedHelp == 0
        let s:firstBufferLine = 1
        return []
    endif

    let header = []

    if g:bufExplorerDetailedHelp == 1
        call add(header, '" Buffer Explorer ('.g:bufexplorer_version.')')
        call add(header, '" --------------------------')
        call add(header, '" <F1> : toggle this help')
        call add(header, '" <enter> or o or Mouse-Double-Click : open buffer under cursor')
        call add(header, '" <shift-enter> or t : open buffer in another tab')
        call add(header, '" B : toggle if to save/use recent tab or not')
        call add(header, '" d : delete buffer')
        call add(header, '" D : wipe buffer')
        call add(header, '" f : toggle find active buffer')
        call add(header, '" p : toggle spliting of file and path name')
        call add(header, '" q : quit')
        call add(header, '" r : reverse sort')
        call add(header, '" R : toggle showing relative or full paths')
        call add(header, '" s : cycle thru "sort by" fields '.string(s:sort_by).'')
        call add(header, '" S : reverse cycle thru "sort by" fields')
        call add(header, '" T : toggle if to show only buffers for this tab or not')
        call add(header, '" u : toggle showing unlisted buffers')
    else
        call add(header, '" Press <F1> for Help')
    endif

    if (!exists("b:displayMode") || b:displayMode != "winmanager") || (b:displayMode == "winmanager" && g:bufExplorerDetailedHelp == 1)
        call add(header, s:GetHelpStatus())
        call add(header, '"=')
    endif

    let s:firstBufferLine = len(header) + 1

    return header
endfunction

" GetBufferInfo {{{2
function! s:GetBufferInfo(bufnr)
    redir => bufoutput

    " Show all buffers including the unlisted ones. [!] tells Vim to show the
    " unlisted ones.
    buffers!
    redir END

    if a:bufnr > 0
        " Since we are only interested in this specified buffer
        " remove the other buffers listed
        let bufoutput = substitute(bufoutput."\n", '^.*\n\(\s*'.a:bufnr.'\>.\{-}\)\n.*', '\1', '')
    endif

    let [all, allwidths, listedwidths] = [[], {}, {}]

    for n in keys(s:types)
        let allwidths[n] = []
        let listedwidths[n] = []
    endfor

    " Loop over each line in the buffer.
    for buf in split(bufoutput, '\n')
        let bits = split(buf, '"')

        " Use first and last components after the split on '"', in case a
        " filename with an embedded '"' is present.
        let b = {"attributes": bits[0], "line": substitute(bits[-1], '\s*', '', '')}

        let name = bufname(str2nr(b.attributes))
        let b["hasNoName"] = empty(name)
        if b.hasNoName
            let name = "[No Name]"
        endif

        for [key, val] in items(s:types)
            let b[key] = fnamemodify(name, val)
        endfor

        if getftype(b.fullname) == "dir" && g:bufExplorerShowDirectories == 1
            let b.shortname = "<DIRECTORY>"
        endif

        call add(all, b)

        for n in keys(s:types)
            call add(allwidths[n], s:StringWidth(b[n]))

            if b.attributes !~ "u"
                call add(listedwidths[n], s:StringWidth(b[n]))
            endif
        endfor
    endfor

    let [s:allpads, s:listedpads] = [{}, {}]

    for n in keys(s:types)
        let s:allpads[n] = repeat(' ', max(allwidths[n]))
        let s:listedpads[n] = repeat(' ', max(listedwidths[n]))
    endfor

    return all
endfunction

" BuildBufferList {{{2
function! s:BuildBufferList()
    let lines = []

    " Loop through every buffer.
    for buf in s:raw_buffer_listing
        " Skip unlisted buffers if we are not to show them.
        if !g:bufExplorerShowUnlisted && buf.attributes =~ "u"
            " Skip unlisted buffers if we are not to show them.
            continue
        endif

        " Skip "No Name" buffers if we are not to show them.
        if g:bufExplorerShowNoName == 0 && buf.hasNoName
            continue
        endif

        " Are we to show only buffer(s) for this tab?
        if g:bufExplorerShowTabBuffer
            let show_buffer = 0

            for bufnr in s:tabSpace[tabpagenr()]
                if buf.attributes =~ '^\s*'.bufnr.'\>'
                    " Only buffers shown on the current tabpagenr
                    let show_buffer = 1
                    break
                endif
            endfor

            if show_buffer == 0
                continue
            endif
        endif

        let line = buf.attributes." "

        " Are we to split the path and file name?
        if g:bufExplorerSplitOutPathName
            let type = (g:bufExplorerShowRelativePath) ? "relativepath" : "path"
            let path = buf[type]
            let pad  = (g:bufExplorerShowUnlisted) ? s:allpads.shortname : s:listedpads.shortname
            let line .= buf.shortname." ".strpart(pad.path, s:StringWidth(buf.shortname))
        else
            let type = (g:bufExplorerShowRelativePath) ? "relativename" : "fullname"
            let path = buf[type]
            let line .= path
        endif

        let pads = (g:bufExplorerShowUnlisted) ? s:allpads : s:listedpads

        if !empty(pads[type])
            let line .= strpart(pads[type], s:StringWidth(path))." "
        endif

        let line .= buf.line

        call add(lines, line)
    endfor

    call setline(s:firstBufferLine, lines)
    call s:SortListing()
endfunction

" SelectBuffer {{{2
function! s:SelectBuffer(...)
    " Sometimes messages are not cleared when we get here so it looks like an
    " error has occurred when it really has not.
    "echo ""

    let _bufNbr = -1

    if (a:0 == 1) && (a:1 == "ask")
        " Ask the user for input.
        call inputsave()
        let cmd = input("Enter buffer number to switch to: ")
        call inputrestore()

        " Clear the message area from the previous prompt.
        redraw | echo

        if strlen(cmd) > 0
            let _bufNbr = str2nr(cmd)
        else
            call s:Error("Invalid buffer number, try again.")
            return
        endif
    else
        " Are we on a line with a file name?
        if line('.') < s:firstBufferLine
            execute "normal! \<CR>"
            return
        endif

        let _bufNbr = str2nr(getline('.'))

        " Check and see if we are running BufferExplorer via WinManager.
        if exists("b:displayMode") && b:displayMode == "winmanager"
            let _bufName = expand("#"._bufNbr.":p")

            if (a:0 == 1) && (a:1 == "tab")
                call WinManagerFileEdit(_bufName, 1)
            else
                call WinManagerFileEdit(_bufName, 0)
            endif

            return
        endif
    endif

    if bufexists(_bufNbr)
        if bufnr("#") == _bufNbr && !exists("g:bufExplorerChgWin")
            return s:Close()
        endif

        " Are we suppose to open the selected buffer in a tab?
        if (a:0 == 1) && (a:1 == "tab")
            " Yes, we are to open the selected buffer in a tab.

            " Restore [BufExplorer] buffer.
            execute "keepjumps silent buffer!".s:originBuffer

            " Get the tab nmber where this bufer is located in.
            let tabNbr = s:GetTabNbr(_bufNbr)

            " Was the tab found?
            if tabNbr == 0
                " _bufNbr is not opened in any tabs. Open a new tab with the selected buffer in it.
                execute "999tab split +buffer" . _bufNbr
            else
                " The _bufNbr is already opened in a tab, go to that tab.
                execute tabNbr . "tabnext"

                " Focus window.
                execute s:GetWinNbr(tabNbr, _bufNbr) . "wincmd w"
            endif
        else
            " No, the user did not ask to open the selected buffer in a tab.

            " Are we suppose to move to the tab where the active buffer is?
            if exists("g:bufExplorerChgWin")
                execute g:bufExplorerChgWin."wincmd w"
            elseif bufloaded(_bufNbr) && g:bufExplorerFindActive
                if g:bufExplorerFindActive
                    call s:Close()
                endif

                " Get the tab number where this buffer is located in.
                let tabNbr = s:GetTabNbr(_bufNbr)

                " Was the tab found?
                if tabNbr != 0
                    " Yes, the buffer is located in a tab. Go to that tab number.
                    execute tabNbr . "tabnext"
                else
                    "Nope, the buffer is not in a tab. Simply switch to that
                    "buffer.
                    let _bufName = expand("#"._bufNbr.":p")
                    execute _bufName ? "drop ".escape(_bufName, " ") : "buffer "._bufNbr
                endif
            endif

            " Switch to the selected buffer.
            execute "keepalt keepjumps silent b!" _bufNbr
        endif

        " Make the buffer 'listed' again.
        call setbufvar(_bufNbr, "&buflisted", "1")

        " Call any associated function references. g:bufExplorerFuncRef may be
        " an individual function reference or it may be a list containing
        " function references. It will ignore anything that's not a function
        " reference.
        "
        " See  :help FuncRef  for more on function references.
        if exists("g:BufExplorerFuncRef")
            if type(g:BufExplorerFuncRef) == 2
                keepj call g:BufExplorerFuncRef()
            elseif type(g:BufExplorerFuncRef) == 3
                for FncRef in g:BufExplorerFuncRef
                    if type(FncRef) == 2
                        keepj call FncRef()
                    endif
                endfor
            endif
        endif
    else
        call s:Error("Sorry, that buffer no longer exists, please select another")
        call s:DeleteBuffer(_bufNbr, "wipe")
    endif
endfunction

" RemoveBuffer {{{2
function! s:RemoveBuffer(mode)
    " Are we on a line with a file name?
    if line('.') < s:firstBufferLine
        return
    endif

    " Do not allow this buffer to be deleted if it is the last one.
    if len(s:MRUList) == 1
        call s:Error("Sorry, you are not allowed to delete the last buffer")
        return
    endif

    " These commands are to temporarily suspend the activity of winmanager.
    if exists("b:displayMode") && b:displayMode == "winmanager"
        call WinManagerSuspendAUs()
    end

    let _bufNbr = str2nr(getline('.'))

    if getbufvar(_bufNbr, '&modified') == 1
        call s:Error("Sorry, no write since last change for buffer "._bufNbr.", unable to delete")
        return
    else
        " Okay, everything is good, delete or wipe the buffer.
        call s:DeleteBuffer(_bufNbr, a:mode)
    endif

    " Reactivate winmanager autocommand activity.
    if exists("b:displayMode") && b:displayMode == "winmanager"
        call WinManagerForceReSize("BufExplorer")
        call WinManagerResumeAUs()
    end
endfunction

" DeleteBuffer {{{2
function! s:DeleteBuffer(buf, mode)
    " This routine assumes that the buffer to be removed is on the current line.
    try
        " Wipe/Delete buffer from Vim.
        if a:mode == "wipe"
            execute "silent bwipe" a:buf
        else
            execute "silent bdelete" a:buf
        endif

        " Delete the buffer from the list on screen.
        setlocal modifiable
        normal! "_dd
        setlocal nomodifiable

        " Delete the buffer from the raw buffer list.
        call filter(s:raw_buffer_listing, 'v:val.attributes !~ " '.a:buf.' "')
    catch
        call s:Error(v:exception)
    endtry
endfunction

" Close {{{2
function! s:Close()
    " Get only the listed buffers.
    let listed = filter(copy(s:MRUList), "buflisted(v:val)")

    " If we needed to split the main window, close the split one.
    if s:splitMode != "" && bufwinnr(s:originBuffer) != -1
        execute "wincmd c"
    endif

    " Check to see if there are anymore buffers listed.
    if len(listed) == 0
        " Since there are no buffers left to switch to, open a new empty
        " buffers.
        execute "enew"
    else
        " Since there are buffers left to switch to, swith to the previous and
        " then the current.
        for b in reverse(listed[0:1])
            execute "keepjumps silent b ".b
        endfor
    endif

    " Clear any messages.
    echo
endfunction

" ToggleSplitOutPathName {{{2
function! s:ToggleSplitOutPathName()
    let g:bufExplorerSplitOutPathName = !g:bufExplorerSplitOutPathName
    call s:RebuildBufferList()
    call s:UpdateHelpStatus()
endfunction

" ToggleShowRelativePath {{{2
function! s:ToggleShowRelativePath()
    let g:bufExplorerShowRelativePath = !g:bufExplorerShowRelativePath
    call s:RebuildBufferList()
    call s:UpdateHelpStatus()
endfunction

" ToggleShowTabBuffer {{{2
function! s:ToggleShowTabBuffer()
    let g:bufExplorerShowTabBuffer = !g:bufExplorerShowTabBuffer
    call s:RebuildBufferList(g:bufExplorerShowTabBuffer)
    call s:UpdateHelpStatus()
endfunction

" ToggleOnlyOneTab {{{2
function! s:ToggleOnlyOneTab()
    let g:bufExplorerOnlyOneTab = !g:bufExplorerOnlyOneTab
    call s:RebuildBufferList()
    call s:UpdateHelpStatus()
endfunction

" ToggleShowUnlisted {{{2
function! s:ToggleShowUnlisted()
    let g:bufExplorerShowUnlisted = !g:bufExplorerShowUnlisted
    let num_bufs = s:RebuildBufferList(g:bufExplorerShowUnlisted == 0)
    call s:UpdateHelpStatus()
endfunction

" ToggleFindActive {{{2
function! s:ToggleFindActive()
    let g:bufExplorerFindActive = !g:bufExplorerFindActive
    call s:UpdateHelpStatus()
endfunction

" RebuildBufferList {{{2
function! s:RebuildBufferList(...)
    setlocal modifiable

    let curPos = getpos('.')

    if a:0 && a:000[0] && (line('$') >= s:firstBufferLine)
        " Clear the list first.
        execute "silent keepjumps ".s:firstBufferLine.',$d _'
    endif

    let num_bufs = s:BuildBufferList()

    call setpos('.', curPos)

    setlocal nomodifiable

    return num_bufs
endfunction

" UpdateHelpStatus {{{2
function! s:UpdateHelpStatus()
    setlocal modifiable

    let text = s:GetHelpStatus()
    call setline(s:firstBufferLine - 2, text)

    setlocal nomodifiable
endfunction

" MRUCmp {{{2
function! s:MRUCmp(line1, line2)
    return index(s:MRUList, str2nr(a:line1)) - index(s:MRUList, str2nr(a:line2))
endfunction

" SortReverse {{{2
function! s:SortReverse()
    let g:bufExplorerReverseSort = !g:bufExplorerReverseSort
    call s:ReSortListing()
endfunction

" SortSelect {{{2
function! s:SortSelect()
    let g:bufExplorerSortBy = get(s:sort_by, index(s:sort_by, g:bufExplorerSortBy) + 1, s:sort_by[0])
    call s:ReSortListing()
endfunction

" ReverseSortSelect {{{2
function! s:ReverseSortSelect()
    let g:bufExplorerSortBy = get(s:sort_by, index(s:sort_by, g:bufExplorerSortBy) - 1, s:sort_by[-1])
    call s:ReSortListing()
endfunction

" ReSortListing {{{2
function! s:ReSortListing()
    setlocal modifiable

    let curPos = getpos('.')

    call s:SortListing()
    call s:UpdateHelpStatus()

    call setpos('.', curPos)

    setlocal nomodifiable
endfunction

" SortListing {{{2
function! s:SortListing()
    let sort = s:firstBufferLine.",$sort".((g:bufExplorerReverseSort == 1) ? "!": "")

    if g:bufExplorerSortBy == "number"
        " Easiest case.
        execute sort 'n'
    elseif g:bufExplorerSortBy == "name"
        if g:bufExplorerSplitOutPathName
            execute sort 'ir /\d.\{7}\zs\f\+\ze/'
        else
            execute sort 'ir /\zs[^\/\\]\+\ze\s*line/'
        endif
    elseif g:bufExplorerSortBy == "fullpath"
        if g:bufExplorerSplitOutPathName
            " Sort twice - first on the file name then on the path.
            execute sort 'ir /\d.\{7}\zs\f\+\ze/'
        endif

        execute sort 'ir /\zs\f\+\ze\s\+line/'
    elseif g:bufExplorerSortBy == "extension"
        execute sort 'ir /\.\zs\w\+\ze\s/'
    elseif g:bufExplorerSortBy == "mru"
        let l = getline(s:firstBufferLine, "$")

        call sort(l, "<SID>MRUCmp")

        if g:bufExplorerReverseSort
            call reverse(l)
        endif

        call setline(s:firstBufferLine, l)
    endif
endfunction

" MRUListShow {{{2
function! s:MRUListShow()
    echomsg "MRUList=".string(s:MRUList)
endfunction

" Error {{{2
" Display a message using ErrorMsg highlight group.
function! s:Error(msg)
    echohl ErrorMsg
    echomsg a:msg
    echohl None
endfunction

" Warning {{{2
" Display a message using WarningMsg highlight group.
function! s:Warning(msg)
    echohl WarningMsg
    echomsg a:msg
    echohl None
endfunction

" GetTabNbr {{{2
function! s:GetTabNbr(bufNbr)
    " Searching buffer bufno, in tabs.
    for i in range(tabpagenr("$"))
        if index(tabpagebuflist(i + 1), a:bufNbr) != -1
            return i + 1
        endif
    endfor

    return 0
endfunction

" GetWinNbr" {{{2
function! s:GetWinNbr(tabNbr, bufNbr)
    " window number in tabpage.
    let tablist = tabpagebuflist(a:tabNbr)
    " Number:     0
    " String:     1
    " Funcref:    2
    " List:       3
    " Dictionary: 4
    " Float:      5
    if type(tablist) == 3
        return index(tabpagebuflist(a:tabNbr), a:bufNbr) + 1
    else
        return 1
    endif
endfunction

" StringWidth" {{{2
if exists('*strwidth')
    function s:StringWidth(s)
        return strwidth(a:s)
    endfunction
else
    function s:StringWidth(s)
        return len(a:s)
    endfunction
endif

" Winmanager Integration {{{2
let g:BufExplorer_title = "\[Buf\ List\]"
call s:Set("g:bufExplorerResize", 1)
call s:Set("g:bufExplorerMaxHeight", 25) " Handles dynamic resizing of the window.

" function! to start display. Set the mode to 'winmanager' for this buffer.
" This is to figure out how this plugin was called. In a standalone fashion
" or by winmanager.
function! BufExplorer_Start()
    let b:displayMode = "winmanager"
    call BufExplorer()
endfunction

" Returns whether the display is okay or not.
function! BufExplorer_IsValid()
    return 0
endfunction

" Handles dynamic refreshing of the window.
function! BufExplorer_Refresh()
    let b:displayMode = "winmanager"
    call BufExplorer()
endfunction

function! BufExplorer_ReSize()
    if !g:bufExplorerResize
        return
    end

    let nlines = min([line("$"), g:bufExplorerMaxHeight])

    execute nlines." wincmd _"

    " The following lines restore the layout so that the last file line is also
    " the last window line. Sometimes, when a line is deleted, although the
    " window size is exactly equal to the number of lines in the file, some of
    " the lines are pushed up and we see some lagging '~'s.
    let pres = getpos(".")

    normal! $

    let _scr = &scrolloff
    let &scrolloff = 0

    normal! z-

    let &scrolloff = _scr

    call setpos(".", pres)
endfunction

" Default values {{{1
call s:Set("g:bufExplorerDisableDefaultKeyMapping", 0)  " Do not disable default key mappings.
call s:Set("g:bufExplorerDefaultHelp", 1)               " Show default help?
call s:Set("g:bufExplorerDetailedHelp", 0)              " Show detailed help?
call s:Set("g:bufExplorerFindActive", 1)                " When selecting an active buffer, take you to the window where it is active?
call s:Set("g:bufExplorerOnlyOneTab", 1)                " If ShowTabBuffer = 1, only store the most recent tab for this buffer.
call s:Set("g:bufExplorerReverseSort", 0)               " Sort in reverse order by default?
call s:Set("g:bufExplorerShowDirectories", 1)           " (Dir's are added by commands like ':e .')
call s:Set("g:bufExplorerShowRelativePath", 0)          " Show listings with relative or absolute paths?
call s:Set("g:bufExplorerShowTabBuffer", 0)             " Show only buffer(s) for this tab?
call s:Set("g:bufExplorerShowUnlisted", 0)              " Show unlisted buffers?
call s:Set("g:bufExplorerShowNoName", 0)                " Show 'No Name' buffers?
call s:Set("g:bufExplorerSortBy", "mru")                " Sorting methods are in s:sort_by:
call s:Set("g:bufExplorerSplitBelow", &splitbelow)      " Should horizontal splits be below or above current window?
call s:Set("g:bufExplorerSplitOutPathName", 1)          " Split out path and file name?
call s:Set("g:bufExplorerSplitRight", &splitright)      " Should vertical splits be on the right or left of current window?
call s:Set("g:bufExplorerSplitVertSize", 0)             " Height for a vertical split. If <=0, default Vim size is used.
call s:Set("g:bufExplorerSplitHorzSize", 0)             " Height for a horizontal split. If <=0, default Vim size is used.
"1}}}

" Default key mapping {{{1
if !hasmapto('BufExplorer') && g:bufExplorerDisableDefaultKeyMapping == 0
    nnoremap <script> <silent> <unique> <Leader>be :BufExplorer<CR>
endif

if !hasmapto('BufExplorerHorizontalSplit') && g:bufExplorerDisableDefaultKeyMapping == 0
    nnoremap <script> <silent> <unique> <Leader>bs :BufExplorerHorizontalSplit<CR>
endif

if !hasmapto('BufExplorerVerticalSplit') && g:bufExplorerDisableDefaultKeyMapping == 0
    nnoremap <script> <silent> <unique> <Leader>bv :BufExplorerVerticalSplit<CR>
endif

" vim:ft=vim foldmethod=marker sw=4
