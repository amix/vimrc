" Description: Omni completion script for cpp files
" Maintainer:  Vissale NEANG
" Last Change: 26 sept. 2007

" Check if we can use omni completion in the current buffer
function! s:CanUseOmnicompletion()
    " For C and C++ files and only if the omnifunc is omni#cpp#complete#Main
    return (index(['c', 'cpp'], &filetype)>=0 && &omnifunc == 'omni#cpp#complete#Main' && !omni#cpp#utils#IsCursorInCommentOrString())
endfunc

" Return the mapping of omni completion
function! omni#cpp#maycomplete#Complete()
    let szOmniMapping = "\<C-X>\<C-O>"

    "   0 = don't select first item
    "   1 = select first item (inserting it to the text, default vim behaviour)
    "   2 = select first item (without inserting it to the text)
    if g:OmniCpp_SelectFirstItem == 0
        " We have to force the menuone option to avoid confusion when there is
        " only one popup item
        set completeopt-=menu
        set completeopt+=menuone
        let szOmniMapping .= "\<C-P>"
    elseif g:OmniCpp_SelectFirstItem == 2
        " We have to force the menuone option to avoid confusion when there is
        " only one popup item
        set completeopt-=menu
        set completeopt+=menuone
        let szOmniMapping .= "\<C-P>"
        let szOmniMapping .= "\<C-R>=pumvisible() ? \"\\<down>\" : \"\"\<cr>"
    endif
    return szOmniMapping
endfunc

" May complete function for dot
function! omni#cpp#maycomplete#Dot()
    if s:CanUseOmnicompletion() && g:OmniCpp_MayCompleteDot
        let g:omni#cpp#items#data = omni#cpp#items#Get(omni#cpp#utils#TokenizeCurrentInstruction('.'))
        if len(g:omni#cpp#items#data)
            let s:bMayComplete = 1
            return '.' . omni#cpp#maycomplete#Complete()
        endif
    endif
    return '.'
endfunc
" May complete function for arrow
function! omni#cpp#maycomplete#Arrow()
    if s:CanUseOmnicompletion() && g:OmniCpp_MayCompleteArrow
        let index = col('.') - 2
        if index >= 0
            let char = getline('.')[index]
            if char == '-'
                let g:omni#cpp#items#data = omni#cpp#items#Get(omni#cpp#utils#TokenizeCurrentInstruction('>'))
                if len(g:omni#cpp#items#data)
                    let s:bMayComplete = 1
                    return '>' . omni#cpp#maycomplete#Complete()
                endif
            endif
        endif
    endif
    return '>'
endfunc

" May complete function for double points
function! omni#cpp#maycomplete#Scope()
    if s:CanUseOmnicompletion() && g:OmniCpp_MayCompleteScope
        let index = col('.') - 2
        if index >= 0
            let char = getline('.')[index]
            if char == ':'
                let g:omni#cpp#items#data = omni#cpp#items#Get(omni#cpp#utils#TokenizeCurrentInstruction(':'))
                if len(g:omni#cpp#items#data)
                    if len(g:omni#cpp#items#data[-1].tokens) && g:omni#cpp#items#data[-1].tokens[-1].value != '::'
                        let s:bMayComplete = 1
                        return ':' . omni#cpp#maycomplete#Complete()
                    endif
                endif
            endif
        endif
    endif
    return ':'
endfunc
