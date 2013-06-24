"*****************************************************************************
"** Name:      crefvim.vim - a C-Reference for Vim                          **
"**                                                                         **
"** Type:      global VIM plugin                                            **
"**                                                                         **
"** Author:    Christian Habermann                                          **
"**            christian(at)habermann-net(point)de                          **
"**                                                                         **
"** Copyright: (c) 2002, 2003 by Christian Habermann                        **
"**                                                                         **
"** License:   GNU General Public License 2 (GPL 2) or later                **
"**                                                                         **
"**            This program is free software; you can redistribute it       **
"**            and/or modify it under the terms of the GNU General Public   **
"**            License as published by the Free Software Foundation; either **
"**            version 2 of the License, or (at your option) any later      **
"**            version.                                                     **
"**                                                                         **
"**            This program is distributed in the hope that it will be      **
"**            useful, but WITHOUT ANY WARRANTY; without even the implied   **
"**            warrenty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      **
"**            PURPOSE.                                                     **
"**            See the GNU General Public License for more details.         **
"**                                                                         **
"** Version:   1.0.0                                                        **
"**            tested under Linux (vim, gvim 6.1) and Win32 (gvim 6.1)      **
"**                                                                         **
"** History:   0.1.0  12. Dec. 2002 - 23. Feb. 2003                         **
"**              initial version, not released                              **
"**            1.0.0   6. Apr. 2003                                         **
"**              no changes, first release                                  **
"**                                                                         **
"**                                                                         **
"*****************************************************************************
"** Description:                                                            **
"**   This script's intention is to provide a C-reference manual that can   **
"**   be accessed from within Vim.                                          **
"**                                                                         **
"**   For futher information see crefvim.txt or do :help crefvim            **
"**                                                                         **
"**                                                                         **
"**   Happy viming...                                                       **
"*****************************************************************************

" allow user to avoid loading this plugin and prevent loading twice
if exists ("loaded_crefvim")
    finish
endif

let loaded_crefvim = 1




"*****************************************************************************
"************************** C O N F I G U R A T I O N ************************
"*****************************************************************************

" the mappings:
if !hasmapto('<Plug>CRV_CRefVimVisual')
    vmap <silent> <unique> <Leader>cr <Plug>CRV_CRefVimVisual
endif
if !hasmapto('<Plug>CRV_CRefVimNormal')
    nmap <silent> <unique> <Leader>cr <Plug>CRV_CRefVimNormal
endif
if !hasmapto('<Plug>CRV_CRefVimAsk')
    map <silent> <unique> <Leader>cw <Plug>CRV_CRefVimAsk
endif
if !hasmapto('<Plug>CRV_CRefVimInvoke')
    map <silent> <unique> <Leader>cc <Plug>CRV_CRefVimInvoke
endif

vmap <silent> <unique> <script> <Plug>CRV_CRefVimVisual  y:call <SID>CRV_CRefVimWord('<c-r>"')<CR>
nmap <silent> <unique> <script> <Plug>CRV_CRefVimNormal   :call <SID>CRV_CRefVimWord(expand("<cword>"))<CR>
map  <silent> <unique> <script> <Plug>CRV_CRefVimAsk      :call <SID>CRV_CRefVimAskForWord()<CR>
map  <silent> <unique> <script> <Plug>CRV_CRefVimInvoke   :call <SID>CRV_CRefVimShowContents()<CR>




"*****************************************************************************
"************************* I N I T I A L I S A T I O N ***********************
"*****************************************************************************


"*****************************************************************************
"****************** I N T E R F A C E  T O  C O R E **************************
"*****************************************************************************

"*****************************************************************************
"** this function separates plugin-core-function from user                  **
"*****************************************************************************
function <SID>CRV_CRefVimWord(str)
  call s:CRefVim(a:str)
endfunction


"*****************************************************************************
"** this function separates plugin-core-function from user                  **
"*****************************************************************************
function <SID>CRV_CRefVimAskForWord()
    call s:CRefVimAskForWord()
endfunction


"*****************************************************************************
"** this function separates plugin-core-function from user                  **
"*****************************************************************************
function <SID>CRV_CRefVimShowContents()
    " show contents of C-reference manual
    call s:LookUp("")
endfunction





"*****************************************************************************
"************************ C O R E  F U N C T I O N S *************************
"*****************************************************************************

"*****************************************************************************
"** ask for a word/phrase and lookup                                        **
"*****************************************************************************
function s:CRefVimAskForWord()
    let l:strng = input("What to lookup: ")
    call s:LookUp(l:strng)
endfunction



"*****************************************************************************
"** input:  "str"                                                           **
"** output: empty string: "str" is not an operator                          **
"**         else:         name of tag to go to                              **
"**                                                                         **
"*****************************************************************************
"** remarks:                                                                **
"**   This function tests whether or not "str" is an operator.              **
"**   If so, the tag to go to is returned.                                  **
"**                                                                         **
"*****************************************************************************
function s:IsItAnOperator(str)

    " get first character
    let l:firstChr = strpart(a:str, 0, 1)

    " is the first character of the help-string an operator?
    if stridx("!&+-*/%,.:<=>?^|~(){}[]", l:firstChr) >= 0
        return "crv-operators"
    else
        return ""
    endif

endfunction



"*****************************************************************************
"** input:  "str"                                                           **
"** output: empty string: "str" is not an escape-sequence                   **
"**         else:         name of tag to go to                              **
"**                                                                         **
"*****************************************************************************
"** remarks:                                                                **
"**   This function tests whether or not "str" is an escape-sequence.       **
"**   If so, the tag to go to is returned.                                  **
"**   Note: currently \' does not work (="\\\'")                            **
"**                                                                         **
"*****************************************************************************
function s:IsItAnEscSequence(str)

    if (a:str == "\\")   || (a:str == "\\\\") || (a:str == "\\0") || (a:str == "\\x") ||
      \(a:str == "\\a")  || (a:str == "\\b")  || (a:str == "\\f") || (a:str == "\\n") ||
      \(a:str == "\\r")  || (a:str == "\\t")  || (a:str == "\\v") || (a:str == "\\?") ||
      \(a:str == "\\\'") || (a:str == "\\\"")
        return "crv-lngEscSeq"
    else
        return ""
    endif
    
endfunction




"*****************************************************************************
"** input:  "str"                                                           **
"** output: empty string: "str" is not a comment                            **
"**         else:         name of tag to go to                              **
"**                                                                         **
"*****************************************************************************
"** remarks:                                                                **
"**   This function tests whether or not "str" is a comment.                **
"**   If so, the tag to go to is returned.                                  **
"**                                                                         **
"*****************************************************************************
function s:IsItAComment(str)

    if (a:str == "//") || (a:str == "/*") || (a:str == "*/")
        return "crv-lngComment"
    else
        return ""
    endif 

endfunction




"*****************************************************************************
"** input:  "str"                                                           **
"** output: empty string: "str" is not a preprocessor                       **
"**         else:         name of tag to go to                              **
"**                                                                         **
"*****************************************************************************
"** remarks:                                                                **
"**   This function tests whether or not "str" is a preprocessor command    **
"**   or a preprocessor operator.                                           **
"**   If so, the tag to go to is returned.                                  **
"**                                                                         **
"**   Nothing is done if the help-string is equal to "if" or "else"         **
"**   because these are statements too. For "if" and "else" it's assumed    **
"**   that the statements are meant. But "#if" and "#else" are treated      **
"**   as preprocessor commands.                                             **
"**                                                                         **
"*****************************************************************************
function s:IsItAPreprocessor(str)

    " get first character
    let l:firstChr = strpart(a:str, 0, 1)
   
    " if first character of the help-string is a #, we have the command/operator
    " string in an appropriate form, so append this help-string to "crv-"
    if l:firstChr == "#"
        return "crv-" . a:str
    else
        " no # in front of the help string, so evaluate which command/operator
        " is meant
        if (a:str == "defined")
            return "crv-defined"
        else
            if (a:str == "define")  ||
              \(a:str == "undef")   ||
              \(a:str == "ifdef")   ||
              \(a:str == "ifndef")  ||
              \(a:str == "elif")    ||
              \(a:str == "endif")   ||
              \(a:str == "include") ||
              \(a:str == "line")    ||
              \(a:str == "error")   ||
              \(a:str == "pragma")
                return "\#" . a:str
            endif
        endif
    endif

endfunction




"*****************************************************************************
"** input:  "str" to lookup in C-reference manual                           **
"** output: none                                                            **
"*****************************************************************************
"** remarks:                                                                **
"**   Lookup string "str".                                                  **
"**   Generally this function calls :help crv-"str" where "str" is the      **
"**   word for which the user wants some help.                              **
"**                                                                         **
"**   But before activating VIM's help-system some tests and/or             **
"**   modifications are done on "str":                                      **
"**   - if help-string is a comment (//, /* or */), go to section           **
"**     describing comments                                                 **
"**   - if help-string is an escape-sequence, go to section describing      **
"**     escape-sequences                                                    **
"**   - if help-string is an operator, go to section dealing with operators **
"**   - if help-string is a preprocessor command/operator, go to section    **
"**     that describes that command/operator                                **
"**   - else call :help crv-"str"                                           **
"**                                                                         **
"**   If the help-string is empty, go to contents of C-reference manual.    **
"**                                                                         **
"*****************************************************************************
function s:LookUp(str)

    if a:str != ""

        let l:helpTag = s:IsItAComment(a:str)
        
        if l:helpTag == ""
            let l:helpTag = s:IsItAnEscSequence(a:str)
            
            if l:helpTag == ""
                let l:helpTag = s:IsItAnOperator(a:str)
                
                if l:helpTag == ""
                    let l:helpTag = s:IsItAPreprocessor(a:str)
                    
                    if l:helpTag == ""
                        let l:helpTag = "crv-" . a:str
                    endif
                    
                endif
                
            endif
            
        endif


        " reset error message
        let v:errmsg = ""
        
        " activate help-system looking for the appropriate topic
        " suppress error messages
        silent! execute ":help " . l:helpTag

        " if there was an error, print message
        if v:errmsg != ""
            echo "  No help found for \"" .a:str . "\""
        endif
    else
        " help string is empty, so show contents of manual
        execute ":help crefvim"
    endif
    
    
endfunction



"*****************************************************************************
"** input:  "str" to lookup in C-reference manual                           **
"** output: none                                                            **
"*****************************************************************************
"** remarks:                                                                **
"**   lookup string "str".                                                  **
"**   If there is no string, ask for word/phrase.                           **
"**                                                                         **
"*****************************************************************************
function s:CRefVim(str)

    let s:strng = a:str

    if s:strng == ""                     " is there a string to search for?
        call s:CRefVimAskForWord()
    else
        call s:LookUp(s:strng)
    endif

endfunction



"*** EOF ***
