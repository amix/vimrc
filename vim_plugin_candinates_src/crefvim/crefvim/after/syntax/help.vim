"*****************************************************************************
"** Name:      help.vim - extend standard syntax highlighting for help      **
"**                                                                         **
"** Type:      syntax file                                                  **
"**                                                                         **
"** Author:    Christian Habermann                                          **
"**            christian (at) habermann-net (point) de                      **
"**                                                                         **
"** Copyright: (c) 2002-2004 by Christian Habermann                         **
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
"** Version:   1.0.1                                                        **
"**            tested under Linux and Win32, VIM and GVIM 6.2               **
"**                                                                         **
"** History:   0.1.0  12. Dec. 2002 - 21. Feb. 2003                         **
"**              initial version, not released                              **
"**            1.0.0   6. Apr. 2003                                         **
"**              no changes, first release                                  **
"**            1.0.1   3. Mar. 2004                                         **
"**              marker changed from 0xa7 to $ in order to avoid problems   **
"**              with fonts that use codes > 0x7f as multibyte characters   **
"**              (e.g. Chinese, Korean, Japanese... fonts)                  **
"**                                                                         **
"**                                                                         **
"*****************************************************************************
"** Description:                                                            **
"**   This syntax file extends the standard syntax highlighting for help    **
"**   files. This is needed in order to view the C-reference manual         **
"**   of the project CRefVim correctly.                                     **
"**   This syntax file is only active for the help file named               **
"**   "crefvim.txt". For other help files no extention on syntax            **
"**   highlighting is applied.                                              **
"**                                                                         **
"**   For futher information see crefvimdoc.txt or do :help crefvimdoc      **
"**                                                                         **
"**   Happy viming...                                                       **
"*****************************************************************************


" extend syntax-highlighting for "crefvim.txt" only (not case-sensitive)

if tolower(expand("%:t"))=="crefvim.txt"
    syn match helpCRVSubStatement  "statement[0-9Ns]*"   contained
    syn match helpCRVSubCondition  "condition[0-9]*"     contained
    syn match helpCRVSubExpression "expression[0-9]*"    contained
    syn match helpCRVSubExpr       "expr[0-9N]"          contained
    syn match helpCRVSubType       "type-name"           contained
    syn match helpCRVSubIdent      "identifier"          contained
    syn match helpCRVSubIdentList  "identifier-list"     contained
    syn match helpCRVSubOperand    "operand[0-9]*"       contained
    syn match helpCRVSubConstExpr  "constant-expression[1-9Ns]*" contained
    syn match helpCRVSubClassSpec  "storage-class-specifier"  contained
    syn match helpCRVSubTypeSpec   "type-specifier"      contained
    syn match helpCRVSubEnumList   "enumerator-list"     contained
    syn match helpCRVSubDecl       "declarator"          contained
    syn match helpCRVSubRetType    "return-type"         contained
    syn match helpCRVSubFuncName   "function-name"       contained
    syn match helpCRVSubParamList  "parameter-list"      contained
    syn match helpCRVSubReplList   "replacement-list"    contained
    syn match helpCRVSubNewLine    "newline"             contained
    syn match helpCRVSubMessage    "message"             contained
    syn match helpCRVSubFilename   "filename"            contained
    syn match helpCRVSubDigitSeq   "digit-sequence"      contained
    syn match helpCRVSubMacroNames "macro-name[s]*"      contained
    syn match helpCRVSubDirective  "directive"           contained


    syn match helpCRVignore     "\$[a-zA-Z0-9\\\*/\._=()\-+%<>&\^|!~\?:,\[\];{}#\'\" ]\+\$" contains=helpCRVstate
    syn match helpCRVstate      "[a-zA-Z0-9\\\*/\._=()\-+%<>&\^|!~\?:,\[\];{}#\'\" ]\+"   contained contains=helpCRVSub.*


    hi helpCRVitalic  term=italic cterm=italic gui=italic
    
    hi def link  helpCRVstate          Comment
    hi def link  helpCRVSubStatement   helpCRVitalic
    hi def link  helpCRVSubCondition   helpCRVitalic
    hi def link  helpCRVSubExpression  helpCRVitalic
    hi def link  helpCRVSubExpr        helpCRVitalic
    hi def link  helpCRVSubOperand     helpCRVitalic
    hi def link  helpCRVSubType        helpCRVitalic
    hi def link  helpCRVSubIdent       helpCRVitalic
    hi def link  helpCRVSubIdentList   helpCRVitalic
    hi def link  helpCRVSubConstExpr   helpCRVitalic
    hi def link  helpCRVSubClassSpec   helpCRVitalic
    hi def link  helpCRVSubTypeSpec    helpCRVitalic
    hi def link  helpCRVSubEnumList    helpCRVitalic
    hi def link  helpCRVSubDecl        helpCRVitalic
    hi def link  helpCRVSubRetType     helpCRVitalic
    hi def link  helpCRVSubFuncName    helpCRVitalic
    hi def link  helpCRVSubParamList   helpCRVitalic
    hi def link  helpCRVSubReplList    helpCRVitalic
    hi def link  helpCRVSubNewLine     helpCRVitalic
    hi def link  helpCRVSubMessage     helpCRVitalic
    hi def link  helpCRVSubFilename    helpCRVitalic
    hi def link  helpCRVSubDigitSeq    helpCRVitalic
    hi def link  helpCRVSubMacroNames  helpCRVitalic
    hi def link  helpCRVSubDirective   helpCRVitalic
    hi def link  helpCRVignore         Ignore
endif

" vim: ts=8 sw=2
