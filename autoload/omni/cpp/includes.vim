" Description: Omni completion script for cpp files
" Maintainer:  Vissale NEANG
" Last Change: 26 sept. 2007

let g:omni#cpp#includes#CACHE_INCLUDES = {}
let g:omni#cpp#includes#CACHE_FILE_TIME = {}

let s:rePreprocIncludePart = '\C#\s*include\s*'
let s:reIncludeFilePart = '\(<\|"\)\(\f\|\s\)\+\(>\|"\)'
let s:rePreprocIncludeFile = s:rePreprocIncludePart . s:reIncludeFilePart

" Get the include list of a file
function! omni#cpp#includes#GetList(...)
    if a:0 > 0
        return s:GetIncludeListFromFile(a:1, (a:0 > 1)? a:2 : 0 )
    else
        return s:GetIncludeListFromCurrentBuffer()
    endif
endfunc

" Get the include list from the current buffer
function! s:GetIncludeListFromCurrentBuffer()
    let listIncludes = []
    let originalPos = getpos('.')

    call setpos('.', [0, 1, 1, 0])
    let curPos = [1,1]
    let alreadyInclude = {}
    while curPos != [0,0]
        let curPos = searchpos('\C\(^'.s:rePreprocIncludeFile.'\)', 'W')
        if curPos != [0,0]
            let szLine = getline('.')
            let startPos = curPos[1]
            let endPos = matchend(szLine, s:reIncludeFilePart, startPos-1)
            if endPos!=-1
                let szInclusion = szLine[startPos-1:endPos-1]
                let szIncludeFile = substitute(szInclusion, '\('.s:rePreprocIncludePart.'\)\|[<>""]', '', 'g')
                let szResolvedInclude = omni#cpp#utils#ResolveFilePath(szIncludeFile)

                " Protection over self inclusion
                if szResolvedInclude != '' && szResolvedInclude != omni#cpp#utils#ResolveFilePath(getreg('%'))
                    let includePos = curPos
                    if !has_key(alreadyInclude, szResolvedInclude)
                        call extend(listIncludes, [{'pos' : includePos, 'include' : szResolvedInclude}])
                        let alreadyInclude[szResolvedInclude] = 1
                    endif
                endif
            endif
        endif
    endwhile

    call setpos('.', originalPos)
    return listIncludes
endfunc

" Get the include list from a file
function! s:GetIncludeListFromFile(szFilePath, bUpdate) 
    let listIncludes = []
    if a:szFilePath == ''
        return listIncludes
    endif

    if !a:bUpdate && has_key(g:omni#cpp#includes#CACHE_INCLUDES, a:szFilePath)
        return copy(g:omni#cpp#includes#CACHE_INCLUDES[a:szFilePath])
    endif

    let g:omni#cpp#includes#CACHE_FILE_TIME[a:szFilePath] = getftime(a:szFilePath)

    let szFixedPath = escape(a:szFilePath, g:omni#cpp#utils#szEscapedCharacters)
    execute 'silent! lvimgrep /\C\(^'.s:rePreprocIncludeFile.'\)/gj '.szFixedPath

    let listQuickFix = getloclist(0)
    let alreadyInclude = {}
    for qf in listQuickFix
        let szLine = qf.text
        let startPos = qf.col
        let endPos = matchend(szLine, s:reIncludeFilePart, startPos-1)
        if endPos!=-1
            let szInclusion = szLine[startPos-1:endPos-1]
            let szIncludeFile = substitute(szInclusion, '\('.s:rePreprocIncludePart.'\)\|[<>""]', '', 'g')
            let szResolvedInclude = omni#cpp#utils#ResolveFilePath(szIncludeFile)
            
            " Protection over self inclusion
            if szResolvedInclude != '' && szResolvedInclude != a:szFilePath
                let includePos = [qf.lnum, qf.col]
                if !has_key(alreadyInclude, szResolvedInclude)
                    call extend(listIncludes, [{'pos' : includePos, 'include' : szResolvedInclude}])
                    let alreadyInclude[szResolvedInclude] = 1
                endif
            endif
        endif
    endfor

    let g:omni#cpp#includes#CACHE_INCLUDES[a:szFilePath] = listIncludes

    return copy(listIncludes)
endfunc

" For debug purpose
function! omni#cpp#includes#Display()
    let szPathBuffer = omni#cpp#utils#ResolveFilePath(getreg('%'))
    call s:DisplayIncludeTree(szPathBuffer, 0)
endfunc

" For debug purpose
function! s:DisplayIncludeTree(szFilePath, indent, ...)
    let includeGuard = {}
    if a:0 >0
        let includeGuard = a:1
    endif
    let szFilePath = omni#cpp#utils#ResolveFilePath(a:szFilePath)
    if has_key(includeGuard, szFilePath)
        return
    else
        let includeGuard[szFilePath] = 1
    endif

    let szIndent = repeat('    ', a:indent)
    echo szIndent . a:szFilePath
    let incList = omni#cpp#includes#GetList(a:szFilePath)
    for inc in incList
        call s:DisplayIncludeTree(inc.include, a:indent+1, includeGuard)
    endfor
endfunc


