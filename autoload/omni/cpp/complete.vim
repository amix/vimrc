" Description: Omni completion script for cpp files
" Maintainer:  Vissale NEANG
" Last Change: 27 sept. 2007

if v:version < 700
    echohl WarningMsg
    echomsg "omni#cpp#complete.vim: Please install vim 7.0 or higher for omni-completion"
    echohl None
    finish
endif

call omni#cpp#settings#Init()
let s:OmniCpp_ShowScopeInAbbr = g:OmniCpp_ShowScopeInAbbr
let s:OmniCpp_ShowPrototypeInAbbr = g:OmniCpp_ShowPrototypeInAbbr
let s:OmniCpp_ShowAccess = g:OmniCpp_ShowAccess
let s:szCurrentWorkingDir = getcwd()

" Cache data
let s:CACHE_TAG_POPUP_ITEMS = {}
let s:CACHE_TAG_FILES = {}
let s:CACHE_TAG_ENV = ''
let s:CACHE_OVERLOADED_FUNCTIONS = {}

" Has preview window?
let s:hasPreviewWindow = match(&completeopt, 'preview')>=0
let s:hasPreviewWindowOld = s:hasPreviewWindow

" Popup item list
let s:popupItemResultList = []

" May complete indicator
let s:bMayComplete = 0

" Init mappings
function! omni#cpp#complete#Init()
    call omni#cpp#settings#Init()
    set omnifunc=omni#cpp#complete#Main
    inoremap <expr> <C-X><C-O> omni#cpp#maycomplete#Complete()
    inoremap <expr> . omni#cpp#maycomplete#Dot()
    inoremap <expr> > omni#cpp#maycomplete#Arrow()
    inoremap <expr> : omni#cpp#maycomplete#Scope()
endfunc

" Find the start position of the completion
function! s:FindStartPositionOfCompletion()
    " Locate the start of the item, including ".", "->" and "[...]".
    let line = getline('.')
    let start = col('.') - 1

    let lastword = -1
    while start > 0
        if line[start - 1] =~ '\w'
            let start -= 1
        elseif line[start - 1] =~ '\.'
            " Searching for dot '.'
            if lastword == -1
                let lastword = start
            endif
            let start -= 1
        elseif start > 1 && line[start - 2] == '-' && line[start - 1] == '>'
            " Searching for '->'
            if lastword == -1
                let lastword = start
            endif
            let start -= 2
        elseif start > 1 && line[start - 2] == ':' && line[start - 1] == ':'
            " Searching for '::' for namespaces and class
            if lastword == -1
                let lastword = start
            endif
            let start -= 2
        elseif line[start - 1] == ']'
            " Skip over [...].
            let n = 0
            let start -= 1
            while start > 0
                let start -= 1
                if line[start] == '['
                    if n == 0
                        break
                    endif
                    let n -= 1
                elseif line[start] == ']'  " nested []
                    let n += 1
                endif
            endwhile
        else
            break
        endif
    endwhile
    if lastword==-1
        " For completion on the current scope
        let lastword = start
    endif
    return lastword
endfunc

" Returns if szKey1.szKey2 is in the cache
" @return
"   - 0 = key not found
"   - 1 = szKey1.szKey2 found
"   - 2 = szKey1.[part of szKey2] found
function! s:IsCached(cache, szKey1, szKey2)
    " Searching key in the result cache
    let szResultKey = a:szKey1 . a:szKey2
    let result = [0, szResultKey]
    if a:szKey2 != ''
        let szKey = a:szKey2
        while len(szKey)>0
            if has_key(a:cache, a:szKey1 . szKey)
                let result[1] = a:szKey1 . szKey
                if szKey != a:szKey2
                    let result[0] = 2
                else
                    let result[0] = 1
                endif
                break
            endif
            let szKey = szKey[:-2]
        endwhile
    else
        if has_key(a:cache, szResultKey)
            let result[0] = 1
        endif
    endif
    return result
endfunc

" Extend a tag item to a popup item
function! s:ExtendTagItemToPopupItem(tagItem, szTypeName)
    let tagItem = a:tagItem

    " Add the access
    let szItemMenu = ''
    let accessChar = {'public': '+','protected': '#','private': '-'}
    if g:OmniCpp_ShowAccess
        if has_key(tagItem, 'access') && has_key(accessChar, tagItem.access)
            let szItemMenu = szItemMenu.accessChar[tagItem.access]
        else
            let szItemMenu = szItemMenu." "
        endif
    endif

    " Formating optional menu string we extract the scope information
    let szName = substitute(tagItem.name, '.*::', '', 'g')
    let szItemWord = szName
    let szAbbr = szName

    if !g:OmniCpp_ShowScopeInAbbr
        let szScopeOfTag = omni#cpp#utils#ExtractScope(tagItem)
        let szItemMenu = szItemMenu.' '.szScopeOfTag[2:]
        let szItemMenu = substitute(szItemMenu, '\s\+$', '', 'g')
    else
        let szAbbr = tagItem.name
    endif
    if g:OmniCpp_ShowAccess
        let szItemMenu = substitute(szItemMenu, '^\s\+$', '', 'g')
    else
        let szItemMenu = substitute(szItemMenu, '\(^\s\+\)\|\(\s\+$\)', '', 'g')
    endif

    " Formating information for the preview window
    if index(['f', 'p'], tagItem.kind[0])>=0
        let szItemWord .= '('
        if g:OmniCpp_ShowPrototypeInAbbr && has_key(tagItem, 'signature')
            let szAbbr .= tagItem.signature
        else
            let szAbbr .= '('
        endif
    endif
    let szItemInfo = ''
    if s:hasPreviewWindow
        let szItemInfo = omni#cpp#utils#GetPreviewWindowStringFromTagItem(tagItem)
    endif

    " If a function is a ctor we add a new key in the tagItem
    if index(['f', 'p'], tagItem.kind[0])>=0
        if match(szName, '^\~') < 0 && a:szTypeName =~ '\C\<'.szName.'$'
            " It's a ctor
            let tagItem['ctor'] = 1
        elseif has_key(tagItem, 'access') && tagItem.access == 'friend'
            " Friend function
            let tagItem['friendfunc'] = 1
        endif
    endif

    " Extending the tag item to a popup item
    let tagItem['word'] = szItemWord
    let tagItem['abbr'] = szAbbr
    let tagItem['menu'] = szItemMenu
    let tagItem['info'] = szItemInfo
    let tagItem['dup'] = (s:hasPreviewWindow && index(['f', 'p', 'm'], tagItem.kind[0])>=0)
    return tagItem
endfunc

" Get tag popup item list
function! s:TagPopupList(szTypeName, szBase)
    let result = []

    " Searching key in the result cache
    let cacheResult = s:IsCached(s:CACHE_TAG_POPUP_ITEMS, a:szTypeName, a:szBase)

    " Building the tag query, we don't forget dtors when a:szBase==''
    if a:szTypeName!=''
        " Scope search
        let szTagQuery = '^' . a:szTypeName . '::' . a:szBase . '\~\?\w\+$'
    else
        " Global search
        let szTagQuery = '^' . a:szBase . '\w\+$'
    endif

    " If the result is already in the cache we return it
    if cacheResult[0]
        let result = s:CACHE_TAG_POPUP_ITEMS[ cacheResult[1] ]
        if cacheResult[0] == 2
            let result = filter(copy(result), 'v:val.name =~ szTagQuery' )
        endif
        return result
    endif

    try
        " Getting tags
        let result = omni#common#utils#TagList(szTagQuery)

        " We extend tag items to popup items
        call map(result, 's:ExtendTagItemToPopupItem(v:val, a:szTypeName)')

        " We store the result in a cache
        if cacheResult[1] != ''
            let s:CACHE_TAG_POPUP_ITEMS[ cacheResult[1] ] = result
        endif
    catch /^TagList:UserInterrupt$/
    endtry

    return result
endfunc

" Find complete matches for a completion on the global scope
function! s:SearchGlobalMembers(szBase)
    if a:szBase != ''
        let tagPopupList = s:TagPopupList('', a:szBase)
        let tagPopupList = filter(copy(tagPopupList), g:omni#cpp#utils#szFilterGlobalScope)
        call extend(s:popupItemResultList, tagPopupList)
    endif
endfunc

" Search class, struct, union members
" @param resolvedTagItem: a resolved tag item
" @param szBase: string base
" @return list of tag items extended to popup items
function! s:SearchMembers(resolvedTagItem, szBase)
    let result = []
    if a:resolvedTagItem == {}
        return result
    endif

    " Get type info without the starting '::'
    let szTagName = omni#cpp#utils#ExtractTypeInfoFromTag(a:resolvedTagItem)[2:]

    " Unnamed type case. A tag item representing an unnamed type is a variable 
    " ('v') a member ('m') or a typedef ('t')
    if index(['v', 't', 'm'], a:resolvedTagItem.kind[0])>=0 && has_key(a:resolvedTagItem, 'typeref')
        " We remove the 'struct:' or 'class:' etc...
        let szTagName = substitute(a:resolvedTagItem.typeref, '^\w\+:', '', 'g')
    endif

    return copy(s:TagPopupList(szTagName, a:szBase))
endfunc

" Return if the tag env has changed
function! s:HasTagEnvChanged()
    if s:CACHE_TAG_ENV == &tags
        return 0
    else
        let s:CACHE_TAG_ENV = &tags
        return 1
    endif
endfunc

" Return if a tag file has changed in tagfiles()
function! s:HasATagFileOrTagEnvChanged()
    if s:HasTagEnvChanged()
        let s:CACHE_TAG_FILES = {}
        return 1
    endif

    let result = 0
    for tagFile in tagfiles()
        if tagFile == ""
            continue
        endif

        if has_key(s:CACHE_TAG_FILES, tagFile)
            let currentFiletime = getftime(tagFile)
            if currentFiletime > s:CACHE_TAG_FILES[tagFile]
                " The file has changed, updating the cache
                let s:CACHE_TAG_FILES[tagFile] = currentFiletime
                let result = 1
            endif
        else
            " We store the time of the file
            let s:CACHE_TAG_FILES[tagFile] = getftime(tagFile)
            let result = 1
        endif
    endfor
    return result
endfunc
" Initialization
call s:HasATagFileOrTagEnvChanged()

" Filter same function signatures of base classes
function! s:FilterOverloadedFunctions(tagPopupList)
    let result = []
    for tagPopupItem in a:tagPopupList
        if has_key(tagPopupItem, 'kind') && index(['f', 'p'], tagPopupItem.kind[0])>=0 && has_key(tagPopupItem, 'signature')
            if !has_key(s:CACHE_OVERLOADED_FUNCTIONS, tagPopupItem.word . tagPopupItem.signature)
                let s:CACHE_OVERLOADED_FUNCTIONS[tagPopupItem.word . tagPopupItem.signature] = 1
                call extend(result, [tagPopupItem])
            endif
        else
            call extend(result, [tagPopupItem])
        endif
    endfor
    return result
endfunc

" Access filter
function! s:GetAccessFilter(szFilter, szAccessFilter)
    let szFilter = a:szFilter
    if g:OmniCpp_DisplayMode == 0
        if a:szAccessFilter == 'public'
            " We only get public members
            let szFilter .= "&& v:val.access == 'public'"
        elseif a:szAccessFilter == 'protected'
            " We get public and protected members
            let szFilter .= "&& v:val.access != 'private'"
        endif
    endif
    return szFilter
endfunc

" Filter class members in the popup menu after a completion with -> or .
function! s:FilterClassMembers(tagPopupList, szAccessFilter)
    let szFilter = "(!has_key(v:val, 'friendfunc') && !has_key(v:val, 'ctor') && has_key(v:val, 'kind') && index(['m', 'p', 'f'], v:val.kind[0])>=0 && has_key(v:val, 'access'))"
    call filter(a:tagPopupList, s:GetAccessFilter(szFilter, a:szAccessFilter))
    call extend(s:popupItemResultList, s:FilterOverloadedFunctions(a:tagPopupList))
endfunc

" Filter class scope members in the popup menu after a completion with ::
" We only display attribute and functions members that
" have an access information. We also display nested
" class, struct, union, and enums, typedefs
function! s:FilterClassScopeMembers(tagPopupList, szAccessFilter)
    let szFilter = "!has_key(v:val, 'friendfunc') && has_key(v:val, 'kind') && (index(['m', 'p', 'f'], v:val.kind[0])>=0 && has_key(v:val, 'access'))"
    let szFilter = s:GetAccessFilter(szFilter, a:szAccessFilter)
    let szFilter .= "|| index(['c','e','g','s','t','u'], v:val.kind[0])>=0"
    call filter(a:tagPopupList, szFilter)
    call extend(s:popupItemResultList, s:FilterOverloadedFunctions(a:tagPopupList))
endfunc

" Filter static class members in the popup menu
function! s:FilterStaticClassMembers(tagPopupList, szAccessFilter)
    let szFilter = "!has_key(v:val, 'friendfunc') && has_key(v:val, 'kind') && (index(['m', 'p', 'f'], v:val.kind[0])>=0 && has_key(v:val, 'access') && match(v:val.cmd, '\\Cstatic')!=-1)"
    let szFilter = s:GetAccessFilter(szFilter, a:szAccessFilter)
    let szFilter = szFilter . "|| index(['c','e','g','n','s','t','u','v'], v:val.kind[0])>=0"
    call filter(a:tagPopupList, szFilter)
    call extend(s:popupItemResultList, s:FilterOverloadedFunctions(a:tagPopupList))
endfunc

" Filter scope members in the popup menu
function! s:FilterNamespaceScopeMembers(tagPopupList)
    call extend(s:popupItemResultList, a:tagPopupList)
endfunc

" Init data at the start of completion
function! s:InitComplete()
    " Reset the popup item list
    let s:popupItemResultList = []
    let s:CACHE_OVERLOADED_FUNCTIONS = {}

    " Reset includes cache when the current working directory has changed
    let szCurrentWorkingDir = getcwd()
    if s:szCurrentWorkingDir != szCurrentWorkingDir
        let s:szCurrentWorkingDir = szCurrentWorkingDir
        let g:omni#cpp#includes#CACHE_INCLUDES = {}
        let g:omni#cpp#includes#CACHE_FILE_TIME = {}
    endif

    " Has preview window ?
    let s:hasPreviewWindow = match(&completeopt, 'preview')>=0

    let bResetCache = 0

    " Reset tag env or tag files dependent caches
    if s:HasATagFileOrTagEnvChanged()
        let bResetCache = 1
    endif

    if  (s:OmniCpp_ShowScopeInAbbr !=  g:OmniCpp_ShowScopeInAbbr)
        \|| (s:OmniCpp_ShowPrototypeInAbbr != g:OmniCpp_ShowPrototypeInAbbr)
        \|| (s:OmniCpp_ShowAccess != g:OmniCpp_ShowAccess)

        let s:OmniCpp_ShowScopeInAbbr = g:OmniCpp_ShowScopeInAbbr
        let s:OmniCpp_ShowPrototypeInAbbr = g:OmniCpp_ShowPrototypeInAbbr
        let s:OmniCpp_ShowAccess = g:OmniCpp_ShowAccess
        let bResetCache = 1
    endif

    if s:hasPreviewWindow != s:hasPreviewWindowOld
        let s:hasPreviewWindowOld = s:hasPreviewWindow
        let bResetCache = 1
    endif

    if bResetCache
        let g:omni#cpp#namespaces#CacheResolve = {}
        let s:CACHE_TAG_POPUP_ITEMS = {}
        let g:omni#cpp#utils#CACHE_TAG_INHERITS = {}
        call garbagecollect()
    endif

    " Check for updates
    for szIncludeName in keys(g:omni#cpp#includes#CACHE_INCLUDES)
        let fTime = getftime(szIncludeName)
        let bNeedUpdate = 0
        if has_key(g:omni#cpp#includes#CACHE_FILE_TIME, szIncludeName)
            if fTime != g:omni#cpp#includes#CACHE_FILE_TIME[szIncludeName]
                let bNeedUpdate = 1
            endif
        else
            let g:omni#cpp#includes#CACHE_FILE_TIME[szIncludeName] = fTime
            let bNeedUpdate = 1
        endif
        
        if bNeedUpdate
            " We have to update include list and namespace map of this file
            call omni#cpp#includes#GetList(szIncludeName, 1)
            call omni#cpp#namespaces#GetMapFromBuffer(szIncludeName, 1)
        endif
    endfor

    let s:bDoNotComplete = 0
endfunc


" This function is used for the 'omnifunc' option.
function! omni#cpp#complete#Main(findstart, base)
    if a:findstart
        "call omni#common#debug#Start()

        call s:InitComplete()

        " Note: if s:bMayComplete==1 g:omni#cpp#items#data is build by MayComplete functions
        if !s:bMayComplete
            " If the cursor is in a comment we go out
            if omni#cpp#utils#IsCursorInCommentOrString()
                " Returning -1 is not enough we have to set a variable to let
                " the second call of omni#cpp#complete knows that the
                " cursor was in a comment
                " Why is there a second call when the first call returns -1 ?
                let s:bDoNotComplete = 1
                return -1
            endif

            " We get items here (whend a:findstart==1) because GetItemsToComplete()
            " depends on the cursor position.
            " When a:findstart==0 the cursor position is modified
            let g:omni#cpp#items#data = omni#cpp#items#Get(omni#cpp#utils#TokenizeCurrentInstruction())
        endif

        " Get contexts stack
        let s:contextStack = omni#cpp#namespaces#GetContexts()

        " Reinit of may complete indicator
        let s:bMayComplete = 0
        return s:FindStartPositionOfCompletion()
    endif

    " If the cursor is in a comment we return an empty result
    if s:bDoNotComplete
        let s:bDoNotComplete = 0
        return []
    endif

    if len(g:omni#cpp#items#data)==0
        " A) CURRENT_SCOPE_COMPLETION_MODE

        " 1) Displaying data of each context
        let szAccessFilter = 'all'
        for szCurrentContext in s:contextStack
            if szCurrentContext == '::'
                continue
            endif

            let resolvedTagItem = omni#cpp#utils#GetResolvedTagItem(s:contextStack, omni#cpp#utils#CreateTypeInfo(szCurrentContext))
            if resolvedTagItem != {}
                " We don't search base classes because bases classes are
                " already in the context stack
                let tagPopupList = s:SearchMembers(resolvedTagItem, a:base)
                if index(['c','s'], resolvedTagItem.kind[0])>=0
                    " It's a class or struct
                    call s:FilterClassScopeMembers(tagPopupList, szAccessFilter)
                    let szAccessFilter = 'protected'
                else
                    " It's a namespace or union, we display all members
                    call s:FilterNamespaceScopeMembers(tagPopupList)
                endif
            endif
        endfor

        " 2) Displaying global scope members
        if g:OmniCpp_GlobalScopeSearch
            call s:SearchGlobalMembers(a:base)
        endif
    else
        let typeInfo = omni#cpp#items#ResolveItemsTypeInfo(s:contextStack, g:omni#cpp#items#data)

        if typeInfo != {}
            if g:omni#cpp#items#data[-1].kind == 'itemScope'
                " B) SCOPE_COMPLETION_MODE
                if omni#cpp#utils#GetTypeInfoString(typeInfo)==''
                    call s:SearchGlobalMembers(a:base)
                else
                    for resolvedTagItem in omni#cpp#utils#GetResolvedTags(s:contextStack, typeInfo)
                        let tagPopupList = s:SearchMembers(resolvedTagItem, a:base)
                        if index(['c','s'], resolvedTagItem.kind[0])>=0
                            let szTypeInfo = omni#cpp#utils#ExtractTypeInfoFromTag(resolvedTagItem)
                            if g:OmniCpp_DisplayMode==0
                                " We want to complete a class or struct
                                " If this class is a base class so we display all class members
                                if index(s:contextStack, szTypeInfo)<0
                                    let szAccessFilter = 'public'
                                    call s:FilterStaticClassMembers(tagPopupList, szAccessFilter)
                                else
                                    let szAccessFilter = (s:contextStack[0] == szTypeInfo)? 'all' : 'protected'
                                    call s:FilterClassScopeMembers(tagPopupList, szAccessFilter)
                                endif
                            else
                                if index(s:contextStack, szTypeInfo)<0
                                    let szAccessFilter = 'public'
                                else
                                    let szAccessFilter = (s:contextStack[0] == szTypeInfo)? 'all' : 'protected'
                                endif
                                call s:FilterClassScopeMembers(tagPopupList, szAccessFilter)
                            endif
                        else
                            " We want to complete a namespace
                            call s:FilterNamespaceScopeMembers(tagPopupList)
                        endif
                    endfor
                endif
            else
                " C) CLASS_MEMBERS_COMPLETION_MODE
                for resolvedTagItem in omni#cpp#utils#GetResolvedTags(s:contextStack, typeInfo)
                    let szTypeInfo = omni#cpp#utils#ExtractTypeInfoFromTag(resolvedTagItem)
                    if index(s:contextStack, szTypeInfo)<0
                        let szAccessFilter = 'public'
                    else
                        let szAccessFilter = (s:contextStack[0] == szTypeInfo)? 'all' : 'protected'
                    endif
                    call s:FilterClassMembers(s:SearchMembers(resolvedTagItem, a:base), szAccessFilter)
                endfor
            endif
        endif
    endif

    "call omni#common#debug#End()

    return s:popupItemResultList
endfunc
