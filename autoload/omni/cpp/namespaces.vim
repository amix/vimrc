" Description: Omni completion script for cpp files
" Maintainer:  Vissale NEANG
" Last Change: 26 sept. 2007

let g:omni#cpp#namespaces#CacheResolve = {}
let g:omni#cpp#namespaces#CacheUsing = {}
" TODO: For the next release
"let g:omni#cpp#namespaces#CacheAlias = {}

" Get the using namespace list from a line
function! s:GetNamespaceAliasListFromLine(szLine)
    let result = {}
    let tokens = omni#cpp#tokenizer#Tokenize(a:szLine)
    let szAlias = ''
    let szNamespace = ''
    let state = 0
    for token in tokens
        if state==0
            let szAlias = ''
            let szNamespace = ''
            if token.value == '/*'
                let state = 1
            elseif token.value == '//'
                " It's a comment
                let state = -1
                break
            elseif token.value == 'namespace'
                let state = 2
            endif
        elseif state==1
            if token.value == '*/'
                let state=0
            endif
        elseif state==2
            if token.kind == 'cppWord'
                let szAlias .= token.value
                let state = 3
            else
                let state = -1
                break
            endif
        elseif state == 3
            if token.value == '='
                let state = 4
            else
                let state = -1
                break
            endif
        elseif state == 4
            if token.value == '::'
                let szNamespace .= token.value
                let state = 5
            elseif token.kind == 'cppWord'
                let szNamespace .= token.value
                let state = 6
                " Maybe end of tokens
            endif
        elseif state==5
            if token.kind == 'cppWord'
                let szNamespace .= token.value
                let state = 6
                " Maybe end of tokens
            else
                " Error, we can't have 'namespace ALIAS = Something::'
                let state = -1
                break
            endif
        elseif state==6
            if token.value == '::'
                let szNamespace .= token.value
                let state = 5
            else
                call extend(result, {szAlias : szNamespace})
                let state = 0
            endif
        endif
    endfor

    if state == 6
        call extend(result, {szAlias : szNamespace})
    endif

    return result
endfunc

" Get the using namespace list from a line
function! s:GetNamespaceListFromLine(szLine)
    let result = []
    let tokens = omni#cpp#tokenizer#Tokenize(a:szLine)
    let szNamespace = ''
    let state = 0
    for token in tokens
        if state==0
            let szNamespace = ''
            if token.value == '/*'
                let state = 1
            elseif token.value == '//'
                " It's a comment
                let state = -1
                break
            elseif token.value == 'using'
                let state = 2
            endif
        elseif state==1
            if token.value == '*/'
                let state=0
            endif
        elseif state==2
            if token.value == 'namespace'
                let state = 3
            else
                " Error, 'using' must be followed by 'namespace'
                let state = -1
                break
            endif
        elseif state==3
            if token.value == '::'
                let szNamespace .= token.value
                let state = 4
            elseif token.kind == 'cppWord'
                let szNamespace .= token.value
                let state = 5
                " Maybe end of tokens
            endif
        elseif state==4
            if token.kind == 'cppWord'
                let szNamespace .= token.value
                let state = 5
                " Maybe end of tokens
            else
                " Error, we can't have 'using namespace Something::'
                let state = -1
                break
            endif
        elseif state==5
            if token.value == '::'
                let szNamespace .= token.value
                let state = 4
            else
                call extend(result, [szNamespace])
                let state = 0
            endif
        endif
    endfor

    if state == 5
        call extend(result, [szNamespace])
    endif

    return result
endfunc

" Get the namespace list from a namespace map
function! s:GetUsingNamespaceListFromMap(namespaceMap, ...)
    let stopLine = 0
    if a:0>0
        let stopLine = a:1
    endif

    let result = []
    let keys = sort(keys(a:namespaceMap), 'omni#common#utils#CompareNumber')
    for i in keys
        if stopLine != 0 && i > stopLine
            break
        endif
        call extend(result, a:namespaceMap[i])
    endfor
    return result
endfunc

" Get global using namespace list from the current buffer
function! omni#cpp#namespaces#GetListFromCurrentBuffer(...)
    let namespaceMap = s:GetAllUsingNamespaceMapFromCurrentBuffer()
    let result = []
    if namespaceMap != {}
        let result = s:GetUsingNamespaceListFromMap(namespaceMap, (a:0 > 0)? a:1 : line('.'))
    endif
    return result
endfunc

" Get global using namespace map from the current buffer and include files recursively
function! s:GetAllUsingNamespaceMapFromCurrentBuffer(...)
    let includeGuard = (a:0>0)? a:1 : {}

    let szBufferName = getreg("%")
    let szFilePath = omni#cpp#utils#ResolveFilePath(szBufferName)
    let szFilePath = (szFilePath=='')? szBufferName : szFilePath

    let namespaceMap = {}
    if has_key(includeGuard, szFilePath)
        return namespaceMap
    else
        let includeGuard[szFilePath] = 1
    endif

    let namespaceMap = omni#cpp#namespaces#GetMapFromCurrentBuffer()

    if g:OmniCpp_NamespaceSearch != 2
        " We don't search included files if OmniCpp_NamespaceSearch != 2
        return namespaceMap
    endif

    for inc in omni#cpp#includes#GetList()
        let lnum = inc.pos[0]
        let tmpMap = s:GetAllUsingNamespaceMapFromFile(inc.include, includeGuard)
        if tmpMap != {}
            if has_key(namespaceMap, lnum)
                call extend(namespaceMap[lnum], s:GetUsingNamespaceListFromMap(tmpMap))
            else
                let namespaceMap[lnum] = s:GetUsingNamespaceListFromMap(tmpMap)
            endif
        endif
    endfor

    return namespaceMap
endfunc

" Get global using namespace map from a file and include files recursively
function! s:GetAllUsingNamespaceMapFromFile(szFilePath, ...)
    let includeGuard = {}
    if a:0 >0
        let includeGuard = a:1
    endif

    let szFilePath = omni#cpp#utils#ResolveFilePath(a:szFilePath)
    let szFilePath = (szFilePath=='')? a:szFilePath : szFilePath

    let namespaceMap = {}
    if has_key(includeGuard, szFilePath)
        return namespaceMap
    else
        let includeGuard[szFilePath] = 1
    endif

    " If g:OmniCpp_NamespaceSearch == 1 (search namespaces only in the current
    " buffer) we don't use cache for the current buffer
    let namespaceMap = omni#cpp#namespaces#GetMapFromBuffer(szFilePath, g:OmniCpp_NamespaceSearch==1)

    if g:OmniCpp_NamespaceSearch != 2
        " We don't search included files if OmniCpp_NamespaceSearch != 2
        return namespaceMap
    endif

    for inc in omni#cpp#includes#GetList(szFilePath)
        let lnum = inc.pos[0]
        let tmpMap = s:GetAllUsingNamespaceMapFromFile(inc.include, includeGuard)
        if tmpMap != {}
            if has_key(namespaceMap, lnum)
                call extend(namespaceMap[lnum], s:GetUsingNamespaceListFromMap(tmpMap))
            else
                let namespaceMap[lnum] = s:GetUsingNamespaceListFromMap(tmpMap)
            endif
        endif
    endfor

    return namespaceMap
endfunc

" Get global using namespace map from a the current buffer
function! omni#cpp#namespaces#GetMapFromCurrentBuffer()
    let namespaceMap = {}
    let originalPos = getpos('.')

    call setpos('.', [0, 1, 1, 0])
    let curPos = [1,1]
    while curPos != [0,0]
        let curPos = searchpos('\C^using\s\+namespace', 'W')
        if curPos != [0,0]
            let szLine = getline('.')
            let startPos = curPos[1]
            let endPos = match(szLine, ';', startPos-1)
            if endPos!=-1
                " We get the namespace list from the line
                let namespaceMap[curPos[0]] = s:GetNamespaceListFromLine(szLine)
            endif
        endif
    endwhile

    call setpos('.', originalPos)
    return namespaceMap
endfunc

" Get global using namespace map from a file
function! omni#cpp#namespaces#GetMapFromBuffer(szFilePath, ...)
    let bUpdate = 0
    if a:0 > 0
        let bUpdate = a:1
    endif

    let szFilePath = omni#cpp#utils#ResolveFilePath(a:szFilePath)
    let szFilePath = (szFilePath=='')? a:szFilePath : szFilePath

    if !bUpdate && has_key(g:omni#cpp#namespaces#CacheUsing, szFilePath)
        return copy(g:omni#cpp#namespaces#CacheUsing[szFilePath])
    endif

    let namespaceMap = {}
    " The file exists, we get the global namespaces in this file
    let szFixedPath = escape(szFilePath, g:omni#cpp#utils#szEscapedCharacters)
    execute 'silent! lvimgrep /\C^using\s\+namespace/gj '.szFixedPath

    " key = line number
    " value = list of namespaces
    let listQuickFix = getloclist(0)
    for qf in listQuickFix
        let szLine = qf.text
        let startPos = qf.col
        let endPos = match(szLine, ';', startPos-1)
        if endPos!=-1
            " We get the namespace list from the line
            let namespaceMap[qf.lnum] = s:GetNamespaceListFromLine(szLine)
        endif
    endfor

    if szFixedPath != ''
        let g:omni#cpp#namespaces#CacheUsing[szFixedPath] = namespaceMap
    endif

    return copy(namespaceMap)
endfunc

" Get the stop position when searching for local variables
function! s:GetStopPositionForLocalSearch()
    " Stop position when searching a local variable
    let originalPos = getpos('.')
    let origPos = originalPos[1:2]
    let stopPosition = origPos
    let curPos = origPos
    while curPos !=[0,0]
        let stopPosition = curPos
        let curPos = searchpairpos('{', '', '}', 'bW', g:omni#cpp#utils#expIgnoreComments)
    endwhile
    call setpos('.', originalPos)

    return stopPosition
endfunc

" Get namespaces alias used at the cursor postion in a vim buffer
" Note: The result depends on the current cursor position
" @return
"   -   Map of namespace alias
function! s:GetNamespaceAliasMap()
    " We store the cursor position because searchpairpos() moves the cursor
    let result = {}
    let originalPos = getpos('.')
    let origPos = originalPos[1:2]

    let stopPos = s:GetStopPositionForLocalSearch()
    let stopLine = stopPos[0]
    let curPos = origPos
    let lastLine = 0 
    let nextStopLine = origPos[0]
    let szReAlias = '\Cnamespace\s\+\w\+\s\+='
    while curPos !=[0,0]
        let curPos = searchpos('}\|\('. szReAlias .'\)', 'bW',stopLine)
        if curPos!=[0,0] && curPos[0]!=lastLine
            let lastLine = curPos[0]

            let szLine = getline('.')
            if origPos[0] == curPos[0]
                " We get the line until cursor position
                let szLine = szLine[:origPos[1]]
            endif

            let szLine = omni#cpp#utils#GetCodeFromLine(szLine)
            if match(szLine, szReAlias)<0
                " We found a '}'
                let curPos = searchpairpos('{', '', '}', 'bW', g:omni#cpp#utils#expIgnoreComments)
            else
                " We get the namespace alias from the line
                call extend(result, s:GetNamespaceAliasListFromLine(szLine))
                let nextStopLine = curPos[0]
            endif
        endif
    endwhile

    " Setting the cursor to the original position
    call setpos('.', originalPos)

    call s:ResolveAliasKeys(result)
    return result
endfunc

" Resolve an alias
" eg: namespace IAmAnAlias1 = Ns1
" eg: namespace IAmAnAlias2 = IAmAnAlias1::Ns2
" => IAmAnAlias2 = Ns1::Ns2
function! s:ResolveAliasKey(mapNamespaceAlias, szAlias)
    let szResult = a:mapNamespaceAlias[a:szAlias]
    " ::Ns1::Ns2::Ns3 => ['Ns1', 'Ns2', 'Ns3']
    let listNamespace = split(szResult, '::')
    if len(listNamespace)
        " szBeginPart = 'Ns1'
        let szBeginPart = remove(listNamespace, 0)

        " Is 'Ns1' an alias ?
        if has_key(a:mapNamespaceAlias, szBeginPart) && szBeginPart != a:szAlias
            " Resolving alias 'Ns1'
            " eg: Ns1 = NsResolved
            let szResult = s:ResolveAliasKey(a:mapNamespaceAlias, szBeginPart)
            " szEndPart = 'Ns2::Ns3'
            let szEndPart = join(listNamespace, '::')
            if szEndPart != ''
                " Concatenation => szResult = 'NsResolved::Ns2::Ns3'
                let szResult .= '::' . szEndPart
            endif
        endif
    endif
    return szResult
endfunc

" Resolve all keys in the namespace alias map
function! s:ResolveAliasKeys(mapNamespaceAlias)
    let mapNamespaceAlias = a:mapNamespaceAlias
    call map(mapNamespaceAlias, 's:ResolveAliasKey(mapNamespaceAlias, v:key)')
endfunc

" Resolve namespace alias
function! omni#cpp#namespaces#ResolveAlias(mapNamespaceAlias, szNamespace)
    let szResult = a:szNamespace
    " ::Ns1::Ns2::Ns3 => ['Ns1', 'Ns2', 'Ns3']
    let listNamespace = split(a:szNamespace, '::')
    if len(listNamespace)
        " szBeginPart = 'Ns1'
        let szBeginPart = remove(listNamespace, 0)

        " Is 'Ns1' an alias ?
        if has_key(a:mapNamespaceAlias, szBeginPart)
            " Resolving alias 'Ns1'
            " eg: Ns1 = NsResolved
            let szResult = a:mapNamespaceAlias[szBeginPart]
            " szEndPart = 'Ns2::Ns3'
            let szEndPart = join(listNamespace, '::')
            if szEndPart != ''
                " Concatenation => szResult = 'NsResolved::Ns2::Ns3'
                let szResult .= '::' . szEndPart
            endif

            " If a:szNamespace starts with '::' we add '::' to the beginning
            " of the result
            if match(a:szNamespace, '^::')>=0
                let szResult = omni#cpp#utils#SimplifyScope('::' .  szResult)
            endif
        endif
    endif
    return szResult
endfunc

" Resolve namespace alias
function! s:ResolveAliasInNamespaceList(mapNamespaceAlias, listNamespaces)
    call map(a:listNamespaces, 'omni#cpp#namespaces#ResolveAlias(a:mapNamespaceAlias, v:val)')
endfunc

" Get namespaces used at the cursor postion in a vim buffer
" Note: The result depends on the current cursor position
" @return
"   -   List of namespace used in the reverse order
function! omni#cpp#namespaces#GetUsingNamespaces()
    " We have to get local using namespace declarations
    " We need the current cursor position and the position of the start of the
    " current scope

    " We store the cursor position because searchpairpos() moves the cursor
    let result = []
    let originalPos = getpos('.')
    let origPos = originalPos[1:2]

    let stopPos = s:GetStopPositionForLocalSearch()

    let stopLine = stopPos[0]
    let curPos = origPos
    let lastLine = 0 
    let nextStopLine = origPos[0]
    while curPos !=[0,0]
        let curPos = searchpos('\C}\|\(using\s\+namespace\)', 'bW',stopLine)
        if curPos!=[0,0] && curPos[0]!=lastLine
            let lastLine = curPos[0]

            let szLine = getline('.')
            if origPos[0] == curPos[0]
                " We get the line until cursor position
                let szLine = szLine[:origPos[1]]
            endif

            let szLine = omni#cpp#utils#GetCodeFromLine(szLine)
            if match(szLine, '\Cusing\s\+namespace')<0
                " We found a '}'
                let curPos = searchpairpos('{', '', '}', 'bW', g:omni#cpp#utils#expIgnoreComments)
            else
                " We get the namespace list from the line
                let result = s:GetNamespaceListFromLine(szLine) + result
                let nextStopLine = curPos[0]
            endif
        endif
    endwhile

    " Setting the cursor to the original position
    call setpos('.', originalPos)

    " 2) Now we can get all global using namespace declaration from the
    " beginning of the file to nextStopLine
    let result = omni#cpp#namespaces#GetListFromCurrentBuffer(nextStopLine) + result

    " Resolving alias in the namespace list
    " TODO: For the next release
    "let g:omni#cpp#namespaces#CacheAlias= s:GetNamespaceAliasMap()
    "call s:ResolveAliasInNamespaceList(g:omni#cpp#namespaces#CacheAlias, result)

    return ['::'] + result
endfunc

" Resolve a using namespace regarding the current context
" For each namespace used:
"   -   We get all possible contexts where the namespace
"       can be define
"   -   We do a comparison test of each parent contexts with the current
"       context list
"           -   If one and only one parent context is present in the
"               current context list we add the namespace in the current
"               context
"           -   If there is more than one of parent contexts in the
"               current context the namespace is ambiguous
" @return
"   - result item
"       - kind = 0|1
"           - 0 = unresolved or error
"           - 1 = resolved
"       - value = resolved namespace
function! s:ResolveNamespace(namespace, mapCurrentContexts)
    let result = {'kind':0, 'value': ''}

    " If the namespace is already resolved we add it in the list of 
    " current contexts
    if match(a:namespace, '^::')>=0
        let result.kind = 1
        let result.value = a:namespace
        return result
    elseif match(a:namespace, '\w\+::\w\+')>=0
        let mapCurrentContextsTmp = copy(a:mapCurrentContexts) 
        let resolvedItem = {}
        for nsTmp in  split(a:namespace, '::')
            let resolvedItem = s:ResolveNamespace(nsTmp, mapCurrentContextsTmp)
            if resolvedItem.kind
                " Note: We don't extend the map
                let mapCurrentContextsTmp = {resolvedItem.value : 1}
            else
                break
            endif
        endfor
        if resolvedItem!={} && resolvedItem.kind
            let result.kind = 1
            let result.value = resolvedItem.value
        endif
        return result
    endif

    " We get all possible parent contexts of this namespace
    let listTagsOfNamespace = []
    if has_key(g:omni#cpp#namespaces#CacheResolve, a:namespace)
        let listTagsOfNamespace = g:omni#cpp#namespaces#CacheResolve[a:namespace]
    else
        let listTagsOfNamespace = omni#common#utils#TagList('^'.a:namespace.'$')
        let g:omni#cpp#namespaces#CacheResolve[a:namespace] = listTagsOfNamespace
    endif

    if len(listTagsOfNamespace)==0
        return result
    endif
    call filter(listTagsOfNamespace, 'v:val.kind[0]=="n"')

    " We extract parent context from tags
    " We use a map to avoid multiple entries
    let mapContext = {}
    for tagItem in listTagsOfNamespace
        let szParentContext = omni#cpp#utils#ExtractScope(tagItem)
        let mapContext[szParentContext] = 1
    endfor
    let listParentContext = keys(mapContext)

    " Now for each parent context we test if the context is in the current
    " contexts list
    let listResolvedNamespace = []
    for szParentContext in listParentContext
        if has_key(a:mapCurrentContexts, szParentContext)
            call extend(listResolvedNamespace, [omni#cpp#utils#SimplifyScope(szParentContext.'::'.a:namespace)])
        endif
    endfor

    " Now we know if the namespace is ambiguous or not
    let len = len(listResolvedNamespace)
    if len==1
        " Namespace resolved
        let result.kind = 1
        let result.value = listResolvedNamespace[0]
    elseif len > 1
        " Ambiguous namespace, possible matches are in listResolvedNamespace
    else
        " Other cases
    endif
    return result
endfunc

" Resolve namespaces
"@return
"   - List of resolved namespaces
function! omni#cpp#namespaces#ResolveAll(namespacesUsed)

    " We add the default context '::'
    let contextOrder = 0
    let mapCurrentContexts  = {}

    " For each namespace used:
    "   -   We get all possible contexts where the namespace
    "       can be define
    "   -   We do a comparison test of each parent contexts with the current
    "       context list
    "           -   If one and only one parent context is present in the
    "               current context list we add the namespace in the current
    "               context
    "           -   If there is more than one of parent contexts in the
    "               current context the namespace is ambiguous
    for ns in a:namespacesUsed
        let resolvedItem = s:ResolveNamespace(ns, mapCurrentContexts)
        if resolvedItem.kind
            let contextOrder+=1
            let mapCurrentContexts[resolvedItem.value] = contextOrder
        endif
    endfor

    " Build the list of current contexts from the map, we have to keep the
    " order
    let mapReorder = {}
    for key in keys(mapCurrentContexts)
        let mapReorder[ mapCurrentContexts[key] ] = key
    endfor
    let result = []
    for key in sort(keys(mapReorder))
        call extend(result, [mapReorder[key]])
    endfor
    return result
endfunc

" Build the context stack
function! s:BuildContextStack(namespaces, szCurrentScope)
    let result = copy(a:namespaces)
    if a:szCurrentScope != '::'
        let tagItem = omni#cpp#utils#GetResolvedTagItem(a:namespaces, omni#cpp#utils#CreateTypeInfo(a:szCurrentScope))
        if has_key(tagItem, 'inherits')
            let listBaseClass = omni#cpp#utils#GetClassInheritanceList(a:namespaces, omni#cpp#utils#CreateTypeInfo(a:szCurrentScope))
            let result = listBaseClass + result
        elseif has_key(tagItem, 'kind') && index(['c', 's', 'u', 'n'], tagItem.kind[0])>=0
            call insert(result, omni#cpp#utils#ExtractTypeInfoFromTag(tagItem))
        endif
    endif
    return result
endfunc

" Returns the class scope at the current position of the cursor
" @return a string that represents the class scope
" eg: ::NameSpace1::Class1
" The returned string always starts with '::'
" Note: In term of performance it's the weak point of the script
function! s:GetClassScopeAtCursor()
    " We store the cursor position because searchpairpos() moves the cursor
    let originalPos = getpos('.')
    let endPos = originalPos[1:2]
    let listCode = []
    let result = {'namespaces': [], 'scope': ''}

    while endPos!=[0,0]
        let endPos = searchpairpos('{', '', '}', 'bW', g:omni#cpp#utils#expIgnoreComments)
        let szReStartPos = '[;{}]\|\%^'
        let startPos = searchpairpos(szReStartPos, '', '{', 'bWn', g:omni#cpp#utils#expIgnoreComments)

        " If the file starts with a comment so the startPos can be [0,0]
        " we change it to [1,1]
        if startPos==[0,0]
            let startPos = [1,1]
        endif

        " Get lines backward from cursor position to last ; or { or }
        " or when we are at the beginning of the file.
        " We store lines in listCode
        if endPos!=[0,0]
            " We remove the last character which is a '{'
            " We also remove starting { or } or ; if exits
            let szCodeWithoutComments = substitute(omni#cpp#utils#GetCode(startPos, endPos)[:-2], '^[;{}]', '', 'g')
            call insert(listCode, {'startLine' : startPos[0], 'code' : szCodeWithoutComments})
        endif
    endwhile
    " Setting the cursor to the original position
    call setpos('.', originalPos)

    let listClassScope = []
    let bResolved = 0
    let startLine = 0
    " Now we can check in the list of code if there is a function
    for code in listCode
        " We get the name of the namespace, class, struct or union
        " and we store it in listClassScope
        let tokens = omni#cpp#tokenizer#Tokenize(code.code)
        let bContinue=0
        let bAddNamespace = 0
        let state=0
        for token in tokens
            if state==0
                if index(['namespace', 'class', 'struct', 'union'], token.value)>=0
                    if token.value == 'namespace'
                        let bAddNamespace = 1
                    endif
                    let state= 1
                    " Maybe end of tokens
                endif
            elseif state==1
                if token.kind == 'cppWord'
                    " eg: namespace MyNs { class MyCl {}; }
                    " => listClassScope = [MyNs, MyCl]
                    call extend( listClassScope , [token.value] )

                    " Add the namespace in result
                    if bAddNamespace
                        call extend(result.namespaces, [token.value])
                        let bAddNamespace = 0
                    endif

                    let bContinue=1
                    break
                endif
            endif
        endfor
        if bContinue==1
            continue
        endif

        " Simple test to check if we have a chance to find a
        " class method
        let aPos = matchend(code.code, '::\s*\~*\s*\w\+\s*(')
        if aPos ==-1
            continue
        endif

        let startLine = code.startLine
        let listTmp = []
        " eg: 'void MyNamespace::MyClass::foo('
        " => tokens = ['MyClass', '::', 'MyNamespace', 'void']
        let tokens = reverse(omni#cpp#tokenizer#Tokenize(code.code[:aPos-1])[:-4])
        let state = 0
        " Reading tokens backward
        for token in tokens
            if state==0
                if token.kind=='cppWord'
                    call insert(listTmp, token.value)
                    let state=1
                endif
            elseif state==1
                if token.value=='::'
                    let state=2
                else
                    break
                endif
            elseif state==2
                if token.kind=='cppWord'
                    call insert(listTmp, token.value)
                    let state=1
                else
                    break
                endif
            endif
        endfor
        
        if len(listTmp)
            if len(listClassScope)
                let bResolved = 1
                " Merging class scopes
                " eg: current class scope = 'MyNs::MyCl1'
                " method class scope = 'MyCl1::MyCl2'
                " If we add the method class scope to current class scope
                " we'll have MyNs::MyCl1::MyCl1::MyCl2 => it's wrong
                " we want MyNs::MyCl1::MyCl2
                let index = 0
                for methodClassScope in listTmp
                    if methodClassScope==listClassScope[-1]
                        let listTmp = listTmp[index+1:]
                        break
                    else
                        let index+=1
                    endif
                endfor
            endif
            call extend(listClassScope, listTmp)
            break
        endif
    endfor

    let szClassScope = '::'
    if len(listClassScope)
        if bResolved
            let szClassScope .= join(listClassScope, '::')
        else
            let szClassScope = join(listClassScope, '::')
            
            " The class scope is not resolved, we have to check using
            " namespace declarations and search the class scope in each
            " namespace
            if startLine != 0
                let namespaces = ['::'] + omni#cpp#namespaces#GetListFromCurrentBuffer(startLine)
                let namespaces = omni#cpp#namespaces#ResolveAll(namespaces)
                let tagItem = omni#cpp#utils#GetResolvedTagItem(namespaces, omni#cpp#utils#CreateTypeInfo(szClassScope))
                if tagItem != {}
                    let szClassScope = omni#cpp#utils#ExtractTypeInfoFromTag(tagItem)
                endif
            endif
        endif
    endif

    let result.scope = szClassScope
    return result
endfunc

" Get all contexts at the cursor position
function! omni#cpp#namespaces#GetContexts()
    " Get the current class scope at the cursor, the result depends on the current cursor position
    let scopeItem = s:GetClassScopeAtCursor()
    let listUsingNamespace = copy(g:OmniCpp_DefaultNamespaces)
    call extend(listUsingNamespace, scopeItem.namespaces)
    if g:OmniCpp_NamespaceSearch && &filetype != 'c'
        " Get namespaces used in the file until the cursor position
        let listUsingNamespace = omni#cpp#namespaces#GetUsingNamespaces() + listUsingNamespace
        " Resolving namespaces, removing ambiguous namespaces
        let namespaces = omni#cpp#namespaces#ResolveAll(listUsingNamespace)
    else
        let namespaces = ['::'] + listUsingNamespace
    endif
    call reverse(namespaces)

    " Building context stack from namespaces and the current class scope
    return s:BuildContextStack(namespaces, scopeItem.scope)
endfunc
