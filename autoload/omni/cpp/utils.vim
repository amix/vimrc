" Description: Omni completion script for cpp files
" Maintainer:  Vissale NEANG
" Last Change: 26 sept. 2007

let g:omni#cpp#utils#CACHE_TAG_INHERITS = {}
let g:omni#cpp#utils#szFilterGlobalScope = "(!has_key(v:val, 'class') && !has_key(v:val, 'struct') && !has_key(v:val, 'union') && !has_key(v:val, 'namespace')"
let g:omni#cpp#utils#szFilterGlobalScope .= "&& (!has_key(v:val, 'enum') || (has_key(v:val, 'enum') && v:val.enum =~ '^\\w\\+$')))"

" Expression used to ignore comments
" Note: this expression drop drastically the performance
"let omni#cpp#utils#expIgnoreComments = 'match(synIDattr(synID(line("."), col("."), 1), "name"), '\CcComment')!=-1'
" This one is faster but not really good for C comments
let omni#cpp#utils#reIgnoreComment = escape('\/\/\|\/\*\|\*\/', '*/\')
let omni#cpp#utils#expIgnoreComments = 'getline(".") =~ g:omni#cpp#utils#reIgnoreComment'

" Characters to escape in a filename for vimgrep
"TODO: Find more characters to escape
let omni#cpp#utils#szEscapedCharacters = ' %#'

" Resolve the path of the file
" TODO: absolute file path
function! omni#cpp#utils#ResolveFilePath(szFile)
    let result = ''
    let listPath = split(globpath(&path, a:szFile), "\n")
    if len(listPath)
        let result = listPath[0]
    endif
    return simplify(result)
endfunc

" Get code without comments and with empty strings
" szSingleLine must not have carriage return
function! omni#cpp#utils#GetCodeFromLine(szSingleLine)
    " We set all strings to empty strings, it's safer for 
    " the next of the process
    let szResult = substitute(a:szSingleLine, '".*"', '""', 'g')

    " Removing c++ comments, we can use the pattern ".*" because
    " we are modifying a line
    let szResult = substitute(szResult, '\/\/.*', '', 'g')

    " Now we have the entire code in one line and we can remove C comments
    return s:RemoveCComments(szResult)
endfunc

" Remove C comments on a line
function! s:RemoveCComments(szLine)
    let result = a:szLine

    " We have to match the first '/*' and first '*/'
    let startCmt = match(result, '\/\*')
    let endCmt = match(result, '\*\/')
    while startCmt!=-1 && endCmt!=-1 && startCmt<endCmt
        if startCmt>0
            let result = result[ : startCmt-1 ] . result[ endCmt+2 : ]
        else
            " Case where '/*' is at the start of the line
            let result = result[ endCmt+2 : ]
        endif
        let startCmt = match(result, '\/\*')
        let endCmt = match(result, '\*\/')
    endwhile
    return result
endfunc

" Get a c++ code from current buffer from [lineStart, colStart] to 
" [lineEnd, colEnd] without c++ and c comments, without end of line
" and with empty strings if any
" @return a string
function! omni#cpp#utils#GetCode(posStart, posEnd)
    let posStart = a:posStart
    let posEnd = a:posEnd
    if a:posStart[0]>a:posEnd[0]
        let posStart = a:posEnd
        let posEnd = a:posStart
    elseif a:posStart[0]==a:posEnd[0] && a:posStart[1]>a:posEnd[1]
        let posStart = a:posEnd
        let posEnd = a:posStart
    endif

    " Getting the lines
    let lines = getline(posStart[0], posEnd[0])
    let lenLines = len(lines)

    " Formatting the result
    let result = ''
    if lenLines==1
        let sStart = posStart[1]-1
        let sEnd = posEnd[1]-1
        let line = lines[0]
        let lenLastLine = strlen(line)
        let sEnd = (sEnd>lenLastLine)?lenLastLine : sEnd
        if sStart >= 0
            let result = omni#cpp#utils#GetCodeFromLine(line[ sStart : sEnd ])
        endif
    elseif lenLines>1
        let sStart = posStart[1]-1
        let sEnd = posEnd[1]-1
        let lenLastLine = strlen(lines[-1])
        let sEnd = (sEnd>lenLastLine)?lenLastLine : sEnd
        if sStart >= 0
            let lines[0] = lines[0][ sStart : ]
            let lines[-1] = lines[-1][ : sEnd ]
            for aLine in lines
                let result = result . omni#cpp#utils#GetCodeFromLine(aLine)." "
            endfor
            let result = result[:-2]
        endif
    endif

    " Now we have the entire code in one line and we can remove C comments
    return s:RemoveCComments(result)
endfunc

" Extract the scope (context) of a tag item
" eg: ::MyNamespace
" @return a string of the scope. a scope from tag always starts with '::'
function! omni#cpp#utils#ExtractScope(tagItem)
    let listKindScope = ['class', 'struct', 'union', 'namespace', 'enum']
    let szResult = '::'
    for scope in listKindScope
        if has_key(a:tagItem, scope)
            let szResult = szResult . a:tagItem[scope]
            break
        endif
    endfor
    return szResult
endfunc

" Simplify scope string, remove consecutive '::' if any
function! omni#cpp#utils#SimplifyScope(szScope)
    let szResult = substitute(a:szScope, '\(::\)\+', '::', 'g')
    if szResult=='::'
        return szResult
    else
        return substitute(szResult, '::$', '', 'g')
    endif
endfunc

" Check if the cursor is in comment
function! omni#cpp#utils#IsCursorInCommentOrString()
    return match(synIDattr(synID(line("."), col(".")-1, 1), "name"), '\C\<cComment\|\<cCppString\|\<cIncluded')>=0
endfunc

" Tokenize the current instruction until the cursor position.
" @return list of tokens
function! omni#cpp#utils#TokenizeCurrentInstruction(...)
    let szAppendText = ''
    if a:0>0
        let szAppendText = a:1
    endif

    let startPos = searchpos('[;{}]\|\%^', 'bWn')
    let curPos = getpos('.')[1:2]
    " We don't want the character under the cursor
    let column = curPos[1]-1
    let curPos[1] = (column<1)?1:column
    return omni#cpp#tokenizer#Tokenize(omni#cpp#utils#GetCode(startPos, curPos)[1:] . szAppendText)
endfunc

" Tokenize the current instruction until the word under the cursor.
" @return list of tokens
function! omni#cpp#utils#TokenizeCurrentInstructionUntilWord()
    let startPos = searchpos('[;{}]\|\%^', 'bWn')

    " Saving the current cursor pos
    let originalPos = getpos('.')

    " We go at the end of the word
    execute 'normal gee'
    let curPos = getpos('.')[1:2]

    " Restoring the original cursor pos
    call setpos('.', originalPos)

    let szCode = omni#cpp#utils#GetCode(startPos, curPos)[1:]
    return omni#cpp#tokenizer#Tokenize(szCode)
endfunc

" Build parenthesis groups
" add a new key 'group' in the token
" where value is the group number of the parenthesis
" eg: (void*)(MyClass*)
"      group1  group0
" if a parenthesis is unresolved the group id is -1      
" @return a copy of a:tokens with parenthesis group
function! omni#cpp#utils#BuildParenthesisGroups(tokens)
    let tokens = copy(a:tokens)
    let kinds = {'(': '()', ')' : '()', '[' : '[]', ']' : '[]', '<' : '<>', '>' : '<>', '{': '{}', '}': '{}'}
    let unresolved = {'()' : [], '[]': [], '<>' : [], '{}' : []}
    let groupId = 0

    " Note: we build paren group in a backward way
    " because we can often have parenthesis unbalanced
    " instruction
    " eg: doSomething(_member.get()->
    for token in reverse(tokens)
        if index([')', ']', '>', '}'], token.value)>=0
            let token['group'] = groupId
            call extend(unresolved[kinds[token.value]], [token])
            let groupId+=1
        elseif index(['(', '[', '<', '{'], token.value)>=0
            if len(unresolved[kinds[token.value]])
                let tokenResolved = remove(unresolved[kinds[token.value]], -1)
                let token['group'] = tokenResolved.group
            else
                let token['group'] = -1
            endif
        endif
    endfor

    return reverse(tokens)
endfunc

" Determine if tokens represent a C cast
" @return
"   - itemCast
"   - itemCppCast
"   - itemVariable
"   - itemThis
function! omni#cpp#utils#GetCastType(tokens)
    " Note: a:tokens is not modified
    let tokens = omni#cpp#utils#SimplifyParenthesis(omni#cpp#utils#BuildParenthesisGroups(a:tokens))

    if tokens[0].value == '('
        return 'itemCast' 
    elseif index(['static_cast', 'dynamic_cast', 'reinterpret_cast', 'const_cast'], tokens[0].value)>=0
        return 'itemCppCast'
    else
        for token in tokens
            if token.value=='this'
                return 'itemThis'
            endif
        endfor
        return 'itemVariable' 
    endif
endfunc

" Remove useless parenthesis
function! omni#cpp#utils#SimplifyParenthesis(tokens)
    "Note: a:tokens is not modified
    let tokens = a:tokens
    " We remove useless parenthesis eg: (((MyClass)))
    if len(tokens)>2
        while tokens[0].value=='(' && tokens[-1].value==')' && tokens[0].group==tokens[-1].group
            let tokens = tokens[1:-2]
        endwhile
    endif
    return tokens
endfunc

" Function create a type info
function! omni#cpp#utils#CreateTypeInfo(param)
    let type = type(a:param)
    return {'type': type, 'value':a:param}
endfunc

" Extract type info from a tag item
" eg: ::MyNamespace::MyClass
function! omni#cpp#utils#ExtractTypeInfoFromTag(tagItem)
    let szTypeInfo = omni#cpp#utils#ExtractScope(a:tagItem) . '::' . substitute(a:tagItem.name, '.*::', '', 'g')
    return omni#cpp#utils#SimplifyScope(szTypeInfo)
endfunc

" Build a class inheritance list
function! omni#cpp#utils#GetClassInheritanceList(namespaces, typeInfo)
    let result = []
    for tagItem in omni#cpp#utils#GetResolvedTags(a:namespaces, a:typeInfo)
        call extend(result, [omni#cpp#utils#ExtractTypeInfoFromTag(tagItem)])
    endfor
    return result
endfunc

" Get class inheritance list where items in the list are tag items.
" TODO: Verify inheritance order
function! omni#cpp#utils#GetResolvedTags(namespaces, typeInfo)
    let result = []
    let tagItem = omni#cpp#utils#GetResolvedTagItem(a:namespaces, a:typeInfo)
    if tagItem!={}
        let szTypeInfo = omni#cpp#utils#ExtractTypeInfoFromTag(tagItem)
        if has_key(g:omni#cpp#utils#CACHE_TAG_INHERITS, szTypeInfo)
            let result = g:omni#cpp#utils#CACHE_TAG_INHERITS[szTypeInfo]
        else
            call extend(result, [tagItem])
            if has_key(tagItem, 'inherits')
                for baseClassTypeInfo in split(tagItem.inherits, ',')
                    let namespaces = [omni#cpp#utils#ExtractScope(tagItem), '::']
                    call extend(result, omni#cpp#utils#GetResolvedTags(namespaces, omni#cpp#utils#CreateTypeInfo(baseClassTypeInfo)))
                endfor
            endif
            let g:omni#cpp#utils#CACHE_TAG_INHERITS[szTypeInfo] = result
        endif
    endif
    return result
endfunc

" Get a tag item after a scope resolution and typedef resolution
function! omni#cpp#utils#GetResolvedTagItem(namespaces, typeInfo)
    let typeInfo = {}
    if type(a:typeInfo) == 1
        let typeInfo = omni#cpp#utils#CreateTypeInfo(a:typeInfo)
    else
        let typeInfo = a:typeInfo
    endif

    let result = {}
    if !omni#cpp#utils#IsTypeInfoValid(typeInfo)
        return result
    endif

    " Unnamed type case eg: '1::2'
    if typeInfo.type == 4
        " Here there is no typedef or namespace to resolve, the tagInfo.value is a tag item
        " representing a variable ('v') a member ('m') or a typedef ('t') and the typename is
        " always in global scope
        return typeInfo.value
    endif

    " Named type case eg:  'MyNamespace::MyClass'
    let szTypeInfo = omni#cpp#utils#GetTypeInfoString(typeInfo)

    " Resolving namespace alias
    " TODO: For the next release
    "let szTypeInfo = omni#cpp#namespaces#ResolveAlias(g:omni#cpp#namespaces#CacheAlias, szTypeInfo)

    if szTypeInfo=='::'
        return result
    endif

    " We can only get members of class, struct, union and namespace
    let szTagFilter = "index(['c', 's', 'u', 'n', 't'], v:val.kind[0])>=0"
    let szTagQuery = szTypeInfo

    if s:IsTypeInfoResolved(szTypeInfo)
        " The type info is already resolved, we remove the starting '::'
        let szTagQuery = substitute(szTypeInfo, '^::', '', 'g')
        if len(split(szTagQuery, '::'))==1
            " eg: ::MyClass
            " Here we have to get tags that have no parent scope
            " That's why we change the szTagFilter
            let szTagFilter .= '&& ' . g:omni#cpp#utils#szFilterGlobalScope
            let tagList = omni#common#utils#TagListNoThrow('^'.szTagQuery.'$')
            call filter(tagList, szTagFilter)
            if len(tagList)
                let result = tagList[0]
            endif
        else
            " eg: ::MyNamespace::MyClass
            let tagList = omni#common#utils#TagListNoThrow('^'.szTagQuery.'$')
            call filter(tagList, szTagFilter)

            if len(tagList)
                let result = tagList[0]
            endif
        endif
    else
        " The type is not resolved
        let tagList = omni#common#utils#TagListNoThrow('^'.szTagQuery.'$')
        call filter(tagList, szTagFilter)

        if len(tagList)
            " Resolving scope (namespace, nested class etc...)
            let szScopeOfTypeInfo = s:ExtractScopeFromTypeInfo(szTypeInfo)
            if s:IsTypeInfoResolved(szTypeInfo)
                let result = s:GetTagOfSameScope(tagList, szScopeOfTypeInfo)
            else
                " For each namespace of the namespace list we try to get a tag
                " that can be in the same scope
                if g:OmniCpp_NamespaceSearch && &filetype != 'c'
                    for scope in a:namespaces
                        let szTmpScope = omni#cpp#utils#SimplifyScope(scope.'::'.szScopeOfTypeInfo)
                        let result = s:GetTagOfSameScope(tagList, szTmpScope)
                        if result!={}
                            break
                        endif
                    endfor
                else
                    let szTmpScope = omni#cpp#utils#SimplifyScope('::'.szScopeOfTypeInfo)
                    let result = s:GetTagOfSameScope(tagList, szTmpScope)
                endif
            endif
        endif
    endif

    if result!={}
        " We have our tagItem but maybe it's a typedef or an unnamed type
        if result.kind[0]=='t'
            " Here we can have a typedef to another typedef, a class, struct, union etc
            " but we can also have a typedef to an unnamed type, in that
            " case the result contains a 'typeref' key
            let namespaces = [omni#cpp#utils#ExtractScope(result), '::']
            if has_key(result, 'typeref')
                let result = omni#cpp#utils#GetResolvedTagItem(namespaces, omni#cpp#utils#CreateTypeInfo(result))
            else
                let szCmd = omni#cpp#utils#ExtractCmdFromTagItem(result)
                let szCode = substitute(omni#cpp#utils#GetCodeFromLine(szCmd), '\C\<'.result.name.'\>.*', '', 'g')
                let szTypeInfo = omni#cpp#utils#ExtractTypeInfoFromTokens(omni#cpp#tokenizer#Tokenize(szCode))
                let result = omni#cpp#utils#GetResolvedTagItem(namespaces, omni#cpp#utils#CreateTypeInfo(szTypeInfo))
                " TODO: Namespace resolution for result
            endif
        endif
    endif

    return result
endfunc

" Returns if the type info is valid
" @return
"   - 1 if valid
"   - 0 otherwise
function! omni#cpp#utils#IsTypeInfoValid(typeInfo)
    if a:typeInfo=={}
        return 0
    else
        if a:typeInfo.type == 1 && a:typeInfo.value==''
            " String case
            return 0
        elseif a:typeInfo.type == 4 && a:typeInfo.value=={}
            " Dictionary case
            return 0
        endif
    endif
    return 1
endfunc

" Get the string of the type info
function! omni#cpp#utils#GetTypeInfoString(typeInfo)
    if a:typeInfo.type == 1
        return a:typeInfo.value
    else
        return substitute(a:typeInfo.value.typeref, '^\w\+:', '', 'g')
    endif
endfunc

" A resolved type info starts with '::'
" @return
"   - 1 if type info starts with '::'
"   - 0 otherwise
function! s:IsTypeInfoResolved(szTypeInfo)
    return match(a:szTypeInfo, '^::')!=-1
endfunc

" A returned type info's scope may not have the global namespace '::'
" eg: '::NameSpace1::NameSpace2::MyClass' => '::NameSpace1::NameSpace2'
" 'NameSpace1::NameSpace2::MyClass' => 'NameSpace1::NameSpace2'
function! s:ExtractScopeFromTypeInfo(szTypeInfo)
    let szScope = substitute(a:szTypeInfo, '\w\+$', '', 'g')
    if szScope =='::'
        return szScope
    else
        return substitute(szScope, '::$', '', 'g')
    endif
endfunc

" @return
"   -   the tag with the same scope
"   -   {} otherwise
function! s:GetTagOfSameScope(listTags, szScopeToMatch)
    for tagItem in a:listTags 
        let szScopeOfTag = omni#cpp#utils#ExtractScope(tagItem)
        if szScopeOfTag == a:szScopeToMatch
            return tagItem
        endif
    endfor
    return {}
endfunc

" Extract the cmd of a tag item without regexp
function! omni#cpp#utils#ExtractCmdFromTagItem(tagItem)
    let line = a:tagItem.cmd
    let re = '\(\/\^\)\|\(\$\/\)'
    if match(line, re)!=-1
        let line = substitute(line, re, '', 'g')
        return line
    else
        " TODO: the cmd is a line number
        return ''
    endif
endfunc

" Extract type from tokens.
" eg: examples of tokens format
"   'const MyClass&'
"   'const map < int, int >&'
"   'MyNs::MyClass'
"   '::MyClass**'
"   'MyClass a, *b = NULL, c[1] = {};
"   'hello(MyClass a, MyClass* b'
" @return the type info string eg: ::std::map
" can be empty
function! omni#cpp#utils#ExtractTypeInfoFromTokens(tokens)
    let szResult = ''
    let state = 0

    let tokens = omni#cpp#utils#BuildParenthesisGroups(a:tokens)

    " If there is an unbalanced parenthesis we are in a parameter list
    let bParameterList = 0
    for token in tokens
        if token.value == '(' && token.group==-1
            let bParameterList = 1
            break
        endif
    endfor

    if bParameterList
        let tokens = reverse(tokens)
        let state = 0
        let parenGroup = -1
        for token in tokens
            if state==0
                if token.value=='>'
                    let parenGroup = token.group
                    let state=1
                elseif token.kind == 'cppWord'
                    let szResult = token.value.szResult
                    let state=2
                elseif index(['*', '&'], token.value)<0
                    break
                endif
            elseif state==1
                if token.value=='<' && token.group==parenGroup
                    let state=0
                endif
            elseif state==2
                if token.value=='::'
                    let szResult = token.value.szResult
                    let state=3
                else
                    break
                endif
            elseif state==3
                if token.kind == 'cppWord'
                    let szResult = token.value.szResult
                    let state=2
                else
                    break
                endif
            endif
        endfor
        return szResult
    endif

    for token in tokens
        if state==0
            if token.value == '::'
                let szResult .= token.value
                let state = 1
            elseif token.kind == 'cppWord'
                let szResult .= token.value
                let state = 2
                " Maybe end of token
            endif
        elseif state==1
            if token.kind == 'cppWord'
                let szResult .= token.value
                let state = 2
                " Maybe end of token
            else
                break
            endif
        elseif state==2
            if token.value == '::'
                let szResult .= token.value
                let state = 1
            else
                break
            endif
        endif
    endfor
    return szResult
endfunc

" Get the preview window string
function! omni#cpp#utils#GetPreviewWindowStringFromTagItem(tagItem)
    let szResult = ''

    let szResult .= 'name: '.a:tagItem.name."\n"
    for tagKey in keys(a:tagItem)
        if index(['name', 'static'], tagKey)>=0
            continue
        endif
        let szResult .= tagKey.': '.a:tagItem[tagKey]."\n"
    endfor

    return substitute(szResult, "\n$", '', 'g')
endfunc
