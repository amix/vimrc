" Description: Omni completion debug functions
" Maintainer:  Vissale NEANG
" Last Change: 26 sept. 2007

let s:CACHE_DEBUG_TRACE = []

" Start debug, clear the debug file
function! omni#common#debug#Start()
    let s:CACHE_DEBUG_TRACE = []
    call extend(s:CACHE_DEBUG_TRACE, ['============ Debug Start ============'])
    call writefile(s:CACHE_DEBUG_TRACE, "Omni.dbg")
endfunc

" End debug, write to debug file
function! omni#common#debug#End()
    call extend(s:CACHE_DEBUG_TRACE, ["============= Debug End ============="])
    call extend(s:CACHE_DEBUG_TRACE, [""])
    call writefile(s:CACHE_DEBUG_TRACE, "Omni.dbg")
endfunc

" Debug trace function
function! omni#common#debug#Trace(szFuncName, ...)
    let szTrace = a:szFuncName
    let paramNum = a:0
    if paramNum>0
        let szTrace .= ':'
    endif
    for i in range(paramNum)
        let szTrace = szTrace .' ('. string(eval('a:'.string(i+1))).')'
    endfor
    call extend(s:CACHE_DEBUG_TRACE, [szTrace])
endfunc
