"CLASS: FlagSet
"============================================================
let s:FlagSet = {}
let g:NERDTreeFlagSet = s:FlagSet

"FUNCTION: FlagSet.addFlag(scope, flag) {{{1
function! s:FlagSet.addFlag(scope, flag)
    let flags = self._flagsForScope(a:scope)
    if index(flags, a:flag) == -1
        call add(flags, a:flag)
    end
endfunction

"FUNCTION: FlagSet.clearFlags(scope) {{{1
function! s:FlagSet.clearFlags(scope)
    let self._flags[a:scope] = []
endfunction

"FUNCTION: FlagSet._flagsForScope(scope) {{{1
function! s:FlagSet._flagsForScope(scope)
    if !has_key(self._flags, a:scope)
        let self._flags[a:scope] = []
    endif
    return self._flags[a:scope]
endfunction

"FUNCTION: FlagSet.New() {{{1
function! s:FlagSet.New()
    let newObj = copy(self)
    let newObj._flags = {}
    return newObj
endfunction

"FUNCTION: FlagSet.removeFlag(scope, flag) {{{1
function! s:FlagSet.removeFlag(scope, flag)
    let flags = self._flagsForScope(a:scope)

    let i = index(flags, a:flag)
    if i >= 0
        call remove(flags, i)
    endif
endfunction

"FUNCTION: FlagSet.renderToString() {{{1
function! s:FlagSet.renderToString()
    let flagstring = ""
    for i in values(self._flags)
        let flagstring .= join(i)
    endfor

    if len(flagstring) == 0
        return ""
    endif

    return '[' . flagstring . ']'
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
