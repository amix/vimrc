"CLASS: KeyMap
"============================================================
let s:KeyMap = {}
let g:NERDTreeKeyMap = s:KeyMap

"FUNCTION: KeyMap.All() {{{1
function! s:KeyMap.All()
    if !exists("s:keyMaps")
        let s:keyMaps = []
    endif
    return s:keyMaps
endfunction

"FUNCTION: KeyMap.FindFor(key, scope) {{{1
function! s:KeyMap.FindFor(key, scope)
    for i in s:KeyMap.All()
         if i.key ==# a:key && i.scope ==# a:scope
            return i
        endif
    endfor
    return {}
endfunction

"FUNCTION: KeyMap.BindAll() {{{1
function! s:KeyMap.BindAll()
    for i in s:KeyMap.All()
        call i.bind()
    endfor
endfunction

"FUNCTION: KeyMap.bind() {{{1
function! s:KeyMap.bind()
    " If the key sequence we're trying to map contains any '<>' notation, we
    " must replace each of the '<' characters with '<lt>' to ensure the string
    " is not translated into its corresponding keycode during the later part
    " of the map command below
    " :he <>
    let specialNotationRegex = '\m<\([[:alnum:]_-]\+>\)'
    if self.key =~# specialNotationRegex
        let keymapInvokeString = substitute(self.key, specialNotationRegex, '<lt>\1', 'g')
    else
        let keymapInvokeString = self.key
    endif

    let premap = self.key == "<LeftRelease>" ? " <LeftRelease>" : " "

    exec 'nnoremap <buffer> <silent> '. self.key . premap . ':call nerdtree#invokeKeyMap("'. keymapInvokeString .'")<cr>'
endfunction

"FUNCTION: KeyMap.Remove(key, scope) {{{1
function! s:KeyMap.Remove(key, scope)
    let maps = s:KeyMap.All()
    for i in range(len(maps))
         if maps[i].key ==# a:key && maps[i].scope ==# a:scope
            return remove(maps, i)
        endif
    endfor
endfunction

"FUNCTION: KeyMap.invoke() {{{1
"Call the KeyMaps callback function
function! s:KeyMap.invoke(...)
    let Callback = function(self.callback)
    if a:0
        call Callback(a:1)
    else
        call Callback()
    endif
endfunction

"FUNCTION: KeyMap.Invoke() {{{1
"Find a keymapping for a:key and the current scope invoke it.
"
"Scope is determined as follows:
"   * if the cursor is on a dir node then "DirNode"
"   * if the cursor is on a file node then "FileNode"
"   * if the cursor is on a bookmark then "Bookmark"
"
"If a keymap has the scope of "all" then it will be called if no other keymap
"is found for a:key and the scope.
function! s:KeyMap.Invoke(key)
    let node = g:NERDTreeFileNode.GetSelected()
    if !empty(node)

        "try file node
        if !node.path.isDirectory
            let km = s:KeyMap.FindFor(a:key, "FileNode")
            if !empty(km)
                return km.invoke(node)
            endif
        endif

        "try dir node
        if node.path.isDirectory
            let km = s:KeyMap.FindFor(a:key, "DirNode")
            if !empty(km)
                return km.invoke(node)
            endif
        endif

        "try generic node
        let km = s:KeyMap.FindFor(a:key, "Node")
        if !empty(km)
            return km.invoke(node)
        endif

    endif

    "try bookmark
    let bm = g:NERDTreeBookmark.GetSelected()
    if !empty(bm)
        let km = s:KeyMap.FindFor(a:key, "Bookmark")
        if !empty(km)
            return km.invoke(bm)
        endif
    endif

    "try all
    let km = s:KeyMap.FindFor(a:key, "all")
    if !empty(km)
        return km.invoke()
    endif
endfunction

"FUNCTION: KeyMap.Create(options) {{{1
function! s:KeyMap.Create(options)
    let newKeyMap = copy(self)
    let opts = extend({'scope': 'all', 'quickhelpText': ''}, copy(a:options))
    let newKeyMap.key = opts['key']
    let newKeyMap.quickhelpText = opts['quickhelpText']
    let newKeyMap.callback = opts['callback']
    let newKeyMap.scope = opts['scope']

    call s:KeyMap.Add(newKeyMap)
endfunction

"FUNCTION: KeyMap.Add(keymap) {{{1
function! s:KeyMap.Add(keymap)
    call s:KeyMap.Remove(a:keymap.key, a:keymap.scope)
    call add(s:KeyMap.All(), a:keymap)
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
