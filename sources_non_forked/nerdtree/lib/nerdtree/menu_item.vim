"CLASS: MenuItem
"============================================================
let s:MenuItem = {}
let g:NERDTreeMenuItem = s:MenuItem

"FUNCTION: MenuItem.All() {{{1
"get all top level menu items
function! s:MenuItem.All()
    if !exists("s:menuItems")
        let s:menuItems = []
    endif
    return s:menuItems
endfunction

"FUNCTION: MenuItem.AllEnabled() {{{1
"get all top level menu items that are currently enabled
function! s:MenuItem.AllEnabled()
    let toReturn = []
    for i in s:MenuItem.All()
        if i.enabled()
            call add(toReturn, i)
        endif
    endfor
    return toReturn
endfunction

"FUNCTION: MenuItem.Create(options) {{{1
"make a new menu item and add it to the global list
function! s:MenuItem.Create(options)
    let newMenuItem = copy(self)

    let newMenuItem.text = a:options['text']
    let newMenuItem.shortcut = a:options['shortcut']
    let newMenuItem.children = []

    let newMenuItem.isActiveCallback = -1
    if has_key(a:options, 'isActiveCallback')
        let newMenuItem.isActiveCallback = a:options['isActiveCallback']
    endif

    let newMenuItem.callback = -1
    if has_key(a:options, 'callback')
        let newMenuItem.callback = a:options['callback']
    endif

    if has_key(a:options, 'parent')
        call add(a:options['parent'].children, newMenuItem)
    else
        call add(s:MenuItem.All(), newMenuItem)
    endif

    return newMenuItem
endfunction

"FUNCTION: MenuItem.CreateSeparator(options) {{{1
"make a new separator menu item and add it to the global list
function! s:MenuItem.CreateSeparator(options)
    let standard_options = { 'text': '--------------------',
                \ 'shortcut': -1,
                \ 'callback': -1 }
    let options = extend(a:options, standard_options, "force")

    return s:MenuItem.Create(options)
endfunction

"FUNCTION: MenuItem.CreateSubmenu(options) {{{1
"make a new submenu and add it to global list
function! s:MenuItem.CreateSubmenu(options)
    let standard_options = { 'callback': -1 }
    let options = extend(a:options, standard_options, "force")

    return s:MenuItem.Create(options)
endfunction

"FUNCTION: MenuItem.enabled() {{{1
"return 1 if this menu item should be displayed
"
"delegates off to the isActiveCallback, and defaults to 1 if no callback was
"specified
function! s:MenuItem.enabled()
    if self.isActiveCallback != -1
        return {self.isActiveCallback}()
    endif
    return 1
endfunction

"FUNCTION: MenuItem.execute() {{{1
"perform the action behind this menu item, if this menuitem has children then
"display a new menu for them, otherwise deletegate off to the menuitem's
"callback
function! s:MenuItem.execute()
    if len(self.children)
        let mc = g:NERDTreeMenuController.New(self.children)
        call mc.showMenu()
    else
        if self.callback != -1
            call {self.callback}()
        endif
    endif
endfunction

"FUNCTION: MenuItem.isSeparator() {{{1
"return 1 if this menuitem is a separator
function! s:MenuItem.isSeparator()
    return self.callback == -1 && self.children == []
endfunction

"FUNCTION: MenuItem.isSubmenu() {{{1
"return 1 if this menuitem is a submenu
function! s:MenuItem.isSubmenu()
    return self.callback == -1 && !empty(self.children)
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
