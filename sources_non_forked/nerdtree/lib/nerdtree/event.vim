"CLASS: Event
"============================================================
let s:Event = {}
let g:NERDTreeEvent = s:Event

function! s:Event.New(nerdtree, subject, action, params) abort
    let newObj = copy(self)
    let newObj.nerdtree = a:nerdtree
    let newObj.subject = a:subject
    let newObj.action = a:action
    let newObj.params = a:params
    return newObj
endfunction
