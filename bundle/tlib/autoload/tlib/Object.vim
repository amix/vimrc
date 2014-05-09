" Object.vim -- Prototype objects?
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-05-01.
" @Last Change: 2011-03-10.
" @Revision:    0.1.126

" :filedoc:
" Provides a prototype plus some OO-like methods.


if &cp || exists("loaded_tlib_object_autoload")
    finish
endif
let loaded_tlib_object_autoload = 1

let s:id_counter = 0
let s:prototype  = {'_class': ['object'], '_super': [], '_id': 0} "{{{2

" :def: function! tlib#Object#New(?fields={})
" This function creates a prototype that provides some kind of 
" inheritance mechanism and a way to call parent/super methods.
"
" The usage demonstrated in the following example works best when every 
" class/prototype is defined in a file of its own.
"
" The reason for why there is a dedicated constructor function is that 
" this layout facilitates the use of templates and that methods are 
" hidden from the user. Other solutions are possible.
"
" EXAMPLES: >
"     let s:prototype = tlib#Object#New({
"                 \ '_class': ['FooBar'],
"                 \ 'foo': 1, 
"                 \ 'bar': 2, 
"                 \ })
"     " Constructor
"     function! FooBar(...)
"         let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
"         return object
"     endf
"     function! s:prototype.babble() {
"       echo "I think, therefore I am ". (self.foo * self.bar) ." months old."
"     }
"
" < This could now be used like this: >
"     let myfoo = FooBar({'foo': 3})
"     call myfoo.babble()
"     => I think, therefore I am 6 months old.
"     echo myfoo.IsA('FooBar')
"     => 1
"     echo myfoo.IsA('object')
"     => 1
"     echo myfoo.IsA('Foo')
"     => 0
"     echo myfoo.RespondTo('babble')
"     => 1
"     echo myfoo.RespondTo('speak')
"     => 0
function! tlib#Object#New(...) "{{{3
    return s:prototype.New(a:0 >= 1 ? a:1 : {})
endf


function! s:prototype.New(...) dict "{{{3
    let object = deepcopy(self)
    let s:id_counter += 1
    let object._id = s:id_counter
    if a:0 >= 1 && !empty(a:1)
        " call object.Extend(deepcopy(a:1))
        call object.Extend(a:1)
    endif
    return object
endf


function! s:prototype.Inherit(object) dict "{{{3
    let class = copy(self._class)
    " TLogVAR class
    let objid = self._id
    for c in get(a:object, '_class', [])
        " TLogVAR c
        if index(class, c) == -1
            call add(class, c)
        endif
    endfor
    call extend(self, a:object, 'keep')
    let self._class = class
    " TLogVAR self._class
    let self._id    = objid
    " let self._super = [super] + self._super
    call insert(self._super, a:object)
    return self
endf


function! s:prototype.Extend(dictionary) dict "{{{3
    let super = copy(self)
    let class = copy(self._class)
    " TLogVAR class
    let objid = self._id
    let thisclass = get(a:dictionary, '_class', [])
    for c in type(thisclass) == 3 ? thisclass : [thisclass]
        " TLogVAR c
        if index(class, c) == -1
            call add(class, c)
        endif
    endfor
    call extend(self, a:dictionary)
    let self._class = class
    " TLogVAR self._class
    let self._id    = objid
    " let self._super = [super] + self._super
    call insert(self._super, super)
    return self
endf


function! s:prototype.IsA(class) dict "{{{3
    return index(self._class, a:class) != -1
endf


function! s:prototype.IsRelated(object) dict "{{{3
    return len(filter(a:object._class, 'self.IsA(v:val)')) > 1
endf


function! s:prototype.RespondTo(name) dict "{{{3
    " return has_key(self, a:name) && type(self[a:name]) == 2
    return has_key(self, a:name)
endf


function! s:prototype.Super(method, arglist) dict "{{{3
    for o in self._super
        " TLogVAR o
        if o.RespondTo(a:method)
            " let self._tmp_method = o[a:method]
            " TLogVAR self._tmp_method
            " return call(self._tmp_method, a:arglist, self)
            return call(o[a:method], a:arglist, self)
        endif
    endfor
    echoerr 'tlib#Object: Does not respond to '. a:method .': '. string(self)
endf


function! tlib#Object#Methods(object, ...) "{{{3
    TVarArg ['pattern', '\d\+']
    let o = items(a:object)
    call filter(o, 'type(v:val[1]) == 2 && string(v:val[1]) =~ "^function(''\\d\\+'')"')
    let acc = {}
    for e in o
        let id = matchstr(string(e[1]), pattern)
        if !empty(id)
            let acc[id] = e[0]
        endif
    endfor
    return acc
endf

