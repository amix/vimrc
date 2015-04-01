" Snippet definition parsing code

function! s:sfile() abort
    return expand('<sfile>')
endfunction

let s:parser_proto = {}

function! s:new_parser(text) abort
    let ret = copy(s:parser_proto)
    let ret.input = a:text
    let ret.len = strlen(ret.input)
    let ret.pos = -1
    let ret.indent = 0
    let ret.value = []
    let ret.vars = {}
    call ret.advance()
    return ret
endfunction

function! s:parser_advance(...) dict abort
    let self.pos += a:0 ? a:1 : 1
    let self.next = self.input[self.pos]
endfunction

function! s:parser_same(tok) dict abort
    if self.next == a:tok
        call self.advance()
        return 1
    else
        return 0
    endif
endfunction

function! s:parser_id() dict abort
    if self.input[(self.pos):(self.pos+5)] == 'VISUAL'
        call self.advance(6)
        return 'VISUAL'
    elseif self.next =~ '\d'
        let end = matchend(self.input, '\d\+', self.pos)
        let res = strpart(self.input, self.pos, end - self.pos)
        call self.advance(end - self.pos)
        return +res " force conversion to Number
    endif
    return -1
endfunction

function! s:parser_add_var(var) dict abort
    let id = a:var[0]
    if !has_key(self.vars, id)
        let self.vars[id] = { 'instances' : [] }
    endif
    call add(self.vars[id].instances, a:var)
endfunction

function! s:parser_var() dict abort
    let ret = []
    if self.same('{')
        let id = self.id()
        if id >= 0
            call add(ret, id)
            call extend(ret, self.varend())
        endif
    else
        let id = self.id()
        if id >= 0
            call add(ret, id)
        endif
    endif
    return ret
endfunction

function! s:parser_varend() dict abort
    let ret = []
    if self.same(':')
        call extend(ret, self.placeholder())
    elseif self.same('/')
        call add(ret, self.subst())
    endif
    call self.same('}')
    return ret
endfunction

function! s:parser_placeholder() dict abort
    return self.parse('}')
endfunction

function! s:parser_subst() dict abort
    let ret = {}
    let ret.pat = join(self.text('/', 1))
    if self.same('/')
        let ret.sub = join(self.text('/}'))
    endif
    if self.same('/')
        let ret.flags = join(self.text('}', 1))
    endif
    return ret
endfunction

function! s:parser_expr() dict abort
    let str = join(self.text('`', 1))
    let ret = eval(str)
    call self.same('`')
    return type(ret) == type('') ? ret : string(ret)
endfunction

function! s:parser_text(...) dict abort
    let res = []
    let val = ''
    if a:0 == 2 && a:2
        let till = '\V' . escape(a:1, '\')
    else
        let till = '[`$' . (a:0 ? a:1 : '') . ']'
    endif

    while self.pos < self.len
        if self.same('\')
            if self.next != "\n"
                let val .= self.next
            endif
            call self.advance()
        elseif self.next =~# till
            break
        elseif self.next == "\n"
            call add(res, val)
            let val = ''
            let self.indent = 0
            call self.advance()
        elseif self.next == "\t"
            let self.indent += 1
            let val .= s:indent(1)
            call self.advance()
        else
            let val .= self.next
            call self.advance()
        endif
    endwhile

    call add(res, val)
    return res
endfunction

function! s:parser_parse(...) dict abort
    let ret = a:0 ? [] : self.value
    while self.pos < self.len
        if self.same('$')
            let var = self.var()
            if !empty(var)
                if var[0] is# 'VISUAL'
                    let add_to = s:visual_placeholder(var, self.indent)
                    if !empty(ret) && type(ret[-1]) == type('')
                        let ret[-1] .= add_to[0]
                    else
                        call add(ret, add_to[0])
                    endif
                    call extend(ret, add_to[1:-1])
                elseif var[0] >= 0
                    call add(ret, var)
                    call self.add_var(var)
                endif
            endif
        elseif self.same('`')
            let add_to = self.expr()
            if !empty(ret) && type(ret[-1]) == type('')
                let ret[-1] .= add_to
            else
                call add(ret, add_to)
            endif
        else
            let text = a:0 ? self.text(a:1) : self.text()
            if exists('add_to')
                let ret[-1] .= text[0]
                call remove(text, 0)
                unlet add_to
            endif
            call extend(ret, text)
        endif
        if a:0 && self.next == a:1
            break
        endif
    endwhile
    return ret
endfunction

call extend(s:parser_proto, snipmate#util#add_methods(s:sfile(), 'parser',
            \ [ 'advance', 'same', 'id', 'add_var', 'var', 'varend',
            \ 'placeholder', 'subst', 'expr', 'text', 'parse' ]), 'error')

function! s:indent(count) abort
    if &expandtab
        let shift = repeat(' ', (&sts > 0) ? &sts : &sw)
    else
        let shift = "\t"
    endif
    return repeat(shift, a:count)
endfunction

function! s:visual_placeholder(var, indent) abort
    let arg = get(a:var, 1, {})
    if type(arg) == type({})
        let pat = get(arg, 'pat', '')
        let sub = get(arg, 'sub', '')
        let flags = get(arg, 'flags', '')
        let content = split(substitute(get(b:, 'snipmate_visual', ''), pat, sub, flags), "\n", 1)
    else
        let content = split(get(b:, 'snipmate_visual', arg), "\n", 1)
    endif

    let indent = s:indent(a:indent)
    call map(content, '(v:key != 0) ? indent . v:val : v:val')

    return content
endfunction

function! snipmate#parse#snippet(text) abort
    let parser = s:new_parser(a:text)
    call parser.parse()
    unlet! b:snipmate_visual
    return [parser.value, parser.vars]
endfunction
