" abolish.vim - Language friendly searches, substitutions, and abbreviations
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.1
" GetLatestVimScripts: 1545 1 :AutoInstall: abolish.vim

" Initialization {{{1

if exists("g:loaded_abolish") || &cp || v:version < 700
  finish
endif
let g:loaded_abolish = 1

if !exists("g:abolish_save_file")
  if isdirectory(expand("~/.vim"))
    let g:abolish_save_file = expand("~/.vim/after/plugin/abolish.vim")
  elseif isdirectory(expand("~/vimfiles")) || has("win32")
    let g:abolish_save_file = expand("~/vimfiles/after/plugin/abolish.vim")
  else
    let g:abolish_save_file = expand("~/.vim/after/plugin/abolish.vim")
  endif
endif

" }}}1
" Utility functions {{{1

function! s:function(name)
  return function(substitute(a:name,'^s:',matchstr(expand('<sfile>'), '<SNR>\d\+_'),''))
endfunction

function! s:send(self,func,...)
  if type(a:func) == type('') || type(a:func) == type(0)
    let Func = get(a:self,a:func,'')
  else
    let Func = a:func
  endif
  let s = type(a:self) == type({}) ? a:self : {}
  if type(Func) == type(function('tr'))
    return call(Func,a:000,s)
  elseif type(Func) == type({}) && has_key(Func,'apply')
    return call(Func.apply,a:000,Func)
  elseif type(Func) == type({}) && has_key(Func,'call')
    return call(Func.call,a:000,s)
  elseif type(Func) == type('') && Func == '' && has_key(s,'function missing')
    return call('s:send',[s,'function missing',a:func] + a:000)
  else
    return Func
  endif
endfunction

let s:object = {}
function! s:object.clone(...)
  let sub = deepcopy(self)
  return a:0 ? extend(sub,a:1) : sub
endfunction

if !exists("g:Abolish")
  let Abolish = {}
endif
call extend(Abolish, s:object, 'force')
call extend(Abolish, {'Coercions': {}}, 'keep')

function! s:throw(msg)
  let v:errmsg = a:msg
  throw "Abolish: ".a:msg
endfunction

function! s:words()
  let words = []
  let lnum = line('w0')
  while lnum <= line('w$')
    let line = getline(lnum)
    let col = 0
    while match(line,'\<\k\k\+\>',col) != -1
      let words += [matchstr(line,'\<\k\k\+\>',col)]
      let col = matchend(line,'\<\k\k\+\>',col)
    endwhile
    let lnum += 1
  endwhile
  return words
endfunction

function! s:extractopts(list,opts)
  let i = 0
  while i < len(a:list)
    if a:list[i] =~ '^-[^=]' && has_key(a:opts,matchstr(a:list[i],'-\zs[^=]*'))
      let key   = matchstr(a:list[i],'-\zs[^=]*')
      let value = matchstr(a:list[i],'=\zs.*')
      if type(get(a:opts,key)) == type([])
        let a:opts[key] += [value]
      elseif type(get(a:opts,key)) == type(0)
        let a:opts[key] = 1
      else
        let a:opts[key] = value
      endif
    else
      let i += 1
      continue
    endif
    call remove(a:list,i)
  endwhile
  return a:opts
endfunction

" }}}1
" Dictionary creation {{{1

function! s:mixedcase(word)
  return substitute(s:camelcase(a:word),'^.','\u&','')
endfunction

function! s:camelcase(word)
  let word = substitute(a:word, '-', '_', 'g')
  if word !~# '_' && word =~# '\l'
    return substitute(word,'^.','\l&','')
  else
    return substitute(word,'\C\(_\)\=\(.\)','\=submatch(1)==""?tolower(submatch(2)) : toupper(submatch(2))','g')
  endif
endfunction

function! s:snakecase(word)
  let word = substitute(a:word,'::','/','g')
  let word = substitute(word,'\(\u\+\)\(\u\l\)','\1_\2','g')
  let word = substitute(word,'\(\l\|\d\)\(\u\)','\1_\2','g')
  let word = substitute(word,'[.-]','_','g')
  let word = tolower(word)
  return word
endfunction

function! s:uppercase(word)
  return toupper(s:snakecase(a:word))
endfunction

function! s:dashcase(word)
  return substitute(s:snakecase(a:word),'_','-','g')
endfunction

function! s:spacecase(word)
  return substitute(s:snakecase(a:word),'_',' ','g')
endfunction

function! s:dotcase(word)
  return substitute(s:snakecase(a:word),'_','.','g')
endfunction

function! s:titlecase(word)
  return substitute(s:spacecase(a:word), '\(\<\w\)','\=toupper(submatch(1))','g')
endfunction

call extend(Abolish, {
      \ 'camelcase':  s:function('s:camelcase'),
      \ 'mixedcase':  s:function('s:mixedcase'),
      \ 'snakecase':  s:function('s:snakecase'),
      \ 'uppercase':  s:function('s:uppercase'),
      \ 'dashcase':   s:function('s:dashcase'),
      \ 'dotcase':    s:function('s:dotcase'),
      \ 'spacecase':  s:function('s:spacecase'),
      \ 'titlecase':  s:function('s:titlecase')
      \ }, 'keep')

function! s:create_dictionary(lhs,rhs,opts)
  let dictionary = {}
  let i = 0
  let expanded = s:expand_braces({a:lhs : a:rhs})
  for [lhs,rhs] in items(expanded)
    if get(a:opts,'case',1)
      let dictionary[s:mixedcase(lhs)] = s:mixedcase(rhs)
      let dictionary[tolower(lhs)] = tolower(rhs)
      let dictionary[toupper(lhs)] = toupper(rhs)
    endif
    let dictionary[lhs] = rhs
  endfor
  let i += 1
  return dictionary
endfunction

function! s:expand_braces(dict)
  let new_dict = {}
  for [key,val] in items(a:dict)
    if key =~ '{.*}'
      let redo = 1
      let [all,kbefore,kmiddle,kafter;crap] = matchlist(key,'\(.\{-\}\){\(.\{-\}\)}\(.*\)')
      let [all,vbefore,vmiddle,vafter;crap] = matchlist(val,'\(.\{-\}\){\(.\{-\}\)}\(.*\)') + ["","","",""]
      if all == ""
        let [vbefore,vmiddle,vafter] = [val, ",", ""]
      endif
      let targets      = split(kmiddle,',',1)
      let replacements = split(vmiddle,',',1)
      if replacements == [""]
        let replacements = targets
      endif
      for i in range(0,len(targets)-1)
        let new_dict[kbefore.targets[i].kafter] = vbefore.replacements[i%len(replacements)].vafter
      endfor
    else
      let new_dict[key] = val
    endif
  endfor
  if exists("redo")
    return s:expand_braces(new_dict)
  else
    return new_dict
  endif
endfunction

" }}}1
" Abolish Dispatcher {{{1

function! s:SubComplete(A,L,P)
  if a:A =~ '^[/?]\k\+$'
    let char = strpart(a:A,0,1)
    return join(map(s:words(),'char . v:val'),"\n")
  elseif a:A =~# '^\k\+$'
    return join(s:words(),"\n")
  endif
endfunction

function! s:Complete(A,L,P)
  let g:L = a:L
  " Vim bug: :Abolish -<Tab> calls this function with a:A equal to 0
  if a:A =~# '^[^/?-]' && type(a:A) != type(0)
    return join(s:words(),"\n")
  elseif a:L =~# '^\w\+\s\+\%(-\w*\)\=$'
    return "-search\n-substitute\n-delete\n-buffer\n-cmdline\n"
  elseif a:L =~# ' -\%(search\|substitute\)\>'
    return "-flags="
  else
    return "-buffer\n-cmdline"
  endif
endfunction

let s:commands = {}
let s:commands.abstract = s:object.clone()

function! s:commands.abstract.dispatch(bang,line1,line2,count,args)
  return self.clone().go(a:bang,a:line1,a:line2,a:count,a:args)
endfunction

function! s:commands.abstract.go(bang,line1,line2,count,args)
  let self.bang = a:bang
  let self.line1 = a:line1
  let self.line2 = a:line2
  let self.count = a:count
  return self.process(a:bang,a:line1,a:line2,a:count,a:args)
endfunction

function! s:dispatcher(bang,line1,line2,count,args)
  let i = 0
  let args = copy(a:args)
  let command = s:commands.abbrev
  while i < len(args)
    if args[i] =~# '^-\w\+$' && has_key(s:commands,matchstr(args[i],'-\zs.*'))
      let command = s:commands[matchstr(args[i],'-\zs.*')]
      call remove(args,i)
      break
    endif
    let i += 1
  endwhile
  try
    return command.dispatch(a:bang,a:line1,a:line2,a:count,args)
  catch /^Abolish: /
    echohl ErrorMsg
    echo   v:errmsg
    echohl NONE
    return ""
  endtry
endfunction

" }}}1
" Subvert Dispatcher {{{1

function! s:subvert_dispatcher(bang,line1,line2,count,args)
  try
    return s:parse_subvert(a:bang,a:line1,a:line2,a:count,a:args)
  catch /^Subvert: /
    echohl ErrorMsg
    echo   v:errmsg
    echohl NONE
    return ""
  endtry
endfunction

function! s:parse_subvert(bang,line1,line2,count,args)
  if a:args =~ '^\%(\w\|$\)'
    let args = (a:bang ? "!" : "").a:args
  else
    let args = a:args
  endif
  let separator = matchstr(args,'^.')
  let split = split(args,separator,1)[1:]
  if a:count || split == [""]
    return s:parse_substitute(a:bang,a:line1,a:line2,a:count,split)
  elseif len(split) == 1
    return s:find_command(separator,"",split[0])
  elseif len(split) == 2 && split[1] =~# '^[A-Za-z]*n[A-Za-z]*$'
    return s:parse_substitute(a:bang,a:line1,a:line2,a:count,[split[0],"",split[1]])
  elseif len(split) == 2 && split[1] =~# '^[A-Za-z]*\%([+-]\d\+\)\=$'
    return s:find_command(separator,split[1],split[0])
  elseif len(split) >= 2 && split[1] =~# '^[A-Za-z]* '
    let flags = matchstr(split[1],'^[A-Za-z]*')
    let rest = matchstr(join(split[1:],separator),' \zs.*')
    return s:grep_command(rest,a:bang,flags,split[0])
  elseif len(split) >= 2 && separator == ' '
    return s:grep_command(join(split[1:],' '),a:bang,"",split[0])
  else
    return s:parse_substitute(a:bang,a:line1,a:line2,a:count,split)
  endif
endfunction

function! s:normalize_options(flags)
  if type(a:flags) == type({})
    let opts = a:flags
    let flags = get(a:flags,"flags","")
  else
    let opts = {}
    let flags = a:flags
  endif
  let g:op1 = copy(opts)
  if flags =~# 'w'
    let opts.boundaries = 2
  elseif flags =~# 'v'
    let opts.boundaries = 1
  elseif !has_key(opts,'boundaries')
    let opts.boundaries = 0
  endif
  let opts.case = (flags !~# 'I' ? get(opts,'case',1) : 0)
  let opts.flags = substitute(flags,'\C[avIiw]','','g')
  let g:op2 = copy(opts)
  return opts
endfunction

" }}}1
" Searching {{{1

function! s:subesc(pattern)
  return substitute(a:pattern,'[][\\/.*+?~%()&]','\\&','g')
endfunction

function! s:sort(a,b)
  if a:a ==? a:b
    return a:a == a:b ? 0 : a:a > a:b ? 1 : -1
  elseif strlen(a:a) == strlen(a:b)
    return a:a >? a:b ? 1 : -1
  else
    return strlen(a:a) < strlen(a:b) ? 1 : -1
  endif
endfunction

function! s:pattern(dict,boundaries)
  if a:boundaries == 2
    let a = '<'
    let b = '>'
  elseif a:boundaries
    let a = '%(<|_@<=|[[:lower:]]@<=[[:upper:]]@=)'
    let b =  '%(>|_@=|[[:lower:]]@<=[[:upper:]]@=)'
  else
    let a = ''
    let b = ''
  endif
  return '\v\C'.a.'%('.join(map(sort(keys(a:dict),function('s:sort')),'s:subesc(v:val)'),'|').')'.b
endfunction

function! s:egrep_pattern(dict,boundaries)
  if a:boundaries == 2
    let a = '\<'
    let b = '\>'
  elseif a:boundaries
    let a = '(\<\|_)'
    let b = '(\>\|_\|[[:upper:]][[:lower:]])'
  else
    let a = ''
    let b = ''
  endif
  return a.'('.join(map(sort(keys(a:dict),function('s:sort')),'s:subesc(v:val)'),'\|').')'.b
endfunction

function! s:c()
  call histdel('search',-1)
  return ""
endfunction

function! s:find_command(cmd,flags,word)
  let opts = s:normalize_options(a:flags)
  let dict = s:create_dictionary(a:word,"",opts)
  " This is tricky.  If we use :/pattern, the search drops us at the
  " beginning of the line, and we can't use position flags (e.g., /foo/e).
  " If we use :norm /pattern, we leave ourselves vulnerable to "press enter"
  " prompts (even with :silent).
  let cmd = (a:cmd =~ '[?!]' ? '?' : '/')
  let @/ = s:pattern(dict,opts.boundaries)
  if opts.flags == "" || !search(@/,'n')
    return "norm! ".cmd."\<CR>"
  elseif opts.flags =~ ';[/?]\@!'
    call s:throw("E386: Expected '?' or '/' after ';'")
  else
    return "exe 'norm! ".cmd.cmd.opts.flags."\<CR>'|call histdel('search',-1)"
    return ""
  endif
endfunction

function! s:grep_command(args,bang,flags,word)
  let opts = s:normalize_options(a:flags)
  let dict = s:create_dictionary(a:word,"",opts)
  if &grepprg == "internal"
    let lhs = "'".s:pattern(dict,opts.boundaries)."'"
  else
    let lhs = "-E '".s:egrep_pattern(dict,opts.boundaries)."'"
  endif
  return "grep".(a:bang ? "!" : "")." ".lhs." ".a:args
endfunction

let s:commands.search = s:commands.abstract.clone()
let s:commands.search.options = {"word": 0, "variable": 0, "flags": ""}

function! s:commands.search.process(bang,line1,line2,count,args)
  call s:extractopts(a:args,self.options)
  if self.options.word
    let self.options.flags .= "w"
  elseif self.options.variable
    let self.options.flags .= "v"
  endif
  let opts = s:normalize_options(self.options)
  if len(a:args) > 1
    return s:grep_command(join(a:args[1:]," "),a:bang,opts,a:args[0])
  elseif len(a:args) == 1
    return s:find_command(a:bang ? "!" : " ",opts,a:args[0])
  else
    call s:throw("E471: Argument required")
  endif
endfunction

" }}}1
" Substitution {{{1

function! Abolished()
  return get(g:abolish_last_dict,submatch(0),submatch(0))
endfunction

function! s:substitute_command(cmd,bad,good,flags)
  let opts = s:normalize_options(a:flags)
  let dict = s:create_dictionary(a:bad,a:good,opts)
  let lhs = s:pattern(dict,opts.boundaries)
  let g:abolish_last_dict = dict
  return a:cmd.'/'.lhs.'/\=Abolished()'."/".opts.flags
endfunction

function! s:parse_substitute(bang,line1,line2,count,args)
  if get(a:args,0,'') =~ '^[/?'']'
    let separator = matchstr(a:args[0],'^.')
    let args = split(join(a:args,' '),separator,1)
    call remove(args,0)
  else
    let args = a:args
  endif
  if len(args) < 2
    call s:throw("E471: Argument required")
  elseif len(args) > 3
    call s:throw("E488: Trailing characters")
  endif
  let [bad,good,flags] = (args + [""])[0:2]
  if a:count == 0
    let cmd = "substitute"
  else
    let cmd = a:line1.",".a:line2."substitute"
  endif
  return s:substitute_command(cmd,bad,good,flags)
endfunction

let s:commands.substitute = s:commands.abstract.clone()
let s:commands.substitute.options = {"word": 0, "variable": 0, "flags": "g"}

function! s:commands.substitute.process(bang,line1,line2,count,args)
  call s:extractopts(a:args,self.options)
  if self.options.word
    let self.options.flags .= "w"
  elseif self.options.variable
    let self.options.flags .= "v"
  endif
  let opts = s:normalize_options(self.options)
  if len(a:args) <= 1
    call s:throw("E471: Argument required")
  else
    let good = join(a:args[1:],"")
    let cmd = a:bang ? "." : "%"
    return s:substitute_command(cmd,a:args[0],good,self.options)
  endif
endfunction

" }}}1
" Abbreviations {{{1

function! s:badgood(args)
  let words = filter(copy(a:args),'v:val !~ "^-"')
  call filter(a:args,'v:val =~ "^-"')
  if empty(words)
    call s:throw("E471: Argument required")
  elseif !empty(a:args)
    call s:throw("Unknown argument: ".a:args[0])
  endif
  let [bad; words] = words
  return [bad, join(words," ")]
endfunction

function! s:abbreviate_from_dict(cmd,dict)
  for [lhs,rhs] in items(a:dict)
    exe a:cmd lhs rhs
  endfor
endfunction

let s:commands.abbrev     = s:commands.abstract.clone()
let s:commands.abbrev.options = {"buffer":0,"cmdline":0,"delete":0}
function! s:commands.abbrev.process(bang,line1,line2,count,args)
  let args = copy(a:args)
  call s:extractopts(a:args,self.options)
  if self.options.delete
    let cmd = "unabbrev"
    let good = ""
  else
    let cmd = "noreabbrev"
  endif
  if !self.options.cmdline
    let cmd = "i" . cmd
  endif
  if self.options.delete
    let cmd = "silent! ".cmd
  endif
  if self.options.buffer
    let cmd = cmd . " <buffer>"
  endif
  let [bad, good] = s:badgood(a:args)
  if substitute(bad,'{.\{-\}.}','','g') !~ '^\k\+$'
    call s:throw("E474: Invalid argument (not a keyword: ".string(bad).")")
  endif
  if !self.options.delete && good == ""
    call s:throw("E471: Argument required".a:args[0])
  endif
  let dict = s:create_dictionary(bad,good,self.options)
  call s:abbreviate_from_dict(cmd,dict)
  if a:bang
    let i = 0
    let str = "Abolish ".join(args," ")
    let file = g:abolish_save_file
    if !isdirectory(fnamemodify(file,':h'))
      call mkdir(fnamemodify(file,':h'),'p')
    endif

    if filereadable(file)
      let old = readfile(file)
    else
      let old = ["\" Exit if :Abolish isn't available.","if !exists(':Abolish')","    finish","endif",""]
    endif
    call writefile(old + [str],file)
  endif
  return ""
endfunction

let s:commands.delete   = s:commands.abbrev.clone()
let s:commands.delete.options.delete = 1

" }}}1
" Maps {{{1

function! s:unknown_coercion(letter,word)
  return a:word
endfunction

call extend(Abolish.Coercions, {
      \ 'c': Abolish.camelcase,
      \ 'm': Abolish.mixedcase,
      \ 's': Abolish.snakecase,
      \ '_': Abolish.snakecase,
      \ 'u': Abolish.uppercase,
      \ 'U': Abolish.uppercase,
      \ '-': Abolish.dashcase,
      \ 'k': Abolish.dashcase,
      \ '.': Abolish.dotcase,
      \ ' ': Abolish.spacecase,
      \ 't': Abolish.titlecase,
      \ "function missing": s:function("s:unknown_coercion")
      \}, "keep")

function! s:coerce(transformation)
  let clipboard = &clipboard
  try
    set clipboard=
    let regbody = getreg('"')
    let regtype = getregtype('"')
    let c = v:count1
    while c > 0
      let c -= 1
      norm! yiw
      let word = @@
      let @@ = s:send(g:Abolish.Coercions,a:transformation,word)
      if !exists('begin')
        let begin = getpos("'[")
      endif
      if word !=# @@
        let changed = 1
        norm! viwpw
      else
        norm! w
      endif
    endwhile
    call setreg('"',regbody,regtype)
    call setpos("'[",begin)
    call setpos(".",begin)
    if exists("changed")
      silent! call repeat#set("\<Plug>Coerce".a:transformation)
    endif
  finally
    let &clipboard = clipboard
  endtry
endfunction

nnoremap <silent> <Plug>Coerce :<C-U>call <SID>coerce(nr2char(getchar()))<CR>

" }}}1

if !exists("g:abolish_no_mappings") || ! g:abolish_no_mappings
  nmap cr  <Plug>Coerce
endif

command! -nargs=+ -bang -bar -range=0 -complete=custom,s:Complete Abolish
      \ :exec s:dispatcher(<bang>0,<line1>,<line2>,<count>,[<f-args>])
command! -nargs=1 -bang -bar -range=0 -complete=custom,s:SubComplete Subvert
      \ :exec s:subvert_dispatcher(<bang>0,<line1>,<line2>,<count>,<q-args>)
if exists(':S') != 2
  command -nargs=1 -bang -bar -range=0 -complete=custom,s:SubComplete S
        \ :exec s:subvert_dispatcher(<bang>0,<line1>,<line2>,<count>,<q-args>)
endif

" vim:set ft=vim sw=2 sts=2:
