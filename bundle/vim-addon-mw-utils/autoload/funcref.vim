" funcref.vim
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2010-01-03.
" @Revision:    0.1.0

" documentation see doc/funcref.txt

" usage:
"  funcref#Function("filename#Function")
"  optionally pass arguments:
"  funcref#Function("filename#Function",{'args': [2]})
"  optionally define self:
"  funcref#Function("filename#Function",{'self': object})
function! funcref#Function(name,...)
  let d = a:0 > 0 ? a:1 : {}
  let d['faked_function_reference'] = a:name
  return d
endfunction

" args : same as used for call(f,[args], self)
" f must be either
"   - a string which can be evaled (use "return 'value'" to return a value)
"   - a Vim function reference created by function('..')
"   - a faked function reference created by funcref#Function(..)
"
" the last "self" argument can be overriden by the function reference
" You can pass arguments in a closure like style
function! funcref#Call(...)
  let args = copy(a:000)

  " add parameters:
  if (len(args) < 2)
    call add(args, [])
  endif


  let isDict = type(args[0]) == type({})

  " prepend parameters which were passed by faked function reference:
  if isDict &&  has_key(args[0], 'args')
    let args[1] = args[0]['args']+args[1]
  endif

  " always pass self. this way you can call functions from dictionaries not
  " refering to self
  if (len(args) < 3)
    call add(args, {})
  endif

  " the funcref overrides self:
  if isDict && has_key(args[0], 'self')
    let args[2] = args[0]['self']
  endif

  if type(a:1) == 2
    " funcref: function must have been laoded
    return call(function('call'), args)
  elseif isDict && has_key(args[0], 'faked_function_reference')
    let Fun = args[0]['faked_function_reference']
    if type(Fun) == type('')
        \ && (Fun[:len('return ')-1] == 'return ' 
              \ || Fun[:len('call ')-1] == 'call '
              \ || Fun[:len('if ')-1] == 'if '
              \ || Fun[:len('let ')-1] == 'let '
              \ || Fun[:len('echo ')-1] == 'echo '
              \ || Fun[:len('exec ')-1] == 'exec '
              \ || Fun[:len('debug ')-1] == 'debug ')
      " it doesn't make sense to list all vim commands here
      " So if you want to execute another action consider using 
      " funcref#Function('exec  '.string('aw')) or such

      " function is a String, call exec
      let ARGS = args[1]
      let SELF = args[2]
      exec Fun
    else 
      " pseudo function, let's load it..
      if type(Fun) == 1
        if !exists('*'.Fun)
          " lazily load function
          let file = substitute(substitute(Fun,'#[^#]*$','',''),'#','/','g')
          exec 'runtime /autoload/'.file.'.vim'
        endif
        let Fun2 = function(Fun)
      else
        let Fun2 = Fun
      endif
      let args[0] = Fun
      return call(function('call'), args)
    endif
  else
    " no function, return the value
    return args[0]
  endif
endfunction
