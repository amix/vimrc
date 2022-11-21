"
" Adapted from https://github.com/vim/vim/blob/master/src/testdir/runtest.vim
"
" When debugging tests it can help to write debug output:
"    call Log('oh noes')
"

function RunTest(test)
  if exists("*SetUp")
    call SetUp()
  endif

  try
    execute 'call '.a:test
  catch
    call Exception()
    let s:errored = 1
  endtry

  if exists("*TearDown")
    call TearDown()
  endif
endfunction

function Log(msg)
  if type(a:msg) == type('')
    call add(s:messages, a:msg)
  elseif type(a:msg) == type([])
    call extend(s:messages, a:msg)
  else
    call add(v:errors, 'Exception: unsupported type: '.type(a:msg))
  endif
endfunction

function Exception()
  call add(v:errors, v:throwpoint.'..'.'Exception: '.v:exception)
endfunction

" Shuffles list in place.
function Shuffle(list)
  " Fisher-Yates-Durstenfeld-Knuth
  let n = len(a:list)
  if n < 2
    return a:list
  endif
  for i in range(0, n-2)
    let j = Random(0, n-i-1)
    let e = a:list[i]
    let a:list[i] = a:list[i+j]
    let a:list[i+j] = e
  endfor
  return a:list
endfunction

" Returns a pseudorandom integer i such that 0 <= i <= max
function Random(min, max)
  if has('unix')
    let i = system('echo $RANDOM')  " 0 <= i <= 32767
  else
    let i = system('echo %RANDOM%')  " 0 <= i <= 32767
  endif
  return i * (a:max - a:min + 1) / 32768 + a:min
endfunction

function FriendlyName(test_name)
  return substitute(a:test_name[5:-3], '_', ' ', 'g')
endfunction

function Align(left, right)
  if type(a:right) == type([])
    let result = []
    for s in a:right
      if empty(result)
        call add(result, printf('%-'.s:indent.'S', a:left).s)
      else
        call add(result, printf('%-'.s:indent.'S',     '').s)
      endif
    endfor
    return result
  endif

  return printf('%-'.s:indent.'S', a:left).a:right
endfunction

let g:testname = expand('%')
let s:errored = 0
let s:done = 0
let s:fail = 0
let s:errors = 0
let s:messages = []
let s:indent = ''

call Log(g:testname.':')

" Source the test script.
try
  source %
catch
  let s:errors += 1
  call Exception()
endtry

" Locate the test functions.
set nomore
redir @q
silent function /^Test_
redir END
let s:tests = split(substitute(@q, 'function \(\k*()\)', '\1', 'g'))

" If there is another argument, filter test-functions' names against it.
if argc() > 1
  let s:tests = filter(s:tests, 'v:val =~ argv(1)')
endif

let s:indent = max(map(copy(s:tests), {_, val -> len(FriendlyName(val))}))

" Run the tests in random order.
for test in Shuffle(s:tests)
  call RunTest(test)
  let s:done += 1

  let friendly_name = FriendlyName(test)
  if len(v:errors) == 0
    call Log(Align(friendly_name, ' - ok'))
  else
    if s:errored
      let s:errors += 1
      let s:errored = 0
    else
      let s:fail += 1
    endif
    call Log(Align(friendly_name, ' - not ok'))

    let i = 0
    for error in v:errors
      if i != 0
        call Log(Align('','   ! ----'))
      endif
      for trace in reverse(split(error, '\.\.'))
        call Log(Align('', '   ! '.trace))
      endfor
      let i += 1
    endfor

    let v:errors = []
  endif
endfor

let summary = [
      \ s:done.(  s:done   == 1 ? ' test'    : ' tests'),
      \ s:errors.(s:errors == 1 ? ' error'   : ' errors'),
      \ s:fail.(  s:fail   == 1 ? ' failure' : ' failures'),
      \ ]
call Log('')
call Log(join(summary, ', '))

split messages.log
call append(line('$'), s:messages)
write

qall!

