call airline#init#bootstrap()

function! Util1()
  let g:count += 1
endfunction
function! Util2()
  let g:count += 2
endfunction
function! Util3(...)
  let g:count = a:0
endfunction

describe 'util'
  before
    let g:count = 0
  end

  it 'has append wrapper function'
    Expect airline#util#append('', 0) == ''
    Expect airline#util#append('1', 0) == '  > 1'
  end

  it 'has prepend wrapper function'
    Expect airline#util#prepend('', 0) == ''
    Expect airline#util#prepend('1', 0) == '1 < '
  end

  it 'has getwinvar function'
    Expect airline#util#getwinvar(1, 'asdf', '123') == '123'
    call setwinvar(1, 'vspec', 'is cool')
    Expect airline#util#getwinvar(1, 'vspec', '') == 'is cool'
  end

  it 'has exec funcrefs helper functions'
    call airline#util#exec_funcrefs([function('Util1'), function('Util2')])
    Expect g:count == 3

    call airline#util#exec_funcrefs([function('Util3')], 1, 2, 3, 4)
    Expect g:count == 4
  end

  it 'should ignore minwidth if less than 0'
    Expect airline#util#append('foo', -1) == '  > foo'
    Expect airline#util#prepend('foo', -1) == 'foo < '
    Expect airline#util#wrap('foo', -1) == 'foo'
  end

  it 'should return empty if winwidth() > minwidth'
    Expect airline#util#append('foo', 99999) == ''
    Expect airline#util#prepend('foo', 99999) == ''
    Expect airline#util#wrap('foo', 99999) == ''
  end
end

