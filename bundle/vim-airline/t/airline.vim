let g:airline_theme = 'dark'
call airline#init#bootstrap()
call airline#init#sections()
source plugin/airline.vim

function! MyFuncref(...)
  call a:1.add_raw('hello world')
  return 1
endfunction

function! MyIgnoreFuncref(...)
  return -1
endfunction

function! MyAppend1(...)
  call a:1.add_raw('hello')
endfunction

function! MyAppend2(...)
  call a:1.add_raw('world')
endfunction

describe 'airline'
  before
    let g:airline_statusline_funcrefs = []
  end

  it 'should run user funcrefs first'
    call airline#add_statusline_func('MyFuncref')
    let &statusline = ''
    call airline#update_statusline()
    Expect airline#statusline(1) =~ 'hello world'
  end

  it 'should not change the statusline with -1'
    call airline#add_statusline_funcref(function('MyIgnoreFuncref'))
    let &statusline = 'foo'
    call airline#update_statusline()
    Expect &statusline == 'foo'
  end

  it 'should support multiple chained funcrefs'
    call airline#add_statusline_func('MyAppend1')
    call airline#add_statusline_func('MyAppend2')
    call airline#update_statusline()
    Expect airline#statusline(1) =~ 'helloworld'
  end

  it 'should allow users to redefine sections'
    let g:airline_section_a = airline#section#create(['mode', 'mode'])
    call airline#update_statusline()
    Expect airline#statusline(1) =~ '%{airline#util#wrap(airline#parts#mode(),0)}%#airline_a#%#airline_a_bold#%{airline#util#wrap(airline#parts#mode(),0)}%#airline_a#'
  end

  it 'should remove funcrefs properly'
    let c = len(g:airline_statusline_funcrefs)
    call airline#add_statusline_func('MyIgnoreFuncref')
    call airline#remove_statusline_func('MyIgnoreFuncref')
    Expect len(g:airline_statusline_funcrefs) == c
  end

  it 'should overwrite the statusline with active and inactive splits'
    wincmd s
    Expect airline#statusline(1) !~ 'inactive'
    Expect airline#statusline(2) =~ 'inactive'
    wincmd c
  end

  it 'should collapse the inactive split if the variable is set true'
    let g:airline_inactive_collapse = 1
    wincmd s
    Expect getwinvar(2, '&statusline') !~ 'airline#parts#mode'
    wincmd c
  end

  it 'should not collapse the inactive split if the variable is set false'
    let g:airline_inactive_collapse = 0
    wincmd s
    Expect getwinvar(2, '&statusline') != 'airline#parts#mode'
    wincmd c
  end

  it 'should include check_mode'
    Expect airline#statusline(1) =~ 'airline#check_mode'
  end
end

