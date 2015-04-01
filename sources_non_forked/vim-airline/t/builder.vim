let g:airline_theme = 'dark'
call airline#init#bootstrap()

describe 'active builder'
  before
    let s:builder = airline#builder#new({'active': 1})
  end

  it 'should start with an empty statusline'
    let stl = s:builder.build()
    Expect stl == ''
  end

  it 'should transition colors from one to the next'
    call s:builder.add_section('Normal', 'hello')
    call s:builder.add_section('Search', 'world')
    let stl = s:builder.build()
    Expect stl =~ '%#Normal#hello%#Normal_to_Search#>%#Search#world'
  end

  it 'should reuse highlight group if background colors match'
    highlight Foo1 ctermfg=1 ctermbg=2
    highlight Foo2 ctermfg=1 ctermbg=2
    call s:builder.add_section('Foo1', 'hello')
    call s:builder.add_section('Foo2', 'world')
    let stl = s:builder.build()
    Expect stl =~ '%#Foo1#hello>world'
  end

  it 'should switch highlight groups if foreground colors differ'
    highlight Foo1 ctermfg=1 ctermbg=2
    highlight Foo2 ctermfg=2 ctermbg=2
    call s:builder.add_section('Foo1', 'hello')
    call s:builder.add_section('Foo2', 'world')
    let stl = s:builder.build()
    Expect stl =~ '%#Foo1#hello%#Foo1_to_Foo2#>%#Foo2#world'
  end

  it 'should split left/right sections'
    call s:builder.split()
    let stl = s:builder.build()
    Expect stl =~ '%='
  end

  it 'after split, sections use the right separator'
    call s:builder.split()
    call s:builder.add_section('Normal', 'hello')
    call s:builder.add_section('Search', 'world')
    let stl = s:builder.build()
    Expect stl =~ 'hello%#Normal_to_Search#<%#Search#world'
  end

  it 'should not repeat the same highlight group'
    call s:builder.add_section('Normal', 'hello')
    call s:builder.add_section('Normal', 'hello')
    let stl = s:builder.build()
    Expect stl == '%#Normal#hello>hello'
  end

  it 'should replace accent groups with the specified group'
    call s:builder.add_section('Normal', '%#__accent_foo#hello')
    let stl = s:builder.build()
    Expect stl == '%#Normal#%#Normal_foo#hello'
  end

  it 'should replace two accent groups with correct groups'
    call s:builder.add_section('Normal', '%#__accent_foo#hello%#__accent_bar#world')
    let stl = s:builder.build()
    Expect stl =~ '%#Normal_foo#hello%#Normal_bar#world'
  end

  it 'should special restore group should go back to previous group'
    call s:builder.add_section('Normal', '%#__restore__#')
    let stl = s:builder.build()
    Expect stl !~ '%#__restore__#'
    Expect stl =~ '%#Normal#'
  end

  it 'should blend colors from the left through the split to the right'
    call s:builder.add_section('Normal', 'hello')
    call s:builder.split()
    call s:builder.add_section('Search', 'world')
    let stl = s:builder.build()
    Expect stl =~ 'Normal_to_Search'
  end
end

describe 'inactive builder'
  before
    let s:builder = airline#builder#new({'active': 0})
  end

  it 'should transition colors from one to the next'
    call s:builder.add_section('Normal', 'hello')
    call s:builder.add_section('Search', 'world')
    let stl = s:builder.build()
    Expect stl =~ '%#Normal_inactive#hello%#Normal_to_Search_inactive#>%#Search_inactive#world'
  end

  it 'should not render accents'
    call s:builder.add_section('Normal', '%#__accent_foo#hello%#foo#foo%#__accent_bar#world')
    let stl = s:builder.build()
    Expect stl == '%#Normal_inactive#hello%#foo_inactive#fooworld'
  end
end

