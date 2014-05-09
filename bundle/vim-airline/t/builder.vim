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
    call s:builder.add_section('NonText', 'world')
    let stl = s:builder.build()
    Expect stl =~ '%#Normal#hello%#Normal_to_NonText#>%#NonText#world'
  end

  it 'should split left/right sections'
    call s:builder.split()
    let stl = s:builder.build()
    Expect stl =~ '%='
  end

  it 'after split, sections use the right separator'
    call s:builder.split()
    call s:builder.add_section('Normal', 'hello')
    call s:builder.add_section('NonText', 'world')
    let stl = s:builder.build()
    Expect stl =~ '%#Normal#hello%#Normal_to_NonText#<%#NonText#world'
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
end

describe 'inactive builder'
  before
    let s:builder = airline#builder#new({'active': 0})
  end

  it 'should transition colors from one to the next'
    call s:builder.add_section('Normal', 'hello')
    call s:builder.add_section('NonText', 'world')
    let stl = s:builder.build()
    Expect stl =~ '%#Normal_inactive#hello%#Normal_to_NonText_inactive#>%#NonText_inactive#world'
  end

  it 'should not render accents'
    call s:builder.add_section('Normal', '%#__accent_foo#hello%#foo#foo%#__accent_bar#world')
    let stl = s:builder.build()
    Expect stl == '%#Normal_inactive#hello%#foo_inactive#fooworld'
  end
end

