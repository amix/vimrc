source plugin/airline.vim
doautocmd VimEnter

describe 'commands'
  it 'should toggle off and on'
    execute 'AirlineToggle'
    Expect exists('#airline') to_be_false
    execute 'AirlineToggle'
    Expect exists('#airline') to_be_true
  end

  it 'should toggle whitespace off and on'
    call airline#extensions#load()
    execute 'AirlineToggleWhitespace'
    Expect exists('#airline_whitespace') to_be_false
    execute 'AirlineToggleWhitespace'
    Expect exists('#airline_whitespace') to_be_true
  end

  it 'should display theme name with no args'
    execute 'AirlineTheme simple'
    Expect g:airline_theme == 'simple'
    execute 'AirlineTheme dark'
    Expect g:airline_theme == 'dark'
  end

  it 'should have a refresh command'
    Expect exists(':AirlineRefresh') to_be_true
  end
end

