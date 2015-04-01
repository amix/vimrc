let g:airline#extensions#default#layout = [
      \ [ 'c', 'a', 'b', 'warning' ],
      \ [ 'x', 'z', 'y' ]
      \ ]

source plugin/airline.vim
doautocmd VimEnter

describe 'default'
  before
    let s:builder = airline#builder#new({'active': 1})
  end

  it 'should use the layout'
    call airline#extensions#default#apply(s:builder, { 'winnr': 1, 'active': 1 })
    let stl = s:builder.build()
    Expect stl =~ 'airline_c_to_airline_a'
    Expect stl =~ 'airline_a_to_airline_b'
    Expect stl =~ 'airline_b_to_airline_warning'
    Expect stl =~ 'airline_x_to_airline_z'
    Expect stl =~ 'airline_z_to_airline_y'
  end

  it 'should only render warning section in active splits'
    wincmd s
    Expect airline#statusline(1) =~ 'warning'
    Expect airline#statusline(2) !~ 'warning'
    wincmd c
  end
end

