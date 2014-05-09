function! SectionSpec()
endfunction

describe 'section'
  before
    call airline#parts#define_text('text', 'text')
    call airline#parts#define_raw('raw', 'raw')
    call airline#parts#define_function('func', 'SectionSpec')
  end

  it 'should be able to reference default parts'
    let s = airline#section#create(['paste'])
    Expect s == '%{airline#util#wrap(airline#parts#paste(),0)}'
  end

  it 'should create sections with no separators'
    let s = airline#section#create(['text', 'raw', 'func'])
    Expect s == '%{airline#util#wrap("text",0)}raw%{airline#util#wrap(SectionSpec(),0)}'
  end

  it 'should create left sections with separators'
    let s = airline#section#create_left(['text', 'text'])
    Expect s == '%{airline#util#wrap("text",0)}%{airline#util#append("text",0)}'
  end

  it 'should create right sections with separators'
    let s = airline#section#create_right(['text', 'text'])
    Expect s == '%{airline#util#prepend("text",0)}%{airline#util#wrap("text",0)}'
  end

  it 'should prefix with accent group if provided and restore afterwards'
    call airline#parts#define('hi', {
          \ 'raw': 'hello',
          \ 'accent': 'red',
          \ })
    let s = airline#section#create(['hi'])
    Expect s == '%#__accent_red#hello%#__restore__#'
  end

  it 'should accent functions'
    call airline#parts#define_function('hi', 'Hello')
    call airline#parts#define_accent('hi', 'bold')
    let s = airline#section#create(['hi'])
    Expect s == '%#__accent_bold#%{airline#util#wrap(Hello(),0)}%#__restore__#'
  end

  it 'should parse out a section from the distro'
    call airline#extensions#load()
    let s = airline#section#create(['whitespace'])
    Expect s =~ 'airline#extensions#whitespace#check'
  end

  it 'should use parts as is if they are not found'
    let s = airline#section#create(['asdf', 'func'])
    Expect s == 'asdf%{airline#util#wrap(SectionSpec(),0)}'
  end

  it 'should force add separators for raw and missing keys'
    let s = airline#section#create_left(['asdf', 'raw'])
    Expect s == 'asdf > raw'
    let s = airline#section#create_left(['asdf', 'aaaa', 'raw'])
    Expect s == 'asdf > aaaa > raw'
    let s = airline#section#create_right(['raw', '%f'])
    Expect s == 'raw < %f'
    let s = airline#section#create_right(['%t', 'asdf', '%{getcwd()}'])
    Expect s == '%t < asdf < %{getcwd()}'
  end

  it 'should empty out parts that do not pass their condition'
    call airline#parts#define_text('conditional', 'conditional')
    call airline#parts#define_condition('conditional', '0')
    let s = airline#section#create(['conditional'])
    Expect s == '%{0 ? airline#util#wrap("conditional",0) : ""}'
  end
end

