# -*- encoding: utf-8 -*-
require 'spec_helper'

def set_file_content(string)
  string = normalize_string_indent(string)
  File.open(filename, 'w'){ |f| f.write(string) }
  vim.edit filename
end

def get_file_content()
  vim.write
  IO.read(filename).strip
end

def before(string)
  options.each { |x| vim.command(x) }
  set_file_content(string)
end

def after(string)
  expect(get_file_content()).to eq normalize_string_indent(string)
end

def type(string)
  string.scan(/<.*?>|./).each do |key|
    if /<.*>/.match(key)
      vim.feedkeys "\\#{key}"
    else
      vim.feedkeys key
    end
  end
end

describe "Multiple Cursors op pending & exit from insert|visual mode" do
  let(:filename) { 'test.txt' }
  let(:options) { ['let g:multi_cursor_exit_from_insert_mode = 0',
                   'let g:multi_cursor_exit_from_visual_mode = 0'] }
  # the default value of g:multi_cursor_normal_maps already works
  # for testing operator-pending

  specify "#paste from unnamed register to 3 cursors" do
    before <<-EOF
      yankme
      a b c
      a b c
      a b c
    EOF

    type 'yiwj<C-n><C-n><C-n>vwwp<Esc>'

    after <<-EOF
      yankme
      a b cyankme
      a b cyankme
      a b cyankme
    EOF
  end

  specify "#paste buffer normal caw then p" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwcaw<Esc>bP<Esc>'

    after <<-EOF
      jan hello world
      feb hello world
      mar hello world
    EOF
  end

  specify "#paste buffer normal C then ABC then p" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwCABC <Esc>p<Esc>'

    after <<-EOF
      hello ABC jan world
      hello ABC feb world
      hello ABC mar world
    EOF
  end

  specify "#paste buffer normal daw then P" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwdawbP<Esc>'

    after <<-EOF
      jan hello world
      feb hello world
      mar hello world
    EOF
  end

  specify "#paste buffer normal D then P" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwwhDbhP<Esc>'

    after <<-EOF
      hello world jan
      hello world feb
      hello world mar
    EOF
  end

  specify "#paste buffer normal s then p" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vws1<Esc>p<Esc>'

    after <<-EOF
      hello 1jan world
      hello 1feb world
      hello 1mar world
    EOF
  end

  specify "#normal mode '0': goes to 1st char of line" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vw0dw<Esc><Esc>'

    after <<-EOF
      jan world
      feb world
      mar world
    EOF
  end

  specify "#normal mode 'd0': deletes backward to 1st char of line" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwd0<Esc><Esc>'

    after <<-EOF
      jan world
      feb world
      mar world
    EOF
  end

end

describe "Multiple Cursors when using insert mappings" do
  let(:filename) { 'test.txt' }
  let(:options) { ['set timeoutlen=10000',
                   'imap jj <esc>',
                   'imap jojo dude',
                   'imap jk <esc>:%s/bla/hey/g<cr>',
                   'let g:multi_cursor_exit_from_insert_mode = 1',
                   'let g:multi_cursor_exit_from_visual_mode = 1'] }
  specify "#mapping doing <Esc>" do
    before <<-EOF
      hello world!
      hello world!
      bla bla bla
      bla bla bla
    EOF

    type 'w<C-n><C-n>cjjidude<Esc>'

    after <<-EOF
      hello dude!
      hello !
      bla bla bla
      bla bla bla
    EOF
  end

  specify "#mapping doing <Esc> and running a command" do
    before <<-EOF
      hello world!
      hello world!
      bla bla bla
      bla bla bla
    EOF

    type 'w<C-n><C-n>ctherejk'

    after <<-EOF
      hello there!
      hello there!
      hey hey hey
      hey hey hey
    EOF
  end

  specify "#mapping using more than 2 characters" do
    before <<-EOF
      hello
      hello
      bla bla bla
      bla bla bla
    EOF

    type '<C-n><C-n>A jojo<Esc>'

    after <<-EOF
      hello dude
      hello dude
      bla bla bla
      bla bla bla
    EOF
  end

  specify "#unused mapping" do
    before <<-EOF
      hello world!
      hello world!
      bla bla bla
      bla bla bla
    EOF

    type 'w<C-n><C-n>chey joseph blah blah blah<Esc>'

    after <<-EOF
      hello hey joseph blah blah blah!
      hello hey joseph blah blah blah!
      bla bla bla
      bla bla bla
    EOF
  end

end

describe "Multiple Cursors when normal_maps is empty" do
  let(:filename) { 'test.txt' }
  let(:options) { ['let g:multi_cursor_normal_maps = {}'] }

  # Operator-pending commands are handled correctly thanks to their inclusion
  # in `g:multi_cursor_normal_maps`.
  #
  # When an operator-pending command like 'd' is missing from that setting's
  # value, then it should result in a no-op, but we should still remain in
  # multicursor mode.
  specify "#normal mode 'd'" do
    before <<-EOF
      hello
      hello
    EOF

    type '<C-n><C-n>vdx<Esc>'

    after <<-EOF
      hell
      hell
    EOF
  end

end

describe "Multiple Cursors when visual_maps is empty" do
  let(:filename) { 'test.txt' }
  let(:options) { ['let g:multi_cursor_visual_maps = {}'] }

  # Operator-pending commands are handled correctly thanks to their inclusion
  # in `g:multi_cursor_visual_maps`.
  #
  # When an operator-pending command like 'f' is missing from that setting's
  # value, then it should result in a no-op, but we should still remain in
  # multicursor mode.
  specify "#visual mode 'i'" do
    before <<-EOF
      hello world x
      hello world x
    EOF

    type 'fw<C-n><C-n>fx<Esc>'

    after <<-EOF
      hello  x
      hello  x
    EOF
  end

end

describe "Multiple Cursors when changing the line count" do
  let(:filename) { 'test.txt' }
  let(:options) { ['set backspace=indent,eol,start'] }

  specify "#backspace on first char of the line, then carriage return" do
    before <<-EOF
      madec

      antoine
      andre
      joseph
    EOF

    type 'Gvip<C-n>i<BS><cr>'

    after <<-EOF
      madec

      antoine
      andre
      joseph
    EOF
  end

  specify "#del at EOL, then carriage return" do
    before <<-EOF
      madec
      antoine
      joseph

      andre
    EOF

    type 'vip<C-n>A<DEL><cr>'

    after <<-EOF
      madec
      antoine
      joseph

      andre
    EOF
  end

end

describe "Multiple Cursors misc" do
  let(:filename) { 'test.txt' }
  let(:options) { ['set autoindent'] }

  specify "#paste buffer normal x then p" do
    before <<-EOF
      jan
      feb
      mar
    EOF

    type '<C-v>jj<C-n>xp<Esc>'

    after <<-EOF
      ajn
      efb
      amr
    EOF
  end

  specify "#paste buffer visual y then p" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwvelywhp<Esc>'

    after <<-EOF
      hello jan jan world
      hello feb feb world
      hello mar mar world
    EOF
  end

  specify "#paste buffer initial visual y then P" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type 'wywb<C-n><C-n><C-n>p<Esc>'

    after <<-EOF
      jan  jan world
      jan  feb world
      jan  mar world
    EOF
  end

  specify "#paste buffer visual y then P" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwvely^P<Esc>'

    after <<-EOF
      jan hello jan world
      feb hello feb world
      mar hello mar world
    EOF
  end

  specify "#paste buffer visual Y then P" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>vwvY^P<Esc>'

    after <<-EOF
      hello jan world
      hello jan world
      hello feb world
      hello feb world
      hello mar world
      hello mar world
    EOF
  end

  specify "#multiline replacement" do
    before <<-EOF
      hello
      hello
      hello
    EOF

    type '<C-n><C-n><C-n>cworld<Esc>'

    after <<-EOF
      world
      world
      world
    EOF
  end

  specify "#single line replacement" do
    before <<-EOF
      hello hello hello
    EOF

    type '<C-n><C-n><C-n>cworld<Esc>'

    after <<-EOF
      world world world
    EOF
  end

  specify "#mixed line replacement" do
    before <<-EOF
      hello hello
      hello
    EOF

    type '<C-n><C-n><C-n>cworld<Esc>'

    after <<-EOF
      world world
      world
    EOF
  end

  specify "#new line in insert mode" do
    before <<-EOF
      hello
      hello
    EOF

    type '<C-n><C-n>chello<CR>world<Esc>'

    after <<-EOF
      hello
      world
      hello
      world
    EOF
  end

  specify "#new line in insert mode middle of line" do
    before <<-EOF
      hello world
      hello world
    EOF

    type '<C-n><C-n>vlxi<cr><Esc>'

    after <<-EOF
      hello
      world
      hello
      world
    EOF
  end

  specify "#multiple new lines on one line in insert mode" do
    before <<-EOF
      'a','b','c','d','e'
    EOF

    type 'f,v<C-n><C-n><C-n>c<CR><Esc>'

    after <<-EOF
      'a'
      'b'
      'c'
      'd'
      'e'
    EOF
  end

  specify "#multiple new lines on one line in insert mode with indents" do
    before <<-EOF
      'a','b','c','d','e'
    EOF

    type '4i<Space><Esc>f,v<C-n><C-n><C-n>c<CR><Esc>:%s/^/^<CR>'

    after <<-EOF
      ^    'a'
      ^    'b'
      ^    'c'
      ^    'd'
      ^    'e'
    EOF
  end

  specify "#normal mode 'o'" do
    before <<-EOF
      hello
      hello
    EOF

    type '<C-n><C-n>voworld<Esc>'

    after <<-EOF
      hello
      world
      hello
      world
    EOF
  end

  specify "#normal mode 'O'" do
    before <<-EOF
      hello
      hello
    EOF

    type '<C-n><C-n>vOworld<Esc>'

    after <<-EOF
      world
      hello
      world
      hello
    EOF
  end

  specify "#find command basic" do
    before <<-EOF
      hello
      hello
    EOF

    vim.normal ':MultipleCursorsFind hello<CR>'
    type 'cworld<Esc>'

    after <<-EOF
      world
      world
    EOF
  end

  specify "#find command start-of-line" do
    before <<-EOF
      hello
      world

      hello
      world
    EOF

    vim.normal ':MultipleCursorsFind ^<CR>'
    type 'Ibegin<Esc>'

    after <<-EOF
      beginhello
      beginworld
      begin
      beginhello
      beginworld
    EOF
  end

  specify "#find command end-of-line" do
    before <<-EOF
      hello
      world

      hello
      world
    EOF

    vim.normal ':MultipleCursorsFind $<CR>'
    type 'Iend<Esc>'

    after <<-EOF
      helloend
      worldend
      end
      helloend
      worldend
    EOF
  end

  specify "#visual line mode replacement" do
    before <<-EOF
      hello world
      hello world
    EOF

    type '<C-n><C-n>Vchi!<Esc>'

    after <<-EOF
      hi!
      hi!
    EOF
  end

  specify "#skip key" do
    before <<-EOF
      hello
      hello
      hello
    EOF

    type '<C-n><C-n><C-x>cworld<Esc>'

    after <<-EOF
      world
      hello
      world
    EOF
  end

  specify "#prev key" do
    before <<-EOF
      hello
      hello
      hello
    EOF

    type '<C-n><C-n><C-n><C-p>cworld<Esc>'

    after <<-EOF
      world
      world
      hello
    EOF
  end

  specify "#visual mode 'i'" do
    before <<-EOF
      hi (hello world jan) bye
      hi (hello world feb) bye
      hi (hello world mar) bye
    EOF

    type 'fw<C-n><C-n><C-n>ibcone<Esc>'

    after <<-EOF
      hi (one) bye
      hi (one) bye
      hi (one) bye
    EOF
  end

  specify "#visual mode 'a'" do
    before <<-EOF
      hi (hello world jan) bye
      hi (hello world feb) bye
      hi (hello world mar) bye
    EOF

    type 'fw<C-n><C-n><C-n>abcone<Esc>'

    after <<-EOF
      hi one bye
      hi one bye
      hi one bye
    EOF
  end

  specify "#visual mode 'f'" do
    before <<-EOF
      hi (hello world jan) bye
      hi (hello world feb) bye
      hi (hello world mar) bye
    EOF

    type 'fw<C-n><C-n><C-n>f)cone<Esc>'

    after <<-EOF
      hi (hello one bye
      hi (hello one bye
      hi (hello one bye
    EOF
  end

  specify "#visual mode 'F'" do
    before <<-EOF
      hi (hello world jan) bye
      hi (hello world feb) bye
      hi (hello world mar) bye
    EOF

    type 'fw<C-n><C-n><C-n>F(cbefore<Esc>'

    after <<-EOF
      hi beforeorld jan) bye
      hi beforeorld feb) bye
      hi beforeorld mar) bye
    EOF
  end

  specify "#visual mode 't'" do
    before <<-EOF
      hello.jan
      hello hi.feb
      hello hi bye.mar
    EOF

    type '<C-n><C-n><C-n>t.cone<Esc>'

    after <<-EOF
      one.jan
      one.feb
      one.mar
    EOF
  end

  specify "#visual mode 'T'" do
    before <<-EOF
      jan.world
      feb.hi world
      mar.bye hi world
    EOF

    type 'fw<C-n><C-n><C-n>T.cbefore<Esc>'

    after <<-EOF
      jan.beforeorld
      feb.beforeorld
      mar.beforeorld
    EOF
  end

  specify "#visual line mode 'f'" do
    before <<-EOF
      hello jan world
      hello feb world
      hello mar world
    EOF

    type '<C-n><C-n><C-n>VfwvAafter<Esc>'

    after <<-EOF
      hello jan wafterorld
      hello feb wafterorld
      hello mar wafterorld
    EOF
  end

  specify "#visual mode 'I'" do
    before <<-EOF
      hello world jan
      hello world feb
      hello world mar
    EOF

    type 'w<C-n><C-n><C-n>Ibefore<Esc>'

    after <<-EOF
      hello beforeworld jan
      hello beforeworld feb
      hello beforeworld mar
    EOF
  end

  specify "#visual mode 'A'" do
    before <<-EOF
      hello world jan
      hello world feb
      hello world mar
    EOF

    type 'w<C-n><C-n><C-n>Aafter<Esc>'

    after <<-EOF
      hello worldafter jan
      hello worldafter feb
      hello worldafter mar
    EOF
  end

  specify "#resize regions visual mode 'I'" do
    before <<-EOF
      hello world jan
      hello world feb
      hello world mar
    EOF

    type 'w<C-n><C-n><C-n>hhhIbefore<Esc>'

    after <<-EOF
      hello beforeworld jan
      hello beforeworld feb
      hello beforeworld mar
    EOF
  end

  specify "#resize regions visual mode 'A'" do
    before <<-EOF
      hello world jan
      hello world feb
      hello world mar
    EOF

    type 'w<C-n><C-n><C-n>hhhAbefore<Esc>'

    after <<-EOF
      hello wobeforerld jan
      hello wobeforerld feb
      hello wobeforerld mar
    EOF
  end

  specify "#no word boundries visual mode 'I'" do
    before <<-EOF
      hello hibye world
      hello hibye world
      hello hibye world
    EOF

    vim.normal ':MultipleCursorsFind bye<CR>'
    type 'Ibefore<Esc>'

    after <<-EOF
      hello hibeforebye world
      hello hibeforebye world
      hello hibeforebye world
    EOF
  end

  specify "#variable-length regions visual mode 'I'" do
    before <<-EOF
      hello hii world
      hello hiiii world
      hello hiiiiii world
    EOF

    vim.normal ':MultipleCursorsFind \<hi*\><CR>'
    type 'Ibefore<Esc>'

    after <<-EOF
      hello beforehii world
      hello beforehiiii world
      hello beforehiiiiii world
    EOF
  end

  specify "#normal mode 'I'" do
    before <<-EOF
      hello
      hello
    EOF

    type '<C-n><C-n>vIworld <Esc>'

    after <<-EOF
      world hello
      world hello
    EOF
  end

  specify "#normal mode 'A'" do
    before <<-EOF
      hello
      hello
    EOF

    type '<C-n><C-n>vA world<Esc>'

    after <<-EOF
      hello world
      hello world
    EOF
  end

  specify "#undo" do
    before <<-EOF
      hello
      hello
    EOF

    type '<C-n><C-n>cworld<Esc>u'

    after <<-EOF
      hello
      hello
    EOF
  end

  specify "#multiline visual mode" do
    before <<-EOF
      hello
      hello
    EOF

    type 'Vj<C-n>A world<Esc>'

    after <<-EOF
      hello world
      hello world
    EOF
  end

  specify "#set paste mode" do
    before <<-EOF
      hello
      hello
    EOF

    type ':set paste<CR><C-n><C-n>cworld<Esc>:set nopaste<CR>'

    after <<-EOF
      world
      world
    EOF
  end

  specify "#multi-byte strings" do
    before <<-EOF
      こんにちわビム
      世界の中心でビムを叫ぶ
      ビム大好き
    EOF

    type '/ビム<CR><C-n><C-n><C-n>cヴィム<ESC>'

    after <<-EOF
      こんにちわヴィム
      世界の中心でヴィムを叫ぶ
      ヴィム大好き
    EOF
  end

end
