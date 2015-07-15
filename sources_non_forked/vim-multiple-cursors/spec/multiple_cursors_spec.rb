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
  get_file_content().should eq normalize_string_indent(string)
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

describe "Multiple Cursors" do
  let(:filename) { 'test.txt' }
  let(:options) { [] }

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

end
