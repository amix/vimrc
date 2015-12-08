require 'vimrunner'
require 'vimrunner/rspec'

Vimrunner::RSpec.configure do |config|

  # Use a single Vim instance for the test suite. Set to false to use an
  # instance per test (slower, but can be easier to manage).
  config.reuse_server = false

  # Decide how to start a Vim instance. In this block, an instance should be
  # spawned and set up with anything project-specific.
  config.start_vim do
    # vim = Vimrunner.start
    # vim = Vimrunner::Server.new("/usr/local/bin/vim").start

    # Or, start a GUI instance:
    vim = Vimrunner.start_gvim

    # Setup your plugin in the Vim instance
    plugin_path = File.expand_path('../..', __FILE__)
    vim.add_plugin(plugin_path, 'plugin/multiple_cursors.vim')

    # The returned value is the Client available in the tests.
    vim
  end
end

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
  set_file_content(string)
end

def after(string)
  get_file_content().should eq normalize_string_indent(string)
  type ":q<CR>"
end

def type(string)
  string.scan(/<.*?>|./).each do |key|
    if /<.*>/.match(key)
      vim.feedkeys "\\#{key}"
    else
      vim.feedkeys key
    end
  end
  sleep 0.2
end

describe "Multiple Cursors" do
  let(:filename) { 'test.txt' }
  let(:options) { [] }

  specify "#benchmark" do
    before <<-EOF
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
      hello
    EOF

    # type ':profile start /tmp/test.result<CR>'
    # type ':profile! file *multiple_cursors.vim<CR>'
    type ':let g:multi_cursor_debug_latency=1<CR>'

    type 'VG<C-n>Vchellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello<Esc>'

    type ':echo multiple_cursors#get_latency_debug_file()<CR>'
    sleep 3
    latency_file = vim.command 'echo multiple_cursors#get_latency_debug_file()'
    puts 'latency file = ' + latency_file

    after <<-EOF
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
      hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello
    EOF
  end

end
