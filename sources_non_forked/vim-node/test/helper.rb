require "minitest/autorun"
require "vimrunner"
require "fileutils"
require "tempfile"

MiniTest::Unit::TestCase.define_singleton_method(:test_order) do :alpha end

begin
  require "minitest/reporters"
  MiniTest::Reporters.use! MiniTest::Reporters::SpecReporter.new
rescue LoadError
end

$vimrc = File.expand_path("../vimrc", __FILE__)
$vim = Vimrunner::Server.new(:vimrc => $vimrc).start
Minitest::Unit.after_tests { $vim.kill }

module WithTemporaryDirectory
  def self.included(base)
    require "tmpdir"
  end

  def setup
    super
    # Mac has the temporary directory symlinked, so need File.realpath to
    # match the paths that Vim returns.
    @dir = File.realpath(Dir.mktmpdir) 
  end

  def teardown
    FileUtils.remove_entry_secure @dir 
    super
  end
end

def touch(path, contents = nil)
  FileUtils.mkpath File.dirname(path)
  return FileUtils.touch(path) if contents.nil? || contents.empty?
  File.open(path, "w") {|f| f.write contents }
end

CORE_MODULES = %w[_debugger _http_agent _http_client _http_common
  _http_incoming _http_outgoing _http_server _linklist _stream_duplex
  _stream_passthrough _stream_readable _stream_transform _stream_writable
  _tls_legacy _tls_wrap assert buffer child_process cluster console
  constants crypto dgram dns domain events freelist fs http https module
  net node os path punycode querystring readline repl smalloc stream
  string_decoder sys timers tls tty url util vm zlib]
