require 'vimrunner'
require 'tempfile'

PWD = File.expand_path File.dirname(__FILE__)

RSpec.configure do |config|

  config.before(:suite) do
    VIM = Vimrunner.start
    VIM.add_plugin(File.expand_path('../..', __FILE__), 'plugin/scala.vim')
  end

  config.after(:suite) do
    VIM.kill
  end
end

def sort_fixture_across_groups(name)
  fixture_path = "#{PWD}/fixtures/#{name}.scala"

  temp_file = Tempfile.new('vim-scala-')
  temp_file.write File.read(fixture_path)
  temp_file.rewind

  VIM.edit temp_file.path

  VIM.command "let g:scala_sort_across_groups=1"
  VIM.command "SortScalaImports"
  VIM.write

  temp_file.rewind
  output = temp_file.read

  temp_file.close
  temp_file.unlink

  output
end

def expected(name)
  path = "#{PWD}/fixtures/#{name}.expected.scala"
  File.read(path)
end

