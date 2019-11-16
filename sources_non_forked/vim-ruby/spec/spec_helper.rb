require 'vimrunner'
require 'vimrunner/rspec'

Vimrunner::RSpec.configure do |config|
  config.reuse_server = true

  config.start_vim do
    vim = Vimrunner.start_gvim
    vim.prepend_runtimepath(File.expand_path('../..', __FILE__))
    vim.add_plugin(File.expand_path('../vim', __FILE__), 'plugin/syntax_test.vim')
    vim.set 'expandtab'
    vim.set 'shiftwidth', 2
    vim
  end

  def assert_correct_indenting(extension='rb', string)
    filename = "test.#{extension}"

    IO.write filename, string

    vim.edit filename
    vim.normal 'gg=G'
    vim.write

    expect(IO.read(filename)).to eq string
  end

  def assert_correct_highlighting(extension='rb', string, patterns, group)
    filename = "test.#{extension}"

    IO.write filename, string

    vim.edit filename

    Array(patterns).each do |pattern|
      # TODO: add a custom matcher
      expect(vim.echo("TestSyntax('#{pattern}', '#{group}')")).to eq '1'
    end
  end
end
