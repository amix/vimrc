require 'vimrunner'
require 'vimrunner/rspec'
require 'vimrunner/server'

# Explicitly enable usage of "should".
RSpec.configure do |config|
    config.expect_with(:rspec) { |c| c.syntax = :should }
end

Vimrunner::RSpec.configure do |config|
  # Use a single Vim instance for the test suite. Set to false to use an
  # instance per test (slower, but can be easier to manage).
  # This requires using gvim, otherwise it hangs after a few tests.
  config.reuse_server = ENV['VIMRUNNER_REUSE_SERVER'] == '1' ? true : false

  config.start_vim do
    exe = config.reuse_server ? Vimrunner::Platform.gvim : Vimrunner::Platform.vim
    vimrc = File.expand_path("../vimrc", __FILE__)
    vim = Vimrunner::Server.new(:executable => exe,
                                :vimrc => vimrc).start
    # More friendly killing.
    # Otherwise profiling information might not be written.
    def vim.kill
      normal(':qall!<CR>')

      Timeout.timeout(5) do
        sleep 0.1 while server.running?
      end
    end

    plugin_path = File.expand_path('../..', __FILE__)
    vim.command "set rtp^=#{plugin_path}"
    vim.command "set filetype=python"

    def shiftwidth
      @shiftwidth ||= vim.echo("exists('*shiftwidth') ? shiftwidth() : &sw").to_i
    end
    def tabstop
      @tabstop ||= vim.echo("&tabstop").to_i
    end
    def indent
      vim.echo("indent('.')").to_i
    end
    def previous_indent
      pline = vim.echo("line('.')").to_i - 1
      vim.echo("indent('#{pline}')").to_i
    end
    def proposed_indent
      line = vim.echo("line('.')")
      col = vim.echo("col('.')")
      indent_value = vim.echo("GetPythonPEPIndent(#{line})").to_i
      vim.command("call cursor(#{line}, #{col})")
      return indent_value
    end
    def multiline_indent(prev, default)
      i = vim.echo("get(g:, 'python_pep8_indent_multiline_string', 0)").to_i
      return (i == -2 ? default : i), i < 0 ? (i == -1 ? prev : default) : i
    end
    def hang_closing
      i = vim.echo("get(g:, 'python_pep8_indent_hang_closing', 0)").to_i
      return (i != 0)
    end
    def set_hang_closing(value)
      i = value ? 1 : 0
      vim.command("let g:python_pep8_indent_hang_closing=#{i}")
    end

    vim
  end
end
