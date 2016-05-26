require 'rspec/expectations'
require 'tmpdir'
require 'vimrunner'

class Buffer
  attr_reader :file, :vim

  def initialize(vim, type)
    @file = "test.#{type}"
    @vim  = vim
  end

  def reindent(code)
    open code do
      # remove all indentation
      vim.normal 'ggVG999<<'
      # force vim to indent the file
      vim.normal 'gg=G'
    end
  end

  def syntax(code, pattern)
    read code
    # move cursor the pattern
    vim.search pattern
    # get a list of the syntax element
    vim.echo <<-EOF
      map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
    EOF
  end

  private

  def open(code)
    read code
    # run vim commands
    yield if block_given?
    vim.write
    IO.read(file)
  end

  def read(code)
    File.open(file, 'w') { |f| f.write code }
    vim.edit file
  end
end

def cleanup(string)
  whitespace = string.scan(/^\s*/).first
  string.gsub(/^#{whitespace}/, '')
end

{ be_elixir_indentation:  :ex,
  be_eelixir_indentation: :eex
}.each do |matcher, type|
  RSpec::Matchers.define matcher do
    buffer = Buffer.new(VIM, type)

    match do |code|
      actual = cleanup(code)
      buffer.reindent(actual) == actual
    end

    failure_message_for_should do |code|
      actual = cleanup(code)
      "expected:\n\n#{actual}\n     got:\n\n#{ buffer.reindent(actual) }\n  after elixir indentation"
    end
  end
end

{ include_elixir_syntax:  :ex,
  include_eelixir_syntax: :eex
}.each do |matcher, type|
  RSpec::Matchers.define matcher do |syntax, pattern|
    buffer = Buffer.new(VIM, type)

    match do |code|
      cleanup(code)
      buffer.syntax(code, pattern).include? syntax
    end

    failure_message_for_should do |code|
      actual = cleanup(code)
      "expected #{buffer.syntax(code, pattern)} to include syntax #{syntax}\nfor pattern: /#{pattern}/\n         in:\n\n#{actual}"
    end

    failure_message_for_should_not do |code|
      actual = cleanup(code)
      "expected #{buffer.syntax(code, pattern)} not to include syntax #{syntax}\nfor pattern: /#{pattern}/\n         in:\n\n#{actual}"
    end
  end
end

RSpec.configure do |config|
  config.before(:suite) do
    VIM = Vimrunner.start_gvim
    VIM.prepend_runtimepath(File.expand_path('../..', __FILE__))
    VIM.command('runtime ftdetect/elixir.vim')
    VIM.command('runtime ftdetect/eelixir.vim')
  end

  config.after(:suite) do
    VIM.kill
  end

  config.around(:each) do |example|
    # cd into a temporary directory for every example.
    Dir.mktmpdir do |dir|
      Dir.chdir(dir) do
        VIM.command("cd #{dir}")
        example.call
      end
    end
  end
end
