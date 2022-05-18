require 'rspec/expectations'
require 'tmpdir'
require 'vimrunner'
require 'vimrunner/rspec'

GVIM_PATH_FILE = File.expand_path('../../.gvim_path', __FILE__)

class Buffer
  FOLD_PLACEHOLDER = '<!-- FOLD -->'.freeze

  def initialize(vim, type)
    @file = ".fixture.#{type}"
    @vim = vim
  end

  def reindent(content)
    with_file content do
      min_indent = content.each_line.map { |line| line[/\s*/].size }.min
      cmd = "ggVG:s/\\s\\{0,#{min_indent}}//" # remove all indentation
      cmd += 'gg=G' # force vim to indent the file
      @vim.normal cmd
    end
  end

  def type(content)
    with_file do
      @vim.normal 'gg'

      lines = content.each_line
      count = lines.count
      @vim.type("i")
      lines.each_with_index do |line, index|
        @vim.type("#{line.strip}")
        @vim.type("<CR>") if index < count - 1
      end
    end
  end

  def syntax(content, pattern)
    with_file content

    # Using this function with a `pattern` that is not in `content` is pointless.
    #
    # @vim.search() silently fails if a pattern is not found and the cursor
    # won't move. So, if the current cursor position happens to sport the
    # expected syntax group already, this can lead to false positive tests.
    #
    # We work around this by using Vim's search() function, which returns 0 if
    # there is no match.
    if @vim.echo("search(#{pattern.inspect})") == '0'
      return []
    end

    syngroups = @vim.echo <<~EOF
    map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
    EOF

    # From: "['elixirRecordDeclaration', 'elixirAtom']"
    # To:   ["elixirRecordDeclaration", "elixirAtom"]
    syngroups.gsub!(/["'\[\]]/, '').split(', ')
  end

  def fold_and_replace(content, fold_on_line)
    with_file content do
      cmd = ":set foldmethod=syntax<CR>"
      cmd += "zO"
      cmd += "#{fold_on_line}G"
      cmd += "zc"
      cmd += "cc#{FOLD_PLACEHOLDER}<Esc>"
      cmd += ":.s/\s*//<CR>"
      @vim.normal(cmd)
    end
  end

  private

  def with_file(content = nil)
    edit_file(content)

    yield if block_given?

    @vim.normal ":w<CR>"
    @vim.normal ":redraw<CR>"
    IO.read(@file)
  end

  def edit_file(content)
    File.write(@file, content) if content
    @vim.edit @file
  end
end

class Differ
  def self.diff(result, expected)
    instance.diff(result, expected)
  end

  def self.instance
    @instance ||= new
  end

  def initialize
    @differ = RSpec::Support::Differ.new(
      object_preparer: -> (object) do
        RSpec::Matchers::Composable.surface_descriptions_in(object)
      end,
      color: RSpec::Matchers.configuration.color?
    )
  end

  def diff(result, expected)
    @differ.diff_as_string(result, expected)
  end
end

module ExBuffer
  def self.new
    Buffer.new(VIM, :ex)
  end
end

module EexBuffer
  def self.new
    Buffer.new(VIM, :eex)
  end
end

module HeexBuffer
  def self.new
    Buffer.new(VIM, :heex)
  end
end

module LeexBuffer
  def self.new
    Buffer.new(VIM, :leex)
  end
end

module SurfaceBuffer
  def self.new
    Buffer.new(VIM, :sface)
  end
end

RSpec::Matchers.define :be_typed_with_right_indent do |syntax|
  buffer = Buffer.new(VIM, syntax || :ex)

  match do |code|
    @typed = buffer.type(code)
    @typed == code
  end

  failure_message do |code|
    <<~EOM
    Expected

    #{@typed}
    to be indented as

    #{code}

    when typed
    EOM
  end
end

{
  be_elixir_indentation:  :ex,
  be_eelixir_indentation: :eex,
  be_heelixir_indentation: :heex,
  be_leelixir_indentation: :leex,
  be_surface_indentation: :sface
}.each do |matcher, type|
  RSpec::Matchers.define matcher do
    buffer = Buffer.new(VIM, type)

    match do |code|
      reindented = buffer.reindent(code)
      reindented == code
    end

    failure_message do |code|
      <<~EOM
      Expected

      #{buffer.reindent(code)}
      to be indented as

      #{code}

      when bulk indented
      EOM
    end
  end
end

{
  include_elixir_syntax:  :ex,
  include_eelixir_syntax: :eex,
  include_heelixir_syntax: :heex,
  include_leelixir_syntax: :leex,
  include_surface_syntax: :sface
}.each do |matcher, type|
  RSpec::Matchers.define matcher do |syntax, pattern|
    buffer = Buffer.new(VIM, type)

    match do |code|
      buffer.syntax(code, pattern).include? syntax.to_s
    end

    failure_message do |code|
      <<~EOF
      expected #{buffer.syntax(code, pattern)}
      to include syntax '#{syntax}'
      for pattern: /#{pattern}/
      in:
        #{code}
      EOF
    end

    failure_message_when_negated do |code|
      <<~EOF
      expected #{buffer.syntax(code, pattern)}
      *NOT* to include syntax '#{syntax}'
      for pattern: /#{pattern}/
      in:
        #{code}
      EOF
    end
  end
end

RSpec::Matchers.define :fold_lines do
  buffer = Buffer.new(VIM, :ex)

  match do |code|
    @code = code

    pattern = /# fold\s*$/

    placeholder_set = false
    @expected = code.each_line.reduce([]) do |acc, line|
      if line =~ pattern
        if !placeholder_set
          placeholder_set = true
          acc << (Buffer::FOLD_PLACEHOLDER + "\n")
        end
      else
        acc << line
      end

      acc
    end.join

    fold_on_line = code.each_line.find_index { |l| l =~ pattern } + 1
    @actual = buffer.fold_and_replace(code, fold_on_line)

    @expected == @actual
  end

  failure_message do |code|
    <<~EOF
    Folded

    #{@code}
    and unexpectedly got

    #{@actual}
    EOF
  end
end

Vimrunner::RSpec.configure do |config|
  config.reuse_server = true

  config.start_vim do
    VIM =
      if File.exists?(GVIM_PATH_FILE)
        Vimrunner::Server.new(executable: File.read(GVIM_PATH_FILE).rstrip).start
      else
        Vimrunner.start_gvim
      end
    VIM.add_plugin(File.expand_path('..', __dir__))
    cmd = ':filetype off<CR>'
    cmd += ':filetype plugin indent on<CR>'
    cmd += ':autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o<CR>' # disable automatic comment continuation
    cmd += ":set ignorecase<CR>" # make sure we test ignorecase
    VIM.normal(cmd)
    VIM
  end
end

RSpec.configure do |config|
  config.order = :random

  # Run a single spec by adding the `focus: true` option
  config.filter_run_including focus: true
  config.run_all_when_everything_filtered = true
end

RSpec::Core::ExampleGroup.instance_eval do
  def i(str)
    gen_tests(:it, str)
  end

  def ip(str)
    gen_tests(:pending, str)
  end

  private

  def gen_tests(method, str)
    send method, "\n#{str}" do
      expect(str).to be_elixir_indentation
    end

    send method, "typed: \n#{str}" do
      expect(str).to be_typed_with_right_indent
    end
  end
end
