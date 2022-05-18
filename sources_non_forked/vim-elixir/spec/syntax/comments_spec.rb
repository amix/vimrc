# frozen_string_literal: true

require 'spec_helper'

describe 'Comments syntax' do
  it 'full line comment' do
    expect(<<~EOF).to include_elixir_syntax('elixirComment', '#\ this\ is\ a\ comment')
    # this is a comment
    EOF
  end

  it 'end line comment' do
    expect(<<~EOF).to include_elixir_syntax('elixirComment', '#\ this\ is\ a\ comment')
    IO.puts "some text" # this is a comment
    EOF
  end

  it 'after arguments' do
    t = <<~EOF
    def foo(<<
      0   :: 1,       # Foo
      1   :: size(1), # Bar
      # Blah
      baz :: 8,       # Baz
    >>), do: baz
    EOF
    expect(t).to include_elixir_syntax('elixirComment', '#\ Foo')
    expect(t).to include_elixir_syntax('elixirComment', '#\ Bar')
    expect(t).to include_elixir_syntax('elixirComment', '#\ Blah')
    expect(t).to include_elixir_syntax('elixirComment', '#\ Baz')
  end
end
