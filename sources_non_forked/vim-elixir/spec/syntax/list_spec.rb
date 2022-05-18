# frozen_string_literal: true

require 'spec_helper'

describe 'List syntax' do
  it 'should properly handle "\\\\" inside' do
    syntax = <<~EOF
      '"\\\\'
      var = 1
    EOF
    expect(syntax).to include_elixir_syntax('elixirId', 'var')
    expect(syntax).not_to include_elixir_syntax('elixirString', 'var')
  end

  it 'recognizes lists' do
    syntax = <<~EOF
    [
      :hello,
      :world
    ]
    EOF
    expect(syntax).to include_elixir_syntax('elixirListDelimiter', '[')
    expect(syntax).to include_elixir_syntax('elixirList', ':hello')
    expect(syntax).to include_elixir_syntax('elixirListDelimiter', ']')
  end

  it 'recognizes lists inside functions' do
    syntax = <<~EOF
    def hello_world do
      [
        :hello,
        :world
      ]
    end
    EOF
    expect(syntax).to include_elixir_syntax('elixirListDelimiter', '[')
    expect(syntax).to include_elixir_syntax('elixirList', ':hello')
    expect(syntax).to include_elixir_syntax('elixirListDelimiter', ']')
  end
end
