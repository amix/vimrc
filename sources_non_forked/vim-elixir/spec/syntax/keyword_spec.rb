# frozen_string_literal: true

require 'spec_helper'

describe 'Keyword syntax' do
  it 'for used as keyword' do
    expect(<<~EOF).to include_elixir_syntax('elixirKeyword', 'for')
    for v <- [1, 3, 3]
    EOF
  end

  it 'case used as keyword' do
    expect(<<~EOF).to include_elixir_syntax('elixirKeyword', 'case')
    case true do
    EOF
  end

  it 'raise used as keyword' do
    expect(<<~EOF).to include_elixir_syntax('elixirKeyword', 'raise')
    raise "oops"
    EOF

    expect(<<~EOF).to include_elixir_syntax('elixirKeyword', 'raise')
    raise ArgumentError, message: "invalid argument foo"
    EOF
  end
end
