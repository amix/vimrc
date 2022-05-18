# frozen_string_literal: true

require 'spec_helper'

describe 'Kernel function syntax' do
  it 'kernel function used as an atom key in a keyword list outside of a block' do
    expect(<<~EOF).not_to include_elixir_syntax('elixirKeyword', 'length')
    do
      plug Plug.Parsers, length: 400_000_000
    end
    EOF
  end

  it 'kernel function used as an atom key in a keyword list contained in a block' do
    expect(<<~EOF).not_to include_elixir_syntax('elixirKeyword', 'length')
    plug Plug.Parsers, length: 400_000_000
    EOF
  end
end
