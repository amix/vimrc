# frozen_string_literal: true

require 'spec_helper'

describe 'Operators' do
  it 'default argument' do
    expect(<<~'EOF').to include_elixir_syntax('elixirOperator', '\\')
      def foo(bar \\ :baz)
    EOF

    expect(<<~EOF).to include_elixir_syntax('elixirOperator', '\/')
      def foo(bar // :baz)
    EOF
  end

  it 'in' do
    expect(<<~EOF).to include_elixir_syntax('elixirOperator', 'in')
      'x' in ['x']
    EOF

    expect(<<~EOF).not_to include_elixir_syntax('elixirOperator', 'in')
      :queue.in x, 5
    EOF
  end

  it 'does not highlight operators inside of elixirIds' do
    expect(<<~EOF).not_to include_elixir_syntax('elixirOperator', 'in')
      incoming
    EOF
  end
end
