# frozen_string_literal: true

require 'spec_helper'

describe 'Anonymous function syntax' do
  it 'anonymous function' do
    expect(<<~'EOF').to include_elixir_syntax('elixirAnonymousFunction', 'fn')
      fn(_, state) -> state end
    EOF
  end

  it 'as a default argument' do
    expect(<<~'EOF').to include_elixir_syntax('elixirAnonymousFunction', 'fn')
      def exec(func \\ fn(_, state) -> state end) do
      end
    EOF
  end

  it 'as a default argument in a module' do
    str = <<~'EOF'
      defmodule HelloWorld do
        def exec(func \\ fn(_, state) -> state end) do
        end
      end
    EOF

    expect(str).to include_elixir_syntax('elixirAnonymousFunction', 'fn')

    # Test that the syntax properly closed
    expect(str).to include_elixir_syntax('elixirBlockDefinition', '^end')
  end
end
