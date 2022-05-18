# frozen_string_literal: true

require 'spec_helper'

describe 'Tuple syntax' do
  it 'tuples' do
    str = %q({:name, "one"})

    expect(str).to include_elixir_syntax('elixirTupleDelimiter', '{')
    expect(str).to include_elixir_syntax('elixirTuple', '{')

    expect(str).to include_elixir_syntax('elixirAtom', ':name')
    expect(str).to include_elixir_syntax('elixirTuple', ':name')

    expect(str).to include_elixir_syntax('elixirTupleDelimiter', '}')
  end
end
