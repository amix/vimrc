# frozen_string_literal: true

require 'spec_helper'

describe 'Map syntax' do
  it 'maps' do
    str = %q(%{name: "one"})
    expect(str).to include_elixir_syntax('elixirMapDelimiter', '%')
    expect(str).to include_elixir_syntax('elixirMapDelimiter', '{')
    expect(str).to include_elixir_syntax('elixirAtom', 'name:')
    expect(str).to include_elixir_syntax('elixirMap', 'name:')
    expect(str).to include_elixir_syntax('elixirMapDelimiter', '}')
  end
end
