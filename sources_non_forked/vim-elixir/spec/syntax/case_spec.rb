# frozen_string_literal: true

require 'spec_helper'

describe 'Syntax case statements' do
  it ':* is recognized as an atom' do
    expect(<<~EOF).to include_elixir_syntax('elixirAtom', '\*')
    case pattern do
      :* -> :ok
      _ -> :error
    end
    EOF
  end
end
