# frozen_string_literal: true

# encoding: utf-8
require 'spec_helper'

describe 'Record syntax' do
  it 'private record symbol' do
    expect(<<~EOF).to include_elixir_syntax('elixirAtom', ':user')
      defrecordp :user, name: "José", age: 25
    EOF
  end
end
