# frozen_string_literal: true

require 'spec_helper'

describe 'defguard syntax' do
  it 'defines `defguard` keyword as elixirGuard' do
    expect(<<~EOF).to include_elixir_syntax('elixirGuard', 'defguard')
      defguard some_guard(x) when is_integer(x)
    EOF
  end

  it 'defines `defguardp` keyword as elixirPrivateGuard' do
    expect(<<~EOF).to include_elixir_syntax('elixirPrivateGuard', 'defguardp')
      defguardp some_private_guard(x) when is_integer(x)
    EOF
  end
end
