# frozen_string_literal: true

require 'spec_helper'

describe 'Alias syntax' do
  it 'colorize only module alias' do
    str = "Enum.empty?(...)"
    expect(str).to include_elixir_syntax('elixirAlias', 'Enum')
    expect(str).to include_elixir_syntax('elixirOperator', '\.')
    expect(str).to include_elixir_syntax('elixirId', 'empty?')
  end

  it 'colorize the module alias even if it starts with `!`' do
    expect(<<~EOF).to include_elixir_syntax('elixirAlias', 'Enum')
      !Enum.empty?(...)
    EOF
  end

  it 'does not colorize the preceding ! in an alias' do
    expect(<<~EOF).not_to include_elixir_syntax('elixirAlias', '!')
      !Enum.empty?(...)
    EOF
  end

  it 'does not colorize words starting with lowercase letters' do
    expect(<<~EOF).not_to include_elixir_syntax('elixirAlias', 'aEnum')
      aEnum.empty?(...)
    EOF
  end

  it 'colorizes numbers in aliases' do
    str = "S3Manager"
    expect(str).to include_elixir_syntax('elixirAlias', 'S')
    expect(str).to include_elixir_syntax('elixirAlias', '3')
    expect(str).to include_elixir_syntax('elixirAlias', 'Manager')
  end

  it 'colorize dots in module alias' do
    str = "Foo.Bar.Baz.fun(...)"
    expect(str).to include_elixir_syntax('elixirAlias', 'Foo')
    expect(str).to include_elixir_syntax('elixirAlias', '\.\(Bar\)\@=')
    expect(str).to include_elixir_syntax('elixirAlias', 'Bar')
    expect(str).to include_elixir_syntax('elixirAlias', '\.\(Baz\)\@=')
    expect(str).to include_elixir_syntax('elixirAlias', 'Baz')
    expect(str).to include_elixir_syntax('elixirOperator', '\.\(fun\)\@=')
    expect(str).to include_elixir_syntax('elixirId', 'fun')
  end
end
