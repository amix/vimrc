# frozen_string_literal: true

require 'spec_helper'

describe 'Defmodule syntax' do
  it 'defines `defmodule` keyword as elixirModuleDefine' do
    expect(<<~EOF).to include_elixir_syntax('elixirModuleDefine', 'defmodule')
      defmodule HelloPhoenix.HelloController do
    EOF
  end

  it 'defines module name as elixirModuleDeclaration' do
    str = "defmodule HelloPhoenix.HelloController do"
    expect(str).to include_elixir_syntax('elixirModuleDeclaration', 'HelloPhoenix')
    expect(str).to include_elixir_syntax('elixirModuleDeclaration', '\.')
    expect(str).to include_elixir_syntax('elixirModuleDeclaration', 'HelloController')
  end

  it 'does not define module name as elixirAlias' do
    expect(<<~EOF).not_to include_elixir_syntax('elixirAlias', 'HelloPhoenix.HelloController')
      defmodule HelloPhoenix.HelloController do
    EOF
  end

  it 'defines `do` keyword as elixirBlock' do
    expect(<<~EOF).to include_elixir_syntax('elixirBlock', 'do')
      defmodule HelloPhoenix.HelloController do
    EOF
  end
end
