# frozen_string_literal: true

require 'spec_helper'

describe 'Atom syntax' do
  KEYWORDS = %w(
    def
    defp
    defmodule
    defprotocol
    defimpl
    defrecord
    defrecordp
    defmacro
    defmacrop
    defdelegate
    defoverridable
    defexception
    defcallback
    defstruct
  )

  it '`atom:` style keyword used as an atom' do
    KEYWORDS.each do |kw|
      expect(<<~EOF).to include_elixir_syntax('elixirAtom', kw), "expected #{kw} to be an elixirAtom"
      defmodule XmlElement do
        require Record
        import Record, only: [#{kw}: 2, extract: 2]
      end
      EOF
    end
  end

  it '`:atom =>` style keyword used as an atom' do
    KEYWORDS.each do |kw|
      expect(<<~EOF).to include_elixir_syntax('elixirAtom', kw), "expected #{kw} to be an elixirAtom"
      defmodule XmlElement do
        require Record
        import Record, only: [:#{kw} => 2, :extract => 2]
      end
      EOF
    end
  end

  it 'atoms as part of a comprehension' do
    s = 'for kvp <- map, do: &atomize_key/1, into: %{}'
    expect(s).to include_elixir_syntax('elixirAtom', 'do')
    expect(s).to include_elixir_syntax('elixirAtom', 'into')
  end

  it 'defoverridable' do
    expect(<<~EOF).to include_elixir_syntax('elixirAtom', 'init:')
    defmodule Test do
      defmacro __using__(_options) do
        quote do
          def init(args) do
            {:ok, args}
          end
          defoverridable init: 1
        end
      end
    end
    EOF
    expect(<<~EOF).to include_elixir_syntax('elixirAtom', 'init:')
    defmodule Test do
      defmacro __using__(_options) do
        quote do
          def init(args) do
            {:ok, args}
          end
          defoverridable [init: 1]
        end
      end
    end
    EOF
  end

  it '`Atom:` style atoms used in keyword list' do
    expect(<<~EOF).to include_elixir_syntax('elixirAtom', 'Protocols:')
    def project do
      [
        docs: [
          groups_for_modules: [
            Protocols: [Enumerable],
          ]
        ]
      ]
    end
    EOF
  end
end
