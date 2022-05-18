# frozen_string_literal: true

require 'spec_helper'

describe 'documentation syntax' do
  describe 'string' do
    it 'doc in double quotes' do
      ex = '@doc "foo"'
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
      expect(ex).to include_elixir_syntax('elixirDocStringDelimiter', '"')
    end

    it 'doc in sigil_S' do
      ex = '@doc ~S(foo)'
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
      expect(ex).to include_elixir_syntax('elixirDocSigilDelimiter', 'S')
    end
  end

  describe 'heredoc' do
    it 'doc with multiline content' do
      ex = <<~'EOF'
        @callbackdoc """
        foo
        """
      EOF
      expect(ex).to include_elixir_syntax('elixirVariable', 'doc')
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
      expect(ex).to include_elixir_syntax('elixirDocStringDelimiter', '"""')
    end

    it 'doc with sigil_S triple double-quoted multiline content' do
      ex = <<~'EOF'
        @doc ~S"""
        foo
        """
      EOF
      expect(ex).to include_elixir_syntax('elixirVariable', 'doc')
      expect(ex).to include_elixir_syntax('elixirDocSigilDelimiter', 'S"""')
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
    end

    it 'doc with sigil_S triple double-quoted multiline content with parentheses' do
      ex = <<~'EOF'
        @doc(~S"""
        foo
        """)
      EOF
      expect(ex).to include_elixir_syntax('elixirVariable', 'doc')
      expect(ex).to include_elixir_syntax('elixirDocSigilDelimiter', 'S"""')
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
    end

    it 'doc with sigil_S triple single-quoted multiline content' do
      ex = <<~'EOF'
        @doc ~S'''
        foo
        '''
      EOF
      expect(ex).to include_elixir_syntax('elixirVariable', 'doc')
      expect(ex).to include_elixir_syntax('elixirDocSigilDelimiter', "S'''")
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
    end

    it 'doc with sigil_S triple single-quoted multiline content with parentheses' do
      ex = <<~'EOF'
        @doc(~S'''
        foo
        ''')
      EOF
      expect(ex).to include_elixir_syntax('elixirVariable', 'doc')
      expect(ex).to include_elixir_syntax('elixirDocSigilDelimiter', "S'''")
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
    end

    it 'doc with triple single-quoted multiline content is not a doc string' do
      ex = <<~'EOF'
        @doc '''
        foo
        '''
      EOF
      expect(ex).not_to include_elixir_syntax('elixirDocString', 'foo')
    end

    it 'doc with multiline escaped' do
      ex = <<~'EOF'
        @doc """
        foo
        ```
        @xxx \"""
        bar
        \"""
        ```
        baz
        """
      EOF
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
      expect(ex).to include_elixir_syntax('elixirDocString', 'bar')
      expect(ex).to include_elixir_syntax('elixirDocString', 'baz')
    end

    it 'doc skip interpolation' do
      ex = <<~'EOF'
        @doc """
        foo #{bar}
        """
      EOF
      expect(ex).to include_elixir_syntax('elixirDocString', 'foo')
      expect(ex).to include_elixir_syntax('elixirDocStringDelimiter', '"""')
      expect(ex).to include_elixir_syntax('elixirInterpolation', 'bar')
    end

    it 'doc with doctest' do
      ex = <<~'EOF'
      @doc """
      doctest

          iex> Enum.map [1, 2, 3], fn(x) ->
          ...>   x * 2
          ...> end
          [2, 4, 6]

      """
      EOF
      expect(ex).to include_elixir_syntax('elixirDocString', 'doctest')
      expect(ex).to include_elixir_syntax('elixirDocTest',   'map')
      expect(ex).to include_elixir_syntax('elixirDocTest',   'x \* 2')
      expect(ex).to include_elixir_syntax('elixirDocTest',   '2, 4, 6')
    end

    describe 'doctest without newline after' do
      it 'with heredoc' do
        ex = <<~'EOF'
        @doc """
        doctest

            iex> 1 + 2
            3
        """
        def some_fun(x), do: x
        EOF
        expect(ex).to include_elixir_syntax('elixirDocString', 'doctest')
        expect(ex).to include_elixir_syntax('elixirDocTest',   '1 + 2')
        expect(ex).to include_elixir_syntax('elixirDefine',    'def')
      end

      it 'with double quote' do
        ex = <<~'EOF'
        @doc "
        doctest

            iex> \"bob\"
            \"bob\"
        "
        def some_fun(x), do: x
        EOF
        expect(ex).to include_elixir_syntax('elixirDocString', 'doctest')
        expect(ex).to include_elixir_syntax('elixirDocTest',   'bob')
        expect(ex).to include_elixir_syntax('elixirDefine',    'def')
      end

      it 'with sigil_S' do
        ex = <<~'EOF'
        @doc ~S(
        doctest

            iex> to_string("bob"\)
            "bob"
        )
        def some_fun(x), do: x
        EOF
        expect(ex).to include_elixir_syntax('elixirDocString', 'doctest')
        expect(ex).to include_elixir_syntax('elixirDocTest',   'bob')
        expect(ex).to include_elixir_syntax('elixirDefine',    'def')
      end

      it 'with sigil_s' do
        ex = <<~'EOF'
        @doc ~s(
        doctest

            iex> to_string("bob"\)
            "bob"
        )
        def some_fun(x), do: x
        EOF
        expect(ex).to include_elixir_syntax('elixirDocString', 'doctest')
        expect(ex).to include_elixir_syntax('elixirDocTest',   'bob')
        expect(ex).to include_elixir_syntax('elixirDefine',    'def')
      end
    end

    it 'doc with inline code' do
      ex = <<~'EOF'
      @doc """
      doctest with inline code `List.wrap([])`
      """
      EOF
      expect(ex).to include_elixir_syntax('elixirDocString', 'doctest')
      expect(ex).to include_elixir_syntax('elixirDocString',   'wrap')
    end

    describe "use markdown for docs" do
      before(:each) { VIM.command("let g:elixir_use_markdown_for_docs = 1") }
      after(:each) { VIM.command("let g:elixir_use_markdown_for_docs = 0") }

      it 'doc with inline code' do
        ex = <<~'EOF'
        @doc """
        doc with inline code `List.wrap([])`
        """
        EOF
        expect(ex).to include_elixir_syntax('elixirDocString', 'inline')
        expect(ex).to include_elixir_syntax('markdownCode',   'wrap')
      end
    end
  end
end
