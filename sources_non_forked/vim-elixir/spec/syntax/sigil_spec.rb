# frozen_string_literal: true

require 'spec_helper'

describe 'Sigil syntax' do
  it 'as function argument' do
    expect('def f(~s(")), do: true').to include_elixir_syntax('elixirSigilDelimiter', '\~s(')
    expect('def f(~s(")), do: true').to include_elixir_syntax('elixirSigil', '"')
    expect("def f(~s(')), do: true").to include_elixir_syntax('elixirSigil', "'")
    expect('def f(~s(")), do: true').not_to include_elixir_syntax('elixirSigilDelimiter', '"')
  end

  it 'as function argument multiline content' do
    ex = <<~'EOF'
      f(
        ~S"""
        foo
        """,
        bar
      )
    EOF

    expect(ex).to include_elixir_syntax('elixirSigilDelimiter', 'S"""')
    expect(ex).to include_elixir_syntax('elixirSigil', 'foo')
  end

  describe 'upper case' do
    it 'string' do
      expect('~S(string)').to include_elixir_syntax('elixirSigilDelimiter', 'S')
      expect('~S(string)').to include_elixir_syntax('elixirSigil', 'string')
    end

    it 'character list' do
      expect('~C(charlist)').to include_elixir_syntax('elixirSigilDelimiter', 'C')
      expect('~C(charlist)').to include_elixir_syntax('elixirSigil', 'charlist')
    end

    it 'regular expression' do
      expect('~R(regex)').to include_elixir_syntax('elixirSigilDelimiter', 'R')
      expect('~R(regex)').to include_elixir_syntax('elixirSigil', 'regex')
    end

    it 'list of words' do
      expect('~W(list of words)').to include_elixir_syntax('elixirSigilDelimiter', 'W')
      expect('~W(list of words)').to include_elixir_syntax('elixirSigil', 'list')
    end

    it 'delimited with parenthesis' do
      expect('~S(foo bar)').to include_elixir_syntax('elixirSigilDelimiter', '(')
      expect('~S(foo bar)').to include_elixir_syntax('elixirSigilDelimiter', ')')
    end

    it 'delimited with braces' do
      expect('~S{foo bar}').to include_elixir_syntax('elixirSigilDelimiter', '{')
      expect('~S{foo bar}').to include_elixir_syntax('elixirSigilDelimiter', '}')
    end

    it 'delimited with brackets' do
      expect('~S[foo bar]').to include_elixir_syntax('elixirSigilDelimiter', '[')
      expect('~S[foo bar]').to include_elixir_syntax('elixirSigilDelimiter', ']')
    end

    it 'escapes double quotes unless only preceded by whitespace' do
      expect(<<~EOF).to include_elixir_syntax('elixirSigilDelimiter', %q(^\s*\zs"""))
        ~r"""
        foo """
        """
      EOF
    end

    it 'escapes single quotes unless only preceded by whitespace' do
      expect(<<~EOF).to include_elixir_syntax('elixirSigilDelimiter', %q(^\s*\zs'''))
        ~r'''
        foo '''
        '''
      EOF
    end

    it 'without escapes' do
      expect('~S(foo \n bar)').not_to include_elixir_syntax('elixirRegexEscape', '\\')
    end

    it 'without interpolation' do
      expect('~S(foo #{bar})').not_to include_elixir_syntax('elixirInterpolation', 'bar')
    end

    it 'without escaped parenthesis' do
      expect('~S(\( )').not_to include_elixir_syntax('elixirRegexEscapePunctuation', '( ')
    end

    it 'Live EEx' do
      expect('~L"""liveview template"""').to include_elixir_syntax('elixirSigilDelimiter', '"""')
    end

    it 'Surface EEx' do
      expect('~H"""surface template"""').to include_elixir_syntax('elixirSigilDelimiter', '"""')
    end

    it 'EEx' do
      expect('~E"""Phoenix.HTML template"""').to include_elixir_syntax('elixirSigilDelimiter', '"""')
      expect('~e"""Phoenix.HTML template"""').to include_elixir_syntax('elixirSigilDelimiter', '"""')
    end
  end

  describe 'lower case' do
    it 'string' do
      expect('~s(string)').to include_elixir_syntax('elixirSigilDelimiter', 's')
      expect('~s(string)').to include_elixir_syntax('elixirSigil', 'string')
    end

    it 'character list' do
      expect('~c(charlist)').to include_elixir_syntax('elixirSigilDelimiter', 'c')
      expect('~c(charlist)').to include_elixir_syntax('elixirSigil', 'charlist')
    end

    it 'regular expression' do
      expect('~r(regex)').to include_elixir_syntax('elixirSigilDelimiter', 'r')
      expect('~r(regex)').to include_elixir_syntax('elixirSigil', 'regex')
    end

    it 'list of words' do
      expect('~w(list of words)').to include_elixir_syntax('elixirSigilDelimiter', 'w')
      expect('~w(list of words)').to include_elixir_syntax('elixirSigil', 'list')
    end

    it 'with escapes' do
      expect('~s(foo \n bar)').to include_elixir_syntax('elixirRegexEscapePunctuation', '\\')
    end

    it 'with interpolation' do
      expect('~s(foo #{bar})').to include_elixir_syntax('elixirInterpolation', 'bar')
    end

    it 'with escaped parenthesis' do
      expect('~s(\( )').to include_elixir_syntax('elixirRegexEscapePunctuation', '( ')
    end

    it 'interpolation with slashes' do
      expect('~s/foo #{bar}/').to include_elixir_syntax('elixirInterpolation', 'bar')
    end

    it 'escapes with slashes' do
      expect('~s/foo \n bar/').to include_elixir_syntax('elixirRegexEscapePunctuation', '\\')
    end
  end
end
