require 'spec_helper'

describe "Sigil syntax" do
  describe "upper case" do
    it "string" do
      '~S(string)'.should include_elixir_syntax('elixirSigilDelimiter', 'S')
      '~S(string)'.should include_elixir_syntax('elixirSigil', 'foo')
    end

    it "character list" do
      '~C(charlist)'.should include_elixir_syntax('elixirSigilDelimiter', 'C')
      '~C(charlist)'.should include_elixir_syntax('elixirSigil', 'charlist')
    end

    it "regular expression" do
      '~R(regex)'.should include_elixir_syntax('elixirSigilDelimiter', 'R')
      '~R(regex)'.should include_elixir_syntax('elixirSigil', 'regex')
    end

    it "list of words" do
      '~W(list of words)'.should include_elixir_syntax('elixirSigilDelimiter', 'W')
      '~W(list of words)'.should include_elixir_syntax('elixirSigil', 'list')
    end

    it "delimited with parans" do
      '~S(foo bar)'.should include_elixir_syntax('elixirSigilDelimiter', '(')
      '~S(foo bar)'.should include_elixir_syntax('elixirSigilDelimiter', ')')
    end

    it "delimited with braces" do
      '~S{foo bar}'.should include_elixir_syntax('elixirSigilDelimiter', '{')
      '~S{foo bar}'.should include_elixir_syntax('elixirSigilDelimiter', '}')
    end

    it "delimited with brackets" do
      '~S[foo bar]'.should include_elixir_syntax('elixirSigilDelimiter', '[')
      '~S[foo bar]'.should include_elixir_syntax('elixirSigilDelimiter', ']')
    end

    it "escapes double quotes unless only preceded by whitespace" do
      <<-EOF
        ~r"""
        foo """
        """
      EOF
      .should include_elixir_syntax('elixirSigilDelimiter', %q(^\s*\zs"""))
    end

    it "escapes single quotes unless only preceded by whitespace" do
      <<-EOF
        ~r'''
        foo '''
        '''
      EOF
      .should include_elixir_syntax('elixirSigilDelimiter', %q(^\s*\zs'''))
    end

    it "without escapes" do
      '~S(foo \n bar)'.should_not include_elixir_syntax('elixirRegexEscape', '\\')
    end

    it "without interpolation" do
      '~S(foo #{bar})'.should_not include_elixir_syntax('elixirInterpolation', 'bar')
    end

    it "without escaped parans" do
      '~S(\( )'.should_not include_elixir_syntax('elixirRegexEscapePunctuation', '( ')
    end
  end

  describe "lower case" do
    it "string" do
      '~s(string)'.should include_elixir_syntax('elixirSigilDelimiter', 's')
      '~s(string)'.should include_elixir_syntax('elixirSigil', 'foo')
    end

    it "character list" do
      '~c(charlist)'.should include_elixir_syntax('elixirSigilDelimiter', 'c')
      '~c(charlist)'.should include_elixir_syntax('elixirSigil', 'charlist')
    end

    it "regular expression" do
      '~r(regex)'.should include_elixir_syntax('elixirSigilDelimiter', 'r')
      '~r(regex)'.should include_elixir_syntax('elixirSigil', 'regex')
    end

    it "list of words" do
      '~w(list of words)'.should include_elixir_syntax('elixirSigilDelimiter', 'w')
      '~w(list of words)'.should include_elixir_syntax('elixirSigil', 'list')
    end

    it "with escapes" do
      '~s(foo \n bar)'.should include_elixir_syntax('elixirRegexEscape', '\\')
    end

    it "with interpolation" do
      '~s(foo #{bar})'.should include_elixir_syntax('elixirInterpolation', 'bar')
    end

    it "with escaped parans" do
      '~s(\( )'.should include_elixir_syntax('elixirRegexEscapePunctuation', '( ')
    end
  end
end
