require 'spec_helper'

describe "Heredoc syntax" do
  describe "binary" do
    it "with multiline content" do
      <<-EOF
        @doc """
        foo
        """
      EOF
      .should include_elixir_syntax('elixirDocString', 'foo')
    end

    it "escapes quotes unless only preceded by whitespace" do
      <<-EOF
        @doc """
        foo """
        """
      EOF
      .should include_elixir_syntax('elixirDocString', %q(^\s*\zs"""))
    end

    it "does not include content on initial line", focus: true do
      <<-EOF
        String.at """, 0
        foo
        end
      EOF
      .should include_elixir_syntax('elixirNumber', '0')
    end

    it "with interpolation" do
      <<-EOF
        @doc """
        foo \#{bar}
        """
      EOF
      .should include_elixir_syntax('elixirInterpolation', 'bar')
    end
  end

  describe "character list" do
    it "with multiline content" do
      <<-EOF
        @doc """
        foo
        """
      EOF
      .should include_elixir_syntax('elixirDocString', 'foo')
    end

    it "escapes quotes unless only preceded by whitespace" do
      <<-EOF
        @doc '''
        foo '''
        '''
      EOF
      .should include_elixir_syntax('elixirDocString', %q(^\s*\zs'''))
    end

    it "with interpolation" do
      <<-EOF
        @doc '''
        foo \#{bar}
        '''
      EOF
      .should include_elixir_syntax('elixirInterpolation', 'bar')
    end
  end
end
