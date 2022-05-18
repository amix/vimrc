# frozen_string_literal: true

require 'spec_helper'

describe 'String syntax' do
  describe 'binary' do
    it 'double quoted string' do
      expect('foo "test"').to include_elixir_syntax('elixirString', 'test')
    end

    it 'double quoted string with escaped quote' do
      expect('"this \"test is all one string"').to include_elixir_syntax('elixirString', 'test')
    end

    it 'charlist with escaped quote' do
      expect(<<-'EOF').to include_elixir_syntax('elixirCharList', 'test')
        'this \'test is all one charlist'
      EOF
    end

    it 'interpolation in string' do
      expect('do_something "foo #{test}"').to include_elixir_syntax('elixirInterpolation', 'test')
    end
  end

  describe 'heredoc' do
    it 'heredoc must be string' do
      ex = <<~EOF
      def function do
        """
        foo "test"
        """
      end
      EOF
      expect(ex).to include_elixir_syntax('elixirString', 'foo')
      expect(ex).to include_elixir_syntax('elixirString', 'test')
    end

    it 'interpolation in string in heredoc' do
      expect(<<~'EOF').to include_elixir_syntax('elixirInterpolation', '#{')
      def function do
        """
        foo "#{test}"
        """
      end
      EOF
    end

    it 'interpolation in heredoc' do
      expect(<<~'EOF').to include_elixir_syntax('elixirInterpolation', '#{')
      def function do
        """
        foo #{test}
        """
      end
      EOF
    end

    it 'correctly terminates heredocs with no spaces at the start of the line' do
      expect(<<~'EOF'.gsub(/^\s+/, '')).to include_elixir_syntax('elixirAtom', ':bar')
      """
      foo
      """
      :bar
      EOF

      expect(<<~'EOF'.gsub(/^\s+/, '')).to include_elixir_syntax('elixirAtom', ':bar')
      '''
      foo
      '''
      :bar
      EOF
    end

    it 'interpolation with a tuple' do
      str = <<~'EOF'
      "Failed sending tasks #{inspect {:unexpected_status_code, s}}"
      EOF
      expect(str).not_to include_elixir_syntax('elixirInterpolationDelimiter', '}}"$')
      expect(str).to include_elixir_syntax('elixirInterpolationDelimiter', '}"$')
    end

    it 'interpolation with a tuple' do
      str = <<~'EOF'
      "Failed sending tasks #{inspect %{unexpected_status_code: s}}"
      EOF
      expect(str).not_to include_elixir_syntax('elixirInterpolationDelimiter', '}}"$')
      expect(str).to include_elixir_syntax('elixirInterpolationDelimiter', '}"$')
    end

    it 'interpolation with a struct' do
      str = <<~'EOF'
      "Failed sending tasks #{inspect %ResponseStruct{unexpected_status_code: s}}"
      EOF
      expect(str).not_to include_elixir_syntax('elixirInterpolationDelimiter', '}}"$')
      expect(str).to include_elixir_syntax('elixirInterpolationDelimiter', '}"$')
    end

    it 'strings with embedded braces' do
      str = <<~EOF
      x = [
        {:text, "asd {"},
        {:text, "qwe"},
      ]
      EOF
      expect(str).to include_elixir_syntax('elixirString', '{"}')
    end
  end
end
