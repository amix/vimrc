require 'spec_helper'

describe "Syntax highlighting" do
  specify "method definitions" do
    str = <<~'EOF'
      def foo bar
      end
    EOF
    assert_correct_highlighting str, %w[def end], 'rubyDefine'
    assert_correct_highlighting str, 'foo',       'rubyMethodName'
  end

  specify "method definitions named 'end'" do
    assert_correct_highlighting <<~'EOF', 'end', 'rubyMethodName'
      def end end
    EOF
    assert_correct_highlighting <<~'EOF', 'end', 'rubyMethodName'
      def
      end
      end
    EOF
  end

  specify "method parameters with symbol default values" do
    assert_correct_highlighting <<~'EOF', ':baz', 'rubySymbol'
      def foo bar=:baz
      end
    EOF
  end

  specify "unparenthesised method parameters with a required trailing keyword then semicolon" do
    assert_correct_highlighting <<~'EOF', 'bar', 'rubySymbol'
      def foo bar:; end
    EOF
  end
end
