require 'spec_helper'

describe "Syntax highlighting" do
  specify "block parameters" do
    assert_correct_highlighting <<~'EOF', 'bar', 'rubySymbol'
      foo { |bar:| 42 }
    EOF
    assert_correct_highlighting <<~'EOF', %w[bar\ze: baz\ze:], 'rubySymbol'
      foo { |bar: 'bar', baz: 'baz'| 42 }
    EOF
  end
  specify "block parameters with default values including '|'" do
    assert_correct_highlighting <<~'EOF', %w[|\zebar qux)\zs|], 'rubyBlockParameterList'
      foo { |bar=(baz|qux)| 42 }
    EOF
  end
end
