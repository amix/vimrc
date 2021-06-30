require 'spec_helper'

describe "Syntax highlighting" do
  specify "percent strings with a modulo-assignment operator look-alike delimiter" do
    assert_correct_highlighting <<~'EOF', '%=', 'rubyPercentStringDelimiter'
      foo = %= bar =
    EOF
  end
end
