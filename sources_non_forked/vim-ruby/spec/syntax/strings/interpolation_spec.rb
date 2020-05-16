require 'spec_helper'

describe "Syntax highlighting" do
  specify "invalid interpolated predefined global variables are literal text" do
    assert_correct_highlighting <<~'EOF', '#\$', 'rubyString'
      "abc(#$)def"
    EOF
  end
end
