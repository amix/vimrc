require 'spec_helper'

describe "Syntax highlighting" do
  specify "useless line continuations" do
    str = <<~'EOF'
      foo = \
        if true
          42
        end
    EOF
    assert_correct_highlighting str, '\\', 'rubyUselessLineContinuation'
    assert_correct_highlighting str, 'if', 'rubyConditional'
  end

  specify "line continuations" do
    str = <<~'EOF'
      foo = 42 \
        if true
    EOF
    assert_correct_highlighting str, '\\', 'rubyLineContinuation'
    assert_correct_highlighting str, 'if', 'rubyConditionalModifier'
  end
end
