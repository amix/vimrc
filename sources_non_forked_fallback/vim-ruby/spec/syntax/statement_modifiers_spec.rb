require 'spec_helper'

describe "Syntax highlighting" do
  specify "only modifiers can appear after regexp literals" do
    # See issue #254
    assert_correct_highlighting <<~'EOF', 'if', 'rubyConditionalModifier'
      def get_regex
        /some regex/ if false
      end
    EOF
  end

  specify "only modifiers can appear after unparenthesised no-arg method calls" do
    [
      "foo    if true",
      "foo?   if true",
      "foo!   if true",
      "foo_   if true",
      "foo_?  if true",
      "foo_!  if true",
      "foo42  if true",
      "foo42? if true",
      "foo42! if true",
      "Foo    if true",
      "Foo?   if true",
      "Foo!   if true"
    ].each do |str|
      assert_correct_highlighting str, 'if', 'rubyConditionalModifier'
    end
  end
end
