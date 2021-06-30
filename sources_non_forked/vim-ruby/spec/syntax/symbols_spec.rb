require 'spec_helper'

describe "Syntax highlighting" do
  # See issue #356
  specify "hashes with symbol keys and values on different lines" do
    assert_correct_highlighting <<~'EOF', 'x', 'rubySymbol'
      h = {
        x:
          really_long_method_name,
        y: 5,
      }
    EOF
  end

  # See issue #44
  specify "1.9 style hash keys with keyword names" do
    assert_correct_highlighting <<~EOF, %w[class if def include case end], 'rubySymbol'
      { class: "hello", if: "world", def: "i am", include: "foo", case: "bar", end: "baz" }
    EOF

    assert_correct_highlighting <<~'EOF', 'end', 'rubyDefine'
      def hello
        { if: "world" }
      end
    EOF
  end

  # See issue #144
  specify "1.9 style hash keys with keyword names in parameter lists" do
    assert_correct_highlighting <<~'EOF', 'prepend', 'rubySymbol'
      {prepend: true}
    EOF
    assert_correct_highlighting <<~'EOF', 'for', 'rubySymbol'
      Subscription.generate(for: topic,
                            to:  subscriber)
    EOF
  end

  # See issue #12
  specify "1.9 style hash keys with keyword names in argument lists" do
    assert_correct_highlighting <<~EOF, %w[:\zsgender in\ze: if\ze: :\zsgender_required?], 'rubySymbol'
      validates_inclusion_of :gender, in: %w(male female), if: :gender_required?
    EOF
  end
end
