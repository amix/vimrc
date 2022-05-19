require 'spec_helper'

describe "Indenting" do
  # Reference: https://docs.ruby-lang.org/en/master/doc/syntax/pattern_matching_rdoc.html
  specify "pattern-matching with case-in" do
    assert_correct_indenting 'rb', <<~EOF
      case {a: a}
      in {a:}
        p a
      end
    EOF

    assert_correct_indenting 'rb', <<~EOF
      users = [{name: "Alice", age: 12}, {name: "Bob", age: 23}]
      users.any? do |user|
        user in {name: /B/, in: 20..}
      end #=> true
    EOF
  end

  specify "does not deindent while typing" do
    assert_correct_indent_in_insert 'rb', <<~EOF, "index = 0", <<~RESULT
      def foo
    EOF
      def foo
        index = 0
    RESULT
  end
end
