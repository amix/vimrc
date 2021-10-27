require 'spec_helper'

describe "Indenting" do
  specify "if-clauses" do
    assert_correct_indenting <<~EOF
      if foo
        bar
      end
    EOF

    assert_correct_indenting <<~EOF
      if foo
        bar
      else
        baz
      end
    EOF

    assert_correct_indenting <<~EOF
      bar if foo
      something_else
    EOF
  end

  specify "heredocs" do
    assert_correct_indenting <<~EOF
      def one
        two = <<-THREE
        four
        THREE
      end
    EOF

    assert_correct_indenting <<~EOF
      def one
        two = <<THREE
      four
      THREE
      end
    EOF

    assert_correct_indenting <<~EOF
      def one
        two = <<~THREE
        four
        THREE
      end
    EOF

    # See https://github.com/vim-ruby/vim-ruby/issues/318 for details
    assert_correct_indenting <<~EOF
      def foo
        <<-EOS
          one
            \#{two} three
              four
        EOS
      end
    EOF
  end

  specify "comments" do
    assert_correct_indenting <<~EOF
      def one
        example do |something|
      =begin
           something that is ignored
      =end
        end
      end
    EOF
  end
end
