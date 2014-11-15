require 'spec_helper'

describe "Indenting" do
  specify "end constructs" do
    assert_correct_indenting <<-EOF
      f do
        g { def h; end }
      end
    EOF

    assert_correct_indenting <<-EOF
      if foo
        bar ; end
      something_else
    EOF

    assert_correct_indenting <<-EOF
      if bar ; end
      something_else
    EOF

    assert_correct_indenting <<-EOF
      foo do
        foo = 3 . class
        foo = lambda { class One; end }
        foo = lambda { |args| class One; end }
        foo = bar; class One; end
      end
    EOF
  end
end
