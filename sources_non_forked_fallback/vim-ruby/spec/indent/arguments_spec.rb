require 'spec_helper'

describe "Indenting" do
  specify "multi-line arguments" do
    assert_correct_indenting <<~EOF
      User.new(
        :first_name => 'Some',
        :second_name => 'Guy'
      )
    EOF

    assert_correct_indenting <<~EOF
      User.new(:first_name => 'Some',
               :second_name => 'Guy')
    EOF

    assert_correct_indenting <<~EOF
      User.new(
        :first_name => 'Some',
        :second_name => 'Guy'
      )
    EOF
  end
end
