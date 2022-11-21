require 'spec_helper'

describe "Indenting" do
  specify "identifiers containing keyword substrings" do
    assert_correct_indenting <<~EOF
      foo_def
      42
    EOF
  end
end
