require "spec_helper"

describe ":SortScalaImports" do

  describe "Sorting across groups" do
    ["vanilla", "no_newline", "no_newline_after", "no_package",
     "multiple_newlines"].each do |name|
      it "should sort vanilla file" do
        actual = sort_fixture_across_groups name
        expected = expected(name)
        actual.should eq(expected)
      end
    end
  end

end
