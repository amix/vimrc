require 'spec_helper'

describe "Indenting" do
  specify "closing html tag after multiline eruby tag" do
    assert_correct_indenting 'erb', <<~EOF
      <form>
        <div>
          <%= text_field_tag :email, nil,
            placeholder: "email" %>
          text
          <%= text_field_tag :password, nil,
            placeholder: "password" %>
        </div>
      </form>
    EOF
  end
end

