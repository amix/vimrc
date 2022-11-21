require 'spec_helper'

describe "Syntax highlighting" do
  specify "heredocs starting after parenthesised method definitions" do
    # See issue #356
    assert_correct_highlighting <<~'EOF', 'HTML', 'rubyHeredocDelimiter'
        def youtube_video(token, width = 360, height = 215)
            <<-HTML if token
      <iframe width="#{width}" height="#{height}" src="http://www.youtube.com/embed/#{token}" frameborder="0" allowfullscreen></iframe>
            HTML
        end
    EOF
  end

  specify "heredocs do not start after string literals" do
    assert_correct_highlighting <<~'EOF', 'FOO', 'rubyConstant'
      "abc" <<FOO
    EOF
    assert_correct_highlighting <<~'EOF', 'FOO', 'rubyConstant'
      'abc' <<FOO
    EOF
    assert_correct_highlighting <<~'EOF', 'FOO', 'rubyConstant'
      `abc` <<FOO
    EOF
  end
end
