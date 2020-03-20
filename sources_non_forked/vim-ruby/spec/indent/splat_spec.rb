require 'spec_helper'

describe "Indenting" do
  specify "splats with blocks in square brackets" do
    assert_correct_indenting <<~EOF
      x = Foo[*
        y do
          z
        end
      ]
    EOF

    assert_correct_indenting <<~EOF
      x = Foo[* # with a comment
        y do
          z
        end
      ]
    EOF
  end

  specify "splats with blocks in assignment" do
    assert_correct_indenting <<~EOF
      x = *
        array.map do
        3
      end
    EOF
  end

  specify "splats with blocks in round brackets" do
    assert_correct_indenting <<~EOF
      x = Foo(*y do
        z
      end)
    EOF

    assert_correct_indenting <<~EOF
      x = Foo(
        *y do
          z
        end
      )
    EOF
  end
end
