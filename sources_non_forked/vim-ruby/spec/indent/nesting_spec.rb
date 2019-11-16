require 'spec_helper'

describe "Indenting" do
  specify "nested blocks" do
    assert_correct_indenting <<~EOF
      var.func1(:param => 'value') do
        var.func2(:param => 'value') do
          puts "test"
        end
      end
    EOF

    assert_correct_indenting <<~EOF
      var.func1(:param => 'value') {
        var.func2(:param => 'value') {
          foo({ bar => baz })
          puts "test one"
          puts "test two"
        }
      }
    EOF

    assert_correct_indenting <<~EOF
      var.
        func1(:param => 'value') {
        var.func2(:param => 'value') {
          puts "test"
        }
      }
    EOF
  end

  specify "nested hashes" do
    assert_correct_indenting <<~EOF
      foo, bar = {
        :bar => {
          :one => 'two',
          :five => 'six'
        }
      }
    EOF

    assert_correct_indenting <<~EOF
      foo,
        bar = {
        :bar => {
          :foo => { 'bar' => 'baz' },
          :one => 'two',
          :three => 'four'
        }
      }
    EOF
  end

  specify "nested blocks with a continuation and function call inbetween" do
    assert_correct_indenting <<~EOF
      var.
        func1(:param => 'value') {
        func1_5(:param => 'value')
        var.func2(:param => 'value') {
          puts "test"
        }
      }
    EOF
  end
end
