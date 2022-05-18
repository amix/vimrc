# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting tuples' do
  i <<~EOF
  def xpto do
    { :a,
      :b,
      :c }
  end
  EOF

  i <<~EOF
  def method do
    {
      :bar,
      path: "deps/umbrella/apps/bar"
    }
  end
  EOF

  i <<~EOF
  x = [
    {:text, "asd {"},
    {:text, "qwe"},
  ]
  EOF
end
