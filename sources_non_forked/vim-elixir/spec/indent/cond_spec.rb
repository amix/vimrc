# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting cond statements' do
  i <<~EOF
  cond do
    foo -> 1
    bar -> 2
  end
  EOF
end
