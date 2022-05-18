# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting strings' do
  it "bulk indenting strings" do
    expect(<<~EOF).to be_elixir_indentation
    defp sql do
      """
      SELECT *
        FROM table
       WHERE column = 123
         AND another_column = 456
      """
    end
    EOF
  end
end
