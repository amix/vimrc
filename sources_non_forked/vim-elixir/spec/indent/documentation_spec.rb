# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting documentation' do
  i <<~EOF
  defmodule Test do
    @doc """
    end
    """
  end
  EOF

  it "bulk indenting doc blocks" do
    expect(<<~EOF).to be_elixir_indentation
    defmodule Test do
      @doc """
        do not reindent
            any indent that i do
          please
      """
    end
    EOF
  end

  i <<~EOF
  defmodule Test do
    @doc """
    it should
    have reasonable
    default start indent when typed
    """
  EOF
end
