# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting *after* comments' do
  i <<~EOF
  # do
  IO.puts :test
  EOF

  i <<~EOF
  defmodule Foo do
    def run do
      list =
        File.read!("/path/to/file")
        |> String.split()
      # now start a new line
      # used to start here
      # but now starts here
    end
  end
  EOF

  i <<~EOF
  defmodule Foo do
    def run(task) when task in [:t1, :t2] do
    end

    # now starts a new line
    # use to start here
    # but now starts here
  end
  EOF

  i <<~EOF
  receive do
    {{:lock_ready, ^key}, ^pid} ->
  after
    # NOTE: @jbodah 2017-03-28: we should do some math to adjust the timeout
    timeout ->
      {:error, :timed_out_waiting_for_lock}
  end
  EOF

  it "bulk indenting comments" do
    expect(<<~EOF).to be_elixir_indentation
    defmodule Test do
      # SELECT *
      #  FROM table
      # WHERE column = 123
      #   AND another_column = 456
    end
    EOF
  end
end

