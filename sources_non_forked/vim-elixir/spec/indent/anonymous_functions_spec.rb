# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting anonymous functions' do
  i <<~EOF
  def do
    some_func = fn  x -> x end
  end
  EOF

  i <<~EOF
  def do
    some_func = function do x -> x end
  end
  EOF

  i <<~EOF
  def test do
    assert_raise Queue.Empty, fn ->
      Q.new |> Q.deq!
    end
  end
  EOF

  i <<~EOF
  defmodule Test do
    def lol do
      Enum.map([1,2,3], fn x ->
        x * 3
      end)
    end
  end
  EOF

  i <<~EOF
  fizzbuzz = fn
    0, 0, _ -> "FizzBuzz"
    0, _, _ -> "Fizz"
    _, 0, _ -> "Buzz"
    _, _, x -> x
  end
  EOF

  i <<~EOF
  fizzbuzz = function do
    0, 0, _ -> "FizzBuzz"
    0, _, _ -> "Fizz"
    _, 0, _ -> "Buzz"
    _, _, x -> x
  end
  EOF

  i <<~EOF
    {:ok, 0} = Mod.exec!(cmd, fn progress ->
      if event_handler do
        event_handler.({:progress_updated, progress})
      end
    end
    )
  EOF

  i <<~EOF
  defp handle_chunk(:err, line, state) do
    update_in(state[:stderr], fn
      true -> true
      false -> false
    end)

    Map.update(state, :stderr, [line], &(&1 ++ [line]))
  end
  EOF

  i <<~EOF
  defp handle_chunk(:err, line, state) do
    update_in(state[:stderr], fn
      hello -> :ok
      world -> :ok
    end)

    Map.update(state, :stderr, [line], &(&1 ++ [line]))
  end
  EOF

  i <<~EOF
  fn ->
  end
  EOF
end
