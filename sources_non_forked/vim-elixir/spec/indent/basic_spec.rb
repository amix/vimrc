# frozen_string_literal: true

require 'spec_helper'

describe 'Basic indenting' do
  i <<~EOF

  defmodule Hello do
  EOF

  i <<~EOF
  defmodule Hello do
    def some_func do
  EOF

  i <<~EOF
  defmodule Hello do
    def some_func do
    end
  EOF

  i <<~EOF
  defmodule Hello do
    def some_func do
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      IO.puts "hello world"
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      IO.puts "hello world"
    end
    def some_other_func do
      IO.puts "hello world"
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      IO.puts "hello world"
    end

    def some_other_func do
      IO.puts "hello world"
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      IO.puts "hello world"
    end

    def some_other_func do
      IO.puts "hello world"
      IO.puts "hello world"
      IO.puts "hello world"
      IO.puts "hello world"
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      IO.puts "hello world"
    end

    def some_other_func do
      if blah? do
        blah
      else
        not_blah
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      IO.puts "hello world"
    end

    def some_other_func do
      if blah? do
        blah
      else
        not_blah
      end
      if blah? do
        blah
      else
        not_blah
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      IO.puts "hello world"
    end

    def some_other_func do
      if blah? do
        blah
        if blah? do
          blah
        else
          not_blah
        end
      else
        not_blah
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      cond do
        {:abc} -> false
        _ -> true
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      cond do
        {:abc} -> false

        _ -> true
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      cond do
        {:abc} ->
          say_hello
          say_goodbye

        _ ->
          say_hello
          say_goodbye
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello.World do
    def some_func do
      cond do
        {:abc} ->
          cond do
            {:abc} ->
              say_hello
              say_goodbye
            _ ->
              say_hello
              say_goodbye
          end
          say_hello
          say_goodbye

        _ ->
          say_hello
          say_goodbye
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      case word do
        :one -> :two
        :high -> :low
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      case word do
        :one -> :two

        :high -> :low
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      case word do
        :one ->
          :two

        :high ->
          :low
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      case word do
        :one ->
          case word do
            :one ->
              :two

            :high ->
              :low
          end
          :two

        :high ->
          :low
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    defmacro hello do
      quote do
        blah
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      unless blah do
        blah
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      if stinky?, do: clean
      if smelly?, do: clean
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      name =
        "one"
      street =
        "two"
    end
  end
  EOF

  %w(= == === != !== <= >= <> && || + - * / ~~~ ^^^ <<< >>> ||| &&&).each do |bin_op|
    i <<~EOF
    defmodule Hello do
      def hello do
        name #{bin_op}
          "one"
        street #{bin_op}
          "two"
      end
    end
    EOF

    i <<~EOF
    defmodule Hello do
      def hello do
        name #{bin_op} "one"
        street #{bin_op} "two"
      end
    end
    EOF
  end

  i <<~EOF
  defmodule Hi do
    def hi do
      fn hello ->
        :world
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def hello do
      name = "one"
      street = "two"
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    def hi do
      fn hello -> :world end
      fn hello -> :world end
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    def hi do
      fn hello ->
        case hello do
          :one ->
            case word do
              :one ->
                :two

              :high ->
                :low
            end
            :two

          :high ->
            :low
        end
      end
    end
  end
  EOF

  i <<~EOF
  hello =
    "str"
    |> Pipe.do_stuff
    |> Pipe.do_stuff

    |> Pipe.do_stuff
    |> Pipe.do_stuff(fn ->
      more stuff
    end)

    |> Pipe.do_stuff
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      :hello
    end

    defp hi do
      :hello
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      [
        :one,
        :two,
        fn ->
          :three
        end,
        :four
      ]
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      {
        :one,
        :two,
        fn ->
          :three
        end,
        :four
      }
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      %Struct{
        :one,
        :two,
        fn ->
          :three
        end,
        :four
      }
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      %{
        :one,
        :two,
        fn ->
          :three
        end,
        :four
      }
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      try do
        raise "boom"
      rescue
        e in errs ->
          IO.puts "one"

        _ ->
          IO.puts "one"
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      try do
        raise "wtf"
      catch
        e ->
          IO.puts "one"

        _ ->
          IO.puts "one"
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      receive do
        {:hello, world} ->
          :ok
      after
        1000 ->
          IO.puts "one"

        2000 ->
          IO.puts "one"
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      receive do
        {:hello, world} ->
          :ok

        _ ->
          :err
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      fn
        :ok ->
          IO.puts :ok
        _ ->
          IO.puts :err
      end
    end
  end
  EOF

  i <<~EOF
  defmodule Hi do
    defp hi do
      fn
        :ok -> IO.puts :ok
        _ -> IO.puts :err
      end
    end
  end
  EOF

  i <<~EOF
  fun2 = fn :foo ->
    :bar
    'end'
  end

  EOF

  i <<~EOF
  fun2 = fn :foo ->
    :bar
    'end'
  end
  EOF

  i <<~EOF
  fun3 = fn :foo ->
    :bar
    :send
  end
  EOF

  i <<~EOF
  defmodule Hi do
    def hello_world do
      "end"
      'end'
    end
  EOF
end
