# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting pipeline' do
  i <<~EOF
  "a,b,c,d"
  |> String.split(",")
  |> Enum.reverse
  EOF

  i <<~EOF
  [ h | t ] = "a,b,c,d"
              |> String.split(",")
              |> Enum.reverse
  EOF

  i <<~EOF
  def test do
    [ h | t ] = "a,b,c,d"
                |> String.split(",")
                |> Enum.reverse

    { :ok, h }
  end
  EOF

  i <<~EOF
  def test do
    my_post = Post
              |> where([p], p.id == 10)
              |> where([p], u.user_id == 1)
              |> select([p], p)
  end
  EOF

  i <<~EOF
  def test do
    "a,b,c,d"
    |> String.split(",")
    |> Enum.first
    |> case do
      "a" -> "A"
      _ -> "Z"
    end
  end
  EOF

  i <<~EOF
  defrecord RECORD, field_a: nil, field_b: nil

  rec = RECORD.new
        |> IO.inspect
  EOF

  i <<~EOF
  defmodule MyMod do
    def export_info(users) do
      {:ok, infos} = users
                     |> Enum.map(fn (u) -> do_something(u) end)
                     |> Enum.map(fn (u) ->
                       do_even_more(u)
                     end)
                     |> finall_thing

      infos
    end
  end
  EOF

  i <<~EOF
  def build_command(input, output) do
    "embedded=here"
    |>
  end
  EOF

  i <<~EOF
  def build_command(input, output) do
    'embedded=here'
    |>
  EOF

  i <<~EOF
  def build_command(input, output) do
    %{:hello => :world}
    |>
  end
  EOF

  %w(<= >= == != === !== =~).each do |op|
    i <<~EOF
    def build_command(input, output) do
      true #{op} false
      |> IO.inspect
    end
    EOF
  end

  i <<~EOF
    upcased_names = names
                    |> Enum.map(fn name ->
                      String.upcase(name)
                    end)

    IO.inspect names
  EOF

  i <<~EOF
    upcased_names = names
                    |> Enum.map(fn name ->
                      String.upcase(name) end)

    IO.inspect names
  EOF

  i <<~EOF
    upcased_names = names
                    |> Enum.map(fn name ->
                      String.upcase(name)
                    end)

                    |> do_stuff
  EOF

  i <<~EOF
  def hello do
    do_something
    |> Pipe.to_me
    {:ok}
  end
  EOF

  i <<~EOF
  defmodule MyModule do
    def do_stuff do
      name =
        "Dr. Zaius"
        |> determine_name

      hello
    end
  end
  EOF
end
