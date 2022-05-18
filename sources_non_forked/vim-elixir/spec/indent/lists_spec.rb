# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting lists' do
  i <<~EOF
  def example do
    [ :foo,
      :bar,
      :baz ]
  end
  EOF

  i <<~EOF
  [
    [
      :foo
    ]
  ]
  EOF

  i <<~EOF
  def project do
    [ name: "mix",
      version: "0.1.0",
      deps: deps ]
  end
  EOF

  i <<~EOF
  def config do
    [ name:
      "John" ]
  end
  EOF

  i <<~EOF
  def test do
    [ { :cowboy, github: "extend/cowboy" },
      { :dynamo, "0.1.0-dev", github: "elixir-lang/dynamo" },
      { :ecto, github: "elixir-lang/ecto" },
      { :pgsql, github: "semiocast/pgsql" } ]
  end
  EOF

  i <<~EOF
  def test do
    [ [:a, :b, :c],
      [:d, :e, :f] ]
  end
  EOF

  i <<~EOF
  def test do
    [ app: :first,
      version: "0.0.1",
      dynamos: [First.Dynamo],
      compilers: [:elixir, :dynamo, :ecto, :app],
      env: [prod: [compile_path: "ebin"]],
      compile_path: "tmp/first/ebin",
      deps: deps ]
  end
  EOF

  i <<~EOF
  def project do
    [
      { :bar, path: "deps/umbrella/apps/bar" },
      { :umbrella, path: "deps/umbrella" }
    ]
  end
  EOF

  i <<~EOF
  def test do
    a = [
      %{
        foo: 1,
        bar: 2
      }
    ]

    b = %{
      [
        :foo,
        :bar
      ]
    }

    [
      a,
      b
    ]
  end
  EOF

  i <<~EOF
  def create(conn, %{
    "grant_type" => "password",
    "username" => username,
    "password" => password
  }) do
    1
  end
  EOF

  i <<~EOF
  def double(x) do
    add(
      x,
      y
    )
  end
  EOF

  i <<~EOF
  def double(x) do
    add(
      x,
      y,
      w,
      z
    )
  end
  EOF

  i <<~EOF
  def double(x) do
    result = add(
      x,
      z
    )
    div(result, 2)
  end
  EOF

  i <<~EOF
  defmodule Module do
    @person1 { name: "name",
      age: 18,
      enabled?: true }
    @person2 { name: "other name",
      age: 21,
      enabled?: false }
  end
  EOF

  i <<~EOF
  def test_another_feature do
    assert json_response(conn, 200) == %{
      "results" => [
        %{
          "id" => result.id,
        }
      ]
    }
  end
  EOF

  i <<~EOF
  defmodule Mod do
    def test do
      foo == %{
      }

      assert json_response == %{
        "id" => "identifier"
      }
    end
  end
  EOF

  i <<~EOF
  defmodule Mod do
    def fun do
      json_logger = Keyword.merge(Application.get_env(:logger, :json_logger, []), options)
      Application.put_env(:logger, :json_logger, json_logger)
      level  = Keyword.get(json_logger, :level)

      %{level: level, output: :console}
    end
  end
  EOF

  i <<~EOF
  defmodule Mod do
    def fun do
      Enum.each(s.routing_keys, fn k -> Queue.bind(chan, s.queue, s.exchange, routing_key: k) end)
      Basic.consume(chan, s.queue, nil, no_ack: true)
    end
  end
  EOF

  i <<~EOF
  def init(_) do
    children = [
      worker(QueueSet, [[name: @queue_set]]),
      worker(Producer, [[name: @producer]]),
      worker(ConsumerSupervisor, [[{@producer, max_demand: @max_executors}]])
    ]

    supervise(children, strategy: :one_for_one)
  end
  EOF
end
