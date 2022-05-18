# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting blocks' do
  i <<~EOF
  do
    something
  end
  EOF

  i <<~EOF
  defmodule Test do
    def lol do
      IO.inspect :end
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def name, do: IO.puts "bobmarley"
    # expect next line starting here

    def name(param) do
      param
    end
  end
  EOF

  i <<~EOF
  defmodule Hello do
    def name, do: IO.puts "bobmarley"

    def name(param) do
      param
    end
  end
  EOF

  i <<~EOF
  def f do
    if true, do: 42
  end
  EOF

  i <<~EOF
  def f do
    x = :do
  end
  EOF

  i <<~EOF
  defmodule Test do
    def test do
      one =
        user
        |> build_assoc(:videos)
        |> Video.changeset()

      other =
        user2
        |> build_assoc(:videos)
        |> Video.changeset()
    end
  end
  EOF

  i <<~EOF
  defmodule MyMod do
    def how_are_you do
      IO.puts "I'm filling bad :("
      IO.puts "really bad"
    end
  end
  EOF

  i <<~EOF
  defmodule MyMod do
    def how_are_you do
      "function return"
    end
  end
  EOF

  i <<~EOF
  scope "/", API do
    pipe_through :api # Use the default browser stack

    get "/url", Controller, :index
    post "/url", Controller, :create
  end
  EOF

  i <<~EOF
    def hello do
      {:ok, _} = TaskRunner.TaskStore.start_link(name: @task_store)
      {:ok, _} = Workspace.start_link
      {:ok, pending_task_sup} = TaskRunner.PendingTaskSupervisor.start_link
    end
  EOF

  i <<~EOF
  def handle_info(:tick, state = %{policy_iteration: []}) do
    state = put_in(state[:policy_iteration], state.policy)
    {:noreply, state}
  end
  EOF
end
