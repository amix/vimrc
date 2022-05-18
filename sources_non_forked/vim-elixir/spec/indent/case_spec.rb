# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting case statements' do
  i <<~EOF
  case some_function do
    :ok ->
      :ok
    { :error, :message } ->
      { :error, :message }
  end
  EOF

  i <<~EOF
  case Connection.open(rabbitmq) do
    {:ok, conn} ->
      Woody.info "CONNECTION_SUCCESSFUL"
      {:ok, chan} = Channel.open(conn)
    {:error, error} ->
      Woody.info "CONNECTION_FAILED"
      :timer.sleep(10000)
  end
  EOF

  i <<~EOF
  defmodule M do
    defp _fetch(result, key, deep_key) do
      case _fetch(result, key) do
        {:ok, val} ->
          case _fetch(val, deep_key) do
            :error -> {:error, :deep}
            res -> res
          end

        :error -> {:error, :shallow}
      end
    end
  EOF

  i <<~EOF
  case Connection.open(rabbitmq) do
    {:ok, conn} ->
      Woody.info "CONNECTION_SUCCESSFUL"
      {:ok, chan} = Channel.open(conn)
    {:error, error} ->
      Woody.info "CONNECTION_FAILED"
      :timer.sleep(10000)
  end
  EOF

  i <<~'EOF'
  decoded_msg = case JSON.decode(msg) do
    {:error, _} ->
      a = "a"
      b = "dasdas"
      ">#{a}<>#{b}<"
    {:ok, decoded} -> decoded
  end
  EOF

  i <<~EOF
  case Repo.insert(changeset) do
    {:ok, user} ->
      conn
      |> put_flash(:info, "%{user.name} created!")
      |> redirect(to: user_path(conn, :index))
    {:error, changeset} ->
      render(conn, "new.html", changeset: changeset)
  end
  EOF

  i <<~EOF
  case st do
    sym ->
      code = if true do
        :ok
      else
        :error
      end
      Logger.info(code)
      st
  end
  EOF

  i <<~EOF
  case world do
    "apple" ->
      IO.puts "its an apple"

      IO.puts "no really, its an apple"
    "orange" ->
      IO.puts "its not an apple"
      IO.puts "believe it or not"
  end
  EOF

  i <<~EOF
  case o do
    a ->
      e(fn -> f end)
  end
  EOF

  i <<~EOF
  case pattern do
    :* -> :ok
    _ -> :error
  end
  EOF
end
