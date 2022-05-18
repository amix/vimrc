# frozen_string_literal: true

require 'spec_helper'

describe 'with' do
  i <<~EOF
  with {:ok, msg} <- Msgpax.unpack(payload) do
    {:ok, rebuild(msg)}
  else
    error -> error
  end
  EOF

  i <<~EOF
  with {:ok, width} <- Map.fetch(opts, :width),
       double_width = width * 2,
       {:ok, height} <- Map.fetch(opts, :height)
  do
    {:ok, double_width * height}
  end
  EOF

  i <<~EOF
    with {:ok, width} <- Map.fetch(opts, :width),
         double_width = width * 2,
         {:ok, height} <- Map.fetch(opts, :height),
         do: {:ok, double_width * height}
  EOF

  i <<~EOF
  with {:ok, width} <- Map.fetch(opts, :width),
       {:ok, height} <- Map.fetch(opts, :height)
  do
    {:ok, width * height}
  else
    :error ->
      {:error, :wrong_data}
  end
  EOF

  i <<~EOF
  with {:ok, width} <- Map.fetch(opts, :width),
       {:ok, height} <- Map.fetch(opts, :height),
       do:
         {:ok,
           width * height * height * height * height * height * height * height * height * height *
             height * height * height * height * height * height * height},
       else: (:error -> {:error, :wrong_data})
  EOF

  i <<~'EOF'
    # This file is responsible for configuring your application
    # and its dependencies with the aid of the Mix.Config module.
    use Mix.Config

    import_config "#{Mix.env}.exs"
  EOF

  i <<~'EOF'
  with {:ok, %File.Stat{size: size}} when size > 0 <- File.stat(first_frame_path) do
    File.rename(first_frame_path, output_path)
    {:ok, %Result{path: output_path}}
  else
    error ->
      {:error, error}
  end
  EOF

  i <<~'EOF'
  def resend_confirmation(username) when is_binary(username) do
    with user = %User{confirmed_at: nil} <- get_by(username: username) do
      {:ok, user} =
        user
        |> DB.add_confirm_token
        |> update_user()
      Log.info(%Log{user: user.id, message: "send new confirmation"})
      send_welcome(user)
      {:ok, user}
    else
      nil ->
        {:error, "not found"}
      %User{email: email} ->
        Email.already_confirmed(email)
        {:error, "already confirmed"}
    end
  end
  EOF

  i <<~'EOF'
  def create_user(params) do
    profile = UserProfile.registration_changeset(%UserProfile{}, params)

    user_cs =
      %User{}
      |> User.registration_changeset(params)
      |> put_assoc(:user_profile, profile)

    with {:ok, user} <- Repo.insert(user_cs, returning: false) do
      Log.info(%Log{user: user.id, message: "user created"})
      send_welcome(user)
      {:ok, user}
    end
  end
  EOF

  i <<~'EOF'
  def my_function do
    with :ok <- some_call,
         :ok <- another_call do

    end
  end
  EOF

  i <<~'EOF'
  with {:ok, foo} <- thing(1),
       {:ok, bar} <- thing(2) do
    foo + bar
  end
  EOF
end
