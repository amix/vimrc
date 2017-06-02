require 'spec_helper'

describe "Indenting" do
  specify "case statements" do
    <<-EOF
      case some_function do
        :ok ->
          :ok
        { :error, :message } ->
          { :error, :message }
      end
    EOF
    .should be_elixir_indentation
  end
end
