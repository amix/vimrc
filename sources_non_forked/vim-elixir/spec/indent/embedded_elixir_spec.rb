require 'spec_helper'

describe "Indenting" do
  describe "Embedded Elixir" do
    it "if-clauses" do
      <<-EOF
      # An Example
      <%= if true do %>
        It is obviously true
      <% end %>
      ---
      EOF
      .should be_eelixir_indentation
    end

    it "if-else-clauses" do
      <<-EOF
      # An Example
      <%= if true do %>
        It is obviously true
      <% else %>
        This will never appear
      <% end %>
      ---
      EOF
      .should be_eelixir_indentation
    end
  end
end
