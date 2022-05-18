# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting eelixir' do
  it 'anonymous function' do
    expect(<<~EOF).to be_eelixir_indentation
    <%= form_for @changeset, user_path(@conn, :create), fn f -> %>
      It is obviously true
    <% end %>
    EOF
  end

  it 'if..do..end' do
    expect(<<~EOF).to be_eelixir_indentation
    <%= if true do %>
      It is obviously true
    <% end %>
    EOF
  end

  it 'if..do..else..end' do
    expect(<<~EOF).to be_eelixir_indentation
    <%= if true do %>
      It is obviously true
    <% else %>
      This will never appear
    <% end %>
    EOF
  end
end
