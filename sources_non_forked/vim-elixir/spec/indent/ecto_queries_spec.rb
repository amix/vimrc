# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting Ecto queries' do
  i <<~EOF
  defmodule New do
    def do_query do
      from user in Users,
        select: user.name,
        join: signup in Signups, where: user.id == signup.user_id
    end
  end
  EOF

  i <<~EOF
  def smth do
    from = 1
    to = 7
  end
  EOF

  i <<~EOF
  fromin,
  EOF

  i <<~EOF
  query = from u in query, select: u.city
  EOF

  i <<~EOF
  def do_query do
    where = [category: "fresh and new"]
    order_by = [desc: :published_at]
    select = [:id, :title, :body]
    from Post, where: ^where, order_by: ^order_by, select: ^select
  end
  EOF

  i <<~EOF
  def alphabetical(query) do
    from c in query, order_by: c.name
  end
  EOF
end
