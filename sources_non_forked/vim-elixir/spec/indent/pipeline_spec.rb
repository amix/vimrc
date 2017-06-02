require 'spec_helper'

describe "Indenting" do
  it "using multiline pipeline" do
    <<-EOF
    "a,b,c,d"
    |> String.split(",")
    |> Enum.reverse
    EOF
    .should be_elixir_indentation
  end

  it "attribuition using multline pipeline operator" do
    <<-EOF
    [ h | t ] = "a,b,c,d"
                |> String.split(",")
                |> Enum.reverse
    EOF
    .should be_elixir_indentation
  end

  it "function with pipeline operator" do
    <<-EOF
    def test do
      [ h | t ] = "a,b,c,d"
                  |> String.split(",")
                  |> Enum.reverse

      { :ok, h }
    end
    EOF
    .should be_elixir_indentation
  end

  it "do not breaks on `==`" do
    <<-EOF
    def test do
      my_post = Post
                |> where([p], p.id == 10)
                |> where([p], u.user_id == 1)
                |> select([p], p)
    end
    EOF
    .should be_elixir_indentation
  end

  it "pipeline operator with block open" do
    <<-EOF
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
    .should be_elixir_indentation
  end

  it "using a record with pipeline" do
    <<-EOF
    defrecord RECORD, field_a: nil, field_b: nil

    rec = RECORD.new
          |> IO.inspect
    EOF
    .should be_elixir_indentation
  end
end
