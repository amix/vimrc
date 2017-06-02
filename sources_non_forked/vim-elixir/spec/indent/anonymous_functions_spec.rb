require 'spec_helper'

describe "Indenting" do
  context "single body functions inside do block" do
    it "is declared with fn syntax" do
      <<-EOF
        def do
          some_func = fn  x -> x end
        end
      EOF
      .should be_elixir_indentation
    end

    it "is declared with function syntax" do
      <<-EOF
        def do
          some_func = function do x -> x end
        end
      EOF
      .should be_elixir_indentation
    end

    it "spans in multiple lines" do
      <<-EOF
        def test do
          assert_raise Queue.Empty, fn ->
            Q.new |> Q.deq!
          end
        end
      EOF
      .should be_elixir_indentation
    end

    it "spans in multiple lines inside parentheses" do
      <<-EOF
        defmodule Test do
          def lol do
            Enum.map([1,2,3], fn x ->
              x * 3
            end)
          end
        end
      EOF
      .should be_elixir_indentation
    end
  end

  context "multiple body functions declaring" do
    it "it with fn syntax" do
      <<-EOF
        fizzbuzz = fn
          0, 0, _ -> "FizzBuzz"
          0, _, _ -> "Fizz"
          _, 0, _ -> "Buzz"
          _, _, x -> x
        end
      EOF
      .should be_elixir_indentation
    end

    it "it with function syntax" do
      <<-EOF
        fizzbuzz = function do
          0, 0, _ -> "FizzBuzz"
          0, _, _ -> "Fizz"
          _, 0, _ -> "Buzz"
          _, _, x -> x
        end
      EOF
      .should be_elixir_indentation
    end
  end
end
