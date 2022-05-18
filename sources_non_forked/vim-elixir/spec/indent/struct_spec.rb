require 'spec_helper'

describe 'defstruct' do
  i <<~EOF
  defmodule A do
    defmodule State do
      defstruct field: nil, field: nil, field: nil,
        field: [], field: nil, field: 0,
        field: false, field: %{}
    end

    defmodule State do
      defstruct field: nil, field: nil, field: nil
    end

    defmodule State do
      defstruct field: nil,
        field: [],
        field: false
    end
  end
  EOF
end
