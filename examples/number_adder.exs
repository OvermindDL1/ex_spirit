defmodule NumberAdder do
  use ExSpirit.Parser, text: true

  defrule added_number(
    seq([
      uint(),
      repeat(char(?,) |> uint())
      ])
  ), map: Enum.sum

  def from_string(input) do
    parse(input, seq([added_number(), ignore(repeat(char(?\s)))]), skipper: repeat(char(?\s)))
  end

end

IO.puts("Input simple number separated by comma's and optionally spaces and press enter: ")
case IO.read(:line) |> NumberAdder.from_string() do
  %{error: nil, result: result, rest: "\n"} -> IO.puts("Result: #{inspect result}")
  %{error: nil, result: result, rest: rest} -> IO.puts("Result: #{inspect result}\nLeftover: #{inspect rest}")
  %{error: error} -> IO.puts("#{Exception.message(error)}")
end
