defmodule SimpleXML do
  use ExSpirit.Parser, text: true

  defrule text( repeat(char(-?<), 1) ), map: :erlang.iolist_to_binary()

  defrule tag_name( repeat(char([?a..?z, ?A..?Z, ?0..?9, ?_, ?-]), 1) )

  defrule tag(
    branch(seq([ lit(?<), tag_name, lit(?>) ]),
      fn name ->
        name = :erlang.iolist_to_binary(name)
        fn context ->
          context |> expect(tag(name, seq([
            repeat(node_()),
            lit("</"), lit(name), lit(?>),
          ])))
        end
      end)
  )

  defrule node_(
    alt([
      tag(),
      text(),
    ])
  )

  def from_string(input) do
    parse(input, node_())
  end

end

IO.puts("Input a single line of xml-like syntax: ")
case IO.read(:line) |> SimpleXML.from_string() do
  %{error: nil, result: result, rest: "\n"} -> IO.puts("Result: #{inspect result}")
  %{error: nil, result: result, rest: rest} -> IO.puts("Result: #{inspect result}\nLeftover: #{inspect rest}")
  %{error: error} -> IO.puts("#{Exception.message(error)}")
end
