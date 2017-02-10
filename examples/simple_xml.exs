defmodule SimpleXML do
  use ExSpirit.Parser, text: true

  defrule text( chars(-?<) )

  defrule tag_name( chars([?a..?z, ?A..?Z, ?0..?9, ?_, ?-]) )

  defrule tag(
    lit(?<) |> tag_name |> put_state(:tagname, :result) |> lit(?>) |> expect(seq([
      get_state_into(:tagname, tag(&1, repeat(node_()))),
      lit("</"), get_state_into(:tagname, lit(&1)), lit(?>)
    ]))
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
