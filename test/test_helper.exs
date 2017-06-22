ExUnit.start()

# Documentation template
_ = """
  #### <PARSER_NAME>

  <PARSER_DESCRIPTION>

  ##### Examples

  ```elixir

  ```
"""

defmodule ExSpirit.Tests.Parser do
  use ExSpirit.Parser, text: true

  defrule testrule(
    seq([ uint(), lit(?\s), uint() ])
    )

  defrule testrule_pipe(
    seq([ uint(), lit(?\s), uint() ])
    ), pipe_result_into: Enum.map(fn i -> i-40 end)

  defrule testrule_fun(
    seq([ uint(), lit(?\s), uint() ])
    ), fun: (fn context -> %{context | result: {"altered", context.result}} end).()

  defrule testrule_context(context) do
    %{context | result: "always success"}
  end

  defrule testrule_context_arg(context, value) do
    %{context | result: value}
  end
end
