ExUnit.start()

defmodule ExSpirit.Tests.Parser do
  use ExSpirit.Parser, text: true

  defrule testrule(
    seq([ uint(), lit(?\s), uint() ])
    )

  defrule testrule_map(
    seq([ uint(), lit(?\s), uint() ])
    ), map: Enum.map(fn i -> i-40 end)

  defrule testrule_fun(
    seq([ uint(), lit(?\s), uint() ])
    ), fun: (fn context -> %{context | result: {"altered", context.result}} end).()

  defrule testrule_context(context) do
    %{context | result: "always success"}
  end
end
