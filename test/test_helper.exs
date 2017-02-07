ExUnit.start()

defmodule ExSpirit.Tests.Parser do
  use ExSpirit.Parser

  defrule testrule, seq([ uint(), lit(?\s), uint() ])
end
