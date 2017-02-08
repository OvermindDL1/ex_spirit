defmodule ExSpirit do
  @moduledoc """
  Documentation for ExSpirit.
  """

  @doc """
  Hello world.

  ## Examples

  ```elixir

    # `lit` matches a specific string or character
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test 42", lit("Test"))
    iex> {context.error, context.result, context.rest}
    {nil, nil, " 42"}

    # `lit` matches a specific string or character
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test 42", lit(?T))
    iex> {context.error, context.result, context.rest}
    {nil, nil, "est 42"}

    # `uint` parses out an unsigned integer, default radix of 10 with a min size of 1 and max of unlimited
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("42", uint())
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}

    # `|>` Returns the result of the last parser in the pipe chain,
    # `lit` always returns nil for example
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("42Test", uint() |> lit("Test"))
    iex> {context.error, context.result, context.rest}
    {nil, nil, ""}

    # `|>` Returns the result of the last parser in the pipe chain
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("42Test64", uint() |> lit("Test") |> uint())
    iex> {context.error, context.result, context.rest}
    {nil, 64, ""}

    # `uint` parsing out base-2
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("101", uint(2))
    iex> {context.error, context.result, context.rest}
    {nil, 5, ""}

    # `uint` parsing out base-16 lower-case, can be mixed too
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("ff", uint(16))
    iex> {context.error, context.result, context.rest}
    {nil, 255, ""}

    # `uint` parsing out base-16 upper-case, can be mixed too
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("FFF", uint(16))
    iex> {context.error, context.result, context.rest}
    {nil, 4095, ""}

    # `seq` parses a sequence returning the return of all of them, removing nils,
    # as a list if more than one or the raw value if only one, if any fail then
    # all fail.
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("42 64", seq([uint(), lit(" "), uint()]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, [42, 64], ""}

    # `seq` Here is sequence only returning a single value
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("42Test", seq([uint(), lit("Test")]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, 42, ""}

    # `alt` parses a set of alternatives in order and returns the first success
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("FF", alt([uint(16), lit("Test")]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, 255, ""}

    # `alt` parses a set of alternatives in order and returns the first success
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("Test", alt([uint(16), lit("Test")]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, nil, ""}

    # You can use `defrule`s as any other terminal parser
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("42 64", testrule())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, [42, 64], ""}

    # `defrule`'s also set up a stack of calls down a context so you know
    # 'where' an error occured, so name the rules descriptively
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("42 fail", testrule())
    iex> {contexts.error.context.rulestack, contexts.result, contexts.rest}
    {[:testrule], nil, "fail"}

    # `defrule`s can map the result to return a different one:
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("42 64", testrule_map())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, [2, 24], ""}

    # `defrule`s can also operate over the context itself to do anything
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("42 64", testrule_fun())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, {"altered", [42, 64]}, ""}

    # `defrule`s can also be a context function by only passing in `context`
    iex> import ExSpirit.Tests.Parser
    iex> contexts = parse("42 64", testrule_context())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, "always success", "42 64"}

    # `tag` can tag the output from a parser
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("ff", tag(:integer, uint(16)))
    iex> {context.error, context.result, context.rest}
    {nil, {:integer, 255}, ""}

    # You can have a skipper too, skippers should be run at the start of any
    # terminal parser, it runs only once per pass, if you want it to repeat then
    # set the skipper up so it repeats, a good one is `repeat(lit(?\\s))` for
    # example
    iex> import ExSpirit.Tests.Parser
    iex> context = parse(" 42 ", uint(), skipper: lit(?\\s))
    iex> {context.error, context.result, context.rest}
    {nil, 42, " "}

    # You can turn off a skipper for a parser as well with `no_skip`
    iex> import ExSpirit.Tests.Parser
    iex> context = parse(" Test:42 ", lit("Test:") |> no_skip(uint()), skipper: lit(?\\s))
    iex> {context.error, context.result, context.rest}
    {nil, 42, " "}
    {nil, 42, " "}

    # You can change a skipper for a parser as well with `skip`
    iex> import ExSpirit.Tests.Parser
    iex> context = parse(" Test:\t42 ", lit("Test:") |> skip(uint(), lit(?\\t)), skipper: lit(?\\s))
    iex> {context.error, context.result, context.rest}
    {nil, 42, " "}

    # `char` can parse out any single character
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", char())
    iex> {context.error, context.result, context.rest}
    {nil, ?T, "est"}

    # `char` can parse out any 'specific' single character as well
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", char(?T))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, "est"}

    # `char` can parse out any 'specific' single character from a range too
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", char(?A..?Z))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, "est"}

    # `char` can parse out any 'specific' single character from a list of
    # characters or ranges too
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", char([?a..?z, ?T]))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, "est"}

    # `ignore` will run the parser but return no result
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", ignore(char([?a..?z, ?T])))
    iex> {context.error, context.result, context.rest}
    {nil, nil, "est"}

    # `branch` is mostly for efficiency, it is useful for when you have a single
    # parser that is then followed by a specific parser.  It takes two arguments,
    # the first of which is the initial 'symbol' parser, the second is a map of
    # %{value => parserFuns}, the output of the symbol parser is used to look up
    # the value in the map, then the found parser is run.  If the symbol parser
    # fails, or the key is not in the map, or the post-parser fails then this
    # fails.  Because of the anonymous functions this has a slight overhead so
    # only use this above `choice` when the amount is at least more than a few.
    # This returns the output of the post-parser only.
    iex> import ExSpirit.Tests.Parser
    iex> symbol_map = %{?b => &uint(&1, 2), ?d => &uint(&1, 10), ?x => &uint(&1, 16)}
    iex> context = parse("b101010", branch(char(), symbol_map))
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}
    iex> context = parse("d213478", branch(char(), symbol_map))
    iex> {context.error, context.result, context.rest}
    {nil, 213478, ""}
    iex> context = parse("xe1DCf", branch(char(), symbol_map))
    iex> {context.error, context.result, context.rest}
    {nil, 925135, ""}

    # `symbols` takes a ExSpirit.TST, which is a structure designed for fast
    # lookups, though slow insertions, so please cache the data structure at
    # compile-time if possible.  This `symbols` parser will take the text input
    # stream and match it on the TST to find the longest-matching string, then
    # it will run the parser on it like it is done in `branch`.  Similar
    # semantics to branch otherwise.
    iex> import ExSpirit.Tests.Parser
    iex> alias ExSpirit.TST, as: TST
    iex> symbol_tst = TST.new() |> TST.add_text("int", &uint(&1)) |> TST.add_text("char", &char(&1))
    iex> context = parse("int42", symbols(symbol_tst))
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}
    iex> context = parse("charT", symbols(symbol_tst))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, ""}


  ```
  """
  defmacro parse(module_ast, rest_ast, parser_ast, opts_ast \\ []) do
    quote do
      unquote(module_ast).parse(unquote(rest_ast), unquote(parser_ast), unquote(opts_ast))
    end
  end
end
