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
    iex> context = ExSpirit.parse("Test 42", lit("Test"))
    iex> {context.error, context.result, context.rest}
    {nil, nil, " 42"}

    # `lit` matches a specific string or character
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("Test 42", lit(?T))
    iex> {context.error, context.result, context.rest}
    {nil, nil, "est 42"}

    # `uint` parses out an unsigned integer, default radix of 10 with a min size of 1 and max of unlimited
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("42", uint())
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}

    # `|>` Returns the result of the last parser in the pipe chain,
    # `lit` always return nil for example
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("42Test", uint() |> lit("Test"))
    iex> {context.error, context.result, context.rest}
    {nil, nil, ""}

    # `|>` Returns the result of the last parser in the pipe chain
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("42Test64", uint() |> lit("Test") |> uint())
    iex> {context.error, context.result, context.rest}
    {nil, 64, ""}

    # `uint` parsing out base-2
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("101", uint(2))
    iex> {context.error, context.result, context.rest}
    {nil, 5, ""}

    # `uint` parsing out base-16 lower-case, can be mixed too
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("ff", uint(16))
    iex> {context.error, context.result, context.rest}
    {nil, 255, ""}

    # `uint` parsing out base-16 upper-case, can be mixed too
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("FF", uint(16))
    iex> {context.error, context.result, context.rest}
    {nil, 255, ""}

    # `seq` parses a sequence returning the return of all of them, removing nils,
    # as a list if more than one or the raw value if only one, if any fail then
    # all fail.
    iex> import ExSpirit.Tests.Parser
    iex> contexts = ExSpirit.parse("42 64", seq([uint(), lit(" "), uint()]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, [42, 64], ""}

    # `seq` Here is sequence only returning a single value
    iex> import ExSpirit.Tests.Parser
    iex> contexts = ExSpirit.parse("42Test", seq([uint(), lit("Test")]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, 42, ""}

    # `alt` parses a set of alternatives in order and returns the first success
    iex> import ExSpirit.Tests.Parser
    iex> contexts = ExSpirit.parse("FF", alt([uint(16), lit("Test")]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, 255, ""}

    # `alt` parses a set of alternatives in order and returns the first success
    iex> import ExSpirit.Tests.Parser
    iex> require ExSpirit
    iex> contexts = ExSpirit.parse("Test", alt([uint(16), lit("Test")]))
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, nil, ""}

    # You can use `defrule`s as any other terminal parser
    iex> import ExSpirit.Tests.Parser
    iex> require ExSpirit
    iex> contexts = ExSpirit.parse("42 64", testrule())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, [42, 64], ""}

    # `defrule`s can map the result to return a different one:
    iex> import ExSpirit.Tests.Parser
    iex> require ExSpirit
    iex> contexts = ExSpirit.parse("42 64", testrule_map())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, [2, 24], ""}

    # `defrule`s can also operate over the context itself to do anything
    iex> import ExSpirit.Tests.Parser
    iex> require ExSpirit
    iex> contexts = ExSpirit.parse("42 64", testrule_fun())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, {"altered", [42, 64]}, ""}

    # `defrule`s can also be a context function by only passing in `context`
    iex> import ExSpirit.Tests.Parser
    iex> require ExSpirit
    iex> contexts = ExSpirit.parse("42 64", testrule_context())
    iex> {contexts.error, contexts.result, contexts.rest}
    {nil, "always success", "42 64"}

    # `tag` can tag the output from a parser
    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("ff", tag(:integer, uint(16)))
    iex> {context.error, context.result, context.rest}
    {nil, {:integer, 255}, ""}


  ```
  """
  defmacro parse(rest, parser, opts \\ []) do
    filename = opts[:filename] || quote do "<unknown>" end
    skipper = opts[:skipper] || quote do &ExSpirit.Parser._no_skip/1 end
    quote location: :keep do
      %ExSpirit.Parser.Context{
        filename: unquote(filename),
        skipper: unquote(skipper),
        rest: unquote(rest),
      } |> unquote(parser)
    end
  end
end
