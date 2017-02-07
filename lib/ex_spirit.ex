defmodule ExSpirit do
  @moduledoc """
  Documentation for ExSpirit.
  """

  @doc """
  Hello world.

  ## Examples

  ```elixir

    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("Test 42", lit("Test"))
    iex> {context.error, context.results, context.rest}
    {nil, [], " 42"}

    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("42", uint())
    iex> {context.error, context.results, context.rest}
    {nil, [42], ""}

    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("42Test", uint() |> lit("Test"))
    iex> {context.error, context.results, context.rest}
    {nil, [42], ""}

    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("101", uint(2))
    iex> {context.error, context.results, context.rest}
    {nil, [5], ""}

    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("ff", uint(16))
    iex> {context.error, context.results, context.rest}
    {nil, [255], ""}

    iex> import ExSpirit.Tests.Parser
    iex> context = ExSpirit.parse("FF", uint(16))
    iex> {context.error, context.results, context.rest}
    {nil, [255], ""}

    iex> import ExSpirit.Tests.Parser
    iex> contexts = ExSpirit.parse("FF", alt([uint(16), lit("Test")]))
    iex> {contexts.error, contexts.results, contexts.rest}
    {nil, [255], ""}

    iex> import ExSpirit.Tests.Parser
    iex> require ExSpirit
    iex> contexts = ExSpirit.parse("Test", alt([uint(16), lit("Test")]))
    iex> {contexts.error, contexts.results, contexts.rest}
    {nil, [], ""}


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
