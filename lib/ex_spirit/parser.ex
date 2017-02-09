defmodule ExSpirit.Parser do
  @moduledoc """

  ExSpirit.Parser is the parsing section of ExSpirit, designed to parse out some
  kind of stream of data (whether via a binary, a list, or perhaps an actual
  stream) into a data structure of your own design.


  # Definitions

  ## Terminal Parser

  A terminal parser is one that does not operate over any other parser, it is
  'terminal' in its location.

  ## Combination Parser

  A combination parser is one that takes a parser as an input and does something
  with it, whether that is repeating it, surrounding it, or ignoring its output
  as a few examples.


  # Usage

  Just add `use ExSpirit.Parser` to a module to make it into a parsing module.

  To add text parsing functions from the `ExSpirit.Parsing.Text` module then add
  `text: true` to the use call.

  ### Example

  ```elixir

    defmodule MyModule do
      use ExSpirit.Parser, text: true
    end

  ```

  ## parse

  The parse function is applied to the input and a parser call, such as in:

  ```elixir

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("42", uint())
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}

  ```

  ## Skippers

  A skipper is a parser that is passed to the `:skipper` key in the `parse` call
  that will be called at the start of every built-in terminal.  So when you
  parse a, for example, `uint()`, then it will be like calling
  `(skipper |> uint())` in its place.  There are a few related function as well.

  ### Examples

  ```elixir

    # A skipper runs only once per terminal, if you want it to repeat the skipper
    # then set the skipper up so it repeats, a good one is `repeat(lit(?\\s))` for
    # example
    iex> import ExSpirit.Tests.Parser
    iex> context = parse(" 42 ", uint(), skipper: lit(?\\s))
    iex> {context.error, context.result, context.rest}
    {nil, 42, " "}

  ```


  # Parsers

  ## Elixir Standard Pipe Operator |>

  The `|>` pipe operator can be used to run a parser, and then another parser,
  however it will only return the result of the last parser in the pipe chain.

  This library does *not* override Elixir's pipe operator, it uses it verbatum.
  Use `seq` for the usual expected sequence parsing.

  ### Examples

  ```elixir

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

  ```

  ## seq

  The Sequence operator runs all of the parsers in the inline list (cannot be a
  variable) and returns their results as a list.  Any `nil`'s returned are not
  added to the result list, and if the result list has only a single value
  returned then it returns that value straight away without being wrapped in a
  list.

  ### Examples

  ```elixir

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

  ```

  ## alt

  The alternative parser runs the parsers in the inline list (cannot be a
  variable) and returns the result of the first one that succeeds, or the error
  of the last one.

  ### Examples

  ```elixir

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

  ```

  ## defrule

  Defining a rule defines a parser as well as some associated information such
  as the name of it for error reporting purposes, a mapping function so you can
  convert the output on the fly (fantastic for in-line AST generation for
  example!), among other uses.  It is used like any other normal terminal rule.

  ### Examples

  All of the following examples use this definition of rules in a module:

  ```elixir

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


  ```

  ```elixir

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

  ```

  ## no_skip

  The `no_skip` combination parser takes a parser and clears the skipper so they
  do no skipping.  Good to parse non-skippable content within a large parser.

  ### Examples

  ```elixir

    # You can turn off a skipper for a parser with `no_skip`
    iex> import ExSpirit.Tests.Parser
    iex> context = parse(" Test:42 ", lit("Test:") |> no_skip(uint()), skipper: lit(?\\s))
    iex> {context.error, context.result, context.rest}
    {nil, 42, " "}

  ```

  ## skipper

  The `skipper` combination parser takes a parser and changes the skipper within
  it to the one you pass in for the duration of the parser that you pass in.

  ### Examples

  ```elixir

    # You can change a skipper for a parser as well with `skipper`
    iex> import ExSpirit.Tests.Parser
    iex> context = parse(" Test:\t42 ", lit("Test:") |> skipper(uint(), lit(?\\t)), skipper: lit(?\\s))
    iex> {context.error, context.result, context.rest}
    {nil, 42, " "}

  ```

  ## ignore

  The `ignore` combination parser takes and runs a parser but ignores the
  result of the parser, instead returning `nil`.

  ### Examples

  ```elixir

    # `ignore` will run the parser but return no result
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", ignore(char([?a..?z, ?T])))
    iex> {context.error, context.result, context.rest}
    {nil, nil, "est"}

  ```

  ## branch

  The `branch` combination parser is designed for efficient branching based on
  the result from another parser.  It allows you to parse something, and using
  the result of that parser you can then either lookup the value in a map or
  call into a user function, either of which can return a parser function that
  will then be used to continue parsing.

  It takes two arguments, the first of which is the initial parser, the second
  is either a user function of `value -> parserFn` or a map of
  `values => parserFn` where the value key is looked up from the result of the
  first parser.  If the parserFn is `nil` then `branch` fails, else the parserFn
  is executed to continue parsing.  Because of the anonymous function calls this
  has a slight overhead so only use this if switching parsers dynamically based
  on a parsed value that is more complex then a simple `alt` parser or the count
  is more than a few branches in size.

  This returns only the output from the parser in the map, not the lookup
  parser.

  ### Examples

  ```elixir

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
    iex> context = parse("a", branch(char(), symbol_map))
    iex> {context.error.message, context.result, context.rest}
    {"Tried to branch to `97` but it was not found in the symbol_map", nil, ""}

    iex> import ExSpirit.Tests.Parser
    iex> symbol_mapper = fn
    iex>   ?b -> &uint(&1, 2)
    iex>   ?d -> &uint(&1, 10)
    iex>   ?x -> &uint(&1, 16)
    iex>   _value -> nil # Always have a default case.  :-)
    iex> end
    iex> context = parse("b101010", branch(char(), symbol_mapper))
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}
    iex> context = parse("d213478", branch(char(), symbol_mapper))
    iex> {context.error, context.result, context.rest}
    {nil, 213478, ""}
    iex> context = parse("xe1DCf", branch(char(), symbol_mapper))
    iex> {context.error, context.result, context.rest}
    {nil, 925135, ""}
    iex> context = parse("a", branch(char(), symbol_mapper))
    iex> {context.error.message, context.result, context.rest}
    {"Tried to branch to `97` but it was not found in the symbol_map", nil, ""}

  ```

  ## tag

  The tagger combination parser will wrap the result of the passed in parser in
  a standard erlang 2-tuple, the first element is the tag that you pass in, the
  second is the result of the parser.

  ### Examples

  ```elixir

    # `tag` can tag the output from a parser
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("ff", tag(:integer, uint(16)))
    iex> {context.error, context.result, context.rest}
    {nil, {:integer, 255}, ""}

  ```

  ### Expect

  The expectation parser takes a parser but if it fails then it returns a hard
  error that will prevent further parsers, even in branch tests, from running.
  The purpose of this parser is to hard mention parsing errors at the correct
  parsing site, so that if you are parsing an `alt` of parsers, but you parse
  out a 'let' for example, followed by an identifier, if the identifier fails
  then you do not want to let the alt try the next one but instead fail out hard
  with an error message related to the proper place the parse failed instead of
  trying other parsers that you know will not succeed anyway.

  ### Examples

  ```elixir

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("do 10", lit("do ") |> expect(uint()))
    iex> {context.error, context.result, context.rest}
    {nil, 10, ""}

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("do nope", lit("do ") |> expect(uint()))
    iex> %ExSpirit.Parser.ExpectationFailureException{} = context.error
    iex> {context.error.message, context.result, context.rest}
    {"Parsing uint with radix of 10 had 0 digits but 1 minimum digits were required", nil, "nope"}

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("do nope", alt([ lit("do ") |> expect(uint()), lit("blah") ]))
    iex> %ExSpirit.Parser.ExpectationFailureException{} = context.error
    iex> {context.error.message, context.result, context.rest}
    {"Parsing uint with radix of 10 had 0 digits but 1 minimum digits were required", nil, "nope"}

    # Difference without the `expect`
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("do nope", alt([ lit("do ") |> uint(), lit("blah") ]))
    iex> %ExSpirit.Parser.ParseException{} = context.error
    iex> {context.error.message, context.result, context.rest}
    {"literal `blah` did not match the input", nil, "do nope"}

  ```

  ## repeat

  The repeat parser repeats over a parser for bounded number of times, returning
  the results as a list.  It does have a slight overhead compared to known
  execution times due to an anonmous function call, but that is necessary when
  performing a dynamic number of repetitions without mutable variables.

  The optional arguments are the minimum number of repeats required, default of
  0, and the maximum number of repeats, default of -1 (infinite).

  ### Examples

  ```elixir

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TTTX", repeat(char(?T)))
    iex> {context.error, context.result, context.rest}
    {nil, [?T, ?T, ?T], "X"}

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TTTX", repeat(char(?T), 1))
    iex> {context.error, context.result, context.rest}
    {nil, [?T, ?T, ?T], "X"}

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TTTX", repeat(char(?T), 1, 10))
    iex> {context.error, context.result, context.rest}
    {nil, [?T, ?T, ?T], "X"}

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TTTX", repeat(char(?T), 1, 2))
    iex> {context.error, context.result, context.rest}
    {nil, [?T, ?T], "TX"}

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TTTX", repeat(char(?T), 4))
    iex> {context.error.message, context.result, context.rest}
    {"Repeating over a parser failed due to not reaching the minimum amount of 4 with only a repeat count of 3", nil, "X"}

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TTT", repeat(char(?T), 4))
    iex> {context.error.message, context.result, context.rest}
    {"Repeating over a parser failed due to not reaching the minimum amount of 4 with only a repeat count of 3", nil, ""}

  ```

  ## repeatFn

  The repeat function parser allows you to pass in a parser function to repeat
  over, but is otherwise identical to `repeat`, especially as `repeat` delegates
  to `repeatFn`.

  ### Examples

  ```elixir

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TTTX", repeatFn(fn c -> c |> char(?T) end))
    iex> {context.error, context.result, context.rest}
    {nil, [?T, ?T, ?T], "X"}

    # See `repeat` for more.

  ```

  ## success

  The success parser always returns the passed in value, default of nil,
  successfully like a parsed value.

  ### Examples

  ```elixir

    iex> import ExSpirit.Tests.Parser
    iex> context = parse("", success(42))
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}

  ```
  """

  defmodule Context do
    defstruct(
      filename: "<unknown>",
      position: 0, # In bytes
      line: 1,
      column: 1,
      rest: "",
      skipper: nil,
      result: nil,
      error: nil,
      rulestack: []
      )
  end

  defmodule ParseException do
    defexception message: "<unknown>", context: %Context{}, extradata: nil

    def message(exc) do
      c = exc.context
      "#{c.filename}:#{c.line}:#{c.column}: Parse error: #{exc.message}\n\tRuleStack: [#{Enum.join(c.rulestack, ", ")}]\n\tInput: #{String.slice(c.rest, 0, 255)}"
    end
  end

  defmodule ExpectationFailureException do
    defexception message: "<unknown>", context: %Context{}, extradata: nil

    def message(exc) do
      c = exc.context
      "#{c.filename}:#{c.line}:#{c.column}: Expectation Failure: #{exc.message}\n\tRuleStack: [#{Enum.join(c.rulestack, ", ")}]\n\tInput: #{String.slice(c.rest, 0, 255)}"
    end

    def makeContextFailed(%{error: nil} = _good_context) do
      throw "Exceptation Failure Failed due to passing in a context that was actually good!"
    end

    def makeContextFailed(%{error: %{message: message, context: context, extradata: extradata}} = bad_context) do
      %{bad_context |
        result: nil,
        error: %__MODULE__{
          message: message,
          context: context,
          extradata: extradata,
        }
      }
    end

    def makeContextFailed(%{error: %{message: message} = exc} = bad_context) do
      %{bad_context |
        result: nil,
        error: %__MODULE__{
          message: message,
          context: bad_context,
          extradata: exc,
        }
      }
    end

    def makeContextFailed(%{error: error} = bad_context) do
      %{bad_context |
        result: nil,
        error: %__MODULE__{
          message: inspect(error),
          context: bad_context,
          extradata: error,
        }
      }
    end
  end



  defmacro defrule({name, _, [parser_ast]}) when is_atom(name), do: defrule_impl(name, parser_ast, [])

  defmacro defrule({name, _, [{:context, _, _} = context_ast]}, do: do_ast) when is_atom(name) do
    quote location: :keep do
      def unquote(name)(unquote(context_ast)) do
        context = unquote(context_ast)
        if !valid_context?(context) do
          context
        else
          rule_context = %{context | rulestack: [unquote(name) | context.rulestack]}
          return_context = unquote(do_ast)
          %{return_context | rulestack: context.rulestack}
        end
      end
    end
  end

  defmacro defrule({name, _, [parser_ast]}, opts) when is_atom(name), do: defrule_impl(name, parser_ast, opts)

  defp defrule_impl(name, parser_ast, opts) do
    orig_context_ast = quote do orig_context end
    context_ast = quote do context end
    quote location: :keep do
      def unquote(name)(unquote(orig_context_ast)) do
        unquote(context_ast) = %{unquote(orig_context_ast) | rulestack: [unquote(name) | unquote(orig_context_ast).rulestack]}
        unquote(context_ast) = unquote(context_ast) |> unquote(parser_ast)
        unquote(context_ast) = unquote(defrule_impl_map(orig_context_ast, context_ast, opts[:map]))
        unquote(context_ast) = unquote(defrule_impl_fun(context_ast, opts[:fun]))
        %{unquote(context_ast) | rulestack: unquote(orig_context_ast).rulestack}
      end
    end
  end

  defp defrule_impl_map(_orig_context_ast, context_ast, nil), do: context_ast
  defp defrule_impl_map(orig_context_ast, context_ast, map_ast) do
    quote location: :keep do
      case unquote(context_ast) do
        %{error: nil} = good_context ->
          result = good_context.result |> unquote(map_ast)
          if Exception.exception?(result) do
            %{unquote(orig_context_ast) | error: result}
          else
            %{good_context | result: result}
          end
        bad_context -> bad_context
      end
    end
  end

  defp defrule_impl_fun(context_ast, nil), do: context_ast
  defp defrule_impl_fun(context_ast, fun_ast) do
    quote location: :keep do
      case unquote(context_ast) do
        %{error: nil} = good_context ->
          good_context |> unquote(fun_ast)
        bad_context -> bad_context
      end
    end
  end


  defmacro __using__(opts) do
    text_use_ast = if(opts[:text], do: quote(do: use ExSpirit.Parser.Text), else: nil)
    quote location: :keep do
      import ExSpirit.Parser, only: [defrule: 1, defrule: 2]


      defmacro parse(rest, parser, opts \\ []) do
        filename = opts[:filename] || quote do "<unknown>" end
        skipper = case opts[:skipper] do
          nil -> nil
          fun -> quote do fn context -> context |> unquote(fun) end end
        end
        quote location: :keep do
          %ExSpirit.Parser.Context{
            filename: unquote(filename),
            skipper: unquote(skipper),
            rest: unquote(rest),
          } |> unquote(parser)
        end
      end


      def valid_context?(%{error: nil}), do: true
      def valid_context?(_), do: false


      defmacro run_skipper(context_ast) do
        quote location: :keep do
          case unquote(context_ast) do
            %{skipper: nil} = context -> context
            %{skipper: skipper} = context ->
              skipped_context = %{context | skipper: nil} |> skipper.()
              %{skipped_context |
                skipper: context.skipper,
                result: context.result,
                error: context.error
              }
          end
        end
      end


      defmacro seq(context, [first_seq | rest_seq] = sequences) do
        quote location: :keep do
          context = unquote(context)
          if !valid_context?(context) do
            context
          else
            context |> unquote(seq_expand(context, first_seq, rest_seq))
          end
        end
      end

      defp seq_expand(context_ast, this_ast, [next_ast | rest_ast]) do
        quote do
          unquote(this_ast) |> case do
            %{error: nil, result: nil} = good_context ->
              good_context |> unquote(seq_expand(context_ast, next_ast, rest_ast))
            %{error: nil, result: result} = good_context ->
              case good_context |> unquote(seq_expand(context_ast, next_ast, rest_ast)) do
                %{error: nil, result: nil} = return_context -> %{return_context | result: result}
                %{error: nil, result: results} = return_context when is_list(results) -> %{return_context | result: [result | results]}
                %{error: nil, result: results} = return_context -> %{return_context | result: [result, results]}
                bad_context -> bad_context
              end
            bad_context -> bad_context
          end
        end
      end
      defp seq_expand(_context_ast, this_ast, []) do
        this_ast
      end


      defmacro alt(context, [first_choice | rest_choices] = choices) do
        quote location: :keep do
          context = unquote(context)
          if !valid_context?(context) do
            context
          else
            context |> unquote(alt_expand(context, first_choice, rest_choices))
          end
        end
      end

      defp alt_expand(context_ast, this_ast, [next_ast | rest_ast]) do
        quote location: :keep do
          unquote(this_ast) |> case do
            %{error: nil} = good_context -> good_context
            %{error: %ExSpirit.Parser.ExpectationFailureException{}} = bad_context -> bad_context
            _bad_context -> unquote(context_ast) |> unquote(alt_expand(context_ast, next_ast, rest_ast))
          end
        end
      end
      defp alt_expand(_context_ast, this_ast, []) do
        this_ast
      end


      defmacro tag(context_ast, tag_ast, parser_ast) do
        quote location: :keep do
          case unquote(context_ast) |> unquote(parser_ast) do
            %{error: nil} = good_context -> %{good_context | result: {unquote(tag_ast), good_context.result}}
            bad_context -> bad_context
          end
        end
      end


      defmacro no_skip(context_ast, parser_ast) do
        quote location: :keep do
          context = unquote(context_ast)
          noskip_context = %{context | skipper: nil}
          return_context = noskip_context |> unquote(parser_ast)
          %{return_context | skipper: context.skipper}
        end
      end


      defmacro skipper(context_ast, parser_ast, skipper_ast) do
        quote location: :keep do
          context = unquote(context_ast)
          skipper = fn context -> context |> unquote(skipper_ast) end
          newskip_context = %{context | skipper: skipper}
          return_context = newskip_context |> unquote(parser_ast)
          %{return_context | skipper: context.skipper}
        end
      end


      defmacro ignore(context_ast, parser_ast) do
        quote location: :keep do
          context = unquote(context_ast)
          return_context = context |> unquote(parser_ast)
          %{return_context | result: context.result}
        end
      end


      defmacro branch(context_ast, parser_ast, symbol_map_ast) do
        quote location: :keep do
          context = unquote(context_ast)
          symbol_map = unquote(symbol_map_ast)
          case context |> unquote(parser_ast) do
            %{error: nil, result: lookup} = lookup_context ->
              if is_function(symbol_map, 1) do
                symbol_map.(lookup)
              else
                symbol_map[lookup]
              end
              |> case do
                nil ->
                  %{lookup_context |
                    result: nil,
                    error:  %ExSpirit.Parser.ParseException{message: "Tried to branch to `#{inspect lookup}` but it was not found in the symbol_map", context: context, extradata: symbol_map},
                  }
                found_parser_fun ->
                  lookup_context |> found_parser_fun.()
              end
            bad_context -> bad_context
          end
        end
      end


      defmacro expect(context_ast, parser_ast) do
        quote location: :keep do
          context = unquote(context_ast)
          case context |> unquote(parser_ast) do
            %{error: nil} = good_context -> good_context
            bad_context -> ExSpirit.Parser.ExpectationFailureException.makeContextFailed(bad_context)
          end
        end
      end


      defmacro repeat(context_ast, parser_ast, minimum \\ 0, maximum \\ -1) do
        quote location: :keep do
          unquote(context_ast) |> repeatFn(fn(c) -> c |> unquote(parser_ast) end, unquote(minimum), unquote(maximum))
        end
      end

      def repeatFn(context, parser, minimum \\ 0, maximum \\ -1) when is_function(parser, 1) do
        repeatFn(context, parser, minimum, maximum, [], 0)
      end
      defp repeatFn(context, _parser, _minimum, maximum, results, maximum) do
        %{context |
          result: :lists.reverse(results),
        }
      end
      defp repeatFn(context, parser, minimum, maximum, results, count) do
        case context |> parser.() do
          %{error: nil, result: result} = good_context -> repeatFn(good_context, parser, minimum, maximum, [result | results], count + 1)
          %{error: %ExSpirit.Parser.ExpectationFailureException{}} = bad_context -> bad_context
          bad_context ->
            if minimum < count do
              %{context |
                result: :lists.reverse(results),
              }
            else
              %{bad_context |
                result: nil,
                error:  %ExSpirit.Parser.ParseException{message: "Repeating over a parser failed due to not reaching the minimum amount of #{minimum} with only a repeat count of #{count}", context: context, extradata: count},
              }
            end
        end
      end


      def success(context, value \\ nil) do
        if !valid_context?(context) do
          context
        else
          %{context |
            result: value
          }
        end
      end


      unquote(text_use_ast)

    end
  end

end
