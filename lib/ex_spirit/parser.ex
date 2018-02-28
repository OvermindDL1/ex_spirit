defmodule ExSpirit.Parser do
  @moduledoc """

  `ExSpirit.Parser` is the parsing section of ExSpirit, designed to parse out some
  kind of stream of data (whether via a binary, a list, or perhaps an actual
  stream) into a data structure of your own design.


  ## Definitions

  - **Terminal Parser**:
    A terminal parser is one that does not operate over any other parser, it is
    'terminal' in its location.

  - **Combination Parser**:
    A combination parser is one that takes a parser as an input and does something
    with it, whether that is repeating it, surrounding it, or ignoring its output
    as a few examples.

  ## Usage

  Just add `use ExSpirit.Parser` to a module to make it into a parsing module.

  To add text parsing functions from the `ExSpirit.Parsing.Text` module then add
  `text: true` to the use call. For example:

      defmodule MyModule do
        use ExSpirit.Parser, text: true
      end

  Note that the `ExSpirit.Parser` module must be `use`ed, and not `import`ed!
  The functions and macros below are meant to be defined inside your own module
  throught code generation orchestrated by the `__using__` macro.

  Importing the module will not bring any useful functions or macros into your scope,
  only "virtual" functions and macros that are used for documentation only.
  """

  defmodule Context do
    @moduledoc """
    This structure carries the state of the parser.
    Contains the following keys:

    * `:filename` - the name of the file (or `"<unknown>"` if no file is specified)
    * `:position` - index of the current byte in the input.
    * `:line` - current line number (starting at `1`)
    * `:column` - current column number (starting at `1`)
    * `:rest` - what's left of the input at this point
    * `:skipper` - the parser to be used as skipper
    * `:result` - the result at this point
    * `:error` - the error at this point. If there is no error, it will be `nil`.
      you can match on this key to see if the previous parser has failed.
      If the following matches, the parser hasn't failed: `%{error: nil} = context`.
    * `:rulestack` - The stack of rules at the current position.
      Useful for debugging purposes
    * `:state` - <TODO>
    * `:userdata` - arbitrary data, defined by the user.
      This is useful for a number of things, like keeping track of indentation levels
      in indentation-sensitive languages, like [Python], [Haskell] or [Pug].

    [Python]: https://www.python.org/
    [Haskell]: https://www.haskell.org/
    [Pug]: https://pugjs.org/api/getting-started.html

    The `Context` is always available at any moment, and ExSpirit provides
    certain utilities to make it easy to access and update the context.

    In fact, ExSprit is a very general parser that can be viewed as a
    pipeline composed of a sequence of transformations, each of which
    takes up a context and return a new context:

        new_context = parse_rule(old_context)

    The new context might depend not only on the remainder of the input (`:rest`),
    but also on any of the values contained in the context.
    This is what makes it possible to use ExSpirit for context-sensitive languages.
    Many parsers hide the context from the user.
    While hiding the context *might* create a cleaner API, it makes it harder
    to have good error reporting and to store position information along the parse tree.
    It also makes it impossible to parse context-sensitive languages, such as
    XML, HTML, as well as most indentation-sensitive languages (as described above).
    ExSpirit, being a fully general parser, has no such limitations.

    When parsing context-free languages (like many programming languages),
    you don't need anything more powerful than a PEG parser, which doesn't require
    access to the context, except maybe for position tagging.

    In that case, you can use ExSpirit while ignoring the context.
    It will be transparently threaded through your rules, and you can focus
    only on the stream you're parsing, and ignore the other parameters.
    Just imagine that the parsers are taking up elements of a stream
    (e.g. characters from a string) and returning a result.

    ## State System

    The state system uses the following parsers to update the state:

    * `ExSpirit.Parser.put_state/3`
    * `ExSpirit.Parser.push_state/3`

    And the following parser tp get the state into another parser:

    * `ExSpirit.Parser.get_state_into/3`

    The state system can be very useful in context-sensitive languages, such as XML.
    An XML document is composed of matched pairs of opening and closing tags
    of the form: `<tag>...</tag>`. The opening tag must match the closing tag.

    This can't be done without access to the state, because otherwise the parser
    that parses the closing tag would be unaware of what the opening tag was.

    Let's look at the following simple XML parser example (full code [here][github_example])


        defmodule SimpleXML do
          use ExSpirit.Parser, text: true

          defrule text( chars(-?<) )
          defrule tag_name( chars([?a..?z, ?A..?Z, ?0..?9, ?_, ?-]) )

          defrule tag(
            # We put the result of the parser into the state...
            lit(?<) |> tag_name() |> put_state(:tagname, :result) |> lit(?>) |> expect(seq([
              # ... we get the restult into the state, and feed it
              # into the parser responsible for parsing the closing tag
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
        end
    """
    defstruct(
      filename: "<unknown>",
      position: 0, # In bytes
      line: 1,
      column: 1,
      rest: "",
      skipper: nil,
      result: nil,
      error: nil,
      rulestack: [],
      state: %{},
      userdata: nil
      )
  end

  defmodule ImportInsteadOfUseException do
    defexception name: nil, kind: nil

    def message(exc) do
      """
      You have tried to call the `#{exc.name}` #{exc.kind}.

      You seem to have imported the `ExSpirit.Parser` module.
      The `ExSpirit.Parser` module must be `use`d and not `import`ed.
      Please add to your module:

        `use ExSpirit.Parser` (or `use ExSpirit.Parser, text: true`) instead of
        `import ExSpirit.Parser`
      """
    end
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


  @doc """
  Defining a rule defines a parser as well as some associated information.

  Such associated information can be the its name for error reporting purposes,
  a mapping function so you can convert the output on the fly
  (fantastic for in-line AST generation for example!), among others.

  It is used like any other normal terminal rule.

  All of the following examples use this definition of rules in a module:

      defmodule ExSpirit.Parser do
        use ExSpirit.Tests.Parser, text: true

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


    ## Examples

      # You can use `defrule`s as any other terminal parser
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42 64", testrule())
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, [42, 64], ""}

      # `defrule`'s also set up a stack of calls down a context so you know
      # 'where' an error occured, so name the rules descriptively
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42 fail", testrule())
      iex> {contexts.error.context.rulestack, contexts.result, contexts.rest}
      {[:testrule], nil, "fail"}

      # `defrule`s can map the result to return a different one:
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42 64", testrule_pipe())
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, [2, 24], ""}

      # `defrule`s can also operate over the context itself to do anything
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42 64", testrule_fun())
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, {"altered", [42, 64]}, ""}

      # `defrule`s can also be a context function by only passing in `context`
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42 64", testrule_context())
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, "always success", "42 64"}

      # `defrule`s with a context can have other arguments too, but context
      # must always be first
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42 64", testrule_context_arg(:success))
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, :success, "42 64"}
  """
  defmacro defrule({name, _, [parser_ast]}) when is_atom(name), do: defrule_impl(name, parser_ast, [])

  defmacro defrule({name, _, [{:context, _, _} = context_ast | rest_args_ast]}, do: do_ast) when is_atom(name) do
    quote location: :keep do
      def unquote(name)(unquote(context_ast), unquote_splicing(rest_args_ast)) do
        context = unquote(context_ast)
        if !valid_context?(context) do
          context
        else
          rule_context = %{context | rulestack: [unquote(name) | context.rulestack]}
          return_context = unquote(do_ast)
          %{return_context |
            rulestack: context.rulestack,
            state: context.state,
          }
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
        unquote(context_ast) = unquote(defrule_impl_pipe(orig_context_ast, context_ast, opts[:pipe_result_into]))
        unquote(context_ast) = unquote(defrule_impl_fun(context_ast, opts[:fun]))
        %{unquote(context_ast) |
          rulestack: unquote(orig_context_ast).rulestack,
          state: unquote(orig_context_ast).state,
        }
      end
    end
  end

  defp defrule_impl_pipe(_orig_context_ast, context_ast, nil), do: context_ast
  defp defrule_impl_pipe(orig_context_ast, context_ast, map_ast) do
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

  @doc """
  The parse function is applied to the input and a parser call, such as in:

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42", uint())
      iex> {context.error, context.result, context.rest}
      {nil, 42, ""}
  """
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

  @doc """
  Tests whether the context is valid.
  """
  defmacro valid_context?(context_ast) do
    quote location: :keep do
      case unquote(context_ast) do
        %{error: nil} -> true
        _ -> false
      end
    end
  end

  @doc false
  defmacro valid_context_matcher(), do: quote(do: %{error: nil})

  @doc """
  Runs the skipper now

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("  a", skip(), skipper: chars(?\\s, 0))
      iex> {context.error, context.result, context.rest}
      {nil, nil, "a"}
  """
  defmacro skip(context_ast) do
    quote location: :keep do
      case unquote(context_ast) do
        %{skipper: nil, error: nil} = context -> context
        %{skipper: skipper, error: nil} = context ->
          case %{context | skipper: nil} |> skipper.() do
            %{error: nil} = skipped_context ->
              %{skipped_context |
                skipper: context.skipper,
                result: context.result,
              }
            bad_skipped_context ->
              %{bad_skipped_context |
                skipper: context.skipper,
              }
          end
        bad_context -> bad_context
      end
    end
  end

  @doc """
  The Sequence operator runs all of the parsers in the inline list (cannot be a
  variable) and returns their results as a list.

  Any `nil`'s returned are not added to the result list, and if the result list
  has only a single value returned then it returns that value straight away
  without being wrapped in a list.

  ## Examples
      # `seq` parses a sequence returning the return of all of them, removing nils,
      # as a list if more than one or the raw value if only one, if any fail then
      # all fail.
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42 64", seq([uint(), lit(" "), uint()]))
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, [42, 64], ""}

      # `seq` Here is sequence only returning a single value
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("42Test", seq([uint(), lit("Test")]))
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, 42, ""}
  """
  defmacro seq(context_ast, [first_seq | rest_seq]) do
    # context_binding = quote do context end
    quote location: :keep do
      case unquote(context_ast) do
        %{error: nil} = context ->
          context |> unquote(seq_expand(first_seq, rest_seq))
        bad_context -> bad_context
      end
    end
  end

  defp seq_expand(this_ast, [next_ast | rest_ast]) do
    quote do
      unquote(this_ast) |> case do
        %{error: nil, result: nil} = good_context ->
          good_context |> unquote(seq_expand(next_ast, rest_ast))
        %{error: nil, result: result} = good_context ->
          case good_context |> unquote(seq_expand(next_ast, rest_ast)) do
            %{error: nil, result: nil} = return_context -> %{return_context | result: result}
            %{error: nil, result: results} = return_context when is_list(results) -> %{return_context | result: [result | results]}
            %{error: nil, result: results} = return_context -> %{return_context | result: [result, results]}
            bad_context -> bad_context
          end
        bad_context -> bad_context
      end
    end
  end
  defp seq_expand(this_ast, []) do
    this_ast
  end

  @doc """
  The alternative parser runs the parsers in the inline list (cannot be a
  variable) and returns the result of the first one that succeeds, or the error
  of the last one.

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("FF", alt([uint(16), lit("Test")]))
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, 255, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> contexts = parse("Test", alt([uint(16), lit("Test")]))
      iex> {contexts.error, contexts.result, contexts.rest}
      {nil, nil, ""}

  """
  defmacro alt(context_ast, [first_choice | rest_choices]) do
    context_binding = Macro.var(:original_context, :alt)
    quote location: :keep do
      case unquote(context_ast) do
        %{error: nil} = unquote(context_binding) ->
          unquote(context_binding) |> unquote(alt_expand(context_binding, [], first_choice, rest_choices))
        bad_context -> bad_context
      end
    end
  end

  defp alt_expand(original_context_ast, err_contexts, this_ast, [next_ast | rest_ast]) do
    bad_context = Macro.var(String.to_atom("bad_context_#{:erlang.unique_integer([:positive])}"), :alt)
    quote location: :keep do
      unquote(this_ast) |> case do
        %{error: nil} = good_context -> good_context
        %{error: %ExSpirit.Parser.ExpectationFailureException{}} = bad_context -> bad_context
        unquote(bad_context) ->
          unquote(original_context_ast)
          |> unquote(alt_expand(original_context_ast, [bad_context | err_contexts], next_ast, rest_ast))
      end
    end
  end
  defp alt_expand(original_context_ast, err_contexts, this_ast, []) do
    bad_context = Macro.var(String.to_atom("bad_context_#{:erlang.unique_integer([:positive])}"), :alt)
    err_contexts = :lists.reverse(err_contexts, [bad_context])
    quote location: :keep do
      unquote(this_ast) |> case do
        %{error: nil} = good_context -> good_context
        %{error: %ExSpirit.Parser.ExpectationFailureException{}} = bad_context -> bad_context
        unquote(bad_context) ->
          %{unquote(original_context_ast)|
            result: nil,
            error: %ExSpirit.Parser.ParseException{
              message: "Alt failed all branches:\n\t\t#{Enum.join(unquote(
                Enum.map(err_contexts, &quote(do: unquote(&1).error.message))
              ), "\n\t\t")}",
              context: unquote(original_context_ast),
              extradata: unquote(err_contexts)
            },
          }
      end
    end
  end

  @doc """
  Wraps the result of the passed in parser in a standard erlang 2-tuple,
  where the first element the tag that you pass in
  and the second is the result of the parser.

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("ff", tag(:integer, uint(16)))
      iex> {context.error, context.result, context.rest}
      {nil, {:integer, 255}, ""}
  """
  defmacro tag(context_ast, tag_ast, parser_ast) do
    quote location: :keep do
      case unquote(context_ast) |> unquote(parser_ast) do
        %{error: nil} = good_context -> %{good_context | result: {unquote(tag_ast), good_context.result}}
        bad_context -> bad_context
      end
    end
  end

  @doc """
  The `no_skip` combination parser takes a parser and clears the skipper so they
  do no skipping.

  Good to parse non-skippable content within a large parser.

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse(" Test:42 ", lit("Test:") |> no_skip(uint()), skipper: lit(?\\s))
      iex> {context.error, context.result, context.rest}
      {nil, 42, " "}
  """
  defmacro no_skip(context_ast, parser_ast) do
    quote location: :keep do
      context_no_skip = unquote(context_ast)
      noskip_context = %{context_no_skip | skipper: nil}
      return_context = noskip_context |> unquote(parser_ast)
      %{return_context | skipper: context_no_skip.skipper}
    end
  end

  @doc """
  The `skipper` combination parser takes a parser and changes the skipper within
  it to the one you pass in for the duration of the parser that you pass in.

  ### Examples
      # You can change a skipper for a parser as well with `skipper`
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse(" Test:\t42 ", lit("Test:") |> skipper(uint(), lit(?\\t)), skipper: lit(?\\s))
      iex> {context.error, context.result, context.rest}
      {nil, 42, " "}
  """
  defmacro skipper(context_ast, parser_ast, skipper_ast) do
    quote location: :keep do
      context_skipper = unquote(context_ast)
      skipper = fn context -> context |> unquote(skipper_ast) end
      newskip_context = %{context_skipper | skipper: skipper}
      return_context = newskip_context |> unquote(parser_ast)
      %{return_context | skipper: context_skipper.skipper}
    end
  end

  @doc """
  Takes and runs a parser but ignores the result of the parser, instead returning `nil`.

  Can be given the option of `pass_result: true` to pass the previous result on.

  ## Examples
      # `ignore` will run the parser but return no result
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("Test", ignore(char([?a..?z, ?T])))
      iex> {context.error, context.result, context.rest}
      {nil, nil, "est"}

      # `ignore` will pass on the previous result if you want it to
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42Test", uint() |> ignore(char([?a..?z, ?T]), pass_result: true))
      iex> {context.error, context.result, context.rest}
      {nil, 42, "est"}
  """
  defmacro ignore(context_ast, parser_ast, opts \\ []) do
    quote location: :keep do
      case unquote(context_ast) do
        %{error: nil} = context ->
          return_context = context |> unquote(parser_ast)
          %{return_context | result: unquote(if(opts[:pass_result], do: quote(do: unquote(context_ast).result), else: quote(do: nil)))}
        bad_context -> bad_context
      end
    end
  end

  @doc """
  The `branch` combination parser is designed for efficient branching based on
  the result from another parser.

  It allows you to parse something, and using the result of that parser
  you can then either lookup the value in a map or call into a user function,
  either of which can return a parser function that will then be used to continue parsing.

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
      iex> import ExSpirit.Parser
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

      iex> import ExSpirit.Parser
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
  """
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

  @doc """
  Takes a parser but if it fails then it returns a hard error
  that will prevent further parsers, even in branch tests, from running.

  The purpose of this parser is to hard mention parsing errors at the correct
  parsing site, so that if you are parsing an `alt` of parsers, but you parse
  out a 'let' for example, followed by an identifier, if the identifier fails
  then you do not want to let the alt try the next one but instead fail out hard
  with an error message related to the proper place the parse failed instead of
  trying other parsers that you know will not succeed anyway.

  ### Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("do 10", lit("do ") |> expect(uint()))
      iex> {context.error, context.result, context.rest}
      {nil, 10, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("do nope", lit("do ") |> expect(uint()))
      iex> %ExSpirit.Parser.ExpectationFailureException{} = context.error
      iex> {context.error.message, context.result, context.rest}
      {"Parsing uint with radix of 10 had 0 digits but 1 minimum digits were required", nil, "nope"}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("do nope", alt([ lit("do ") |> expect(uint()), lit("blah") ]))
      iex> %ExSpirit.Parser.ExpectationFailureException{} = context.error
      iex> {context.error.message, context.result, context.rest}
      {"Parsing uint with radix of 10 had 0 digits but 1 minimum digits were required", nil, "nope"}

      # Difference without the `expect`
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("do nope", alt([ lit("do ") |> uint(), lit("blah") ]))
      iex> %ExSpirit.Parser.ParseException{} = context.error
      iex> {context.error.message =~ "Alt failed all branches:", context.result, context.rest}
      {true, nil, "do nope"}
  """
  defmacro expect(context_ast, parser_ast) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        case context |> unquote(parser_ast) do
          %{error: nil} = good_context -> good_context
          bad_context -> ExSpirit.Parser.ExpectationFailureException.makeContextFailed(bad_context)
        end
      end
    end
  end

  @doc """
  Repeats over a parser for bounded number of times, returning the results as a list.

  It does have a slight overhead compared to known execution times
  due to an anonmous function call, but that is necessary when
  performing a dynamic number of repetitions without mutable variables.

  The optional arguments are the minimum number of repeats required, default of
  `0`, and the maximum number of repeats, default of `-1` (infinite).

  ## Examples
      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("TTTX", repeat(char(?T)))
      iex> {context.error, context.result, context.rest}
      {nil, [?T, ?T, ?T], "X"}

      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("TTTX", repeat(char(?T), 1))
      iex> {context.error, context.result, context.rest}
      {nil, [?T, ?T, ?T], "X"}

      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("TTTX", repeat(char(?T), 1, 10))
      iex> {context.error, context.result, context.rest}
      {nil, [?T, ?T, ?T], "X"}

      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("TTTX", repeat(char(?T), 1, 2))
      iex> {context.error, context.result, context.rest}
      {nil, [?T, ?T], "TX"}

      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("TTTX", repeat(char(?T), 4))
      iex> {context.error.message, context.result, context.rest}
      {"Repeating over a parser failed due to not reaching the minimum amount of 4 with only a repeat count of 3", nil, "X"}

      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("TTT", repeat(char(?T), 4))
      iex> {context.error.message, context.result, context.rest}
      {"Repeating over a parser failed due to not reaching the minimum amount of 4 with only a repeat count of 3", nil, ""}

      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("", repeat(char(?T)))
      iex> {context.error, context.result, context.rest}
      {nil, [], ""}
  """
  defmacro repeat(context_ast, parser_ast, minimum \\ 0, maximum \\ -1) do
    quote location: :keep do
      unquote(context_ast) |> repeatFn(fn(c) -> c |> unquote(parser_ast) end, unquote(minimum), unquote(maximum))
    end
  end

  @doc """
  The repeat function parser allows you to pass in a parser function to repeat
  over, but is otherwise identical to `repeat`, especially as `repeat` delegates
  to `repeatFn`.

  See `ExSpirit.Parser.repeat/4` for more.

  ## Examples
      iex> import ExSpirit.Parser, only: :macros
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("TTTX", repeatFn(fn c -> c |> char(?T) end))
      iex> {context.error, context.result, context.rest}
      {nil, [?T, ?T, ?T], "X"}
  """
  # TODO:  Refactor
  # Until refactored, keep in sync with the implementation in `__using__`
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
        if minimum <= count do
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

  @doc """
  The success parser always returns the passed in value, default of nil,
  successfully like a parsed value.

  ## Examples
      iex> import ExSpirit.Parser
      iex> context = parse("", success(42))
      iex> {context.error, context.result, context.rest}
      {nil, 42, ""}
  """
  def success(context, value \\ nil) do
    if !valid_context?(context) do
      context
    else
      %{context |
        result: value
      }
    end
  end

  @doc """
  The fail parser always fails, documenting the user information passed in

  ## Examples

      iex> import ExSpirit.Parser
      iex> context = parse("", fail(42))
      iex> {context.error.extradata, context.result, context.rest}
      {42, nil, ""}
  """
  def fail(context, reason \\ nil) do
    if !valid_context?(context) do
      context
    else
      %{context |
        result: nil,
        error:  %ExSpirit.Parser.ParseException{message: "Fail parser called with user reason of: #{inspect reason}", context: context, extradata: reason},
      }
    end
  end

  @doc """
  Runs a function with the context.

  TODO: Expand this *a lot*.

  ### Examples
      iex> import ExSpirit.Parser
      iex> fun = fn c -> %{c|result: 42} end
      iex> context = parse("a", pipe_context_into(fun.()))
      iex> {context.error, context.result, context.rest}
      {nil, 42, "a"}
  """
  defmacro pipe_context_into(context_ast, mapper_ast) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        context |> unquote(mapper_ast)
      end
    end
  end

  @doc """
  Runs a function with the result

  ### Examples
      iex> import ExSpirit.Parser
      iex> fun = fn nil -> 42 end
      iex> context = parse("a", pipe_result_into(fun.()))
      iex> {context.error, context.result, context.rest}
      {nil, 42, "a"}
  """
  defmacro pipe_result_into(context_ast, mapper_ast) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        result = context.result |> unquote(mapper_ast)
        if Exception.exception?(result) do
          %{context |
            error: result,
            result: nil,
          }
        else
          %{context |
            result: result,
          }
        end
      end
    end
  end

  @doc """
  Runs a function and parser with the both the context before and after the
  function call.

  ### Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> fun = fn {pre, post} -> %{post|result: {pre, post}} end
      iex> context = parse("42", pipe_context_around(fun.(), uint()))
      iex> {pre, post} = context.result
      iex> {context.error, pre.column, post.column, context.rest}
      {nil, 1, 3, ""}
  """
  defmacro pipe_context_around(context_ast, mapper_ast, parser_ast) do
    quote location: :keep do
      context_map_context_around = unquote(context_ast)
      if !valid_context?(context_map_context_around) do
        context_map_context_around
      else
        case context_map_context_around |> unquote(parser_ast) do
          %{error: nil} = new_context ->
            {context_map_context_around, new_context} |> unquote(mapper_ast)
          bad_context -> bad_context
        end
      end
    end
  end

  @doc """
  Puts something into the state at the specified key

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42", uint() |> put_state(:test, :result))
      iex> {context.error, context.result, context.rest, context.state}
      {nil, 42, "", %{test: 42}}
  """
  defmacro put_state(context_ast, key, from)
  defmacro put_state(context_ast, key, :context) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        %{context |
          state: Map.put(context.state, unquote(key), context),
        }
      end
    end
  end
  defmacro put_state(context_ast, key, :result) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        %{context |
          state: Map.put(context.state, unquote(key), context.result),
        }
      end
    end
  end

  @doc """
  Puts something into the state at the specified key

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42", uint() |> push_state(:test, :result))
      iex> {context.error, context.result, context.rest, context.state}
      {nil, 42, "", %{test: [42]}}
  """
  defmacro push_state(context_ast, key, from)
  defmacro push_state(context_ast, key, :context) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        %{context |
          state: Map.update(context.state, unquote(key), [context], fn x -> [context | List.wrap(x)] end),
        }
      end
    end
  end
  defmacro push_state(context_ast, key, :result) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        %{context |
          state: Map.update(context.state, unquote(key), [context.result], fn x -> [context.result | List.wrap(x)] end),
        }
      end
    end
  end

  @doc """
  Get something(s) from the state and put it into the locations in the parser
  that are marked with &1-* bindings

  ### Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("A:A", char() |> put_state(:test, :result) |> lit(?:) |> get_state_into([:test], char(&1)))
      iex> {context.error, context.result, context.rest}
      {nil, ?A, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("A:B", char() |> put_state(:test, :result) |> lit(?:) |> get_state_into([:test], char(&1)))
      iex> {String.starts_with?(context.error.message, "Tried parsing out any of the the characters of"), context.result, context.rest}
      {true, nil, "B"}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("A:B", char() |> put_state(:test, :result) |> lit(?:) |> get_state_into(:test, :result))
      iex> {context.error, context.result, context.rest}
      {nil, ?A, "B"}
  """
  defmacro get_state_into(context_ast, key, :result) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        %{context |
          result: context.state[unquote(key)]
        }
      end
    end
  end

  defmacro get_state_into(context_ast, key, :context) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        case context.state[unquote(key)] do
          %ExSpirit.Parser.Context{} = old_context -> old_context
          _ ->
            %{context |
              result: nil,
              error:  %ExSpirit.Parser.ParseException{message: "Attempted to get a context out of the state at `#{inspect unquote(key)}` but there was no context there", context: context, extradata: key},
            }
        end
      end
    end
  end

  defmacro get_state_into(context_ast, keys, parser_ast) do
    keys = if !is_list(keys), do: [keys], else: keys
    context_binding = quote do context end
    parser_ast = Macro.postwalk(parser_ast, fn
      {:&, _, [0]} -> quote do unquote(context_binding).state end
      {:&, _, [pos]} = orig_ast ->
        case Enum.at(keys, pos-1) do
          nil -> orig_ast
          {key, default} -> quote do Map.get(unquote(context_binding).state, unquote(key), unquote(default)) end
          key -> quote do unquote(context_binding).state[unquote(key)] end
        end
      ast -> ast
    end)
    quote location: :keep do
      unquote(context_binding) = unquote(context_ast)
      if !valid_context?(unquote(context_binding)) do
        unquote(context_binding)
      else
        unquote(context_binding) |> unquote(parser_ast)
      end
    end
  end

  @doc """
  Looks ahead to confirm success, but does not update the context when
  successful.

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("AA", lit(?A) |> lookahead(lit(?A)) |> char())
      iex> {context.error, context.result, context.rest}
      {nil, ?A, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("AB", lit(?A) |> lookahead(lit(?A)) |> char())
      iex> {String.starts_with?(context.error.message, "Lookahead failed"), context.result, context.rest}
      {true, nil, "B"}
  """
  defmacro lookahead(context_ast, parser_ast) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        case context |> unquote(parser_ast) do
          %{error: nil} -> context
          bad_context ->
            %{context |
              result: nil,
              error:  %ExSpirit.Parser.ParseException{message: "Lookahead failed", context: context, extradata: bad_context},
            }
        end
      end
    end
  end

  @doc """
  Looks ahead to confirm failure, but does not update the context when
  failed.

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("AB", lit(?A) |> lookahead_not(lit(?A)) |> char())
      iex> {context.error, context.result, context.rest}
      {nil, ?B, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("AA", lit(?A) |> lookahead_not(lit(?A)) |> char())
      iex> {String.starts_with?(context.error.message, "Lookahead_not failed"), context.result, context.rest}
      {true, nil, "A"}
  """
  defmacro lookahead_not(context_ast, parser_ast) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        case context |> unquote(parser_ast) do
          %{error: nil} = bad_context ->
            %{context |
              result: nil,
              error:  %ExSpirit.Parser.ParseException{message: "Lookahead_not failed", context: context, extradata: bad_context},
            }
          _context -> context
        end
      end
    end
  end

  @doc """
  Returns the entire parsed text from the parser, regardless of the actual return value.

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("A256B", lexeme(char() |> uint()))
      iex> {context.error, context.result, context.rest}
      {nil, "A256", "B"}
  """
  defmacro lexeme(context_ast, parser_ast) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        case context |> unquote(parser_ast) do
          %{error: nil} = good_context ->
            bytes = good_context.position - context.position
            case context.rest do
              <<parsed::binary-size(bytes), newRest::binary>> ->
                %{good_context |
                  result: parsed,
                  rest: newRest,
                }
              _ ->
                %{context |
                  result: nil,
                  error:  %ExSpirit.Parser.ParseException{message: "Lexeme failed, should be impossible, length needed is #{bytes} but available is only #{byte_size(context.rest)}", context: context, extradata: good_context},
                }
            end
          bad_context -> bad_context
        end
      end
    end
  end

  @doc """
  Success if there at the "End Of Input", else fails.

  If the argument is statically `pass_result: true`
  then it passes on the prior return value.

  If the argument is statically `result: whatever` with `whatever` being what
  you want to return, then it will set the result to that value on success.
  `pass_result` must be set to false to use `result: value` or it is skipped.

  ## Examples
      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42", uint() |> eoi())
      iex> {context.error, context.result, context.rest}
      {nil, nil, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42", uint() |> eoi(pass_result: true))
      iex> {context.error, context.result, context.rest}
      {nil, 42, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42", uint() |> eoi(result: :success))
      iex> {context.error, context.result, context.rest}
      {nil, :success, ""}

      iex> import ExSpirit.Parser
      iex> import ExSpirit.Tests.Parser
      iex> context = parse("42a", uint() |> eoi())
      iex> {is_map(context.error), context.result, context.rest}
      {true, nil, "a"}
  """
  defmacro eoi(context_ast, opts \\ []) do
    quote location: :keep do
      context = unquote(context_ast)
      if !valid_context?(context) do
        context
      else
        case context do
          %{rest: ""} ->
            unquote(if(opts[:pass_result], do: quote(do: context), else: quote(do: %{context | result: unquote(opts[:result])})))
          _ ->
            %{context |
              result: nil,
              error:  %ExSpirit.Parser.ParseException{message: "eoi failed, not at End Of Input", context: context},
            }
        end
      end
    end
  end

  @doc """
  When this module is `use`d then it will import what is required, define some inline functions for speed, and load in
  other parsers.

  ## Paramters:

  * text: true|false -> Will use the Text Parsing module as well
  """
  defmacro __using__(opts) do
    text_use_ast = if(opts[:text], do: quote(do: use ExSpirit.Parser.Text), else: nil)
    quote location: :keep do
      import ExSpirit.Parser, only: :macros

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
            if minimum <= count do
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

      unquote(text_use_ast)

    end
  end

end
