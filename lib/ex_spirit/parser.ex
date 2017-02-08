defmodule ExSpirit.Parser do
  @moduledoc """
  Documentation for ExSpirit.Parser.
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
    defexception message: "Unknown parse error", context: %Context{}, extradata: nil

    def message(exc) do
      c = exc.context
      "#{c.filename}:#{c.line}:#{c.column}: #{exc.message}\n\tRuleStack: [#{Enum.join(c.rulestack, ", ")}]\n\tInput: #{String.slice(c.rest, 0, 255)}"
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

      defmacro skip(context_ast, parser_ast, skipper_ast) do
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
          %{} = symbol_map = unquote(symbol_map_ast)
          case context |> unquote(parser_ast) do
            %{error: nil, result: lookup} = lookup_context ->
              case symbol_map[lookup] do
                nil ->
                  %{lookup_context |
                    error:  %ExSpirit.Parser.ParseException{message: "Tried to branch to `#{inspect lookup}` but it was not found in the symbol_map", context: context, extradata: symbol_map},
                  }
                found_parser_fun ->
                  lookup_context |> found_parser_fun.()
              end
            bad_context -> bad_context
          end
        end
      end


      unquote(text_use_ast)

    end
  end

end
