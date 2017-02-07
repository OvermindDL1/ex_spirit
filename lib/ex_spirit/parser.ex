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
    defexception message: "Unknonwn parse error", context: %Context{}

    def message(exc) do
      c = exc.context
      "#{c.filename}:#{c.line}:#{c.column}: #{exc.message}\n\tRuleStack: #{to_string c.rulestack}\n\tInput: #{String.slice(c.rest, 0, 255)}"
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


  defmacro __using__(_) do
    quote location: :keep do
      require ExSpirit
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



      def lit(context, literal) when is_binary(literal) do
        if !valid_context?(context) do
          context
        else
          context = run_skipper(context)
          if String.starts_with?(context.rest, literal) do
            lit_size = byte_size(literal)
            lit_bitsize = lit_size * 8
            lit_chars = String.length(literal)
            <<_::size(lit_bitsize), rest::binary>> = context.rest
             %{context |
              rest: rest,
              position: context.position + lit_size,
              column: context.column + lit_chars,
              result: nil
            }
          else
            %{context |
              error: %ExSpirit.Parser.ParseException{message: "literal `#{literal}` did not match the input", context: context}
            }
          end
        end
      end

      def lit(context, literal) when is_integer(literal) do
        if !valid_context?(context) do
          context
        else
          context = run_skipper(context)
          <<first::utf8, rest::binary>> = context.rest
          lit_size = byte_size(<<first::utf8>>)
          if first === literal do
             %{context |
              rest: rest,
              position: context.position + lit_size,
              column: context.column + 1,
              result: nil
            }
          else
            %{context |
              error: %ExSpirit.Parser.ParseException{message: "literal `#{<<literal::utf8>>}` did not match the input", context: context}
            }
          end
        end
      end


      def uint(context, radix \\ 10, minDigits \\ 1, maxDigits \\ -1) do
        if !valid_context?(context) do
          context
        else
          context = run_skipper(context)
          if radix <= 10 do
            uint_10(context, radix, minDigits, maxDigits, 0, 0)
          else
            uint_36(context, radix, minDigits, maxDigits, 0, 0)
          end
        end
      end

      defp uint_10(context, _radix, minDigits, maxDigits, num, maxDigits)  do
        %{context | result: num, position: context.position + maxDigits, column: context.column + maxDigits}
      end
      defp uint_10(%{rest: <<c::utf8, rest::binary>>} = context, radix, minDigits, maxDigits, num, digits) when c>=?0 and c<=?0+radix-1 do
        uint_10(%{context|rest: rest}, radix, minDigits, maxDigits, (num*radix)+(c-?0), digits+1)
      end
      defp uint_10(context, _radix, minDigits, _maxDigits, num, digits) when minDigits<=digits do
        %{context | result: num, position: context.position + digits, column: context.column + digits}
      end
      defp uint_10(context, radix, minDigits, _maxDigits, num, digits) when minDigits>digits do
        %{context |
          error: %ExSpirit.Parser.ParseException{message: "Parsing uint with radix of #{radix} had #{digits} digits but #{minDigits} minimum digits were required", context: context}
        }
      end

      defp uint_36(context, _radix, minDigits, maxDigits, num, maxDigits)  do
        %{context | result: num, position: context.position + maxDigits, column: context.column + maxDigits}
      end
      defp uint_36(%{rest: <<c::utf8, rest::binary>>} = context, radix, minDigits, maxDigits, num, digits) when (c>=?0 and c<=?0+radix-1) or (c>=?a and c<=?a+radix-11) or (c>=?A and c<=?A+radix-11) do
        num = if c > ?9 do
          c = if c >= ?a do c else c + (?a - ?A) end
          (num*radix)+(c-?a+10)
        else
          (num*radix)+(c-?0+10)
        end
        uint_36(%{context|rest: rest}, radix, minDigits, maxDigits, num, digits+1)
      end
      defp uint_36(context, _radix, minDigits, _maxDigits, num, digits) when minDigits<=digits do
        %{context | result: num, position: context.position + digits, column: context.column + digits}
      end
      defp uint_36(context, radix, minDigits, _maxDigits, num, digits) when minDigits>digits do
        %{context |
          error: %ExSpirit.Parser.ParseException{message: "Parsing uint with radix of #{radix} had #{digits} digits but #{minDigits} minimum digits were required", context: context}
        }
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

    end
  end

end
