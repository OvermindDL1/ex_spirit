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
      skipper: &ExSpirit.Parser._no_skip/1,
      results: [],
      error: nil
      )
  end

  defmodule ParseException do
    defexception message: "Unknonwn parse error", context: %Context{}

    def message(exc) do
      c = exc.context
      "#{c.filename}:#{c.line}:#{c.column}: #{exc.message}\n\tInput: #{String.slice(c.rest, 0, 255)}"
    end
  end

  # @doc """
  #     iex> import ExSpirit.Parser
  #     iex> defrule tester, integer(8) |> expect(double), do: IO.inspect
  #     nil
  # """
  # defmacro defrule({name, _, nil}, rule_ast, opts \\ []) when is_atom(name) do
  #   _do_ast = opts[:do] || []
  #   IO.inspect {:RULE, name, rule_ast, opts}
  #   rule_ast
  # end
  #
  # defp process_rule_ast({func, meta, args}) when is_atom(func) do
  #
  # end

  def _no_skip(context), do: context

  defmacro __using__(_) do
    quote do
      def valid_context?(%{error: nil}), do: true
      def valid_context?(_), do: false

      def lit(context, literal, opt \\ []) do
        if !valid_context?(context) do
          context
        else
          if String.starts_with?(context.rest, literal) do
            lit_size = byte_size(literal)
            lit_bitsize = lit_size * 8
            <<_::size(lit_bitsize), rest::binary>> = context.rest
             %{context |
              rest: rest,
              position: context.position + lit_size,
              column: context.column + lit_size,
            }
          else
            %{context |
              error: %ExSpirit.Parser.ParseException{message: "literal `#{literal}`` did not match the input", context: context}
            }
          end
        end
      end

      defp uint_impl_10(context, valid_chars)

      def uint(context, radix \\ 10, minDigits \\ 1, maxDigits \\ -1) do
        if !valid_context?(context) do
          context
        else
          if radix <= 10 do
            uint_10(context, radix, minDigits, maxDigits, 0, 0)
          else
            uint_36(context, radix, minDigits, maxDigits, 0, 0)
          end
        end
      end

      def uint_10(context, _radix, minDigits, maxDigits, num, maxDigits)  do
        %{context | results: context.results++[num], position: context.position + maxDigits, column: context.column + maxDigits}
      end
      def uint_10(%{rest: <<c::utf8, rest::binary>>} = context, radix, minDigits, maxDigits, num, digits) when c>=?0 and c<=?0+radix-1 do
        uint_10(%{context|rest: rest}, radix, minDigits, maxDigits, (num*radix)+(c-?0), digits+1)
      end
      def uint_10(context, _radix, minDigits, _maxDigits, num, digits) when minDigits<=digits do
        %{context | results: context.results++[num], position: context.position + digits, column: context.column + digits}
      end

      def uint_36(context, _radix, minDigits, maxDigits, num, maxDigits)  do
        %{context | results: context.results++[num], position: context.position + maxDigits, column: context.column + maxDigits}
      end
      def uint_36(%{rest: <<c::utf8, rest::binary>>} = context, radix, minDigits, maxDigits, num, digits) when (c>=?0 and c<=?0+radix-1) or (c>=?a and c<=?a+radix-11) or (c>=?A and c<=?A+radix-11) do
        num = if c > ?9 do
          c = if c >= ?a do c else c + (?a - ?A) end
          (num*radix)+(c-?a+10)
        else
          (num*radix)+(c-?0+10)
        end
        uint_36(%{context|rest: rest}, radix, minDigits, maxDigits, num, digits+1)
      end
      def uint_36(context, _radix, minDigits, _maxDigits, num, digits) when minDigits<=digits do
        %{context | results: context.results++[num], position: context.position + digits, column: context.column + digits}
      end
    end
  end

end
