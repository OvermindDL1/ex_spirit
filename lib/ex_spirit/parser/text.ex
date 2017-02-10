defmodule ExSpirit.Parser.Text do
  @moduledoc """

  ExSpirit.Parser.Text is a set of parser specifically for parsing out utf-8
  text from a binary.


  # Parsers

  ## `lit`

  The literal parser matches out a specific string or character, entirely
  ignoring the result and returning `nil`.

  ### Examples

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

  ```

  ## `uint`

  The unsigned integer parser parses a plain number from the input with a few
  options.

  - The first argument is the radix, defaults to 10, everything from 2 to 36 is
    supported.
  - The second argument is the minimum character count, defaults 1, valid at 1+.
    If the characters able to be parsed as a number is less than this value then
    the parser fails.
  - The third argument is the maximum character count, defaults -1 (unlimited),
    valid values are -1, or 1+.  It stops parsing at this amount of characters
    and returns what it has parsed so far, if there are more number characters
    still to be parsed then they will be handled by the next parser.

  ### Examples

  ```elixir

    # `uint` parses out an unsigned integer, default radix of 10 with a min size of 1 and max of unlimited
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("42", uint())
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}

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

  ```




  ## Text (UTF-8 Binaries) parsing

  ```elixir

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

    # `char` can parse out anything 'but' a 'specific' single character too,
    # just negate it, don't mix positive and negative matchers in the same set
    # unless there is only one negative matcher and it is at the end of the list
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Nope", char(-?T))
    iex> {context.error, context.result, context.rest}
    {nil, ?N, "ope"}

    # `char` can parse out any 'specific' single character from a range too
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", char(?A..?Z))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, "est"}

    # `char` can parse out any but a 'specific' single character from a range
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("42", char(-?A..-?Z))
    iex> {context.error, context.result, context.rest}
    {nil, ?4, "2"}

    # `char` can parse out any 'specific' single character from a list of
    # characters or ranges too
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", char([?a..?z, ?T]))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, "est"}

    # `char` can parse out any but a 'specific' single character from a list of
    # characters or ranges too
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("42", char([?a..?z, -?T]))
    iex> {context.error, context.result, context.rest}
    {nil, ?4, "2"}

    # a mixed list is fine if the negated ones are at the end of it, only
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Test", char([?a..?z, ?T, -?A..-?Z]))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, "est"}

    # a mixed list is fine if the negated ones are at the end of it, only,
    # here is how a failure looks
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("Rest", char([?a..?z, ?T, -?A..-?Z]))
    iex> {String.starts_with?(context.error.message, "Tried parsing out any of the the characters of"), context.result, context.rest}
    {true, nil, "Rest"}

    # `chars` parser is like char but it parses all matching as a binary
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("TEST42", chars(?A..?Z))
    iex> {context.error, context.result, context.rest}
    {nil, "TEST", "42"}

    # `chars` parser is like char but it parses all matching as a binary, can
    # also take an initial single-char matcher
    iex> import ExSpirit.Tests.Parser
    iex> context = parse("_TEST42", chars(?_, ?A..?Z))
    iex> {context.error, context.result, context.rest}
    {nil, "_TEST", "42"}

    # `symbols` takes a ExSpirit.TreeMap, which is a structure designed for fast
    # lookups, though slow insertions, so please cache the data structure at
    # compile-time if possible.  This `symbols` parser will take the text input
    # stream and match it on the TreeMap to find the longest-matching string,
    # then it will take the return value, if a function then it will run it as
    # as parserFn, else it will return it as a value
    iex> import ExSpirit.Tests.Parser
    iex> alias ExSpirit.TreeMap, as: TreeMap
    iex> symbol_TreeMap = TreeMap.new() |> TreeMap.add_text("int", &uint(&1)) |> TreeMap.add_text("char", &char(&1))
    iex> context = parse("int42", symbols(symbol_TreeMap))
    iex> {context.error, context.result, context.rest}
    {nil, 42, ""}
    iex> context = parse("charT", symbols(symbol_TreeMap))
    iex> {context.error, context.result, context.rest}
    {nil, ?T, ""}
    iex> context = parse("in", symbols(symbol_TreeMap))
    iex> {String.starts_with?(context.error.message, "Tried matching out symbols and got to `i` but failed"), context.result, context.rest}
    {true, nil, "in"}
    iex> context = parse("", symbols(symbol_TreeMap))
    iex> {String.starts_with?(context.error.message, "Tried matching out symbols and got the end of the line but failed to find a value"), context.result, context.rest}
    {true, nil, ""}

    iex> import ExSpirit.Tests.Parser
    iex> alias ExSpirit.TreeMap, as: TreeMap
    iex> symbol_TreeMap = TreeMap.new() |> TreeMap.add_text("let", 1) |> TreeMap.add_text("letmap", 2) |> TreeMap.add_text("", 0)
    iex> context = parse("", symbols(symbol_TreeMap))
    iex> {context.error, context.result, context.rest}
    {nil, 0, ""}
    iex> context = parse("A", symbols(symbol_TreeMap))
    iex> {context.error, context.result, context.rest}
    {nil, 0, "A"}
    iex> context = parse("let", symbols(symbol_TreeMap))
    iex> {context.error, context.result, context.rest}
    {nil, 1, ""}
    iex> context = parse("letmap", symbols(symbol_TreeMap))
    iex> {context.error, context.result, context.rest}
    {nil, 2, ""}
    iex> context = parse("letma", symbols(symbol_TreeMap))
    iex> {context.error, context.result, context.rest}
    {nil, 1, "ma"}


  ```
  """

  defmacro __using__(_) do
    quote location: :keep do


      # Binary literal
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

      # Character literal
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


      # Unisgned Integer parsing
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

      defp uint_10(context, _radix, minDigits, maxDigits, num, maxDigits) do
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

      defp uint_36(context, _radix, minDigits, maxDigits, num, maxDigits) do
        %{context | result: num, position: context.position + maxDigits, column: context.column + maxDigits}
      end
      defp uint_36(%{rest: <<c::utf8, rest::binary>>} = context, radix, minDigits, maxDigits, num, digits) when (c>=?0 and c<=?0+radix-1) or (c>=?a and c<=?a+radix-11) or (c>=?A and c<=?A+radix-11) do
        num = if c > ?9 do
          c = if c >= ?a do c else c + (?a - ?A) end
          (num*radix)+(c-?a+10)
        else
          (num*radix)+(c-?0)
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



      # Parse out any character
      def char(context) do
        if !valid_context?(context) do
          context
        else
          case run_skipper(context) do
            %{rest: <<c::utf8, rest::binary>>} = good_context ->
              %{good_context |
                result: c,
                rest: rest,
                position: good_context.position + byte_size(<<c::utf8>>),
                column: if(c===?\n, do: 1, else: good_context.column+1),
                line: good_context.line + if(c===?\n, do: 1, else: 0),
              }
            bad_context ->
              %{bad_context |
                error: %ExSpirit.Parser.ParseException{message: "Tried parsing out a character but the end of input was reached", context: context},
              }
          end
        end
      end

      # Parse out a single character
      def char(context, c) when is_integer(c), do: char(context, [c])

      # Parse out a single character from a range of characters
      def char(context, _.._ = rangeMatcher), do: char(context, [rangeMatcher])

      # Parse out a single character from a list of acceptable characters and/or ranges
      def char(context, characterMatchers) when is_list(characterMatchers) do
        if !valid_context?(context) do
          context
        else
          case run_skipper(context) do
            %{rest: <<c::utf8, rest::binary>>} = matched_context ->
              if char_charrangelist_matches(c, characterMatchers) do
                %{matched_context |
                  result: c,
                  rest: rest,
                  position: matched_context.position + byte_size(<<c::utf8>>),
                  column: if(c===?\n, do: 1, else: matched_context.column+1),
                  line: matched_context.line + if(c===?\n, do: 1, else: 0),
                }
              else
                %{matched_context |
                  error: %ExSpirit.Parser.ParseException{message: "Tried parsing out any of the the characters of `#{inspect characterMatchers}` but failed due to the input character not matching", context: context},
                }
              end
            bad_context ->
              %{bad_context |
                error: %ExSpirit.Parser.ParseException{message: "Tried parsing out any of the the characters of `#{inspect characterMatchers}` but failed due to end of input", context: context},
              }
          end
        end
      end

      def char_charrangelist_matches(c, matchers, defaultValue \\ false)
      def char_charrangelist_matches(c, c, _defaultValue), do: true
      def char_charrangelist_matches(c, first..last, _defaultValue) when first<=last and c>=first and c<=last, do: true
      def char_charrangelist_matches(c, first..last, _defaultValue) when first>=last and c<=first and c>=last, do: true
      def char_charrangelist_matches(c, first..last, _defaultValue) when first<=last and -c>=first and -c<=last, do: true
      def char_charrangelist_matches(c, first..last, _defaultValue) when first>=last and -c<=first and -c>=last, do: true
      def char_charrangelist_matches(c, [], defaultValue), do: defaultValue
      def char_charrangelist_matches(c, [c | rest], _defaultValue), do: true
      def char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first<=last and c>=first and c<=last, do: true
      def char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first>=last and c<=first and c>=last, do: true
      def char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first<=last and -c>=first and -c<=last, do: false
      def char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first>=last and -c<=first and -c>=last, do: false
      def char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first<0 or last<0, do: char_charrangelist_matches(c, rest, true)
      def char_charrangelist_matches(c, [d | rest], defaultValue) when -c === d, do: false
      def char_charrangelist_matches(c, [d | rest], defaultValue) when d<0, do: char_charrangelist_matches(c, rest, true)
      def char_charrangelist_matches(c, [_ | rest], defaultValue), do: char_charrangelist_matches(c, rest, defaultValue)
      def char_charrangelist_matches(c, _d, defaultValue), do: defaultValue


      def chars_increment_while_matching("", position, column, line, _matchers), do: {position, column, line}
      def chars_increment_while_matching(matchers, <<c::utf8, rest::binary>>, position, column, line) do
        if char_charrangelist_matches(c, matchers) do
          if c == ?\n do
            chars_increment_while_matching(matchers, rest, position+byte_size(<<c::utf8>>), 1, line+1)
          else
            chars_increment_while_matching(matchers, rest, position+byte_size(<<c::utf8>>), column+1, line)
          end
        else
          {position, column, line}
        end
      end

      defmacro chars(context_ast, matchers_ast) do
        quote location: :keep do
          context = unquote(context_ast)
          matchers = unquote(matchers_ast)
          if !valid_context?(context) do
            context
          else
            context = run_skipper(context)
            {position, column, line} = chars_increment_while_matching(matchers, context.rest, 0, context.column, context.line)
            <<result::binary-size(position), result_rest::binary>> = context.rest
            %{context |
              result: result,
              rest: result_rest,
              position: context.position + position,
              column: column,
              line: line,
            }
          end
        end
      end

      defmacro chars(context_ast, firstMatcher_ast, matchers_ast) do
        quote location: :keep do
          context = unquote(context_ast)
          firstMatcher = unquote(firstMatcher_ast)
          matchers = unquote(matchers_ast)
          if !valid_context?(context) do
            context
          else
            case run_skipper(context) do
              %{rest: <<c::utf8, rest::binary>>} = first_matched_context ->
                if char_charrangelist_matches(c, firstMatcher) do
                  {position, column, line} = chars_increment_while_matching(matchers, rest, byte_size(<<c::utf8>>), if(c===?\n, do: 1, else: first_matched_context.column+1), first_matched_context.line + if(c===?\n, do: 1, else: 0))
                  <<result::binary-size(position), result_rest::binary>> = first_matched_context.rest
                  %{first_matched_context |
                    result: result,
                    rest: result_rest,
                    position: first_matched_context.position + position,
                    column: column,
                    line: line,
                  }
                else
                  %{first_matched_context |
                    error: %ExSpirit.Parser.ParseException{message: "Tried parsing out any of the the characters of `#{inspect firstMatcher}` but failed due to the input character not matching", context: context},
                  }
                end
              bad_context ->
                %{bad_context |
                  error: %ExSpirit.Parser.ParseException{message: "Tried parsing out any of the the characters of `#{inspect firstMatcher}` but failed due to end of input", context: context},
                }
            end
          end
        end
      end


      def symbols(context, %ExSpirit.TreeMap{root: root}) do
        if !valid_context?(context) do
          context
        else
          context = run_skipper(context)
          %{context | result: nil}
          |> symbols_(root)
        end
      end

      defp symbols_(%{rest: ""} = context, map) do
        case map[[]] do
          nil ->
            %{context |
              error: %ExSpirit.Parser.ParseException{message: "Tried matching out symbols and got the end of the line but failed to find a value in `#{inspect map}`", context: context},
            }
          parser when is_function(parser, 1) ->
            context |> parser.()
          value ->
            %{context |
              result: value,
            }
        end
      end
      defp symbols_(%{rest: <<c::utf8, rest::binary>>} = context, map) do
        case map[c] do
          nil ->
            case map[[]] do
              nil ->
                %{context |
                  error: %ExSpirit.Parser.ParseException{message: "Tried matching out symbols and got to `#{<<c::utf8>>}` but failed to find it in `#{inspect map}`", context: context},
                }
              parser when is_function(parser, 1) ->
                %{context |
                  position: context.position + byte_size(<<c::utf8>>),
                  column: if(c===?\n, do: 1, else: context.column+1),
                  line: context.line + if(c===?\n, do: 1, else: 0),
                } |> parser.()
              value ->
                %{context |
                  position: context.position + byte_size(<<c::utf8>>),
                  column: if(c===?\n, do: 1, else: context.column+1),
                  line: context.line + if(c===?\n, do: 1, else: 0),
                  result: value,
                }
            end
          submap ->
            %{context |
              rest: rest,
              position: context.position + byte_size(<<c::utf8>>),
              column: if(c===?\n, do: 1, else: context.column+1),
              line: context.line + if(c===?\n, do: 1, else: 0),
            } |> symbols_(submap)
            |> case do
              %{error: nil} = good_context -> good_context
              bad_context -> # Does this level have a value then?
                case map[[]] do
                  nil ->
                    %{context |
                      error: %ExSpirit.Parser.ParseException{message: "Tried matching out symbols and got to `#{<<c::utf8>>}` but failed to find a longest match in `#{inspect map}`", context: context},
                    }
                  parser when is_function(parser, 1) ->
                    %{context |
                      position: context.position + byte_size(<<c::utf8>>),
                      column: if(c===?\n, do: 1, else: context.column+1),
                      line: context.line + if(c===?\n, do: 1, else: 0),
                    } |> parser.()
                  value ->
                    %{context |
                      position: context.position + byte_size(<<c::utf8>>),
                      column: if(c===?\n, do: 1, else: context.column+1),
                      line: context.line + if(c===?\n, do: 1, else: 0),
                      result: value,
                    }
                end
            end
        end
      end


    end
  end

end
