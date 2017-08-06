defmodule ExSpirit.Parserx do
  @moduledoc """

  ExSpirit.Parsers is the parsing section of ExSpirit, designed to parse out some
  kind of stream of data (whether via a binary, a list, or perhaps an actual
  stream) into a data structure of your own design.

  This parser differs from ExSpirit.Parser in that it uses an experimental Expression Template support.
  """


  defmacro __using__([]) do
    quote do
      import ExSpirit.Parserx
      # Module.register_attribute(__MODULE__, :spirit_rules, accumulate: true)
      # @before_compile ExSpirit.Parserx

      # In-Module Helpers

      defp char_charrangelist_matches(c, matchers, defaultValue \\ false)
      defp char_charrangelist_matches(c, c, _defaultValue), do: true
      defp char_charrangelist_matches(c, d, _defaultValue) when is_integer(d), do: d<0 and -c !== d
      defp char_charrangelist_matches(c, first..last, _defaultValue) when first<=last and c>=first and c<=last, do: true
      defp char_charrangelist_matches(c, first..last, _defaultValue) when first>=last and c<=first and c>=last, do: true
      defp char_charrangelist_matches(c, first..last, _defaultValue) when first<=last and -c>=first and -c<=last, do: false
      defp char_charrangelist_matches(c, first..last, _defaultValue) when first>=last and -c<=first and -c>=last, do: false
      defp char_charrangelist_matches(c, [], defaultValue), do: defaultValue
      defp char_charrangelist_matches(c, [c | rest], _defaultValue), do: true
      defp char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first<=last and c>=first and c<=last, do: true
      defp char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first>=last and c<=first and c>=last, do: true
      defp char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first<=last and -c>=first and -c<=last, do: false
      defp char_charrangelist_matches(c, [first..last | rest], _defaultValue) when first>=last and -c<=first and -c>=last, do: false
      defp char_charrangelist_matches(c, [first..last | rest], _defaultValue), do: char_charrangelist_matches(c, rest, first<0)
      defp char_charrangelist_matches(c, [d | rest], defaultValue) when -c===d, do: false
      defp char_charrangelist_matches(c, [d | rest], _defaultValue), do: char_charrangelist_matches(c, rest, d<0)
      defp char_charrangelist_matches(c, _d, defaultValue), do: defaultValue

      defp chars_increment_while_matching(_matchers, _maximumChars, "", position, column, line, chars), do: {position, column, line, chars}
      defp chars_increment_while_matching(_matchers, maximumChars, _rest, position, column, line, maximumChars), do: {position, column, line, maximumChars}
      defp chars_increment_while_matching(matchers, maximumChars, <<c::utf8, rest::binary>>, position, column, line, chars) do
        if char_charrangelist_matches(c, matchers) do
          if c == ?\n do
            chars_increment_while_matching(matchers, maximumChars, rest, position+byte_size(<<c::utf8>>), 1, line+1, chars+1)
          else
            chars_increment_while_matching(matchers, maximumChars, rest, position+byte_size(<<c::utf8>>), column+1, line, chars+1)
          end
        else
          {position, column, line, chars}
        end
      end

      defp unquote(:'$repeat$')(_sep, _fun, _minimum, maximum, context, count) when count >= maximum, do: %{context | result: []}
      defp unquote(:'$repeat$')(sep, fun, minimum, maximum, context, count) do
        context = %{context | result: []}
        case fun.(context) do
          %{error: nil, result: result} = new_context ->
            case unquote(:'$repeat$')(nil, sep || fun, minimum, maximum, new_context, count+1) do
              %{error: nil, result: results} = last_context when is_list(results) -> %{last_context | result: [result | results]}
              %{error: nil, result: results} = last_context -> throw :whaaa
              error_context -> error_context
            end
          _ when minimum > count ->
            %{context |
              result: nil,
              error:  %ExSpirit.Parser.ParseException{message: "Repeating over a parser failed due to not reaching the minimum amount of #{minimum} with only a repeat count of #{count}", context: context, extradata: count},
            }
          _ -> context
        end
      end
    end
  end

  # defmacro __before_compile__(_env) do
  #   quote do
  #     def __spirit_rules__(), do: @spirit_rules
  #   end
  # end


  ## Primary interface:  `parse`

  defmacro parse(rest, parser, opts \\ []) do
    filename = opts[:filename] || quote do "<unknown>" end
    skipper = case opts[:skipper] do
      nil -> nil
      skipper ->
        env = defrule_env_new(__CALLER__.module)
        {_env, skipper} = parser_to_ast(env, skipper)
        quote do fn context -> context |> unquote(skipper) end end
      # fun -> quote do fn context -> context |> unquote(fun) end end
    end

    env = defrule_env_new(__CALLER__.module)
    env = %{env | skipable: skipper !== nil}

    # {_env, body} = defrule_gen_body(env, parser)
    {_env, parser} = parser_to_ast(env, parser)

    # parser |> Macro.to_string |> IO.puts()

    quote location: :keep do
      case unquote(rest) do
        %ExSpirit.Parser.Context{}=context -> context |> unquote(parser)
        input ->
          %ExSpirit.Parser.Context{
            filename: unquote(filename),
            skipper: unquote(skipper),
            rest: unquote(rest),
          } |> unquote(parser)
      end
    end
  end


  ## Primary interface:  `defrule`

  # defmacro defrule(head, bodies) do
  #   head = defrule_fixup_head(head)
  #   name = defrule_head_name(head)
  #
  #   do_body = bodies[:do]       || throw "`defrule` must have a `do` body specified"
  #   after_body = bodies[:after] || nil
  #   else_body = bodies[:else]   || nil # quote do context -> raise %ParseException{message: unquote("Rule `#{name}` "), context: context} end
  #
  #   # quote do
  #   #   @spirit_rules {unquote(name), unquote(Macro.escape do_body), unquote(Macro.escape after_body), unquote(Macro.escape else_body)}
  #   # end
  #   quote do
  #     def unquote(head) do
  #       {name, unquote(do_body), unquote(after_body), unquote(else_body)}
  #     end
  #   end
  # end


  ## Primary interface:  `defgrammar`

  defmacro defgrammar(head, bodies) do
    info = acquire_rule_information(head, bodies)
    context = Macro.var(:context, __MODULE__)
    quote do
      def unquote(info.name)(unquote(context), unquote_splicing(info.args)) do
        ExSpirit.Parserx.parse(unquote(context), unquote(info.do))
      end
    end
  end

  ## Rule body grabber

  defp acquire_rule_information(head, bodies) do
    # head = defrule_fixup_head(head)
    # name = defrule_head_name(head)
    {name, _head_meta, args} = defrule_fixup_head(head)

    do_body = bodies[:do]       || nil # throw "`defrule` must have a `do` body specified"
    after_body = bodies[:after] || nil
    else_body = bodies[:else]   || nil # quote do context -> raise %ParseException{message: unquote("Rule `#{name}` "), context: context} end

    %{
      name: name,
      args: args,
      do: do_body,
      after: after_body,
      else: else_body,
    }
  end

  ### Parser AST Generator

  # defp parser_to_ast(parser) do
  #   env = defrule_env_new()
  #   {_env, ast} = parser_to_ast(env, parser)
  #   ast
  # end

  defp parser_to_ast(env, ast)
  defp parser_to_ast(env, {name, meta, scope}) when is_atom(name) and is_atom(scope) do
    parser_to_ast(env, {name, meta, []}) # Just delegating to the function call for now
  end
  defp parser_to_ast(%{commands: commands} = env, {name, _meta, args}) when is_atom(name) and is_list(args) do
    arity = length(args)
    case commands[{name, arity}] || commands[name] do
      nil -> throw {:INVALID_RULE_COMMAND, name, args, Map.keys(commands)}
      func when is_function(func, 1) -> func.(args)
      func when is_function(func, 2) -> func.(env, args)
      func when is_function(func, 3) -> func.(env, name, args)
      unknown -> throw {:INVALID_RULE_COMMAND_TYPE, name, args, unknown}
    end
  end
  defp parser_to_ast(_env, ast), do: throw {:INVALID_RULE, ast}















  # defmacro defrules(head, bodies) do
  #
  #   head = defrule_fixup_head(head)
  #   name = defrule_head_name(head)
  #
  #   do_body = bodies[:do]       || throw "`defrule` must have a `do` body specified"
  #   after_body = bodies[:after] || nil
  #   else_body = bodies[:else]   || nil # quote do context -> raise %ParseException{message: unquote("Rule `#{name}` "), context: context} end
  #
  #   {_env, do_body} = defrule_gen_body(do_body)
  #
  #   IO.puts("Do:")
  #   Macro.to_string(do_body) |> IO.puts
  #   IO.puts("After:")
  #   Macro.to_string(after_body) |> IO.puts
  #   IO.puts("Else:")
  #   Macro.to_string(else_body) |> IO.puts
  #
  #   quote do
  #     @spirit_rules {unquote(name), unquote(Macro.escape do_body), unquote(Macro.escape after_body), unquote(Macro.escape else_body)}
  #   end
  #
  #   # throw {head, name, bodies, do_body, after_body, else_body, env}
  # end

  defp defrule_fixup_head(head)
  defp defrule_fixup_head({name, meta, scope}) when is_atom(name) and is_atom(scope) do
    defrule_fixup_head({name, meta, []})
  end
  defp defrule_fixup_head({name, meta, args}) when is_atom(name) and is_list(args) do
    args = Enum.map(args, &defrule_fixup_head_arg/1)
    {name, meta, args}
  end
  defp defrule_fixup_head(head), do: throw {:INVALID_RULE_HEAD, head}

  defp defrule_fixup_head_arg(arg)
  defp defrule_fixup_head_arg({name, meta, scope}) when is_atom(name) and is_atom(scope) do
    {name, meta, scope}
  end
  defp defrule_fixup_head_arg(arg), do: throw {:INVALID_RULE_HEAD_ARG, arg}

  # defp defrule_head_name(head)
  # defp defrule_head_name({name, _meta, _args}), do: name
  #
  # defp defrule_gen_body(do_body) do
  #   env = defrule_env_new()
  #   defrule_gen_body(env, do_body)
  # end
  # defp defrule_gen_body(env, do_body)
  # defp defrule_gen_body(env, {name, meta, scope}) when is_atom(name) and is_atom(scope) do
  #   case defrule_env_get_binding(env, name) do
  #     nil -> defrule_perform_command(env, name, [])
  #     x -> throw x
  #   end
  # end
  # defp defrule_gen_body(env, {name, meta, args}) when is_atom(name) and is_list(args) do
  #   defrule_perform_command(env, name, args)
  # end
  # defp defrule_gen_body(_env, do_body), do: throw {:INVALID_RULE_BODY_EXPRESSION, do_body}
  #
  # defp defrule_perform_command(env, name, args)
  # defp defrule_perform_command(%{commands: commands} = env, name, args) do
  #   arity = length(args)
  #   case commands[{name, arity}] || commands[name] do
  #     nil -> throw {:INVALID_RULE_COMMAND, name, args, Map.keys(commands)}
  #     func when is_function(func, 1) -> func.(args)
  #     func when is_function(func, 2) -> func.(env, args)
  #     func when is_function(func, 3) -> func.(env, name, args)
  #     result -> throw {:INVALID_RULE_COMMAND_TYPE, name, args, result, Map.keys(commands)}
  #   end
  # end

  # defp defrule_context_if(env, bodies)
  # defp defrule_context_if(%{failable: false} = env, bodies) do
  #   context = defrule_env_get_context(env)
  #   case bodies[:do] do
  #     nil ->  quote do case do unquote(context) -> unquote(context) end
  #     body -> quote do case do unquote(context) -> unquote(context) |> unquote(body) end
  #   end
  # end
  # defp defrule_context_if(env, bodies) do
  #   # env = defrule_env_incr_context(env)
  #   context = defrule_env_get_context(env)
  #   do_body = bodies[:do] || quote do case do unquote(context) -> unquote(context) end end
  #   else_body = bodies[:else] || quote do case do unquote(context) -> unquote(context) end end
  #   ast =
  #     quote do
  #       case do
  #         %{error: nil} = unquote(context) -> unquote(context) |> unquote(do_body)
  #         %{} = unquote(context) -> unquote(context) |> unquote(else_body)
  #       end
  #     end
  #   {env, ast}
  # end

  # defmacrop defrule_context_if_skipper(%{failable: true, skipable: true} = env, bodies) do
  #   quote bind_quoted: [env: env, bodies: bodies] do
  #     context = defrule_env_get_context(env)
  #     do_body = bodies[:do] || quote do case do unquote(context) -> unquote(context) end end
  #     else_body = bodies[:else] || quote do case do unquote(context) -> unquote(context) end end
  #     ast =
  #       quote do
  #         case do
  #           %{error: nil, skipper: skipper} = unquote(context) ->
  #             case skipper do
  #               nil -> unquote(context)
  #               skipper -> unquote(context) |> skipper.()
  #             end |> unquote(do_body)
  #           unquote(context) -> unquote(context) |> unquote(else_body)
  #         end
  #       end
  #     {env, ast}
  #   end
  # end
  # defmacrop defrule_context_if_skipper(%{failable: true, skipable: false} = env, bodies) do
  #   quote bind_quoted: [env: env, bodies: bodies] do
  #     context = defrule_env_get_context(env)
  #     do_body = bodies[:do] || quote do case do unquote(context) -> unquote(context) end end
  #     else_body = bodies[:else] || quote do case do unquote(context) -> unquote(context) end end
  #     ast =
  #       quote do
  #         case do
  #           %{error: nil, skipper: skipper} = unquote(context) ->
  #             case skipper do
  #               nil -> unquote(context)
  #               skipper -> unquote(context) |> skipper.()
  #             end |> unquote(do_body)
  #           unquote(context) -> unquote(context) |> unquote(else_body)
  #         end
  #       end
  #     {env, ast}
  #   end
  # end
  # defmacrop defrule_context_if_skipper(%{failable: false, skipable: true} = env, bodies) do
  #   quote bind_quoted: [env: env, bodies: bodies] do
  #     context = defrule_env_get_context(env)
  #     do_body = bodies[:do] || quote do case do unquote(context) -> unquote(context) end end
  #     ast =
  #       quote do
  #         case do
  #           %{skipper: skipper} = unquote(context) ->
  #             case skipper do
  #               nil -> unquote(context)
  #               skipper ->
  #                 %{unquote(context) |
  #
  #                 }
  #             end
  #         end |> unquote(do_body)
  #       end
  #     {env, ast}
  #   end
  # end


  defp defrule_env_new(name), do: %{
    failable: true,
    skipable: true,
    context_level: 0,
    bindings: %{},
    commands: default_commands(),
    defps: [],
    name: name,
    counter: 0,
    scope: __MODULE__,
  }

  defp defrule_env_get_context(env, level_delta \\ 0)
  defp defrule_env_get_context(%{scope: scope, context_level: level}, delta) do
    Macro.var(String.to_atom("context" <> Integer.to_string(level + delta)), scope)
  end

  # defp defrule_env_incr_context(%{context_level: context_level} = env, delta \\ 1) do
  #   %{env | context_level: context_level + delta}
  # end
  #
  # defp defrule_env_get_binding(env, name) do
  #   env.bindings[name]
  # end






  defmacrop context_if(env, bodies) do
    quote bind_quoted: [env: env, bodies: bodies] do
      context = defrule_env_get_context(env)
      do_body = bodies[:do] || quote do case do unquote(context) -> unquote(context) end end
      else_body = bodies[:else] || quote do case do unquote(context) -> unquote(context) end end
      ast =
        case env do
          %{failable: true} ->
            quote do
              case do
                %{error: nil} = unquote(context) -> unquote(context) |> unquote(do_body)
                unquote(context) -> unquote(context) |> unquote(else_body)
              end
            end
          _ ->
            quote do
              unquote(do_body)
            end
        end
      {env, ast}
    end
  end

  defmacrop skip_do(env) do
    quote bind_quoted: [env: env] do
      context = defrule_env_get_context(env)
      context1 = defrule_env_get_context(env, 1)
      ast =
        case env do
          %{skipable: true} ->
            quote do
              case do
                %{skipper: skipper} = unquote(context) ->
                  case skipper do
                    nil -> unquote(context)
                    skipper ->
                      %{unquote(context) |
                        skipper: nil,
                      } |> skipper.()
                      |> case do
                        %{error: nil} = unquote(context1) ->
                          %{unquote(context1) |
                            skipper: unquote(context).skipper,
                            result: unquote(context).result,
                          }
                        _bad_skipper -> unquote(context)
                      end
                  end
              end
            end
          _ ->
            quote do
              case do
                unquote(context) -> unquote(context)
              end
            end
        end
      {env, ast}
    end
  end

  defmacrop context_skip_if(env, bodies) do
    quote bind_quoted: [env: env, bodies: bodies] do
      context = defrule_env_get_context(env)
      {env, skipper} = skip_do(env)
      do_body =
        case bodies[:do] do
          nil -> skipper
          body -> quote do unquote(skipper) |> unquote(body) end
        end
      context_if(env, [do: do_body]++bodies)
    end
  end



  def default_commands(), do: %{
    {:char, 0} => &cmd_char_0/2,
    :char => &cmd_char_n/2,
    :chars => &cmd_chars_n/2,
    :lit => &cmd_lit_n/2,
    {:|>, 2} => &cmd_seq_2/2,
    :seq => &cmd_seq_n/2,
    {:||, 2} => &cmd_alt_2/2,
    :alt => &cmd_alt_n/2,
    {:skip, 0} => &cmd_skip_0/2,
    {:skip, 2} => &cmd_skip_2/2,
    {:no_skip, 1} => &cmd_no_skip_1/2,
    {:lexeme, 1} => &cmd_lexeme_1/2,
    {:ignore, 1} => &cmd_ignore_1/2,
    {:repeat, 1} => &cmd_repeat_1_4/2,
    {:repeat, 2} => &cmd_repeat_1_4/2,
    {:repeat, 3} => &cmd_repeat_1_4/2,
    {:repeat, 4} => &cmd_repeat_1_4/2,
  }



  defp cmd_char_0(env, []) do
    context_skip_if(env) do
      quote do
        case do
          %{rest: <<c::utf8, rest::binary>>} = context ->
            %{context |
              result: c,
              rest: rest,
              position: context.position + byte_size(<<c::utf8>>),
              column: if(c===?\n, do: 1, else: context.column+1),
              line: context.line + if(c===?\n, do: 1, else: 0),
            }
          bad_context ->
            %{bad_context |
              error: %ExSpirit.Parser.ParseException{message: "Tried parsing out a character but the end of input was reached", context: bad_context},
            }
        end
      end
    end
  end

  defp cmd_char_n(env, matchers) do
    c_ast = Macro.var(:c, __MODULE__)
    context_skip_if(env) do
      quote do
        case do
          %{rest: <<unquote(c_ast)::utf8, rest::binary>>} = matched_context ->
            if unquote(cmd_char_n_matchers(c_ast, matchers)) do
              %{matched_context |
                result: c,
                rest: rest,
                position: matched_context.position + byte_size(<<c::utf8>>),
                column: if(c===?\n, do: 1, else: matched_context.column+1),
                line: matched_context.line + if(c===?\n, do: 1, else: 0),
              }
            else
              %{matched_context |
                error: %ExSpirit.Parser.ParseException{message: "Tried parsing out any of the the characters of `#{inspect unquote(matchers)}` but failed due to the input character not matching", context: matched_context},
              }
            end
          bad_context ->
            %{bad_context |
              error: %ExSpirit.Parser.ParseException{message: "Tried parsing out any of the the characters of `#{inspect unquote(matchers)}` but failed due to end of input", context: bad_context},
            }
        end
      end
    end
  end

  defp cmd_char_n_matcher(c_ast, matcher)
  defp cmd_char_n_matcher(c_ast, matcher) when is_integer(matcher) and matcher>=0 do
    {true, quote do unquote(c_ast) === unquote(matcher) end}
  end
  defp cmd_char_n_matcher(c_ast, matcher) when is_integer(matcher) and matcher<0 do
    {false, quote do not(unquote(c_ast) === unquote(matcher)) end}
  end
  defp cmd_char_n_matcher(c_ast, {:-, _, [matcher]}) when is_integer(matcher) and matcher>0 do
    {false, quote do not(unquote(c_ast) === unquote(matcher)) end}
  end
  defp cmd_char_n_matcher(c_ast, {:.., _meta, [l, r]}) when is_integer(l) and is_integer(r) and l>=0 and r>=0 do
    {true, quote do unquote(c_ast) >= unquote(l) and unquote(c_ast) <= unquote(r) end}
  end
  defp cmd_char_n_matcher(c_ast, {:.., _meta, [l, r]}) when is_integer(l) and is_integer(r) and l<0 and r<0 do
    {false, quote do not(unquote(c_ast) >= unquote(l) and unquote(c_ast) <= unquote(r)) end}
  end
  defp cmd_char_n_matcher(c_ast, {:.., _meta, [{:-, _, [l]}, {:-, _, [r]}]}) when is_integer(l) and is_integer(r) and l>0 and r>0 do
    {false, quote do not(unquote(c_ast) >= unquote(l) and unquote(c_ast) <= unquote(r)) end}
  end
  defp cmd_char_n_matcher(_c_ast, [matcher | _rest]), do: throw {:INVALID_CHAR_MATCHER_TYPE, matcher}

  defp cmd_char_n_matchers(c_ast, matchers) when is_list(matchers) do
    matchers
    |> Enum.sort_by(fn
      l.._r -> l
      c -> c
    end, &>/2)
    |> Enum.reduce(nil, fn
      (matcher, nil) ->
        {_is_pos, left} = cmd_char_n_matcher(c_ast, matcher)
        left
      (matcher, left) ->
        case cmd_char_n_matcher(c_ast, matcher) do
          {true, ast} -> quote do unquote(left) or unquote(ast) end
          {false, ast} -> quote do unquote(left) and unquote(ast) end
        end
      end)
  end


  defp cmd_chars_n(env, matchers) do
    {matchers, opts} = Enum.split_while(matchers, &not(Keyword.keyword?(&1)))
    opts = List.flatten(opts)
    minimumChars = opts[:min] || 1
    maximumChars = opts[:max] || -1
    context_skip_if(env) do
      quote do
        case do
          %{rest: rest} = matched_context ->
            {position, column, line, chars} = chars_increment_while_matching(unquote(matchers), unquote(maximumChars), matched_context.rest, 0, matched_context.column, matched_context.line, 0)
            if chars < unquote(minimumChars) do
              %{matched_context |
                error: %ExSpirit.Parser.ParseException{message: "Tried parsing out characters of `#{inspect unquote(matchers)}` but failed due to not meeting the minimum characters required of #{unquote(minimumChars)}", context: matched_context},
              }
            else
              case matched_context.rest do
                <<result::binary-size(position), result_rest::binary>> ->
                  %{matched_context |
                    result: result,
                    rest: result_rest,
                    position: matched_context.position + position,
                    column: column,
                    line: line,
                  }
              end
            end
        end
      end
    end
  end


  defp cmd_lit_n(env, orig_lits) do
    lits =
      orig_lits
      |> Enum.map(fn # Turn single characters into a binary
        c when is_integer(c) -> <<c::utf8>>
        s -> s
      end)
      |> Enum.sort_by(&String.length/1, &>/2) # Sort to match longest first
      |> Enum.uniq()
      |> Enum.flat_map(fn s -> # flat_map'ing because `->` returns wrapped in a list because of Elixir oddness
          quote do
            %{rest: unquote(s)<>rest} = context -> %{context | rest: rest, result: nil}
          end
      end)
    lits =
      lits ++ quote do # `->` already gets returned as a list...
        context ->
          %{context |
            error: %ExSpirit.Parser.ParseException{message: "Failed matching out a literal of one of `#{inspect unquote(orig_lits)}`", context: context},
          }
      end
    context_skip_if(env) do
      {:case, [], [[do: lits]]}
    end
  end


  defp cmd_seq_2(env, seq) do
    seq = cmd_seq_2_flatten({:|>, [], seq})
    cmd_seq_n(env, seq)
  end

  defp cmd_seq_2_flatten(ast)
  defp cmd_seq_2_flatten({:|>, _, [left, right]}) do
    left = List.wrap(cmd_seq_2_flatten(left))
    right = List.wrap(cmd_seq_2_flatten(right))
    left ++ right
  end
  defp cmd_seq_2_flatten(ast), do: List.wrap(ast)

  defp cmd_seq_n(env, seqs) do
    [first_seq | rest_seq] = Enum.flat_map(seqs, &cmd_seq_2_flatten/1)
    {env, inner} = cmd_seq_n_expand(env, first_seq, rest_seq)
    context_skip_if(env) do
      inner
    end
  end

  defp cmd_seq_n_expand(env, this_ast, [next_ast | rest_ast]) do
    {env, this_ast} = parser_to_ast(env, this_ast)
    {env, inner} = cmd_seq_n_expand(env, next_ast, rest_ast)
    context_skip_if(env) do
      quote do
        unquote(this_ast)
        |> case do
          %{result: result} = context -> {result, context |> unquote(inner)}
        end
        |> case do
            {nil, %{error: nil} = context} -> context
            {result, %{error: nil, result: nil} = context} -> %{context | result: result}
            # {result, %{error: nil, result: results} = context} when is_tuple(results) -> %{context | result: :erlang.append_element(results, result)} # backwards
            # {result, %{error: nil, result: results} = context} -> %{context | result: {result, results}}
            {result, %{error: nil, result: results} = context} when is_list(results) -> %{context | result: [result | results]}
            {result, %{error: nil, result: results} = context} -> %{context | result: [result, results]}
            {_result, context} -> context
          end
      end
    end
  end
  defp cmd_seq_n_expand(env, this_ast, []) do
    parser_to_ast(env, this_ast)
  end


  defp cmd_alt_2(env, alt) do
    alt = cmd_alt_2_flatten({:||, [], alt})
    cmd_alt_n(env, alt)
  end

  defp cmd_alt_2_flatten(ast)
  defp cmd_alt_2_flatten({:||, _, [left, right]}) do
    left = List.wrap(cmd_alt_2_flatten(left))
    right = List.wrap(cmd_alt_2_flatten(right))
    left ++ right
  end
  defp cmd_alt_2_flatten(ast), do: List.wrap(ast)

  defp cmd_alt_n(env, choices) do
    [first_choice | rest_choices] = Enum.flat_map(choices, &cmd_alt_2_flatten/1)
    context_binding = Macro.var(:original_context_alt, __MODULE__)
    {env, inner} = cmd_alt_n_expand(env, context_binding, first_choice, rest_choices)
    context_skip_if(env) do
      quote do
        case do
          unquote(context_binding) -> unquote(context_binding) |> unquote(inner)
        end
      end
    end
  end

  defp cmd_alt_n_expand(env, original_context_ast, this_ast, [next_ast | rest_ast]) do
    {env, this_ast} = parser_to_ast(env, this_ast)
    {env, inner} = cmd_alt_n_expand(env, original_context_ast, next_ast, rest_ast)
    ast =
      quote location: :keep do
        unquote(this_ast) |> case do
          %{error: nil} = good_context -> good_context
          %{error: %ExSpirit.Parser.ExpectationFailureException{}} = bad_context -> bad_context
          _bad_context -> unquote(original_context_ast) |> unquote(inner)
        end
      end
    {env, ast}
  end
  defp cmd_alt_n_expand(env, _context_ast, this_ast, []) do
    parser_to_ast(env, this_ast)
  end


  defp cmd_skip_0(env, []) do
    context_skip_if(env) do
      quote do
        case do context ->
          %{context | result: nil}
        end
      end
    end
  end


  defp cmd_skip_2(env, [parser, skipper]) do
    {env, parser} = parser_to_ast(env, parser)
    {env, skipper} = parser_to_ast(env, skipper)
    skipper_fn = quote do fn context -> context |> unquote(skipper) end end
    context_if(env) do
      quote do
        case do
          %{skipper: original_skipper} = context ->
            %{context | skipper: unquote(skipper_fn)}
            |> unquote(parser)
            |> case do
              context -> %{context | skipper: original_skipper}
            end
        end
      end
    end
  end


  defp cmd_no_skip_1(env, [parser]) do
    {env, parser} = parser_to_ast(env, parser)
    context_if(env) do
      quote do
        case do
          %{skipper: skipper} = context ->
            %{context | skipper: nil}
            |> unquote(parser)
            |> case do
              context -> %{context | skipper: skipper}
            end
        end
      end
    end
  end


  defp cmd_lexeme_1(env, [parser]) do
    {env, parser} = parser_to_ast(env, parser)
    context_skip_if(env) do
      quote do
        case do
          %{position: position} = context ->
            case context |> unquote(parser) do
              %{error: nil, position: new_position} = good_context ->
                bytes = new_position - position
                case context.rest do
                  <<parsed::binary-size(bytes), rest::binary>> ->
                    %{good_context |
                      result: parsed,
                      rest: rest,
                    }
                  _ ->
                    %{good_context |
                      result: nil,
                      error:  %ExSpirit.Parser.ParseException{message: "Lexeme failed, length needed is #{bytes} but available is only #{byte_size(context.rest)}", context: context, extradata: good_context},
                    }
                end
              context -> context
            end
        end
      end
    end
  end


  defp cmd_ignore_1(env, [parser]) do
    {env, parser} = parser_to_ast(env, parser)
    context_if(env) do
      quote do
        case do
          context -> %{(context |> unquote(parser)) | result: nil}
        end
      end
    end
  end


  defp cmd_repeat_1_4(env, opts) do
    {parser, opts} =
      case opts do
        [parser] -> {parser, []}
        [parser, opts] -> {parser, List.flatten(opts)}
      end
    minimum = opts[:min] || 1
    maximum = opts[:max] || :infinite
    sep     = opts[:sep] || nil

    {env, ast_parser} = parser_to_ast(env, parser)
    fparser = quote do fn context -> context |> unquote(ast_parser) end end
    {env, fsep} =
      case sep do
        nil -> {env, nil}
        sep ->
          {env, sep} = parser_to_ast(env, quote do ignore(unquote(sep)) |> unquote(parser) end)
          f_ast = quote do fn context -> context |> unquote(sep) end end
          {env, f_ast}
      end

    context_if(env) do
      quote do
        case do
          context -> unquote(:'$repeat$')(unquote(fsep), unquote(fparser), unquote(minimum), unquote(maximum), context, 0)
        end
      end
    end
  end

end
