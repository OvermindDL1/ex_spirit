defmodule ExSpirit.Parser.SymbolTest do
  use ExUnit.Case
  use ExSpirit.Parser, text: true
  use ExUnitProperties
  alias ExSpirit.TreeMap

  defp example_tree_map() do
    TreeMap.new()
      |> TreeMap.add_text("x", "x")
      |> TreeMap.add_text("yy", "yy")
      |> TreeMap.add_text("zzz", "zzz")
      |> TreeMap.add_text("olá", "olá")
      |> TreeMap.add_text("こんにちは", "こんにちは")
  end

  defp tree_map_from_strings(strings) do
    Enum.reduce strings, TreeMap.new(), fn string, tree_map ->
      tree_map |> TreeMap.add_text(string, string)
    end
  end

  test "`symbols` parser return context with right position (ASCII test case)" do
    # rest == ""
    assert %{result: "x", position: 1, rest: ""} = parse "x", symbols(example_tree_map())
    assert %{result: "yy", position: 2, rest: ""} = parse "yy", symbols(example_tree_map())
    assert %{result: "zzz", position: 3, rest: ""} = parse "zzz", symbols(example_tree_map())
    # rest != ""
    assert %{result: "x", position: 1, rest: "1"} = parse "x1", symbols(example_tree_map())
    assert %{result: "yy", position: 2, rest: "1"} = parse "yy1", symbols(example_tree_map())
    assert %{result: "zzz", position: 3, rest: "1"} = parse "zzz1", symbols(example_tree_map())
  end

  test "`symbols` parser return context with right position (UTF8 test case)" do
    pt_pos = byte_size("olá")
    jp_pos = byte_size("こんにちは")
    # rest == ""
    assert %{result: "olá", position: ^pt_pos, rest: ""} = parse "olá", symbols(example_tree_map())
    assert %{result: "こんにちは", position: ^jp_pos, rest: ""} = parse "こんにちは", symbols(example_tree_map())
    # rest != ""
    assert %{result: "olá", position: ^pt_pos, rest: "1"} = parse "olá1", symbols(example_tree_map())
    assert %{result: "こんにちは", position: ^jp_pos, rest: "1"} = parse "こんにちは1", symbols(example_tree_map())
  end

  property "`symbols` parser return context with right position" do
    check all word <- string(:printable),
              word != "",
              rest <- string(:printable) do

      tree_map = tree_map_from_strings([word])
      expected_position = byte_size(word)

      assert %{result: ^word, position: ^expected_position, rest: ^rest} =
        parse (word <> rest), symbols(tree_map)
    end
  end

  test "`symbols` parser composes with `lexeme` (ASCII test case)" do
    # rest == ""
    assert parse("x", symbols(example_tree_map())) == parse("x", lexeme(symbols(example_tree_map())))
    assert parse("yy", symbols(example_tree_map())) == parse("yy", lexeme(symbols(example_tree_map())))
    assert parse("zzz", symbols(example_tree_map())) == parse("zzz", lexeme(symbols(example_tree_map())))
    # rest != ""
    assert parse("x1", symbols(example_tree_map())) == parse("x1", lexeme(symbols(example_tree_map())))
    assert parse("yy1", symbols(example_tree_map())) == parse("yy1", lexeme(symbols(example_tree_map())))
    assert parse("zzz1", symbols(example_tree_map())) == parse("zzz1", lexeme(symbols(example_tree_map())))
  end

  test "`symbols` parser composes with `lexeme` (UTF8 test case)" do
    # rest == ""
    assert parse("olá", symbols(example_tree_map())) == parse("olá", lexeme(symbols(example_tree_map())))
    assert parse("こんにちは", symbols(example_tree_map())) == parse("こんにちは", lexeme(symbols(example_tree_map())))
    # rest != ""
    assert parse("olá1", symbols(example_tree_map())) == parse("olá1", lexeme(symbols(example_tree_map())))
    assert parse("こんにちは1", symbols(example_tree_map())) == parse("こんにちは1", lexeme(symbols(example_tree_map())))
  end

  # Note: the success and failure cases are separated because it's probably quite rare
  # to have a success case with purely random strings, and that's the most interesting case.
  property "`symbols` parser composes with `lexeme` - in case of success" do
    check all word <- string(:printable),
              word != "",
              rest <- string(:printable) do

      tree_map = tree_map_from_strings([word])
      input = word <> rest

      assert parse(input, lexeme(symbols(tree_map))) == parse(input, symbols(tree_map))
    end
  end

  property "`symbols` parser composes with `lexeme` - in case of failure" do
    check all word <- string(:printable),
              word != "",
              prefix <- string(:printable),
              prefix != "",
              not String.starts_with?(prefix <> word, word),
              rest <- string(:printable) do

      tree_map = tree_map_from_strings([word])
      input = prefix <> word <> rest

      assert parse(input, lexeme(symbols(tree_map))) == parse(input, symbols(tree_map))
    end
  end

  property "`symbols` matches the longest string" do
    check all short_word <- string(:printable),
              short_word != "",
              suffix <- string(:printable),
              suffix != "",
              rest <- string(:printable) do

      long_word = short_word <> suffix
      assert byte_size(long_word) > byte_size(short_word) # self documenting
      tree_map = tree_map_from_strings([short_word, long_word])
      expected_position = byte_size(long_word)
      input = long_word <> rest

      assert %{result: ^long_word, position: ^expected_position, rest: ^rest}
        = parse(input, symbols(tree_map))
    end
  end

  property "`symbols` parser matches only at the begining of the input (trivial)" do
    check all word <- string(:printable),
              word != "",
              prefix <- string(:printable),
              prefix != "",
              not String.starts_with?(prefix <> word, word),
              rest <- string(:printable) do

      tree_map = tree_map_from_strings([word])
      input = prefix <> word <> rest

      assert %{result: nil, position: 0, rest: ^input, error: error} = parse(input, symbols(tree_map))
      assert error != nil
    end
  end
end



defmodule ExSpirit.ParserDocTests do
  use ExUnit.Case
  doctest ExSpirit.Parser
  doctest ExSpirit.Parser.Text
end
