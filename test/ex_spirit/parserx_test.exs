defmodule ExSpirit.ParserxTest do
  use ExUnit.Case
  # doctest ExSpirit.Parserx

  use ExSpirit.Parserx
  alias ExSpirit.Parser.ParseException


  # defmodule TestParser do
  #   use ExSpirit.Parserx
  #
  #   defrule single_char0a, do: char
  #   # defrule single_char0b, do: char()
  #   # defrule single_char0c(), do: char
  #   # defrule single_char0d(), do: char()
  # end

  test "parse: char" do
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char()
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?a)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?a, ?b)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?b, ?a)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?a..?b)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(-?b)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(-?b..-?z)
    assert %{error: %ParseException{}, result: nil, rest: "b"} = parse "b", char(?a)
    assert %{error: %ParseException{}, result: nil, rest: ""} = parse "", char()
    assert %{error: %ParseException{}, result: nil, rest: ""} = parse "", char(?a)
  end

  test "parse: chars" do
    assert %{error: nil, result: "a", rest: ""} = parse "a", chars(?a)
    assert %{error: nil, result: "aa", rest: ""} = parse "aa", chars(?a)
    assert %{error: nil, result: "aaa", rest: ""} = parse "aaa", chars(?a)
    assert %{error: nil, result: "a", rest: "b"} = parse "ab", chars(?a)
    assert %{error: nil, result: "aa", rest: "b"} = parse "aab", chars(?a)
    assert %{error: nil, result: "aaa", rest: "b"} = parse "aaab", chars(?a)
    assert %{error: nil, result: "a", rest: ""} = parse "a", chars(?a, ?b)
    assert %{error: nil, result: "a", rest: ""} = parse "a", chars(?b, ?a)
    assert %{error: nil, result: "aa", rest: ""} = parse "aa", chars(?a, min: 2)
    assert %{error: nil, result: "a", rest: "a"} = parse "aa", chars(?a, max: 1)
    assert %{error: nil, result: "a", rest: ""} = parse "a", chars(?a, min: 1, max: 2)
    assert %{error: nil, result: "aa", rest: ""} = parse "aa", chars(?a, min: 1, max: 2)
    assert %{error: nil, result: "aa", rest: "a"} = parse "aaa", chars(?a, min: 1, max: 2)
    assert %{error: %ParseException{}, result: nil, rest: ""} = parse "", chars(?a)
    assert %{error: %ParseException{}, result: nil, rest: "b"} = parse "b", chars(?a)
    assert %{error: %ParseException{}, result: nil, rest: "a"} = parse "a", chars(?a, min: 2)
  end

  test "parse: lit" do
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit(?t)
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit("t")
    assert %{error: nil, result: nil, rest: ""} = parse "test", lit("test")
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit(?t, ?a)
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit(?a, ?t)
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit(?t, "a")
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit("t", ?a)
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit("t", "a")
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit("t", ?t)
    assert %{error: nil, result: nil, rest: ""} = parse "t", lit(?t, "t")
    assert %{error: nil, result: nil, rest: ""} = parse "test", lit("te", "test")
    assert %{error: nil, result: nil, rest: ""} = parse "test", lit("test", "te")
  end

  test "parse: |>" do
    assert %{error: nil, result: 'ab', rest: "cd"} = parse "abcd", char(?a) |> char(?b)
    assert %{error: nil, result: 'abc', rest: "d"} = parse "abcd", char(?a) |> char(?b) |> char(?c)
    assert %{error: nil, result: 'abcd', rest: ""} = parse "abcd", char(?a) |> char(?b) |> char(?c) |> char(?d)
    assert %{error: %ParseException{}, result: nil, rest: "abcd"} = parse "abcd", char(?b) |> char(?c)
  end

  test "parse: seq" do
    assert %{error: nil, result: 'ab', rest: "cd"} = parse "abcd", seq(char(?a), char(?b))
    assert %{error: nil, result: 'abc', rest: "d"} = parse "abcd", seq(char(?a), char(?b), char(?c))
    assert %{error: nil, result: 'abcd', rest: ""} = parse "abcd", seq(char(?a), char(?b), char(?c), char(?d))
    assert %{error: %ParseException{}, result: nil, rest: "abcd"} = parse "abcd", seq(char(?b), char(?c))
  end

  test "parse: ||" do
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?a) || char(?b)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?b) || char(?a)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?a) || char(?b) || char(?c)
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char(?c) || char(?b) || char(?a)
  end

  test "parse: alt" do
    assert %{error: nil, result: ?a, rest: ""} = parse "a", alt(char(?a), char(?b))
    assert %{error: nil, result: ?a, rest: ""} = parse "a", alt(char(?b), char(?a))
    assert %{error: nil, result: ?a, rest: ""} = parse "a", alt(char(?a), char(?b), char(?c))
    assert %{error: nil, result: ?a, rest: ""} = parse "a", alt(char(?c), char(?b), char(?a))
  end

  test "parse: skipper" do
    assert %{error: nil, result: ?a, rest: ""} = parse "a", char, skipper: char(?\s)
    assert %{error: nil, result: ?a, rest: ""} = parse " a", char, skipper: char(?\s)
    assert %{error: nil, result: ?\s, rest: "a"} = parse "  a", char, skipper: char(?\s)
    assert %{error: nil, result: ?a, rest: ""} = parse "  a", char, skipper: chars(?\s)
    assert %{error: nil, result: 'aa', rest: ""} = parse "aa", char |> char, skipper: char(?\s)
    assert %{error: nil, result: 'aa', rest: ""} = parse "a a", char |> char, skipper: char(?\s)
    assert %{error: nil, result: 'a ', rest: "a"} = parse "a  a", char |> char, skipper: char(?\s)

    # Explicit skip
    assert %{error: nil, result: 'aa', rest: ""} = parse "a  a", char |> skip |> char, skipper: char(?\s)

    # Turn off skip
    assert %{error: nil, result: ?a, rest: ""} = parse "a", no_skip(char), skipper: char(?\s)
    assert %{error: nil, result: ?\s, rest: "a"} = parse " a", no_skip(char), skipper: char(?\s)

    # Change skip
    assert %{error: nil, result: ?a, rest: ""} = parse "ba", skip(char, char(?b)), skipper: char(?\s)
  end

  test "parse: lexeme" do
    assert %{error: nil, result: "a", rest: ""} = parse "a", lexeme(char)
    assert %{error: nil, result: "a", rest: "b"} = parse "ab", lexeme(char)
    assert %{error: nil, result: "ab", rest: ""} = parse "ab", lexeme(char |> char)
  end

  test "parse: ignore" do
    assert %{error: nil, result: nil, rest: ""} = parse "a", ignore(char)
    assert %{error: nil, result: nil, rest: "b"} = parse "ab", ignore(char)
    assert %{error: nil, result: nil, rest: ""} = parse "ab", ignore(char |> char)
  end

  test "parse: repeat" do
    assert %{error: nil, result: 'a', rest: ""} = parse "a", repeat(char)
    assert %{error: nil, result: 'aaa', rest: ""} = parse "aaa", repeat(char)
    assert %{error: nil, result: 'aa', rest: ""} = parse "aa", repeat(char)
    assert %{error: %ParseException{}, result: nil, rest: ""} = parse "", repeat(char)
    assert %{error: %ParseException{}, result: nil, rest: ""} = parse "a", repeat(char, min: 2)
    assert %{error: nil, result: 'aa', rest: ""} = parse "aa", repeat(char, min: 2)
    assert %{error: nil, result: 'aaa', rest: ""} = parse "aaa", repeat(char, max: 3)
    assert %{error: nil, result: 'aaa', rest: "a"} = parse "aaaa", repeat(char, max: 3)
    assert %{error: nil, result: 'a', rest: ""} = parse "a", repeat(char(-?,), sep: char(?,))
    assert %{error: nil, result: 'a', rest: "a"} = parse "aa", repeat(char(-?,), sep: char(?,))
    assert %{error: nil, result: 'aa', rest: ""} = parse "a,a", repeat(char(-?,), sep: char(?,))
    assert %{error: nil, result: 'aaaa', rest: ""} = parse "a,a,a,a", repeat(char(-?,), sep: char(?,))
    assert %{error: nil, result: 'aaaa', rest: "a"} = parse "a,a,a,aa", repeat(char(-?,), sep: char(?,))
  end
end
