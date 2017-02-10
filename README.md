# ExSpirit

Spirit-style PEG-like parsing library for Elixir.

Please see `ExSpirit.Parser` for details about the parser.

## Installation

Available in [hex.pm](https://hex.pm/packages/ex_spirit).

Add to dependencies with:

```elixir
def deps do
  [{:ex_spirit, "~> 0.1"}]
end
```

Full docs can be found at: <https://hexdocs.pm/ex_spirit>

## Examples

See the examples directory for examples and run them with `mix run examples/<filename>`.

Current examples are:

### number_adder.exs

Takes a list of simple integers of base 10, separated by commas, with optional spaces between them, adds them together, and returns them (all within the parser), requires at least one number.

Example Run:

```text
$ mix run examples/number_adder.exs
Input simple number separated by comma's and optionally spaces and press enter:

<unknown>:1:1: Parse error: Parsing uint with radix of 10 had 0 digits but 1 minimum digits were required
        RuleStack: [added_number]
        Input:

$ mix run examples/number_adder.exs
d
Input simple number separated by comma's and optionally spaces and press enter:
<unknown>:1:1: Parse error: Parsing uint with radix of 10 had 0 digits but 1 minimum digits were required
        RuleStack: [added_number]
        Input: d

$ mix run examples/number_adder.exs
Input simple number separated by comma's and optionally spaces and press enter:
42
Result: 42

$ mix run examples/number_adder.exs
Input simple number separated by comma's and optionally spaces and press enter:
1,2,3 , 4,   5    ,6 , 7
Result: 28

$ mix run examples/number_adder.exs
Input simple number separated by comma's and optionally spaces and press enter:
1 ,
Result: 1
Leftover: " ,\n"
```

### roman_numerals.exs

Takes a typed in roman numeral from stdin and an enter, parses out the number up to the thousands position and reports back any errors and remaining leftovers.

Example Run:

```text
$ mix run examples/roman_numerals.exs
Input Roman Numerals and press enter:
MDMXXIV
Result: 1924

$ mix run examples/roman_numerals.exs
Input Roman Numerals and press enter:
zzzz
Result: 0
Leftover: "zzzz\n"

$ mix run examples/roman_numerals.exs
Input Roman Numerals and press enter:
XVIzzz
Result: 16
Leftover: "zzz\n"
```

### simple_xml.exs

A simple xml parser, no attributes, just nodes and text.

Example Run:

```text
$ mix run examples/simple_xml.exs
Input a single line of xml-like syntax:
<test1>Some text<test2>Hi</test2> and more</test1>
Result: {"test1", ["Some text", {"test2", ["Hi"]}, " and more"]}

$ mix run examples/simple_xml.exs
Input a single line of xml-like syntax:
<a-tag>How about an improperly terminated tag</b-tag>
<unknown>:1:48: Expectation Failure: literal `a-tag` did not match the input
        RuleStack: [tag, node_]
        Input: b-tag>

$ mix run examples/simple_xml.exs
Input a single line of xml-like syntax:
<
<unknown>:1:1: Parse error: Repeating over a parser failed due to not reaching the minimum amount of 1 with only a repeat count of 0
        RuleStack: [text, node_]
        Input: <
```
