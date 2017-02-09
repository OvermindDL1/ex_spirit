defmodule RomanNumerals do
  use ExSpirit.Parser, text: true

  defrule thousands(
    alt([
        lit(?M) |> success(1000),
        success(0),
      ])
  )

  defrule hundreds(context) do
    import ExSpirit.TreeMap
    symbols_ = new()
    |> add_text(""    , 0)
    |> add_text("C"   , 100)
    |> add_text("CC"  , 200)
    |> add_text("CCC" , 300)
    |> add_text("CD"  , 400)
    |> add_text("D"   , 500)
    |> add_text("DC"  , 600)
    |> add_text("DCC" , 700)
    |> add_text("DCCC", 800)
    |> add_text("DM"  , 900)
    context |> symbols(symbols_)
  end

  defrule tens(context) do
    import ExSpirit.TreeMap
    symbols_ = new()
    |> add_text(""     , 0)
    |> add_text("X"    , 10)
    |> add_text("XX"   , 20)
    |> add_text("XXX"  , 30)
    |> add_text("XL"   , 40)
    |> add_text("L"    , 50)
    |> add_text("LX"   , 60)
    |> add_text("LXX"  , 70)
    |> add_text("LXXX" , 80)
    |> add_text("XC"   , 90)
    context |> symbols(symbols_)
  end

  defrule ones(context) do
    import ExSpirit.TreeMap
    symbols_ = new()
    |> add_text(""     , 0)
    |> add_text("I"    , 1)
    |> add_text("II"   , 2)
    |> add_text("III"  , 3)
    |> add_text("IV"   , 4)
    |> add_text("V"    , 5)
    |> add_text("VI"   , 6)
    |> add_text("VII"  , 7)
    |> add_text("VIII" , 8)
    |> add_text("IX"   , 9)
    context |> symbols(symbols_)
  end

  defrule roman_numerals(
    seq([
        thousands(),
        hundreds(),
        tens(),
        ones(),
      ])
  ), map: Enum.sum()

  def from_string(input) do
    parse(input, roman_numerals())
  end

end

IO.puts("Input Roman Numerals and press enter: ")
case IO.read(:line) |> RomanNumerals.from_string() do
  %{error: nil, result: result, rest: ""} -> IO.puts("Result: #{result}")
  %{error: nil, result: result, rest: rest} -> IO.puts("Result: #{result}\nLeftover: #{rest}")
  %{error: error} -> IO.puts("#{Exception.message(error)}")
end
