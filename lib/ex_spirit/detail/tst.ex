defmodule ExSpirit.TST do

  defstruct root: %{}

  def new(), do: %__MODULE__{}

  def add_text(tst, str, value) when is_binary(str), do: add(tst, String.to_charlist(str), value)
  def add(tst, bin, value) when is_binary(bin), do: add(tst, :erlang.binary_to_list(bin), value)
  def add(tst, list, value) when is_list(list) do
    %{tst |
      root: add_(tst.root, list, value)
    }
  end

  defp add_(map, [], value), do: Map.put(map, [], value)
  defp add_(map, [c | rest], value) do
    case map[c] do
      nil -> Map.put_new(map, c, add_(%{}, rest, value))
      submap -> Map.put(map, c, add_(submap, rest, value))
    end
  end


end
