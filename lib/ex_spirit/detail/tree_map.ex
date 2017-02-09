defmodule ExSpirit.TreeMap do

  defstruct root: %{}

  def new(), do: %__MODULE__{}

  def add_text(tm, str, value) when is_binary(str), do: add(tm, String.to_charlist(str), value)
  def add(tm, bin, value) when is_binary(bin), do: add(tm, :erlang.binary_to_list(bin), value)
  def add(tm, list, value) when is_list(list) do
    %{tm |
      root: add_(tm.root, list, value)
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
