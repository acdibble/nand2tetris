defmodule UtilsTest do
  use ExUnit.Case
  doctest Utils

  test "take_reduce" do
    {rest, results} =
      Utils.take_reduce([1, 2, :pause, 3, 4, 5, :stop, 6], &reduce/1)

    assert rest == [6]
    assert results = [[5, 4, 3], [2, 1]]
  end

  defp reduce(els, acc \\ [])

  defp reduce([:pause | rest], acc) do
    {:cont, rest, acc}
  end

  defp reduce([:stop | rest], acc) do
    {:halt, rest, acc}
  end

  defp reduce([num | rest], acc) when is_integer(num) do
    reduce(rest, [num | acc])
  end
end
