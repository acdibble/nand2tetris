defmodule Utils do
  defmacro __using__(_) do
    quote do
      def todo!(message \\ "todo"), do: raise(message)
    end
  end

  def remove_comments(line) do
    String.replace(line, ~r/\/\/.+/, "")
    |> String.replace(~r/\/\*.+?\*\//, "")
    |> String.trim()
  end

  def remove_comments(line, in_comment) do
    case in_comment do
      true ->
        case String.contains?(line, "*/") do
          true ->
            {[String.replace(line, ~r/.+?\*\//, "") |> String.trim()], false}

          false ->
            {[], true}
        end

      false ->
        case String.contains?(line, "/*") && !String.contains?(line, "*/") do
          true -> {[String.replace(line, ~r/\/\*.+/, "") |> String.trim()], true}
          false -> {[remove_comments(line)], false}
        end
    end
  end

  def line_empty?(""), do: true
  def line_empty?(_), do: false

  def take_reduce(enum, fun) do
    {enum, list} = take_reduce(enum, fun, [])

    {enum, Enum.reverse(list)}
  end

  defp take_reduce(enum, fun, list) do
    case fun.(enum) do
      {:cont, enum} -> take_reduce(enum, fun, list)
      {:cont, enum, result} -> take_reduce(enum, fun, [result | list])
      {:halt, enum} -> {enum, list}
      {:halt, enum, result} -> {enum, [result | list]}
    end
  end

  def readdir(dir, extension) do
    File.ls!(dir)
    |> Enum.filter(&String.ends_with?(&1, extension))
    |> Enum.map(&Path.join(dir, &1))
  end
end
