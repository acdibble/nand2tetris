opts =
  System.argv()
  |> OptionParser.parse(strict: [source: :string, debug: :boolean])
  |> elem(0)

Keyword.get(opts, :source)
|> case do
  nil -> raise("please provide a --source")
  path -> File.stream!(path, :line)
end
|> Stream.map(&String.trim/1)
|> Stream.filter(&(String.length(&1) != 0))
|> Enum.with_index(fn bin, index ->
  disassembled = Disassembler.disassemble_instruction(bin)

  if Keyword.get(opts, :debug, false) do
    Integer.to_string(index)
    |> String.pad_trailing(2, " ")
    |> Kernel.<>(" ")
    |> Kernel.<>(bin)
    |> Kernel.<>(": ")
    |> Kernel.<>(disassembled)
  else
    disassembled
  end
  |> IO.puts()
end)
