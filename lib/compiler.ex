defmodule Compiler do
  alias Compiler.Parser, as: Parser
  alias Compiler.Symbols, as: Symbols
  alias Compiler.Tokenizer, as: Tokenizer
  alias Compiler.Writer, as: Writer

  def compile(fileOrDir) do
    is_dir? = File.dir?(fileOrDir)

    classes =
      case is_dir? do
        true -> Utils.readdir(fileOrDir, ".jack")
        false -> [fileOrDir]
      end
      |> Stream.map(fn file ->
        {file,
         Tokenizer.tokenize!(file)
         |> Enum.into([])
         |> Parser.parse_tokens!()}
      end)
      |> Enum.into([])

    symbols = Symbols.new(Enum.map(classes, &elem(&1, 1)))

    Enum.each(classes, fn {_, class} ->
      Writer.write!(class, symbols)
      |> Enum.each(&IO.puts/1)
    end)
  end
end
