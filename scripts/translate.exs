System.argv()
|> OptionParser.parse(strict: [source: :string])
|> elem(0)
|> Keyword.get(:source)
|> case do
  nil -> raise("please provide a --source")
  path -> path
end
|> Translator.translate()
|> Enum.each(&IO.puts/1)