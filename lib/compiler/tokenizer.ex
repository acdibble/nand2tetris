defmodule Compiler.Tokenizer do
  def tokenize!(file) do
    File.stream!(file)
    |> Stream.transform(false, &Utils.remove_comments/2)
    |> Stream.reject(&Utils.line_empty?/1)
    |> Stream.flat_map(&(tokenize_line!(&1, []) |> Enum.reverse()))
    |> Stream.concat([:eof])
  end

  defp tokenize_line!("", tokens), do: tokens

  defp tokenize_line!(" " <> rest, tokens), do: tokenize_line!(rest, tokens)

  defp tokenize_line!("\"" <> rest, tokens) do
    {token, rest} = tokenize_string(rest, "")
    tokenize_line!(rest, [token | tokens])
  end

  [
    {"(", :"("},
    {")", :")"},
    {"{", :"{"},
    {"}", :"}"},
    {"[", :"["},
    {"]", :"]"},
    {";", :";"},
    {"&", :&},
    {"|", :|},
    {".", :.},
    {",", :","},
    {"<", :<},
    {">", :>},
    {"~", :"~"},
    {"=", :=},
    {"/", :/},
    {"-", :-},
    {"*", :*},
    {"+", :+},
  ]
  |> Enum.map(fn {pattern, token} ->
    defp tokenize_line!(unquote(pattern) <> rest, tokens) do
      tokenize_line!(rest, [unquote(token) | tokens])
    end
  end)

  String.graphemes("0123456789")
  |> Enum.map(fn ch ->
    defp tokenize_line!(unquote(ch) <> rest, tokens) do
      {num, rest} = tokenize_number(rest, unquote(ch))
      tokenize_line!(rest, [{:integer, num} | tokens])
    end
  end)

  defp tokenize_number(line, acc) do
    {ch, rest} = String.split_at(line, 1)

    case Regex.match?(~r/\d/, ch) do
      true ->
        tokenize_number(rest, acc <> ch)

      false ->
        case Integer.parse(acc) do
          {num, ""} when num in 0..0x7FFF -> {num, line}
        end
    end
  end

  defp tokenize_line!(line, tokens) do
    case Regex.match?(~r/^\w/i, line) do
      true ->
        {token, rest} = tokenize_ident(line, "")
        tokenize_line!(rest, [token | tokens])

      false ->
        raise("failed to parse")
    end
  end

  defp tokenize_string(line, acc) do
    {char, rest} = String.split_at(line, 1)

    case char do
      "\"" -> {{:string, acc}, rest}
      _ -> tokenize_string(rest, acc <> char)
    end
  end

  defp tokenize_ident(line, acc) do
    {char, rest} = String.split_at(line, 1)

    case Regex.match?(~r/[\w\d]/i, char) do
      true -> tokenize_ident(rest, acc <> char)
      false -> finish_ident(line, acc)
    end
  end

  defp finish_ident(rest, acc) do
    token =
      case acc do
        "class" -> :class
        "constructor" -> :constructor
        "do" -> :do
        "else" -> :else
        "false" -> false
        "field" -> :field
        "function" -> :function
        "if" -> :if
        "int" -> :int
        "let" -> :let
        "null" -> :null
        "method" -> :method
        "return" -> :return
        "static" -> :static
        "this" -> :this
        "true" -> true
        "var" -> :var
        "while" -> :while
        "void" -> :void
        _ -> {:ident, acc}
      end

    {token, rest}
  end
end
