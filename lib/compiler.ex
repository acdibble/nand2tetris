defmodule Compiler do
  defmodule Tokenizer do
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
      {"+", :+}
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
          IO.inspect(line)
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

  defmodule Parser do
    def parse_tokens!(tokens) do
      {[:eof], decls} = Utils.take_reduce(tokens, &parse_class/1)
      decls
    end

    defp parse_class([:class, {:ident, name}, :"{" | tokens]) do
      {tokens, fields} = Utils.take_reduce(tokens, &parse_field/1)
      {tokens, funcs} = Utils.take_reduce(tokens, &parse_function/1)
      [:"}" | tokens] = tokens
      {:cont, tokens, {:class, name, fields, funcs}}
    end

    defp parse_class(tokens), do: {:halt, tokens}

    defp parse_field([field, type | tokens])
         when field in [:field, :static] do
      type = unwrap_type(type)

      {tokens, idents} = parse_identifier_list(tokens)

      {:cont, tokens, {field, type, idents}}
    end

    defp parse_field(tokens), do: {:halt, tokens}

    defp parse_function([type, return, {:ident, name} | tokens])
         when type in [:constructor, :method, :function] do
      return = unwrap_type(return)
      {tokens, params} = parse_parameter_list(tokens)
      {tokens, stmts} = parse_block(tokens)
      {:cont, tokens, {type, name, return, params, stmts}}
    end

    defp parse_function(tokens) do
      {:halt, tokens}
    end

    defp parse_parameter_list([:"(" | tokens]) do
      Utils.take_reduce(tokens, fn
        [:"," | tokens] -> {:cont, tokens}
        [:")" | tokens] -> {:halt, tokens}
        [type, {:ident, name} | tokens] -> {:cont, tokens, {:param, unwrap_type(type), name}}
      end)
    end

    defp parse_block([:"{" | tokens]) do
      {tokens, stmts} = Utils.take_reduce(tokens, &parse_statement/1)
      [:"}" | tokens] = tokens
      {tokens, stmts}
    end

    defp parse_statement(tokens) do
      case hd(tokens) do
        :do ->
          {tokens, stmt} = parse_do_statement(tokens)
          {:cont, tokens, stmt}

        :return ->
          {tokens, stmt} = parse_return_statement(tokens)
          {:cont, tokens, stmt}

        :var ->
          {tokens, stmt} = parse_var_statement(tokens)
          {:cont, tokens, stmt}

        :let ->
          {tokens, stmt} = parse_let_statement(tokens)
          {:cont, tokens, stmt}

        :while ->
          {tokens, stmt} = parse_while_statement(tokens)
          {:cont, tokens, stmt}

        :if ->
          {tokens, stmt} = parse_if_statement(tokens)
          {:cont, tokens, stmt}

        _ ->
          {:halt, tokens}
      end
    end

    defp parse_do_statement([:do | tokens]) do
      {_, tokens, call} = parse_call_expression(tokens)
      [:";" | tokens] = tokens
      {tokens, {:do, call}}
    end

    defp parse_return_statement([:return | tokens]) do
      {tokens, stmt} =
        case parse_expression(tokens) do
          {_, tokens} -> {tokens, {:return, nil}}
          {_, tokens, expr} -> {tokens, {:return, expr}}
        end

      [:";" | tokens] = tokens

      {tokens, stmt}
    end

    defp parse_var_statement([:var, type | tokens]) do
      type = unwrap_type(type)

      {tokens, identifiers} =
        parse_identifier_list(tokens)

      {tokens, {:var, type, identifiers}}
    end

    defp parse_let_statement([:let | tokens]) do
      {_, tokens, target} = parse_index_expression(tokens)
      [:= | tokens] = tokens
      {_, tokens, expr} = parse_expression(tokens)
      [:";" | tokens] = tokens
      {tokens, {:let, target, expr}}
    end

    defp parse_while_statement([:while, :"(" | tokens]) do
      {_, tokens, expr} = parse_expression(tokens)
      [:")" | tokens] = tokens
      {tokens, stmts} = parse_block(tokens)
      {tokens, {:while, expr, stmts}}
    end

    defp parse_if_statement([:if, :"(" | tokens]) do
      {_, tokens, condition} = parse_expression(tokens)
      [:")" | tokens] = tokens
      {tokens, then} = parse_block(tokens)

      {tokens, otherwise} =
        case tokens do
          [:else | tokens] -> parse_block(tokens)
          _ -> {tokens, nil}
        end

      {tokens, {:if, condition, then, otherwise}}
    end

    def parse_expression(tokens) do
      parse_or_expression(tokens)
    end

    def parse_or_expression(tokens) do
      case parse_and_expression(tokens) do
        {_, [:| | tokens], left} ->
          {_, tokens, right} = parse_or_expression(tokens)
          {:cont, tokens, {:|, left, right}}

        result ->
          result
      end
    end

    def parse_and_expression(tokens) do
      case parse_equality_expression(tokens) do
        {_, [:& | tokens], left} ->
          {_, tokens, right} = parse_and_expression(tokens)
          {:cont, tokens, {:&, left, right}}

        result ->
          result
      end
    end

    defp parse_equality_expression(tokens) do
      case parse_comparison_expression(tokens) do
        {_, [:= | tokens], left} ->
          {_, tokens, right} = parse_equality_expression(tokens)
          {:cont, tokens, {:=, left, right}}

        result ->
          result
      end
    end

    defp parse_comparison_expression(tokens) do
      case parse_term_expression(tokens) do
        {_, [op | tokens], left} when op == :< or op == :> ->
          {_, tokens, right} = parse_comparison_expression(tokens)
          {:cont, tokens, {op, left, right}}

        result ->
          result
      end
    end

    defp parse_term_expression(tokens) do
      case parse_factor_expression(tokens) do
        {_, [op | tokens], left} when op == :+ or op == :- ->
          {_, tokens, right} = parse_term_expression(tokens)
          {:cont, tokens, {op, left, right}}

        result ->
          result
      end
    end

    defp parse_factor_expression(tokens) do
      case parse_unary_expression(tokens) do
        {_, [op | tokens], left} when op == :* or op == :/ ->
          {_, tokens, right} = parse_factor_expression(tokens)
          {:cont, tokens, {op, left, right}}

        result ->
          result
      end
    end

    defp parse_unary_expression([op | tokens]) when op in [:-, :"~"] do
      {_, tokens, right} = parse_unary_expression(tokens)
      {:cont, tokens, {op, right}}
    end

    defp parse_unary_expression(tokens), do: parse_call_expression(tokens)

    defp parse_call_expression(tokens) do
      case parse_index_expression(tokens) do
        {_, [:"(" | tokens], ident = {:var_name, _, _}} ->
          {tokens, exprs} =
            Utils.take_reduce(tokens, fn
              [:")" | tokens] -> {:halt, tokens}
              [:"," | tokens] -> {:cont, tokens}
              tokens -> parse_expression(tokens)
            end)

          {:cont, tokens, {:call, ident, exprs}}

        result ->
          result
      end
    end

    defp parse_index_expression(tokens) do
      case parse_primary_expression(tokens) do
        {_, [:"[" | tokens], target} ->
          {:var_name, _, _} = target
          {_, tokens, expr} = parse_expression(tokens)
          [:"]" | tokens] = tokens
          {:cont, tokens, {:index, target, expr}}

        {_, tokens, target} ->
          {:cont, tokens, target}

        result ->
          result
      end
    end

    defp parse_primary_expression([:"(" | tokens]) do
      {_, tokens, expr} = parse_expression(tokens)
      [:")" | tokens] = tokens
      {:cont, tokens, expr}
    end

    defp parse_primary_expression([{:ident, class}, :., {:ident, prop} | tokens]) do
      {:cont, tokens, {:var_name, prop, class}}
    end

    defp parse_primary_expression([{:ident, name} | tokens]) do
      {:cont, tokens, {:var_name, name, nil}}
    end

    defp parse_primary_expression([{type, value} | tokens])
         when type in [:string, :integer] do
      {:cont, tokens, {:literal, value}}
    end

    defp parse_primary_expression([keyword | tokens])
         when keyword in [:this, :null, true, false] do
      {:cont, tokens, keyword}
    end

    defp parse_primary_expression(tokens) do
      {:halt, tokens}
    end

    defp unwrap_type({:ident, name}), do: name
    defp unwrap_type(:int), do: :int
    defp unwrap_type(:void), do: :void
    defp unwrap_type(:boolean), do: :boolean
    defp unwrap_type(:char), do: :char

    defp parse_identifier_list(tokens) do
      Utils.take_reduce(tokens, fn
        [:";" | tokens] -> {:halt, tokens}
        [:"," | tokens] -> {:cont, tokens}
        [{:ident, name} | tokens] -> {:cont, tokens, name}
      end)
    end
  end

  def compile(fileOrDir) do
    case File.dir?(fileOrDir) do
      true ->
        {:ok, files} = File.ls(fileOrDir)
        files |> Enum.filter(&String.ends_with?(&1, ".vm")) |> Enum.map(&Path.join(fileOrDir, &1))

      false ->
        [fileOrDir]
    end
    |> Stream.each(fn file ->
      Tokenizer.tokenize!(file)
      |> Enum.into([])
      |> Parser.parse_tokens!()
      |> IO.inspect()
    end)
    |> Stream.run()
  end
end
