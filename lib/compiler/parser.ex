defmodule Compiler.Parser do
  def parse_tokens!(tokens) do
    {[:eof], [class]} = Utils.take_reduce(tokens, &parse_class/1)
    class
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

    {locals, body} = Enum.split_while(stmts, &(elem(&1, 0) == :var))

    {:cont, tokens, {type, name, return, params, locals, body}}
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
