defmodule Compiler.Writer do
  alias Compiler.Symbols, as: Symbols

  @binary_ops [:+, :-, :*, :<, :>, :=, :&, :|, :/]

  def write!({:class, _name, _, functions} = class, symbols) do
    symbols = Symbols.set_class(symbols, class)

    Stream.flat_map(functions, &write!(&1, symbols))
  end

  def write!(
        {:method, name, _return, _params, locals, stmts} = fun,
        %Compiler.Symbols{} = symbols
      ) do
    symbols = Symbols.set_method(symbols, fun)

    [
      "function #{symbols.current_class}.#{name} #{locals_length(locals) + 1}",
      "push argument 0",
      "pop pointer 0"
      | Enum.flat_map_reduce(stmts, symbols, &write!/2) |> elem(0),
    ]
  end

  def write!({:function, name, _return, _params, locals, stmts} = fun, %Symbols{} = symbols) do
    symbols = Symbols.set_method(symbols, fun)

    [
      "function #{symbols.current_class}.#{name} #{locals_length(locals)}"
      | Enum.flat_map_reduce(stmts, symbols, &write!/2) |> elem(0),
    ]
  end

  def write!({:constructor, name, return, _params, locals, stmts} = fun, %Symbols{} = symbols) do
    symbols = Symbols.set_method(symbols, fun)
    size = Symbols.get_size(symbols, return)
    {ops, _symbols} = Enum.flat_map_reduce(stmts, symbols, &write!/2)

    [
      "function #{symbols.current_class}.#{name} #{locals_length(locals)}",
      "push constant #{size}",
      "call Memory.alloc 1",
      "pop pointer 0"
      | ops
    ]
  end

  def write!({:do, {:call, {:var_name, method, class}, _} = call}, symbols) do
    {ops, symbols} = write!(call, symbols)

    {return, _} = Symbols.get_method_type(symbols, method, class)

    ops =
      ops ++
        case return do
          :void -> ["pop temp 0"]
          _ -> []
        end

    {ops, symbols}
  end

  def write!({:call, {:var_name, method, obj}, params}, symbols) do
    {_, kind} = Symbols.get_method_type(symbols, method, obj)

    {ops, symbols} = Enum.flat_map_reduce(params, symbols, &write!/2)

    ops =
      case {kind, obj} do
        {:method, nil} ->
          [
            "push pointer 0"
            | ops,
          ]

        {:method, _} ->
          {_type, kind, index} = Symbols.get_variable(symbols, obj)

          [
            "push #{kind} #{index}"
            | ops,
          ]

        _ ->
          ops
      end

    param_count = length(params) + if kind == :method, do: 1, else: 0

    ops =
      ops ++
        [
          "call #{Symbols.get_method_name(symbols, obj, method)} #{param_count}",
        ]

    {ops, symbols}
  end

  def write!({op, left, right}, symbols) when op in @binary_ops do
    {ops, symbols} = Enum.flat_map_reduce([left, right], symbols, &write!/2)

    op =
      case op do
        :+ -> "add"
        :- -> "sub"
        :* -> "call Math.multiply 2"
        :/ -> "call Math.divide 2"
        :< -> "lt"
        :> -> "gt"
        := -> "eq"
        :& -> "and"
        :| -> "or"
      end

    {ops ++ [op], symbols}
  end

  def write!({op, operand}, symbols) when op in [:-, :"~"] do
    ops =
      (write!(operand, symbols) |> elem(0)) ++
        [
          case op do
            :- -> "neg"
            :"~" -> "not"
          end,
        ]

    {ops, symbols}
  end

  def write!({:literal, num}, symbols) when is_integer(num) do
    {["push constant #{num}"], symbols}
  end

  def write!({:literal, string}, symbols) when is_binary(string) do
    {ops, symbols} =
      write!(
        {:call, {:var_name, "new", "String"}, [{:literal, String.length(string)}]},
        symbols
      )

    ops =
      ops ++
        (String.to_charlist(string)
         |> Enum.flat_map(&["push constant #{&1}", "call String.appendChar 2"]))

    {ops, symbols}
  end

  def write!({:let, {:var_name, name, nil}, expr}, symbols) do
    {_type, kind, index} = Symbols.get_variable(symbols, name)

    {ops, symbols} = write!(expr, symbols)

    {ops ++
       [
         "pop #{kind} #{index}",
       ], symbols}
  end

  def write!({:let, {:index, {:var_name, _, _} = var_expr, index_expr}, expr}, symbols) do
    {var_ops, symbols} = write!(var_expr, symbols)
    {index_ops, symbols} = write!(index_expr, symbols)
    {value_ops, symbols} = write!(expr, symbols)

    ops =
      flatten([
        var_ops,
        index_ops,
        "add",
        value_ops,
        "call Memory.poke 2",
      ])

    {ops, symbols}
  end

  def write!({:var_name, name, nil}, symbols) do
    {_type, kind, index} = Symbols.get_variable(symbols, name)

    {
      [
        "push #{kind} #{index}",
      ],
      symbols
    }
  end

  def write!({:while, condition, stmts}, symbols) do
    {cond_label, symbols} = Symbols.inc_label(symbols)
    {done_label, symbols} = Symbols.inc_label(symbols)
    cond_label = "LABEL_#{cond_label}"
    done_label = "LABEL_#{done_label}"

    {cond_ops, symbols} = write!(condition, symbols)
    {body_ops, symbols} = Enum.flat_map_reduce(stmts, symbols, &write!/2)

    ops =
      flatten([
        "label #{cond_label}",
        cond_ops,
        "not",
        "if-goto #{done_label}",
        body_ops,
        "goto #{cond_label}",
        "label #{done_label}",
      ])

    {ops, symbols}
  end

  def write!({:if, condition, then, nil}, symbols) do
    {done_label, symbols} = Symbols.inc_label(symbols)
    done_label = "LABEL_#{done_label}"

    {cond_ops, symbols} = write!(condition, symbols)
    {then_ops, symbols} = Enum.flat_map_reduce(then, symbols, &write!/2)

    ops =
      flatten([
        cond_ops,
        "not",
        "if-goto #{done_label}",
        then_ops,
        "label #{done_label}",
      ])

    {ops, symbols}
  end

  def write!({:if, condition, then, otherwise}, symbols) do
    {else_label, symbols} = Symbols.inc_label(symbols)
    {done_label, symbols} = Symbols.inc_label(symbols)
    else_label = "LABEL_#{else_label}"
    done_label = "LABEL_#{done_label}"

    {cond_ops, symbols} = write!(condition, symbols)
    {then_ops, symbols} = Enum.flat_map_reduce(then, symbols, &write!/2)
    {else_ops, symbols} = Enum.flat_map_reduce(otherwise, symbols, &write!/2)

    ops =
      flatten([
        cond_ops,
        "not",
        "if-goto #{else_label}",
        then_ops,
        "goto #{done_label}",
        "label #{else_label}",
        else_ops,
        "label #{done_label}",
      ])

    {ops, symbols}
  end

  def write!({:return, nil}, symbols), do: write!({:return, {:literal, 0}}, symbols)

  def write!({:return, expr}, symbols) do
    {ops, symbols} = write!(expr, symbols)
    {ops ++ ["return"], symbols}
  end

  # doesn't necessarily have to be a variable as the first expression
  def write!({:index, {:var_name, _, nil} = var_expr, index_expr}, symbols) do
    {var_ops, symbols} = write!(var_expr, symbols)
    {index_ops, symbols} = write!(index_expr, symbols)

    ops =
      flatten([
        var_ops,
        index_ops,
        "add",
        "call Memory.peek 1",
      ])

    {ops, symbols}
  end

  def write!(:this, symbols), do: {["push pointer 0"], symbols}
  def write!(true, symbols), do: {["push constant 0", "not"], symbols}
  def write!(false, symbols), do: {["push constant 0"], symbols}
  def write!(:null, symbols), do: {["push constant 0"], symbols}

  defp locals_length(locals) do
    Enum.reduce(locals, 0, fn {_, _, names}, acc -> acc + length(names) end)
  end

  defp flatten(list, acc \\ [])
  defp flatten([], acc), do: Enum.reverse(acc)

  defp flatten([head | tail], acc) when is_binary(head) do
    flatten(tail, [head | acc])
  end

  defp flatten([[head | rest] | tail], acc) when is_binary(head) do
    flatten([rest | tail], [head | acc])
  end

  defp flatten([[] | tail], acc) do
    flatten(tail, acc)
  end
end
