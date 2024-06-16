defmodule Compiler.Symbols do
  alias __MODULE__, as: Symbols

  defstruct current_class: nil, classes: %{}, class_scope: %{}, method_scope: %{}, label: 0

  @os_classes %{
    "Array" => %{
      "dispose" => {:void, :method},
      "new" => {"Array", :constructor},
    },
    "Keyboard" => %{
      "init" => {:void, :function},
      "keyPressed" => {:char, :function},
      "readChar" => {:char, :function},
      "readInt" => {:int, :function},
      "readLine" => {"String", :function},
    },
    "Math" => %{
      "abs" => {:int, :function},
      "divide" => {:int, :function},
      "init" => {:void, :function},
      "max" => {:int, :function},
      "min" => {:int, :function},
      "multiply" => {:int, :function},
      "sqrt" => {:int, :function},
    },
    "Memory" => %{
      "init" => {:void, :function},
      "peek" => {:int, :function},
      "poke" => {:void, :function},
      "alloc" => {"Array", :function},
      "deAlloc" => {:void, :function},
    },
    "Output" => %{
      "init" => {:void, :function},
      "moveCursor" => {:void, :function},
      "printChar" => {:void, :function},
      "printString" => {:void, :function},
      "printInt" => {:void, :function},
      "println" => {:void, :function},
      "backSpace" => {:void, :function},
    },
    "Screen" => %{
      "init" => {:void, :function},
      "clearScreen" => {:void, :function},
      "setColor" => {:void, :function},
      "drawPixel" => {:void, :function},
      "drawLine" => {:void, :function},
      "drawRectangle" => {:void, :function},
      "drawCircle" => {:void, :function},
    },
    "String" => %{
      "appendChar" => {"String", :method},
      "backSpace" => {:char, :function},
      "charAt" => {:char, :method},
      "dispose" => {:void, :method},
      "doubleQuote" => {:char, :function},
      "eraseLastChar" => {:void, :method},
      "intValue" => {:int, :method},
      "length" => {:int, :method},
      "new" => {"String", :constructor},
      "newLine" => {:char, :function},
      "setCharAt" => {:void, :method},
      "setInt" => {:void, :method},
    },
    "Sys" => %{
      "init" => {:void, :function},
      "halt" => {:void, :function},
      "error" => {:void, :function},
      "wait" => {:void, :function},
    },
  }

  def new(classes) do
    %Symbols{
      classes:
        Enum.into(classes, %{}, fn {:class, class_name, fields, functions} ->
          {
            class_name,
            Enum.into(
              functions,
              %{
                size:
                  Enum.reduce(fields, 0, fn
                    {:field, _type, names}, acc -> acc + length(names)
                    _, acc -> acc
                  end),
              },
              fn {kind, name, type, _, _, _} ->
                {name, {type, kind}}
              end
            )
          }
        end)
        |> Map.merge(@os_classes),
    }
  end

  def set_class(%Symbols{} = symbols, {:class, name, fields, _}) do
    {fields, statics} = Enum.split_with(fields, &(elem(&1, 0) == :field))

    fields =
      Enum.reduce(fields, %{}, fn {:field, type, names}, acc ->
        Enum.reduce(names, acc, fn name, acc ->
          Map.put(acc, name, {type, :this, map_size(acc)})
        end)
      end)

    statics =
      Enum.reduce(statics, %{}, fn {:static, type, names}, acc ->
        Enum.reduce(names, acc, fn name, acc ->
          Map.put(acc, name, {type, :static, map_size(acc)})
        end)
      end)

    %{symbols | current_class: name, class_scope: Map.merge(fields, statics), method_scope: %{}}
  end

  def set_method(%Symbols{} = symbols, {type, _name, _return, params, locals, _stmts}) do
    offset = if(type == :method, do: 1, else: 0)

    args =
      Enum.reduce(params, %{}, fn {:param, type, name}, acc ->
        Map.put(acc, name, {type, :argument, offset + map_size(acc)})
      end)

    locals =
      Enum.reduce(locals, %{}, fn {:var, type, names}, acc ->
        Enum.reduce(names, acc, fn name, acc ->
          Map.put(acc, name, {type, :local, map_size(acc)})
        end)
      end)

    %{symbols | method_scope: Map.merge(args, locals)}
  end

  def get_method_type(%Symbols{} = symbols, method, nil) do
    Map.get(symbols.classes, symbols.current_class)
    |> Map.get(method)
  end

  def get_method_type(%Symbols{} = symbols, method, obj) do
    class = get_class_name(symbols, obj)

    Map.get(symbols.classes, class) |> Map.get(method)
  end

  def get_method_name(%Symbols{} = symbols, nil, method) do
    symbols.current_class <> "." <> method
  end

  def get_method_name(%Symbols{} = symbols, obj, method) do
    class = get_class_name(symbols, obj)
    class <> "." <> method
  end

  def get_variable(%Symbols{} = symbols, name) do
    Map.get(symbols.method_scope, name) || Map.get(symbols.class_scope, name)
  end

  def inc_label(%Symbols{} = symbols) do
    {symbols.label, %Symbols{symbols | label: symbols.label + 1}}
  end

  defp get_class_name(%Symbols{} = symbols, obj) do
    case Map.has_key?(symbols.classes, obj) do
      true ->
        obj

      false ->
        {type, _, _} = get_variable(symbols, obj)
        type
    end
  end

  def get_size(%Symbols{} = symbols, class) do
    Map.get(symbols.classes, class) |> Map.get(:size)
  end
end
