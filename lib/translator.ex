defmodule Translator do
  defmodule Parser do
    def parse!(file) do
      File.stream!(file, :line)
      |> Stream.map(&remove_comments/1)
      |> Stream.reject(&line_empty?/1)
      |> Stream.map(&parse_line/1)
    end

    defp parse_line("neg"), do: {:unary, :neg}
    defp parse_line("not"), do: {:unary, :not}

    defp parse_line("add"), do: {:arithmetic, :add}
    defp parse_line("sub"), do: {:arithmetic, :sub}
    defp parse_line("and"), do: {:arithmetic, :and}
    defp parse_line("or"), do: {:arithmetic, :or}

    defp parse_line("eq"), do: {:comparison, :eq}
    defp parse_line("lt"), do: {:comparison, :lt}
    defp parse_line("gt"), do: {:comparison, :gt}

    defp parse_line("push " <> rest) do
      [type, num] = String.split(rest, " ")
      type = get_reg_type(type)
      {num, ""} = Integer.parse(num)
      {:push, type, num}
    end

    defp parse_line("pop " <> rest) do
      [type, num] = String.split(rest, " ")

      type = get_reg_type(type)

      {num, ""} = Integer.parse(num)

      {:pop, type, num}
    end

    defp parse_line("label " <> label) do
      {:label, label}
    end

    defp parse_line("if-goto " <> label) do
      {:goto, :jne, label}
    end

    defp parse_line("goto " <> label) do
      {:goto, :jmp, label}
    end

    defp get_reg_type(type) do
      case type do
        "local" -> :local
        "argument" -> :arg
        "this" -> :this
        "that" -> :that
        "temp" -> :temp
        "constant" -> :constant
        "pointer" -> :pointer
        "static" -> :static
      end
    end

    defp remove_comments(line) do
      String.split(line, "//") |> Enum.at(0) |> String.trim()
    end

    defp line_empty?(""), do: true
    defp line_empty?(_), do: false
  end

  defmodule Writer do
    def write!(stream, static_name) do
      Stream.transform(stream, %{jumps: 0, static_name: static_name}, &write_op/2)
    end

    defp write_op({:unary, op}, state) do
      ops =
        [
          "@SP",
          "A=M",
          "A=A-1",
          case op do
            :not -> "M=!M"
            :neg -> "M=-M"
          end
        ]

      {ops, state}
    end

    defp write_op({:arithmetic, op}, state) do
      ops =
        [
          "A=M",
          "D=M",
          "A=A-1",
          case op do
            :add -> "M=D+M"
            :sub -> "M=M-D"
            :and -> "M=D&M"
            :or -> "M=D|M"
          end
        ]
        |> decrement_sp()

      {ops, state}
    end

    defp write_op({:push, :constant, num}, state) do
      ops = [
        "@#{num}",
        "D=A",
        "@SP",
        "A=M",
        "M=D"
        | increment_sp()
      ]

      {ops, state}
    end

    defp write_op({:push, :temp, offset}, state) do
      get_temp_reg(offset) |> push_aliased_register(state)
    end

    defp write_op({:push, :pointer, offset}, state) do
      get_pointer_reg(offset) |> push_aliased_register(state)
    end

    defp write_op({:push, :static, offset}, state) do
      get_static_reg(offset, state) |> push_aliased_register(state)
    end

    defp write_op({:push, type, offset}, state) do
      reg = get_special_reg(type)

      ops = [
        reg,
        "D=M",
        "@#{offset}",
        "A=D+A",
        "D=M",
        "@SP",
        "A=M",
        "M=D"
        | increment_sp()
      ]

      {ops, state}
    end

    defp write_op({:comparison, type}, state) do
      if_true = "jmp_if_true_#{state[:jumps]}"
      done_label = "jmp_done_#{state[:jumps]}"

      jmp =
        case type do
          :eq -> "JEQ"
          :lt -> "JLT"
          :gt -> "JGT"
        end

      jmp_ops = [
        "D=M-D",
        "@" <> if_true,
        "D;" <> jmp,
        "@SP",
        "A=M-1",
        "M=0",
        "@" <> done_label,
        "0;JMP",
        "(" <> if_true <> ")",
        "@SP",
        "A=M-1",
        "M=-1",
        "(" <> done_label <> ")"
      ]

      ops =
        [
          "A=M",
          "D=M",
          "A=A-1"
          | jmp_ops
        ]
        |> decrement_sp()

      {ops, Map.update!(state, :jumps, &(&1 + 1))}
    end

    defp write_op({:pop, :temp, offset}, state) do
      get_temp_reg(offset) |> pop_aliased_register(state)
    end

    defp write_op({:pop, :pointer, offset}, state) do
      get_pointer_reg(offset) |> pop_aliased_register(state)
    end

    defp write_op({:pop, :static, offset}, state) do
      get_static_reg(offset, state) |> pop_aliased_register(state)
    end

    defp write_op({:pop, type, offset}, state) do
      ops =
        [
          # save popped value to R13
          "A=M",
          "D=M",
          "@R13",
          "M=D",
          # save dest address to R14
          get_special_reg(type),
          "D=M",
          "@#{offset}",
          "D=D+A",
          "@R14",
          "M=D",
          # save R13 to *R14
          "@R13",
          "D=M",
          "@R14",
          "A=M",
          "M=D"
        ]
        |> decrement_sp()

      {ops, state}
    end

    defp write_op({:label, label}, state), do: {["(" <> label <> ")"], state}

    defp write_op({:goto, :jne, label}, state) do
      ops =
        [
          "A=M",
          "D=M",
          "@#{label}",
          "D;JNE"
        ]
        |> decrement_sp()

      {ops, state}
    end

    defp write_op({:goto, :jmp, label}, state) do
      ops =
        [
          "@#{label}",
          "0;JMP"
        ]

      {ops, state}
    end

    defp increment_sp() do
      [
        "@SP",
        "M=M+1"
      ]
    end

    defp decrement_sp(rest) do
      [
        "@SP",
        "M=M-1"
        | rest
      ]
    end

    defp get_pointer_reg(offset), do: "@R#{3 + offset}"
    defp get_temp_reg(offset), do: "@R#{5 + offset}"
    defp get_static_reg(offset, state), do: "@#{state[:static_name]}.#{offset}"

    defp push_aliased_register(reg, state) do
      ops = [
        reg,
        "D=M",
        "@SP",
        "A=M",
        "M=D"
        | increment_sp()
      ]

      {ops, state}
    end

    defp pop_aliased_register(reg, state) do
      ops =
        [
          "A=M",
          "D=M",
          reg,
          "M=D"
        ]
        |> decrement_sp()

      {ops, state}
    end

    defp get_special_reg(type) do
      case type do
        :local -> "@LCL"
        :arg -> "@ARG"
        :this -> "@THIS"
        :that -> "@THAT"
      end
    end
  end

  def translate(file) do
    [static_name | _] = Path.basename(file) |> String.split(".")

    Parser.parse!(file)
    |> Writer.write!(static_name)
  end
end
