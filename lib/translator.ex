defmodule Translator do
  defmodule Parser do
    def parse!(file) do
      File.stream!(file, :line)
      |> Stream.map(&String.trim/1)
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

    defp parse_line("push constant " <> value) do
      {num, ""} = Integer.parse(value)
      {:push, :constant, num}
    end

    defp line_empty?("//" <> _), do: true
    defp line_empty?(""), do: true
    defp line_empty?(_), do: false
  end

  defmodule Writer do
    def write!(stream) do
      Stream.transform(stream, %{jumps: 0}, &write_op/2)
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
        "M=D" | increment_sp()
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
  end

  def translate(file) do
    Parser.parse!(file)
    |> Writer.write!()
  end
end
