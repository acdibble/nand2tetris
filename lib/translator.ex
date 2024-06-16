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
      [type, num] = String.split(rest)
      type = get_reg_type(type)
      num = parse_int!(num)
      {:push, type, num}
    end

    defp parse_line("pop " <> rest) do
      [type, num] = String.split(rest)

      type = get_reg_type(type)

      num = parse_int!(num)

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

    defp parse_line("function " <> rest) do
      [name, locals] = String.split(rest)

      locals = parse_int!(locals)

      {:function, name, locals}
    end

    defp parse_line("return"), do: :return

    defp parse_line("call " <> rest) do
      [name, arity] = String.split(rest)

      arity = parse_int!(arity)

      {:call, name, arity}
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

    defp parse_int!(string, base \\ 10) do
      {int, ""} = Integer.parse(string, base)
      int
    end
  end

  defmodule Writer do
    defmodule Frame do
      defstruct name: nil, jumps: 0, returns: 0, enclosing: nil, labels: [], locals: []

      def new(name) when is_binary(name) do
        %Frame{name: name, jumps: 0, enclosing: nil}
      end

      def nest(%Frame{} = frame, name) do
        %Frame{name: name, enclosing: frame}
      end

      def unnest(%Frame{} = frame) do
        case frame.labels do
          [] -> frame.enclosing
          _ -> frame
        end
      end

      def jump_labels(%Frame{} = frame) do
        jump_count = Integer.to_string(frame.jumps)

        {
          label(frame, "if_true_" <> jump_count),
          label(frame, "if_done_" <> jump_count),
          Map.update!(frame, :jumps, &(&1 + 1))
        }
      end

      def return_label(%Frame{} = frame, name) do
        {
          label(frame, "return_" <> name <> "_" <> Integer.to_string(frame.returns)),
          Map.update!(frame, :returns, &(&1 + 1))
        }
      end

      def label(%Frame{} = frame, name), do: frame.name <> "$" <> name

      def track_label(%Frame{} = frame, name) do
        {
          label(frame, name),
          %Frame{frame | labels: [name | frame.labels]}
        }
      end

      def untrack_label(%Frame{} = frame, name) do
        {
          label(frame, name),
          %Frame{frame | labels: Enum.reject(frame.labels, &(&1 == name))}
        }
      end
    end

    def write!(stream, static_name) do
      Stream.transform(
        stream,
        %{
          static_name: static_name,
          frame: Frame.new("global"),
        },
        &write_op/2
      )
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
          end,
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
          end,
        ]
        |> decrement_sp()

      {ops, state}
    end

    defp write_op({:push, :constant, num}, state) do
      {push_constant("@#{num}"), state}
    end

    defp write_op({:push, :temp, offset}, state) do
      {get_temp_reg(offset) |> push_aliased_register(), state}
    end

    defp write_op({:push, :pointer, offset}, state) do
      {get_pointer_reg(offset) |> push_aliased_register(), state}
    end

    defp write_op({:push, :static, offset}, state) do
      {get_static_reg(offset, state) |> push_aliased_register(), state}
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

    defp write_op({:comparison, type}, state = %{frame: frame}) do
      {if_true, done, frame} = Frame.jump_labels(frame)

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
        "@" <> done,
        "0;JMP",
        "(" <> if_true <> ")",
        "@SP",
        "A=M-1",
        "M=-1",
        "(" <> done <> ")",
      ]

      ops =
        [
          "A=M",
          "D=M",
          "A=A-1"
          | jmp_ops
        ]
        |> decrement_sp()

      {ops, %{state | frame: frame}}
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
          "M=D",
        ]
        |> decrement_sp()

      {ops, state}
    end

    defp write_op({:label, label}, state) do
      {label, frame} = Frame.track_label(state[:frame], label)

      {
        ["(" <> label <> ")"],
        %{state | frame: frame}
      }
    end

    defp write_op({:goto, :jne, label}, state) do
      {label, frame} = Frame.untrack_label(state[:frame], label)

      ops =
        [
          "A=M",
          "D=M",
          "@#{label}",
          "D;JNE",
        ]
        |> decrement_sp()

      {ops, %{state | frame: frame}}
    end

    defp write_op({:goto, :jmp, label}, state) do
      {label, frame} = Frame.untrack_label(state[:frame], label)

      ops =
        [
          "@#{label}",
          "0;JMP",
        ]

      {ops, %{state | frame: frame}}
    end

    defp write_op({:function, name, locals}, state) do
      ops = [
        "(" <> name <> ")"
        | Enum.take(0..locals, locals)
          |> Enum.flat_map(fn _ ->
            [
              "@SP",
              "A=M",
              "M=0"
              | increment_sp()
            ]
          end),
      ]

      state = %{state | frame: Frame.nest(state[:frame], name)}
      {ops, state}
    end

    defp write_op(:return, state) do
      ops = [
        # save LCL for later
        "@LCL",
        "D=M",
        "@R13",
        "M=D",
        # save return address for later
        "@5",
        "A=D-A",
        "D=M",
        "@R14",
        "M=D",
        # move the return value to the callee's call stack
        "@SP",
        "A=M-1",
        "D=M",
        "@ARG",
        "A=M",
        "M=D",
        # increment callee's call stack
        "@ARG",
        "D=M+1",
        "@SP",
        "M=D",
        # restore THAT
        "@R13",
        "A=M",
        "A=A-1",
        "D=M",
        "@THAT",
        "M=D",
        # restore THIS
        "@R13",
        "D=M",
        "@2",
        "A=D-A",
        "D=M",
        "@THIS",
        "M=D",
        # restore ARG
        "@R13",
        "D=M",
        "@3",
        "A=D-A",
        "D=M",
        "@ARG",
        "M=D",
        # restore LCL
        "@R13",
        "D=M",
        "@4",
        "A=D-A",
        "D=M",
        "@LCL",
        "M=D",
        # jump back
        "@R14",
        "A=M",
        "0;JMP",
      ]

      state = %{state | frame: Frame.unnest(state[:frame])}

      {ops, state}
    end

    defp write_op({:call, name, arity}, state) do
      {return, frame} = Frame.return_label(state[:frame], name)

      ops =
        push_constant("@" <> return) ++
          push_aliased_register("@LCL") ++
          push_aliased_register("@ARG") ++
          push_aliased_register("@THIS") ++
          push_aliased_register("@THAT") ++
          [
            "@SP",
            "D=M",
            "@LCL",
            "M=D",
            "@#{arity + 5}",
            "D=D-A",
            "@ARG",
            "M=D",
            "@" <> name,
            "0;JMP",
            "(" <> return <> ")",
          ]

      {ops, Map.put(state, :frame, frame)}
    end

    defp push_constant(value) do
      [
        value,
        "D=A",
        "@SP",
        "A=M",
        "M=D"
        | increment_sp()
      ]
    end

    defp increment_sp() do
      [
        "@SP",
        "M=M+1",
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

    defp push_aliased_register(reg) do
      [
        reg,
        "D=M",
        "@SP",
        "A=M",
        "M=D"
        | increment_sp()
      ]
    end

    defp pop_aliased_register(reg, state) do
      ops =
        [
          "A=M",
          "D=M",
          reg,
          "M=D",
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

    def bootstrap do
      {ops, _} = write_op({:call, "Sys.init", 0}, %{frame: Frame.new("global")})

      [
        "@256",
        "D=A",
        "@SP",
        "M=D"
        | ops
      ]
    end
  end

  def translate(fileOrDir) do
    case File.dir?(fileOrDir) do
      true ->
        {:ok, files} = File.ls(fileOrDir)

        files =
          files
          |> Enum.filter(&String.ends_with?(&1, ".vm"))
          |> Enum.map(&Path.join(fileOrDir, &1))

        [Writer.bootstrap()] ++ files

      false ->
        [fileOrDir]
    end
    |> Stream.flat_map(fn
      list when is_list(list) ->
        list

      file when is_binary(file) ->
        [static_name | _] = Path.basename(file) |> String.split(".")

        Parser.parse!(file)
        |> Writer.write!(static_name)
    end)
  end
end
