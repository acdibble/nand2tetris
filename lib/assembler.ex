defmodule Assembler do
  def assemble(path) do
    {symbols, _} =
      File.stream!(path)
      |> Stream.map(&String.trim/1)
      |> Enum.reduce({%{}, 0}, fn line, {table, pc} ->
        case assemble_instruction(line) do
          {:ok, _} -> {table, pc + 1}
          :empty -> {table, pc}
          {:label, name} -> {Map.put(table, name, pc), pc}
          {:variable, _} -> {table, pc + 1}
        end
      end)

    File.stream!(path)
    |> Stream.map(&String.trim/1)
    |> Stream.transform({0x10, %{}}, fn line, {ram, vars} ->
      case assemble_instruction(line) do
        {:ok, bin} ->
          {[bin], {ram, vars}}

        :empty ->
          {[], {ram, vars}}

        {:label, _} ->
          {[], {ram, vars}}

        {:variable, name} ->
          case Map.get(symbols, name) || Map.get(vars, name) do
            nil ->
              {
                [int_to_a_inst(ram) |> elem(1)],
                {ram + 1, Map.put(vars, name, ram)}
              }

            int ->
              {[int_to_a_inst(int) |> elem(1)], {ram, vars}}
          end
      end
    end)
  end

  def assemble_instruction("//" <> _), do: :empty
  def assemble_instruction(""), do: :empty

  def assemble_instruction("(" <> label) do
    label = String.trim_trailing(label, ")")

    {:label, label}
  end

  Enum.each(0..15, fn n ->
    def assemble_instruction(unquote("@R#{n}")), do: int_to_a_inst(unquote(n))
  end)

  def assemble_instruction("@SP"), do: int_to_a_inst(0)
  def assemble_instruction("@LCL"), do: int_to_a_inst(1)
  def assemble_instruction("@ARG"), do: int_to_a_inst(2)
  def assemble_instruction("@THIS"), do: int_to_a_inst(3)
  def assemble_instruction("@THAT"), do: int_to_a_inst(4)
  def assemble_instruction("@SCREEN"), do: int_to_a_inst(0x4000)
  def assemble_instruction("@KBD"), do: int_to_a_inst(0x6000)

  def assemble_instruction("@" <> number) do
    case Integer.parse(number) do
      {num, ""} -> int_to_a_inst(num)
      :error -> {:variable, number}
    end
  end

  def assemble_instruction(inst) do
    {dest, rest} = parse_dest(inst)
    {comp, jump} = parse_jump(rest)
    a = String.contains?(comp, "M") |> to_bit()
    comp = String.replace(comp, "M", "A")

    bin = "111" <> a <> assemble_comp(comp) <> assemble_dest(dest) <> assemble_jump(jump)

    {:ok, bin}
  end

  defp parse_dest(line) do
    case String.split(line, "=") do
      [rest] -> {nil, rest}
      [dest, rest] -> {dest, rest}
    end
  end

  defp parse_jump(line) do
    case String.split(line, ";") do
      [comp] -> {comp, nil}
      [comp, jump] -> {comp, jump}
    end
  end

  defp assemble_comp("0"), do: "101010"
  defp assemble_comp("1"), do: "111111"
  defp assemble_comp("-1"), do: "111010"
  defp assemble_comp("D"), do: "001100"
  defp assemble_comp("A"), do: "110000"
  defp assemble_comp("!D"), do: "001101"
  defp assemble_comp("!A"), do: "110001"
  defp assemble_comp("-D"), do: "001111"
  defp assemble_comp("-A"), do: "110011"
  defp assemble_comp("D+1"), do: "011111"
  defp assemble_comp("A+1"), do: "110111"
  defp assemble_comp("D-1"), do: "001110"
  defp assemble_comp("A-1"), do: "110010"
  defp assemble_comp("D+A"), do: "000010"
  defp assemble_comp("D-A"), do: "010011"
  defp assemble_comp("A-D"), do: "000111"
  defp assemble_comp("D&A"), do: "000000"
  defp assemble_comp("D|A"), do: "010101"

  defp assemble_jump(nil), do: "000"
  defp assemble_jump("JGT"), do: "001"
  defp assemble_jump("JEQ"), do: "010"
  defp assemble_jump("JGE"), do: "011"
  defp assemble_jump("JLT"), do: "100"
  defp assemble_jump("JNE"), do: "101"
  defp assemble_jump("JLE"), do: "110"
  defp assemble_jump("JMP"), do: "111"

  defp assemble_dest(dest) do
    dest = dest || ""

    ["A", "D", "M"]
    |> Enum.map(&(String.contains?(dest, &1) |> to_bit()))
    |> Enum.join()
  end

  defp to_bit(true), do: "1"
  defp to_bit(false), do: "0"

  defp int_to_a_inst(inst) do
    {:ok, Integer.to_string(inst, 2) |> String.pad_leading(16, "0")}
  end
end
