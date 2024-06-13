defmodule Disassembler do
  def disassemble(path) do
    File.stream!(path, :line)
    |> Stream.map(&disassemble_instruction/1)
  end

  def disassemble_instruction("0" <> rest) do
    {int, ""} = Integer.parse(rest, 2)
    "@#{int}"
  end

  def disassemble_instruction(
        <<"111", comp::binary-size(7), dest::binary-size(3), jmp::binary-size(3)>>
      ) do
    dissassemble_dest(dest) <> disassemble_comp(comp) <> disassemble_jmp(jmp)
  end

  defp dissassemble_dest("000"), do: ""

  defp dissassemble_dest(bits) do
    String.to_charlist(bits)
    |> Enum.with_index(&disassemble_dest_bit/2)
    |> Enum.join()
    |> case do
      "" -> ""
      dest -> dest <> "="
    end
  end

  defp disassemble_dest_bit(?0, _), do: ""
  defp disassemble_dest_bit(?1, 0), do: "A"
  defp disassemble_dest_bit(?1, 1), do: "D"
  defp disassemble_dest_bit(?1, 2), do: "M"

  defp disassemble_jmp("000"), do: ""

  defp disassemble_jmp(bits) do
    ";" <>
      case bits do
        "001" -> "JGT"
        "010" -> "JEQ"
        "011" -> "JGE"
        "100" -> "JLT"
        "101" -> "JNE"
        "110" -> "JLE"
        "111" -> "JMP"
      end
  end

  defp disassemble_comp("0101010"), do: "0"
  defp disassemble_comp("0111111"), do: "1"
  defp disassemble_comp("0111010"), do: "-1"
  defp disassemble_comp("0001100"), do: "D"
  defp disassemble_comp(<<a::binary-size(1), "110000">>), do: get_register(a)
  defp disassemble_comp("0001101"), do: "!D"
  defp disassemble_comp(<<a::binary-size(1), "110001">>), do: "!" <> get_register(a)
  defp disassemble_comp("0001111"), do: "-D"
  defp disassemble_comp(<<a::binary-size(1), "110011">>), do: "-" <> get_register(a)
  defp disassemble_comp("0011111"), do: "D+1"
  defp disassemble_comp(<<a::binary-size(1), "110111">>), do: get_register(a) <> "+1"
  defp disassemble_comp("0001110"), do: "D-1"
  defp disassemble_comp(<<a::binary-size(1), "110010">>), do: get_register(a) <> "-1"
  defp disassemble_comp(<<a::binary-size(1), "000010">>), do: "D+" <> get_register(a)
  defp disassemble_comp(<<a::binary-size(1), "010011">>), do: "D-" <> get_register(a)
  defp disassemble_comp(<<a::binary-size(1), "000111">>), do: get_register(a) <> "-D"
  defp disassemble_comp(<<a::binary-size(1), "000000">>), do: "D&" <> get_register(a)
  defp disassemble_comp(<<a::binary-size(1), "010101">>), do: "D|" <> get_register(a)

  defp get_register("0"), do: "A"
  defp get_register("1"), do: "M"
end
