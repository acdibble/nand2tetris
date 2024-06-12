defmodule Disassembler do
  def disassemble("0" <> rest) do
    {int, ""} = Integer.parse(rest, 2)
    "@#{int}"
  end

  def disassemble(<<"111", comp::binary-size(7), dest::binary-size(3), jmp::binary-size(3)>>) do
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

"""
0011000000111001
1110110000010000
0101101110100000
1110000111110000
0000001111101011
1110001100001000
0000001111101100
1110001110011000
0000001111101000
1111010011110000
0000000000001110
1110001100000100
0000001111100111
1111110111100000
1110001100101000
0000000000010101
1110011111000010
0000000000000010
1110000010111000
1111110111001000
1111110010101000
0000001111101000
1110111010010000
1110001100000001
1110001100000010
1110001100000011
1110001100000100
1110001100000101
1110001100000110
1110001100000111
1110101010010000
1110001100000001
1110001100000010
1110001100000011
1110001100000100
1110001100000101
1110001100000110
1110001100000111
1110111111010000
1110001100000001
1110001100000010
1110001100000011
1110001100000100
1110001100000101
1110001100000110
1110001100000111
1110001100000111
0111111111111111
"""
|> String.trim()
|> String.split("\n")
|> Enum.with_index(fn bin, index ->
  Disassembler.disassemble(bin)
  |> IO.inspect(label: bin <> " " <> (Integer.to_string(index) |> String.pad_leading(2, " ")))
end)
