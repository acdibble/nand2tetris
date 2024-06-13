defmodule AssemblerTest do
  use ExUnit.Case
  doctest Assembler

  def assemble_instruction(str) do
    {:ok, str} = Assembler.assemble_instruction(str)
    str
  end

  def int_to_a_inst(int), do: Integer.to_string(int, 2) |> String.pad_leading(16, "0")

  test "assembles A instructions" do
    assert assemble_instruction("@0") == "0000000000000000"
    assert assemble_instruction("@12345") == "0011000000111001"
    assert assemble_instruction("@32767") == "0111111111111111"

    Enum.each(0..15, &assert(assemble_instruction("@R#{&1}") == int_to_a_inst(&1)))
    assert assemble_instruction("@SP") == int_to_a_inst(0)
    assert assemble_instruction("@LCL") == int_to_a_inst(1)
    assert assemble_instruction("@ARG") == int_to_a_inst(2)
    assert assemble_instruction("@THIS") == int_to_a_inst(3)
    assert assemble_instruction("@THAT") == int_to_a_inst(4)
    assert assemble_instruction("@SCREEN") == int_to_a_inst(0x4000)
    assert assemble_instruction("@KBD") == int_to_a_inst(0x6000)
  end

  test "assembles C instructions" do
    jmps = [
      {"", "000"},
      {";JGT", "001"},
      {";JEQ", "010"},
      {";JGE", "011"},
      {";JLT", "100"},
      {";JNE", "101"},
      {";JLE", "110"},
      {";JMP", "111"}
    ]

    dests = [
      {"", "000"},
      {"M=", "001"},
      {"D=", "010"},
      {"MD=", "011"},
      {"A=", "100"},
      {"AM=", "101"},
      {"AD=", "110"},
      {"AMD=", "111"}
    ]

    ams = [
      {"A", "0"},
      {"M", "1"}
    ]

    for {jm, j} <- jmps, {dm, d} <- dests, {amm, am} <- ams do
      assert assemble_instruction(dm <> "0" <> jm) == "1110101010" <> d <> j
      assert assemble_instruction(dm <> "1" <> jm) == "1110111111" <> d <> j
      assert assemble_instruction(dm <> "-1" <> jm) == "1110111010" <> d <> j
      assert assemble_instruction(dm <> "D" <> jm) == "1110001100" <> d <> j
      assert assemble_instruction(dm <> amm <> jm) == "111" <> am <> "110000" <> d <> j
      assert assemble_instruction(dm <> "!D" <> jm) == "1110001101" <> d <> j

      assert assemble_instruction(dm <> "!" <> amm <> jm) ==
               "111" <> am <> "110001" <> d <> j

      assert assemble_instruction(dm <> "-D" <> jm) == "1110001111" <> d <> j

      assert assemble_instruction(dm <> "-" <> amm <> jm) ==
               "111" <> am <> "110011" <> d <> j

      assert assemble_instruction(dm <> "D+1" <> jm) == "1110011111" <> d <> j

      assert assemble_instruction(dm <> amm <> "+1" <> jm) ==
               "111" <> am <> "110111" <> d <> j

      assert assemble_instruction(dm <> "D-1" <> jm) == "1110001110" <> d <> j

      assert assemble_instruction(dm <> amm <> "-1" <> jm) ==
               "111" <> am <> "110010" <> d <> j

      assert assemble_instruction(dm <> "D+" <> amm <> jm) ==
               "111" <> am <> "000010" <> d <> j

      assert assemble_instruction(dm <> "D-" <> amm <> jm) ==
               "111" <> am <> "010011" <> d <> j

      assert assemble_instruction(dm <> amm <> "-D" <> jm) ==
               "111" <> am <> "000111" <> d <> j

      assert assemble_instruction(dm <> "D&" <> amm <> jm) ==
               "111" <> am <> "000000" <> d <> j

      assert assemble_instruction(dm <> "D|" <> amm <> jm) ==
               "111" <> am <> "010101" <> d <> j
    end
  end
end
