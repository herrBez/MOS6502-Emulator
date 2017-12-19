with Ada.Text_IO; use Ada.Text_IO;
with MOS; use MOS;
with Instruction; use Instruction;


package Main_Loop is
    procedure Main_Loop_f (This : in out MOS_T; Instruction : in Fun_Array);
end Main_Loop;
