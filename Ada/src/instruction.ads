with Ada.Text_IO; use Ada.Text_IO;

with MOS; use MOS;

package Instruction is

type Fun_T is access procedure(This : in out MOS_T);

type Fun_Array is array(Byte_T range <>) of Fun_T;

function Initialize_Instruction_Table return Fun_Array;

private 
    procedure Not_Yet_Supported_Operation(This : in out MOS_T);
    procedure And_Imm(This : in out MOS_T);
end Instruction;
