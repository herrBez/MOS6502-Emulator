with Ada.Text_IO; use Ada.Text_IO;
with MOS; use MOS;
with Instruction; use Instruction;
with Ada.Real_Time; 

package Main_Loop is
    use type Ada.Real_Time.Time;
    use type Ada.Real_Time.Time_Span;
    The_Clock : Ada.Real_Time.Time := Ada.Real_Time.Clock;
    procedure Main_Loop_f (This : in out MOS_T; Instruction : in Fun_Array);
end Main_Loop;
