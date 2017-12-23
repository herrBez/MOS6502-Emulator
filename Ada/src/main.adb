with Ada.Text_IO; use Ada.Text_IO;
with Mos; use Mos;
with Ada.Command_Line; use Ada.Command_Line;
with Instruction; use Instruction;
with Main_Loop; use Main_Loop;




procedure Main is
    Emulator : MOS_T;
begin
	if Argument_Count < 1 then
		Put("Usage: ");
		Put(Command_Name);
		Put(" <input-file> ");
		New_Line;
	else
		Load_Program_Into_Memory(Emulator, Argument(1));
		Main_Loop_f(Emulator, Initialize_Instruction_Table);
	end if;
end Main;
