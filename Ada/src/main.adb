with Ada.Text_IO; use Ada.Text_IO;
with Mos; use Mos;
with Ada.Command_Line; use Ada.Command_Line;
with Instruction; use Instruction;
with Main_Loop; use Main_Loop;




procedure Main is
	Program : Program_T(0..100);
	Emulator : MOS_T;
begin
	if Argument_Count < 1 then
		Put("Usage: ");
		Put(Command_Name);
		Put(" <input-file> ");
		New_Line;
	else
		Program := Read_File(Argument(1));
		Load_Program_Into_Memory(Emulator, Program);
		Main_Loop_f(Emulator, Initialize_Instruction_Table);
	end if;
end Main;
