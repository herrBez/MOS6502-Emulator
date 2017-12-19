with Ada.Text_IO; use Ada.Text_IO;
with mos; use mos;
with Ada.Command_Line; use Ada.Command_Line;

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
		Print_Status(Emulator);
		Load_Program_Into_Memory(Emulator, Program);
		Emulate_Cycle(Emulator);
		Print_Status(Emulator);
	end if;
end Main;
