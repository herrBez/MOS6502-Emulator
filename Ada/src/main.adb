with Ada.Text_IO; use Ada.Text_IO;
with mos; use mos;
with Ada.Command_Line; use Ada.Command_Line;

procedure Main is
	Game : Game_T(0..1) := (16#30#, 16#30#);
	Emulator : MOS_T;
begin
	if Argument_Count < 1 then
		Put("Usage: ");
		Put(Command_Name);
		Put(" <input-file> ");
		New_Line;
	else
		
		Print_Status(Emulator);
		Load_Game_Into_Memory(Emulator, Game);
		Emulate_Cycle(Emulator);
		Print_Status(Emulator);
	end if;
end Main;
