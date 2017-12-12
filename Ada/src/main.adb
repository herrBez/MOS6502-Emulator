with Ada.Text_IO; use Ada.Text_IO;
with mos; use mos;
with Ada.Command_Line; use Ada.Command_Line;

procedure Main is
	Game : Game_T(0..1) := (16#30#, 16#30#);
	Emulator : MOS_T;
begin
	Put(Command_Name);
	Put(Argument_Count);
	PrintStatus(Emulator);
	LoadGameIntoMemory(Emulator, Game);
	EmulateCycle(Emulator);
	PrintStatus(Emulator);
end Main;
