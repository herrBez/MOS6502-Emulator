with Ada.Text_IO; use Ada.Text_IO;
with mos; use mos;
procedure Main is
   Game : Game_T(0..1) := (16#30#, 16#30#);
   Emulator : MOS_T;
begin
   PrintStatus(Emulator);
   LoadGameIntoMemory(Emulator, Game);
   EmulateCycle(Emulator);
   PrintStatus(Emulator);
end Main;
