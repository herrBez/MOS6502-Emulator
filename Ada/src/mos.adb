package body MOS is
   -- Load Game into Memory
	procedure Load_Game_Into_Memory(This : in out MOS_T;
								Game : in Game_T) is
	I : Short_T := This.PC;
	begin
		for J in Game'Range loop
		 This.Mem(I+J) := Game(J);
		end loop;
	end Load_Game_Into_Memory;

	procedure Print_Status(This : in MOS_T) is
	begin
		Put_Line("=== Status ===");
		Put_Register("A", Integer(This.A));
		Put_Register("X", Integer(This.X));
		Put_Register("Y", Integer(This.Y));
		Put_Register("PC", Integer(This.PC));
		Put_Register("S", Integer(This.S));
		Print_Memory(This, This.PC-2, This.PC+2);
		Put_Line("=== End ===");
	end Print_Status;
	
	function Read_File(File_Name : String) return Game_T is
		File : File_Type;
		Game : Game_T(0..1) := (others => 0);
		s : Unbounded_String;
		begin
		Open(File => File,
			Mode => In_File,
			Name => File_Name);
		Put("Opened");
		loop
			exit when End_Of_File (File);
			s := To_Unbounded_String(Get_Line (File));
			Put(Integer'Value(To_String(s)));
		end loop;
		Close (File);
		return Game;
	end Read_File;

	
	-- Print Memory from IntervalStart to Interval End
	procedure Print_Memory (This : in MOS_T;
						  Interval_Start : in Short_T;
						  Interval_End : in Short_T) is
	begin
		for K in Interval_Start..Interval_End loop
			Put("Mem[");
			Put_Hex(Integer(K));
			Put("] = ");
			Put_Hex(Integer(This.Mem(K)));
			New_Line;
		end loop;
	end Print_Memory;

	procedure Put_Hex(Num : in Integer) is
	begin
		Put(Num, Base => 16);
	end Put_Hex;

	procedure Put_Register(Name : in String; Val : in Integer) is
	begin
		Put(Name);
		Put_Hex(Val);
		New_Line;
	end Put_Register;



	procedure Emulate_Cycle (This : in out MOS_T) is
		opcode : Byte_T := This.Mem(This.PC);
	begin
		-- Debug information
		Put_Line("Opcode is ");
		Put_Hex(Integer(opcode));
		New_Line;
		This.PC := This.PC + 1;
	end Emulate_Cycle;

	procedure Main_Loop (This : in MOS_T) is

	begin

		null;
	end Main_Loop;

	procedure And_Imm(This : in out MOS_T) is
	begin
		null;
	end And_Imm;

end MOS;
