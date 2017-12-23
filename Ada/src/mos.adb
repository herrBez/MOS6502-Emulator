package body MOS is
   -- Load Program into Memory
	procedure Load_Program_Into_Memory(This : in out MOS_T;
								Program_Path : in String) is
    Program : Program_T := Read_File(Program_Path);
	I : Short_T := This.PC;
	begin
		for J in Program'Range loop
		 This.Mem(I+J) := Program(J);
		end loop;
	end Load_Program_Into_Memory;

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
	
	function Read_File(File_Name : String) return Program_T is
		File : File_Type;
		Program : Program_T(0..MOS_MAX_PROGRAM_SIZE) := (others => 0);
		s : Unbounded_String;
		I : Short_T := 0;
		begin
		Open(File => File,
			Mode => In_File,
			Name => File_Name);
		Put("Opened");
        New_Line;
        Put(Integer(MOS_MAX_PROGRAM_SIZE-1));
        New_Line;
		loop
			exit when End_Of_File (File);
            s := To_Unbounded_String(Get_Line (File));
			Put(Integer(I));
            Put_Hex(Integer'Value(To_String(s)));
            New_Line;
			Program(I) := Byte_T(Integer'Value(To_String(s)));
			I := I + 1;
		end loop;
		Close (File);
		return Program;
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



end MOS;
