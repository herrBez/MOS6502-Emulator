package body MOS is
   -- Load Game into Memory
	procedure LoadGameIntoMemory(This : in out MOS_T;
								Game : in Game_T) is
	I : Short_T := This.PC;
	begin
		for J in Game'Range loop
		 This.Mem(I+J) := Game(J);
		end loop;
	end LoadGameIntoMemory;

	procedure PrintStatus(This : in MOS_T) is
	begin
		Put_Line("=== Status ===");
		PutRegister("A", Integer(This.A));
		PutRegister("X", Integer(This.X));
		PutRegister("Y", Integer(This.Y));
		PutRegister("PC", Integer(This.PC));
		PutRegister("S", Integer(This.S));
		PrintMemory(This, This.PC-2, This.PC+2);
		Put_Line("=== End ===");
	end PrintStatus;


	-- Print Memory from IntervalStart to Interval End
	procedure PrintMemory (This : in MOS_T;
						  IntervalStart : in Short_T;
						  IntervalEnd : in Short_T) is
	begin
		for K in IntervalStart..IntervalEnd loop
			Put("Mem[");
			PutHex(Integer(K));
			Put("] = ");
			PutHex(Integer(This.Mem(K)));
			New_Line;
		end loop;
	end PrintMemory;

	procedure PutHex(Num : in Integer) is
	begin
		Put(Num, Base => 16);
	end PutHex;

	procedure PutRegister(Name : in String; Val : in Integer) is
	begin
		Put(Name);
		PutHex(Val);
		New_Line;
	end PutRegister;



	procedure EmulateCycle (This : in out MOS_T) is
		opcode : Byte_T := This.Mem(This.PC);
	begin
		-- Debug information
		Put_Line("Opcode is ");
		PutHex(Integer(opcode));
		New_Line;
		This.PC := This.PC + 1;
	end EmulateCycle;

	procedure MainLoop (This : in MOS_T) is

	begin

		null;
	end MainLoop;

	procedure AndImm(This : in out MOS_T) is
	begin
		null;
	end AndImm;

end MOS;
