with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package MOS is
	type Byte_T is mod 2**8; --[0,255]
	type Short_T is mod 2**16; -- 2 Byte

    MOS_START_PC : constant Short_T := 16#600#;
	MOS_MAX_MEMORY : constant Short_T := 16#1000#;
    MOS_MAX_PROGRAM_SIZE : constant Short_T := MOS_MAX_MEMORY - 1 - MOS_START_PC;

	-- the Range of the stack pointer is fixed!!
	-- otherwise an exception will be thrown. Thank You Ada :)
	type StackPointer_T is range 16#100#..16#1FF#;

	type Program_T is array(Short_T range <>) of Byte_T;
	-- Initializing RAM
	type Ram_T is array(Short_T range <>) of Byte_T;
	
	type MOS_T is tagged

	record
		-- Registers
		A  : Byte_T  := 0; -- Accumulator
		X  : Byte_T  := 0; -- Index Register X
		Y  : Byte_T  := 0; -- Index Register Y
		P  : Byte_T  := 0; -- Process Status Register
		PC : Short_T := MOS_START_PC; -- Program Coutner
		S  : StackPointer_T := 16#1FF#; -- Stack Pointer from 0x1FF to 0x100
		Mem : Ram_T(0..MOS_MAX_MEMORY-1) := (others => 0);
	end record;
	
	


	-- type MOS_T_REF is access all MOS_T'Class;

	procedure Load_Program_Into_Memory ( This : in out MOS_T;
								  Program_Path : in String);
	
	
	
    procedure Put_Hex(Num : in Integer);
    procedure Print_Status (This : in MOS_T);
	procedure Print_Memory (This : in MOS_T;
							Interval_Start : in Short_T;
							Interval_End : in Short_T);
	procedure Put_Register(Name : in String; Val : in Integer);

	-- This low level/debug functions must be only accessible withing the package
	private
    function Read_File(File_Name : String) return Program_T;
    
    
	-----------------------------------------
	-- Debug Functions
	-----------------------------------------
	
	


end MOS;
