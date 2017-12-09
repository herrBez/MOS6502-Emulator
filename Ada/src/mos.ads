with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package MOS is
   type Byte_T is mod 2**8; --[0,255]
   type Short_T is mod 2**16; -- 2 Byte


   MOS_MAX_MEMORY : Short_T := 16#1000#;

   -- the Range of the stack pointer is fixed!!
   -- otherwise an exception will be thrown. Thank You Ada :)
   type StackPointer_T is range 16#100#..16#1FF#;

   type Game_T is array(Short_T range <>) of Byte_T;
   -- Initializing RAM
   type Ram_T is array(Short_T range <>) of Byte_T;
   -- declare your program here. Now it is static
   -- TODO read from file


   type MOS_T is tagged
      record
         -- Registers
         A  : Byte_T  := 0; -- Accumulator
         X  : Byte_T  := 0; -- Index Register X
         Y  : Byte_T  := 0; -- Index Register Y
         P  : Byte_T  := 0; -- Process Status Register
         PC : Short_T := 16#600#; -- Program Coutner
         S  : StackPointer_T := 16#1FF#; -- Stack Pointer from 0x1FF to 0x100
         Mem : Ram_T(0..MOS_MAX_MEMORY-1) := (others => 0);

      end record;

   -- type MOS_T_REF is access all MOS_T'Class;

   procedure LoadGameIntoMemory ( This : in out MOS_T;
                                  Game : in Game_T);
   procedure PrintStatus (This : in MOS_T);
   procedure PrintMemory (This : in MOS_T;
                          IntervalStart : in Short_T;
                          IntervalEnd : in Short_T);


   procedure EmulateCycle (This : in out MOS_T);
   procedure MainLoop (This : in MOS_T);

   type Fun is access procedure(This : in out MOS_T);
   -- This low level debug functions must be only accessible withing the package
   private
   procedure PutHex(Num : in Integer);
   procedure PutRegister(Name : in String; Val : in Integer);

   procedure AndImm(This : in out MOS_T);

end MOS;
