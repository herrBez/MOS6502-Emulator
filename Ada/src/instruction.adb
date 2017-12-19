package body Instruction is
    function Initialize_Instruction_Table return Fun_Array is
        Instruction_Table : Fun_Array(0..255) := (others => Not_Yet_Supported_Operation'Access);
    begin
        return Instruction_Table;
    end Initialize_Instruction_Table;
    
    
    procedure Not_Yet_Supported_Operation(This : in out MOS_T) is
    begin
        Put("Operation");
        Put_Hex(Integer(This.Mem(This.PC)));
        Put("Not Supported");
		New_Line;
    end Not_Yet_Supported_Operation;
    
    procedure And_Imm(This : in out MOS_T) is
        C : Byte_T := This.Mem(This.PC+1);
    begin
        This.A := This.A and C;
	end And_Imm;

end Instruction;
