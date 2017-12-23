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
    
    -----------------------------------------
    -- ADC FUNCTIONS
    -----------------------------------------
    procedure Adc_Immediate (This : in out MOS_T) is
        C : Byte_T := This.Mem(This.PC+1);
    begin
        This.A := This.A and C;
	end Adc_Immediate;
    
    procedure Adc_Absolute (This : in out MOS_T) is
    begin
        null;
    end Adc_Absolute;
    
    procedure Adc_Zero_Page (This : in out MOS_T) is
    begin
        null;
    end Adc_Zero_Page;
    
    procedure Adc_Absolute_X (This : in out MOS_T) is
    begin
        null;
    end Adc_Absolute_X;
    
    procedure Adc_Absolute_Y (This : in out MOS_T) is
    begin
        null;
    end Adc_Absolute_Y;
    
    procedure Adc_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
        null;
    end Adc_Zero_Page_X_Indexed_Indirect;
    
    procedure Adc_Zero_Page_X_Indirect_Indexed  (This : in out MOS_T) is
    begin
        null;
    end Adc_Zero_Page_X_Indirect_Indexed;
    
    -----------------------------------------
    -- AND FUNCTIONS
    -----------------------------------------
    procedure And_Immediate (This : in out MOS_T) is
        C : Byte_T := This.Mem(This.PC+1);
    begin
        This.A := This.A and C;
        
	end And_Immediate;
    
    procedure And_Absolute (This : in out MOS_T) is
    begin
        null;
    end And_Absolute;
    
    procedure And_Zero_Page (This : in out MOS_T) is
    begin
        null;
    end And_Zero_Page;
    
    procedure And_Absolute_X (This : in out MOS_T) is
    begin
        null;
    end And_Absolute_X;
    
    procedure And_Absolute_Y (This : in out MOS_T) is
    begin
        null;
    end And_Absolute_Y;
    
    procedure And_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
        null;
    end And_Zero_Page_X_Indexed_Indirect;
    
    procedure And_Zero_Page_X_Indirect_Indexed (This : in out MOS_T) is
    begin
        null;
    end And_Zero_Page_X_Indirect_Indexed;
    
    
    
    

end Instruction;
