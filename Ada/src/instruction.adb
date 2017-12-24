package body Instruction is
    function Initialize_Instruction_Table return Fun_Array is
        Instruction_Table : Fun_Array(0..255) := (others => Not_Yet_Supported_Operation'Access);
    begin
        Instruction_Table(16#EA#) := NOP'Access;
        return Instruction_Table;
    end Initialize_Instruction_Table;
    
    
    procedure Not_Yet_Supported_Operation(This : in out MOS_T) is
    begin
        Put("Operation");
        Put_Hex(Integer(This.Mem(This.PC)));
        Put("Not Supported");
		New_Line;
    end Not_Yet_Supported_Operation;
    
        --------------------------------------------------------------------
    -- ADC FUNCTIONS
    -- Add memory to accumulator with carry
    --------------------------------------------------------------------
    procedure ADC_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ADC_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ADC_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ADC_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ADC_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ADC_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ADC_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- AND FUNCTIONS:
    -- "AND" memory with accumulator
    --------------------------------------------------------------------
    procedure AND_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure AND_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure AND_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure AND_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure AND_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure AND_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure AND_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- ASL:
    -- Shift left one bit (Memory or Accumulator)
    --------------------------------------------------------------------
    procedure ASL_Accumulator (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ASL_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ASL_Zero_Page_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ASL_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ASL_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BCC:
    -- Branch on carry clear
    --------------------------------------------------------------------
    procedure BCC (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BCS:
    -- Branch on carry set
    --------------------------------------------------------------------
    procedure BCS (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BEQ:
    -- Branch on result zero
    --------------------------------------------------------------------
    procedure BEQ (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BIT:
    -- Test bits in memory with accumulator
    --------------------------------------------------------------------
    procedure BIT (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BMI:
    -- Branch on result minus
    --------------------------------------------------------------------
    procedure BMI (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BNE
    -- Branch on result not zero
    --------------------------------------------------------------------
    procedure BNE (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BPL
    -- Branch on result plus
    --------------------------------------------------------------------
    procedure BPL (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BRK
    -- Force Break
    --------------------------------------------------------------------
    procedure BRK (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BVC
    -- Branch on overflow clear
    --------------------------------------------------------------------
    procedure BVC (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- BVS
    -- Branch on overflow set
    --------------------------------------------------------------------
    procedure BVS (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- CLC
    -- Clear carry flag
    --------------------------------------------------------------------
    procedure CLC (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- CLC
    -- Clear decimal mode
    --------------------------------------------------------------------
    procedure CLD (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- CLI
    -- Clear interrupt disable status
    --------------------------------------------------------------------
    procedure CLI (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- CLV
    -- Clear overflow flag
    --------------------------------------------------------------------
    procedure CLV (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- CMP FUNCTIONS
    -- Compare memory and accumulator
    --------------------------------------------------------------------
    procedure CMP_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CMP_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CMP_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CMP_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CMP_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CMP_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CMP_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- CPX FUNCTIONS
    -- Compare memory and index X
    --------------------------------------------------------------------
    procedure CPX_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CPX_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CPX_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- CPY FUNCTIONS
    -- Compare memory and index Y
    --------------------------------------------------------------------
    procedure CPY_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CPY_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure CPY_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- DEC
    -- Decrement memory by one
    --------------------------------------------------------------------
    procedure DEC_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure DEC_Zero_Page_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure DEC_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure DEC_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- DEX
    -- Decrement index X by one
    --------------------------------------------------------------------
    procedure DEX (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- DEY
    -- Decrement index X by one
    --------------------------------------------------------------------
    procedure DEY (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- EOR FUNCTIONS
    -- Exclusive-OR memory and accumulator
    --------------------------------------------------------------------
    procedure EOR_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure EOR_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure EOR_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure EOR_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure EOR_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure EOR_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure EOR_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- INC
    -- Increment memory by one
    --------------------------------------------------------------------
    procedure INC_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure INC_Zero_Page_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure INC_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure INC_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- INX
    -- Increment index X by one
    --------------------------------------------------------------------
    procedure INX (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- INY
    -- Increment index X by one
    --------------------------------------------------------------------
    procedure INY (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- JMP
    -- Jump to new location
    --------------------------------------------------------------------
    procedure JMP_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure JMP_Absolute_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- JSR
    -- Jump to new location saving return address
    --------------------------------------------------------------------
    procedure JSR_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- LDA
    -- Load accumulator with memory
    --------------------------------------------------------------------
    procedure LDA_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDA_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDA_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDA_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDA_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDA_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDA_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- LDX
    -- Load index X with memory
    --------------------------------------------------------------------
    procedure LDX_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDX_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDX_Zero_Page_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDX_Zero_Page_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDX_Zero_Page_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    
    --------------------------------------------------------------------
    -- LDY
    -- Load index Y with memory
    --------------------------------------------------------------------
    procedure LDY_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDY_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDY_Zero_Page_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDY_Zero_Page_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LDY_Zero_Page_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- LSR
    -- Shift right one bit (memory or accumulator)
    --------------------------------------------------------------------
    procedure LSR_Accumulator (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LSR_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LSR_Zero_Page_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LSR_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure LSR_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- NOP
    -- No operation
    --------------------------------------------------------------------
    procedure NOP (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- OR FUNCTIONS:
    -- "OR" memory with accumulator
    --------------------------------------------------------------------
    procedure OR_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure OR_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure OR_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure OR_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure OR_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure OR_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure OR_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- PHA
    -- Push accumulator on stack
    --------------------------------------------------------------------
    procedure PHA (This : in out MOS_T) is
    begin
       null;
    end;
  
    --------------------------------------------------------------------
    -- PHP
    -- Push processor status on stack
    --------------------------------------------------------------------
    procedure PHP (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- PLA
    -- Pull accumulator from stack
    --------------------------------------------------------------------
    procedure PLA (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- PLP
    -- Pull accumulator from stack
    --------------------------------------------------------------------
    procedure PLP (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- ROL
    -- Rotate one bit left (memory or accumulator)
    --------------------------------------------------------------------
    procedure ROL_Accumulator (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROL_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROL_Zero_Page_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROL_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROL_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- ROR
    -- Rotate one bit right (memory or accumulator)
    --------------------------------------------------------------------
    procedure ROR_Accumulator (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROR_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROR_Zero_Page_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROR_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure ROR_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- RTI:
    -- return from interrupt
    --------------------------------------------------------------------
    procedure RTI (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- RTS:
    -- return from subroutine
    --------------------------------------------------------------------
    procedure RTS (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- SBC:
    -- Subtract memory from accumulator with borrow
    --------------------------------------------------------------------
    procedure SBC_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure SBC_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure SBC_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure SBC_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure SBC_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure SBC_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure SBC_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- SEC
    -- Set carry flag
    --------------------------------------------------------------------
    procedure SEC (This : in out MOS_T) is
    begin
       null;
    end;
  
    --------------------------------------------------------------------
    -- SED
    -- Set decimal mode
    --------------------------------------------------------------------
    procedure SED (This : in out MOS_T) is
    begin
       null;
    end;
   
    --------------------------------------------------------------------
    -- SEI
    -- Set interrupt disable status
    --------------------------------------------------------------------
    procedure SEI (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- STA:
    -- Stone accumulator in memory
    --------------------------------------------------------------------
    procedure STA_Immediate (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STA_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STA_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STA_Absolute_X (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STA_Absolute_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STA_Zero_Page_X_Indexed_Indirect (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STA_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- STX
    -- Store index X in memory
    --------------------------------------------------------------------
    procedure STX_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STX_Zero_Page_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STX_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- STY
    -- Store index Y in memory
    --------------------------------------------------------------------
    procedure STY_Zero_Page (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STY_Zero_Page_Y (This : in out MOS_T) is
    begin
       null;
    end;

    procedure STY_Absolute (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- TAX
    -- Transfer accumulator to index X 
    --------------------------------------------------------------------
    procedure TAX (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- TAY
    -- Transfer accumulator to index Y 
    --------------------------------------------------------------------
    procedure TAY (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- TSX
    -- Transfer stack pointer to index X 
    --------------------------------------------------------------------
    procedure TSX (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- TXA
    -- Transfer index X to accumulator 
    --------------------------------------------------------------------
    procedure TXA (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- TXS
    -- Transfer index X to stack pointer 
    --------------------------------------------------------------------
    procedure TXS (This : in out MOS_T) is
    begin
       null;
    end;

    --------------------------------------------------------------------
    -- TYA
    -- Transfer index Y to accumulator
    --------------------------------------------------------------------
    procedure TYA (This : in out MOS_T) is
    begin
       null;
    end;

    
end Instruction;

