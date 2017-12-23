with Ada.Text_IO; use Ada.Text_IO;

with MOS; use MOS;

package Instruction is

type Fun_T is access procedure(This : in out MOS_T);

type Fun_Array is array(Byte_T range <>) of Fun_T;

function Initialize_Instruction_Table return Fun_Array;

private
    procedure Not_Yet_Supported_Operation(This : in out MOS_T);

    --------------------------------------------------------------------
    -- ADC FUNCTIONS
    -- Add memory to accumulator with carry
    --------------------------------------------------------------------
    procedure ADC_Immediate (This : in out MOS_T);
    procedure ADC_Absolute (This : in out MOS_T);
    procedure ADC_Zero_Page (This : in out MOS_T);
    procedure ADC_Absolute_X (This : in out MOS_T);
    procedure ADC_Absolute_Y (This : in out MOS_T);
    procedure ADC_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure ADC_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T);
    --------------------------------------------------------------------
    -- AND FUNCTIONS:
    -- "AND" memory with accumulator
    --------------------------------------------------------------------
    procedure AND_Immediate (This : in out MOS_T);
    procedure AND_Absolute (This : in out MOS_T);
    procedure AND_Zero_Page (This : in out MOS_T);
    procedure AND_Absolute_X (This : in out MOS_T);
    procedure AND_Absolute_Y (This : in out MOS_T);
    procedure AND_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure AND_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T);
    --------------------------------------------------------------------
    -- ASL:
    -- Shift left one bit (Memory or Accumulator)
    --------------------------------------------------------------------
    procedure ASL_Accumulator (This : in out MOS_T);
    procedure ASL_Zero_Page (This : in out MOS_T);
    procedure ASL_Zero_Page_X (This : in out MOS_T);
    procedure ASL_Absolute (This : in out MOS_T);
    procedure ASL_Absolute_X (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BCC:
    -- Branch on carry clear
    --------------------------------------------------------------------
    procedure BCC (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BCS:
    -- Branch on carry set
    --------------------------------------------------------------------
    procedure BCS (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BEQ:
    -- Branch on result zero
    --------------------------------------------------------------------
    procedure BEQ (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BIT:
    -- Test bits in memory with accumulator
    --------------------------------------------------------------------
    procedure BIT (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BMI:
    -- Branch on result minus
    --------------------------------------------------------------------
    procedure BMI (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BNE
    -- Branch on result not zero
    --------------------------------------------------------------------
    procedure BNE (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BPL
    -- Branch on result plus
    --------------------------------------------------------------------
    procedure BPL (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BRK
    -- Force Break
    --------------------------------------------------------------------
    procedure BRK (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BVC
    -- Branch on overflow clear
    --------------------------------------------------------------------
    procedure BVC (This : in out MOS_T);
    --------------------------------------------------------------------
    -- BVS
    -- Branch on overflow set
    --------------------------------------------------------------------
    procedure BVS (This : in out MOS_T);
    --------------------------------------------------------------------
    -- CLC
    -- Clear carry flag
    --------------------------------------------------------------------
    procedure CLC (This : in out MOS_T);
    --------------------------------------------------------------------
    -- CLC
    -- Clear decimal mode
    --------------------------------------------------------------------
    procedure CLD (This : in out MOS_T);
    --------------------------------------------------------------------
    -- CLI
    -- Clear interrupt disable status
    --------------------------------------------------------------------
    procedure CLI (This : in out MOS_T);
    --------------------------------------------------------------------
    -- CLV
    -- Clear overflow flag
    --------------------------------------------------------------------
    procedure CLV (This : in out MOS_T);
    --------------------------------------------------------------------
    -- CMP FUNCTIONS
    -- Compare memory and accumulator
    --------------------------------------------------------------------
    procedure CMP_Immediate (This : in out MOS_T);
    procedure CMP_Absolute (This : in out MOS_T);
    procedure CMP_Zero_Page (This : in out MOS_T);
    procedure CMP_Absolute_X (This : in out MOS_T);
    procedure CMP_Absolute_Y (This : in out MOS_T);
    procedure CMP_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure CMP_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T);
    --------------------------------------------------------------------
    -- CPX FUNCTIONS
    -- Compare memory and index X
    --------------------------------------------------------------------
    procedure CPX_Immediate (This : in out MOS_T);
    procedure CPX_Absolute (This : in out MOS_T);
    procedure CPX_Zero_Page (This : in out MOS_T);
    --------------------------------------------------------------------
    -- CPY FUNCTIONS
    -- Compare memory and index Y
    --------------------------------------------------------------------
    procedure CPY_Immediate (This : in out MOS_T);
    procedure CPY_Absolute (This : in out MOS_T);
    procedure CPY_Zero_Page (This : in out MOS_T);
    --------------------------------------------------------------------
    -- DEC
    -- Decrement memory by one
    --------------------------------------------------------------------
    procedure DEC_Zero_Page (This : in out MOS_T);
    procedure DEC_Zero_Page_X (This : in out MOS_T);
    procedure DEC_Absolute (This : in out MOS_T);
    procedure DEC_Absolute_X (This : in out MOS_T);
    --------------------------------------------------------------------
    -- DEX
    -- Decrement index X by one
    --------------------------------------------------------------------
    procedure DEX (This : in out MOS_T);
    --------------------------------------------------------------------
    -- DEY
    -- Decrement index X by one
    --------------------------------------------------------------------
    procedure DEY (This : in out MOS_T);
    --------------------------------------------------------------------
    -- EOR FUNCTIONS
    -- Exclusive-OR memory and accumulator
    --------------------------------------------------------------------
    procedure EOR_Immediate (This : in out MOS_T);
    procedure EOR_Absolute (This : in out MOS_T);
    procedure EOR_Zero_Page (This : in out MOS_T);
    procedure EOR_Absolute_X (This : in out MOS_T);
    procedure EOR_Absolute_Y (This : in out MOS_T);
    procedure EOR_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure EOR_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T);
    --------------------------------------------------------------------
    -- INC
    -- Increment memory by one
    --------------------------------------------------------------------
    procedure INC_Zero_Page (This : in out MOS_T);
    procedure INC_Zero_Page_X (This : in out MOS_T);
    procedure INC_Absolute (This : in out MOS_T);
    procedure INC_Absolute_X (This : in out MOS_T);
    --------------------------------------------------------------------
    -- INX
    -- Increment index X by one
    --------------------------------------------------------------------
    procedure INX (This : in out MOS_T);
    --------------------------------------------------------------------
    -- INY
    -- Increment index X by one
    --------------------------------------------------------------------
    procedure INY (This : in out MOS_T);
    --------------------------------------------------------------------
    -- JMP
    -- Jump to new location
    --------------------------------------------------------------------
    procedure JMP_Absolute (This : in out MOS_T);
    procedure JMP_Absolute_Indirect (This : in out MOS_T);
    --------------------------------------------------------------------
    -- JSR
    -- Jump to new location saving return address
    --------------------------------------------------------------------
    procedure JSR_Absolute (This : in out MOS_T);
    --------------------------------------------------------------------
    -- LDA
    -- Load accumulator with memory
    --------------------------------------------------------------------
    procedure LDA_Immediate (This : in out MOS_T);
    procedure LDA_Absolute (This : in out MOS_T);
    procedure LDA_Zero_Page (This : in out MOS_T);
    procedure LDA_Absolute_X (This : in out MOS_T);
    procedure LDA_Absolute_Y (This : in out MOS_T);
    procedure LDA_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure LDA_Zero_Page_Y_Indirect_Indexed (This : in out MOS_T);
    --------------------------------------------------------------------
    -- LDX
    -- Load index X with memory
    --------------------------------------------------------------------
    procedure LDX_Immediate (This : in out MOS_T);
    procedure LDX_Zero_Page (This : in out MOS_T);
    procedure LDX_Zero_Page_Y (This : in out MOS_T);
    procedure LDX_Zero_Page_Absolute (This : in out MOS_T);
    procedure LDX_Zero_Page_Absolute_Y (This : in out MOS_T);
    
    --------------------------------------------------------------------
    -- LDY
    -- Load index Y with memory
    --------------------------------------------------------------------
    procedure LDY_Immediate (This : in out MOS_T);
    procedure LDY_Zero_Page (This : in out MOS_T);
    procedure LDY_Zero_Page_Y (This : in out MOS_T);
    procedure LDY_Zero_Page_Absolute (This : in out MOS_T);
    procedure LDY_Zero_Page_Absolute_Y (This : in out MOS_T);
    --------------------------------------------------------------------
    -- LSR
    -- Shift right one bit (memory or accumulator)
    --------------------------------------------------------------------
    procedure LSR_Accumulator (This : in out MOS_T);
    procedure LSR_Zero_Page (This : in out MOS_T);
    procedure LSR_Zero_Page_X (This : in out MOS_T);
    procedure LSR_Absolute (This : in out MOS_T);
    procedure LSR_Absolute_X (This : in out MOS_T);
    --------------------------------------------------------------------
    -- NOP
    -- No operation
    --------------------------------------------------------------------
    procedure NOP (This : in out MOS_T);
    --------------------------------------------------------------------
    -- OR FUNCTIONS:
    -- "OR" memory with accumulator
    --------------------------------------------------------------------
    procedure OR_Immediate (This : in out MOS_T);
    procedure OR_Absolute (This : in out MOS_T);
    procedure OR_Zero_Page (This : in out MOS_T);
    procedure OR_Absolute_X (This : in out MOS_T);
    procedure OR_Absolute_Y (This : in out MOS_T);
    procedure OR_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure OR_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T);
    --------------------------------------------------------------------
    -- PHA
    -- Push accumulator on stack
    --------------------------------------------------------------------
    procedure PHA (This : in out MOS_T);  
    --------------------------------------------------------------------
    -- PHP
    -- Push processor status on stack
    --------------------------------------------------------------------
    procedure PHP (This : in out MOS_T);
    --------------------------------------------------------------------
    -- PLA
    -- Pull accumulator from stack
    --------------------------------------------------------------------
    procedure PLA (This : in out MOS_T);
    --------------------------------------------------------------------
    -- PLP
    -- Pull accumulator from stack
    --------------------------------------------------------------------
    procedure PLP (This : in out MOS_T);
    --------------------------------------------------------------------
    -- ROL
    -- Rotate one bit left (memory or accumulator)
    --------------------------------------------------------------------
    procedure ROL_Accumulator (This : in out MOS_T);
    procedure ROL_Zero_Page (This : in out MOS_T);
    procedure ROL_Zero_Page_X (This : in out MOS_T);
    procedure ROL_Absolute (This : in out MOS_T);
    procedure ROL_Absolute_X (This : in out MOS_T);
    --------------------------------------------------------------------
    -- ROR
    -- Rotate one bit right (memory or accumulator)
    --------------------------------------------------------------------
    procedure ROR_Accumulator (This : in out MOS_T);
    procedure ROR_Zero_Page (This : in out MOS_T);
    procedure ROR_Zero_Page_X (This : in out MOS_T);
    procedure ROR_Absolute (This : in out MOS_T);
    procedure ROR_Absolute_X (This : in out MOS_T);
    --------------------------------------------------------------------
    -- RTI:
    -- return from interrupt
    --------------------------------------------------------------------
    procedure RTI (This : in out MOS_T);
    --------------------------------------------------------------------
    -- RTS:
    -- return from subroutine
    --------------------------------------------------------------------
    procedure RTS (This : in out MOS_T);
    --------------------------------------------------------------------
    -- SBC:
    -- Subtract memory from accumulator with borrow
    --------------------------------------------------------------------
    procedure SBC_Immediate (This : in out MOS_T);
    procedure SBC_Absolute (This : in out MOS_T);
    procedure SBC_Zero_Page (This : in out MOS_T);
    procedure SBC_Absolute_X (This : in out MOS_T);
    procedure SBC_Absolute_Y (This : in out MOS_T);
    procedure SBC_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure SBC_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T);
    --------------------------------------------------------------------
    -- SEC
    -- Set carry flag
    --------------------------------------------------------------------
    procedure SEC (This : in out MOS_T);  
    --------------------------------------------------------------------
    -- SED
    -- Set decimal mode
    --------------------------------------------------------------------
    procedure SED (This : in out MOS_T);   
    --------------------------------------------------------------------
    -- SEI
    -- Set interrupt disable status
    --------------------------------------------------------------------
    procedure SEI (This : in out MOS_T);
    --------------------------------------------------------------------
    -- STA:
    -- Stone accumulator in memory
    --------------------------------------------------------------------
    procedure STA_Immediate (This : in out MOS_T);
    procedure STA_Absolute (This : in out MOS_T);
    procedure STA_Zero_Page (This : in out MOS_T);
    procedure STA_Absolute_X (This : in out MOS_T);
    procedure STA_Absolute_Y (This : in out MOS_T);
    procedure STA_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure STA_Zero_Page_Y_Indirect_Indexed  (This : in out MOS_T);
    --------------------------------------------------------------------
    -- STX
    -- Store index X in memory
    --------------------------------------------------------------------
    procedure STX_Zero_Page (This : in out MOS_T);
    procedure STX_Zero_Page_Y (This : in out MOS_T);
    procedure STX_Absolute (This : in out MOS_T);
    --------------------------------------------------------------------
    -- STY
    -- Store index Y in memory
    --------------------------------------------------------------------
    procedure STY_Zero_Page (This : in out MOS_T);
    procedure STY_Zero_Page_Y (This : in out MOS_T);
    procedure STY_Absolute (This : in out MOS_T);
    --------------------------------------------------------------------
    -- TAX
    -- Transfer accumulator to index X 
    --------------------------------------------------------------------
    procedure TAX (This : in out MOS_T);
    --------------------------------------------------------------------
    -- TAY
    -- Transfer accumulator to index Y 
    --------------------------------------------------------------------
    procedure TAY (This : in out MOS_T);
    --------------------------------------------------------------------
    -- TSX
    -- Transfer stack pointer to index X 
    --------------------------------------------------------------------
    procedure TSX (This : in out MOS_T);
    --------------------------------------------------------------------
    -- TXA
    -- Transfer index X to accumulator 
    --------------------------------------------------------------------
    procedure TXA (This : in out MOS_T);
    --------------------------------------------------------------------
    -- TXS
    -- Transfer index X to stack pointer 
    --------------------------------------------------------------------
    procedure TXS (This : in out MOS_T);
    --------------------------------------------------------------------
    -- TYA
    -- Transfer index Y to accumulator
    --------------------------------------------------------------------
    procedure TYA (This : in out MOS_T);
    
end Instruction;
