with Ada.Text_IO; use Ada.Text_IO;

with MOS; use MOS;

package Instruction is

type Fun_T is access procedure(This : in out MOS_T);

type Fun_Array is array(Byte_T range <>) of Fun_T;

function Initialize_Instruction_Table return Fun_Array;

private 
    procedure Not_Yet_Supported_Operation(This : in out MOS_T);
    
    -----------------------------------------
    -- ADC FUNCTIONS
    -----------------------------------------
    procedure Adc_Immediate (This : in out MOS_T);
    procedure Adc_Absolute (This : in out MOS_T);
    procedure Adc_Zero_Page (This : in out MOS_T);
    procedure Adc_Absolute_X (This : in out MOS_T);
    procedure Adc_Absolute_Y (This : in out MOS_T);
    procedure Adc_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure Adc_Zero_Page_X_Indirect_Indexed (This : in out MOS_T);
    -----------------------------------------
    -- AND FUNCTIONS
    -----------------------------------------
    procedure And_Immediate (This : in out MOS_T);
    procedure And_Absolute (This : in out MOS_T);
    procedure And_Zero_Page (This : in out MOS_T);
    procedure And_Absolute_X (This : in out MOS_T);
    procedure And_Absolute_Y (This : in out MOS_T);
    procedure And_Zero_Page_X_Indexed_Indirect (This : in out MOS_T);
    procedure And_Zero_Page_X_Indirect_Indexed  (This : in out MOS_T);
    
end Instruction;
