package body Main_Loop is

procedure Main_Loop_f (This : in out MOS_T; Instruction : in Fun_Array) is
    Next_Release : Ada.Real_Time.Time := 0; 
    Release_Interval : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (1); -- 1 MHz
    
    begin
        Next_Release :=  The_Clock + Release_Interval;
        While_Loop :
            while This.Mem(This.PC) /= 0 loop
                Print_Status(This);
                Instruction(This.Mem(This.PC))(This);
                This.PC := This.PC + 2;
                delay until Next_Release;
                Next_Release := Next_Release + Release_Interval;
            end loop While_Loop;
    end Main_Loop_f;

end Main_Loop;
