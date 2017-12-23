package body Main_Loop is

procedure Main_Loop_f (This : in out MOS_T; Instruction : in Fun_Array) is
    -- Frequency is expressed in MHz
    -- 1 MHz = 10**(-6) seconds = 1 microsecond = 1000 nanoseconds
    -- 3 MHz = 3*10**(-6) seconds = 0.333333.. microseconds = 333 nanosceonds
    Next_Release : Ada.Real_Time.Time := The_Clock; 
    Frequency : Integer := 3;
    Period : Integer := 1000/Frequency; 
    Release_Interval : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Nanoseconds (Period); 
    
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
