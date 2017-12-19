package body Main_Loop is

procedure Main_Loop_f (This : in out MOS_T; Instruction : in Fun_Array) is

	begin
        While_Loop :
            while This.Mem(This.PC) /= 0 loop
                Print_Status(This);
                Instruction(This.Mem(This.PC))(This);
                Print_Status(This);
                This.PC := This.PC + 2;
            end loop While_Loop;
    end Main_Loop_f;

end Main_Loop;
