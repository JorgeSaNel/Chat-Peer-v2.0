with Ada.Strings.Unbounded;
with Lower_Layer_UDP;

-------------------------------
-- Procedure Private Message --
-------------------------------
package Private_Message is
	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;
	
	procedure Analyze_Message (Message: out ASU.Unbounded_String; N: in out Positive; EP_H_Creat: LLU.End_Point_Type);

end Private_Message;
