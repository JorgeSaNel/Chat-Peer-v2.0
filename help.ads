-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Lower_Layer_UDP;
with Ada.Strings.Unbounded;

--------------------------------
-- Procedures for print HELP --
--------------------------------
package Help is
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	
	procedure Message_h;

	procedure Message_LM;
	
	procedure Message_NB;

	procedure Message_SB;

	procedure Message_SD;
	
	procedure Change_debug;

	procedure Change_prompt;

	procedure Super_Node;

	procedure Message_WAI(Nick: ASU.Unbounded_String; EP_H, EP_R: LLU.End_Point_Type);

	procedure Message_Fault (Fault: Natural);

	procedure Message_Delay (Min, Max: Natural);
	
	function Get_Prompt_Status return Boolean;

end Help;
