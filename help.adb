-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Generic_Funct_Types;
with Chat_Handler;
with Pantalla;
with Debug;

package body Help is
	package GFT renames Generic_Funct_Types;
	package CH renames Chat_Handler;

	--------------------------------
	-- Procedures for print HELP --
	--------------------------------
	Debug_Status: Boolean := True;
	Prompt_Status: Boolean := False;
	
	procedure Message_h is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		Debug.Put_Line("              Comandos            Efectos", Pantalla.Rojo);
		Debug.Put_Line("              =================   =======", Pantalla.Rojo);
		Debug.Put_Line("              .nb .neighbors      lista de vecinos", Pantalla.Rojo);
		Debug.Put_Line("              .lm .latest_msgs    lista de últimos mensajes recibidos", Pantalla.Rojo);
		Debug.Put_Line("              .sb .sender_buff    mensajes pendientes de ser asentidos", Pantalla.Rojo);
		Debug.Put_Line("              .sd .sender_dest    vecinos cuyos mensajes han asentido", Pantalla.Rojo);
		Debug.Put_Line("              .debug              toggle para info de debug", Pantalla.Rojo);
		Debug.Put_Line("              .wai .whoami        Muestra en pantalla: nick | EP_H | EP_R", Pantalla.Rojo);
        Debug.Put_Line("              .fault              Muestra porcentaje fallo", Pantalla.Rojo);
        Debug.Put_Line("              .delay              Muestra retardo [Min, Max]", Pantalla.Rojo);
 		Debug.Put_Line("              .prompt             toggle para mostrar prompt", Pantalla.Rojo);
		Debug.Put_Line("              .h .help            muestra esta información de ayuda", Pantalla.Rojo);
		Debug.Put_Line("              .salir              termina el programa", Pantalla.Rojo);
		
		Debug.Set_Status(Debug_Status);
	end Message_h;

	procedure Change_debug is
	begin
		if Debug.Get_Status then
			Debug.Put_Line("Desactivada información de debug", Pantalla.Rojo);
			Debug.Set_Status(False);
		else
			Debug.Set_Status(True);
			Debug.Put_Line("Activada información de debug", Pantalla.Rojo);
		end if;
	end Change_debug;

	procedure Change_prompt is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		if Prompt_Status then
			Debug.Put_Line("Desactivado el prompt", Pantalla.Rojo);
			Prompt_Status := False;
		else
			Debug.Put_Line("Activado el prompt", Pantalla.Rojo);
			Prompt_Status := True;
		end if;

		Debug.Set_Status(Debug_Status);
	end Change_prompt;

	function Get_Prompt_Status return Boolean is
	begin
		return Prompt_Status;
	end Get_Prompt_Status;

	procedure Message_LM is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		Debug.Put_Line("                      Latest_Msgs", Pantalla.Rojo);
		CH.Latest_Msgs.Print_Map(CH.L_M_List);

		Debug.Set_Status(Debug_Status);
	end Message_LM;
	
	procedure Message_NB is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		Debug.Put_Line("                      Neighbors", Pantalla.Rojo);
		CH.Neighbors.Print_Map(CH.N_List);

		Debug.Set_Status(Debug_Status);
	end Message_NB;

	procedure Message_SB is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		Debug.Put_Line("                      Sender_Buffering", Pantalla.Rojo);
		CH.Sender_Buffering.Print_Map(CH.Sender_Buff_List);                 

		Debug.Set_Status(Debug_Status);
	end Message_SB;

	procedure Message_SD is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		Debug.Put_Line("                      Sender_Dest", Pantalla.Rojo);
		CH.Sender_Dest.Print_Map(CH.Sender_Dest_List);                 

		Debug.Set_Status(Debug_Status);
	end Message_SD;

	procedure Super_Node is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		Debug.Put_Line("                      Super_Node", Pantalla.Rojo);
		CH.SuperNode.Print_Map(CH.Super_List);                 

		Debug.Set_Status(Debug_Status);
	end Super_Node;

	procedure Message_WAI(Nick: ASU.Unbounded_String; EP_H, EP_R: LLU.End_Point_Type) is
	begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);

		Debug.Put_Line("Nick: " & ASU.To_String(Nick) & " | EP_H: " & GFT.Print_EP(EP_H) & " | EP_R: " & GFT.Print_EP(EP_R), Pantalla.Rojo);

		Debug.Set_Status(Debug_Status);
	end Message_WAI;

	procedure Message_Fault (Fault: Natural) is
    begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);
        
        Debug.Put_Line("Fallo del" & Natural'Image(Fault) & "%", Pantalla.Rojo);

        Debug.Set_Status(Debug_Status);
    end Message_Fault;

	procedure Message_Delay (Min, Max: Natural) is
    begin
		Debug_Status := Debug.Get_Status;
		Debug.Set_Status(True);
        
        Debug.Put_Line("Retardo entre [" & Natural'Image(Min) & "," & Natural'Image(Max) & "]", Pantalla.Rojo);

        Debug.Set_Status(Debug_Status);
    end Message_Delay;

end Help;
