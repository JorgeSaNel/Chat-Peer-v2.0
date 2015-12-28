-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE
-- Encargada de hacer la parte privada de los mensajes

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Command_Line;

with Chat_Handler;
with Generic_Funct_Types;

with Debug;
with Pantalla;
with Users;
with Retransmissions;
with Timed_Handlers;

package body Private_Message is
	package ATI renames ADA.Text_IO;
	package CH renames Chat_Handler;
	package GFT renames Generic_Funct_Types;
	use type LLU.End_Point_Type;
	use type CH.Seq_N_T;
    use type Ada.Calendar.Time;

	Session_Id: Positive := 3;

	procedure Writer (EP_H_Creat, EP_H_Client: LLU.End_Point_Type; Name: ASU.Unbounded_String; 
                      Message: ASU.Unbounded_String; Session: Positive; Seq_N: in out CH.Seq_N_T) is 
		Hour: Ada.Calendar.Time;
		package ACL renames Ada.Command_Line;
	begin
		Users.Add_LM(EP_H_Creat, Seq_N, Session_Id);

		CH.P_Buffer_Main := new LLU.Buffer_Type(1024);
		-- Introduce Private into Buffer --
		CH.Message_Type'Output(CH.P_Buffer_Main, CH.Priv);
        Positive'Output(CH.P_Buffer_Main, Session);
		LLU.End_Point_Type'Output(CH.P_Buffer_Main, EP_H_Creat); --EP_H_Creat: node that created the message
		CH.Seq_N_T'Output(CH.P_Buffer_Main, Seq_N);                 --Seq_N assigned by the node EP_H_Creat
		ASU.Unbounded_String'Output(CH.P_Buffer_Main, Name); --Nick of the node that created the message
		ASU.Unbounded_String'Output(CH.P_Buffer_Main, ASU.To_Unbounded_String(ACL.Argument(2)));
		ASU.Unbounded_String'Output(CH.P_Buffer_Main, Message);
        -------------------------------------

		Debug.Put("Send Private Message to ", Pantalla.Amarillo);
        Debug.Put_Line(GFT.Print_EP(EP_H_Client));

		LLU.Send(EP_H_Client, CH.P_Buffer_Main);

        Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
        Debug.Put("    ");
    	Users.Add_Sender_Buffering(EP_H_Creat, Seq_N, Hour, Session_Id, CH.P_Buffer_Main);
        Debug.Put("    ");
        Users.Add_Reject(EP_H_Client, EP_H_Creat, Seq_N, Session_Id); --Add only one end_point
        Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);

        Debug.Put_Line("");
	end Writer;


	procedure Analyze_Message (Message: out ASU.Unbounded_String; N: in out Positive; EP_H_Creat: LLU.End_Point_Type) is
		Name, Mess: ASU.Unbounded_String;
		EP_H_Client: LLU.End_Point_Type;
		Success: Boolean;
		Seq_N: CH.Seq_N_T := 0;
	begin
		Session_Id := Session_Id + 1;
		Message := ASU.Tail(Message, ASU.Length(Message)-N);
	
		N := ASU.Index(Message, " ");
		Name := ASU.Head(Message, N-1);

		N := ASU.Index(Message, " ");
		Message := ASU.Tail(Message, ASU.Length(Message)-N);
	    CH.Supernode.Get(CH.Super_List, Name, EP_H_Client, Success);
            
		if Success then
			Seq_N := Seq_N + 1;
 		    Writer(EP_H_Creat, EP_H_Client, Name, Message, Session_Id, Seq_N);
		else
			Debug.put_line("No sabemos quien es " & ASU.To_String(Name), Pantalla.Rojo);
		end if;
	end Analyze_Message;


end Private_Message;
