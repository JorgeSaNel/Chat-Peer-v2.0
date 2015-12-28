-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Debug;
with Pantalla;

package body Debug_Message is

	-- Print RCV --
	---------------
	procedure Print_RCV_Reject(EP_H: LLU.End_Point_Type; Nick: ASU.Unbounded_String) is
	begin
		Debug.Put("RCV Reject ", Pantalla.Amarillo);
		Debug.Put_Line(GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
	end Print_RCV_Reject;
	
	procedure Print_RCV_Init(EP_H_Creat, EP_H_Rsnd: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String; Session:Positive) is
	begin
		Debug.Put("RCV Init ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat)& " Session:" & Positive'Image(Session) & " => " & Seq_N_T'Image(Seq_N) & " " &                              GFT.Print_EP(EP_H_Rsnd)); 
		Debug.Put_Line(" ... " & ASU.To_String(Nick));
	end Print_RCV_Init;

	procedure Print_RCV_Writer(EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick, Message: ASU.Unbounded_String;
							   Session: Positive) is
	begin
		Debug.Put("RCV Writer ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & " Session:" & Positive'Image(Session) & " => " & 
				  Seq_N_T'Image(Seq_N) & " " & GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
		Debug.Put_Line(" " & ASU.To_String(Message));
	end Print_RCV_Writer;

	procedure Print_RCV_Confirm(EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String;
                                Session: Positive) is
	begin
		Debug.Put("RCV Confirm ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & " Session:" & Positive'Image(Session) & " => " & Seq_N_T'Image(Seq_N) & "  " & 
                  GFT.Print_EP(EP_H));
		Debug.Put_Line(" " & ASU.To_String(Nick));
	end Print_RCV_Confirm;

	procedure Print_RCV_Logout(EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String;                          								   Sent:Boolean; Session:Positive) is
	begin
		Debug.Put("RCV Logout ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & " Session:" & Positive'Image(Session) & " => " & Seq_N_T'Image(Seq_N) & " " &              							       GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
		Debug.Put_Line(" " & Boolean'Image(Sent));
	end Print_RCV_Logout;

	procedure Print_RCV_Ack(EP_H_Ack: LLU.End_Point_Type; Mess_Id: Generic_funct_types.Mess_Id_T) is
	begin
        Debug.Put("RCV Ack ", Pantalla.Amarillo);
        Debug.Put_Line(GFT.Print_EP(EP_H_Ack) & " asiente " & GFT.Print_Mess_Id(Mess_Id));
        Debug.Put("     ");
	end Print_RCV_Ack;

	-- Print FLOODING --
	--------------------
	procedure Print_Flood_Init (EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String) is
	begin
		Debug.Put("FLOOD Init ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & "  " & GFT.Print_EP(EP_H));
		Debug.Put_Line(" ... " & ASU.To_String(Nick));
	end Print_Flood_Init;

	procedure Print_Flood_Writer(EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick_Name, Message: ASU.Unbounded_String) is
	begin
		Debug.Put("FLOOD Writer ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " & GFT.Print_EP(EP_H));
		Debug.Put_Line(" " & ASU.To_String(Nick_Name) & " " & ASU.To_String(Message));
	end Print_Flood_Writer;

	procedure Print_Flood_Confirm(EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String) is
	begin
		Debug.Put("FLOOD Confirm ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & "  " & GFT.Print_EP(EP_H));
		Debug.Put_Line(" ... " & ASU.To_String(Nick));
	end Print_Flood_Confirm;

	procedure Print_Flood_Logout(EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String; Sent: Boolean) is
	begin
		Debug.Put("FLOOD Logout ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " & GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
		Debug.Put_Line(" " & Boolean'Image(Sent));
	end Print_Flood_Logout;

	-- Print NOFLOOD --
	-------------------

	procedure Print_NOFLOOD_Init (EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String) is
	begin
		Debug.Put("    NOFLOOD Init ", Pantalla.Amarillo);
		Debug.Put_Line(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " & GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
	end Print_NOFLOOD_Init;

	procedure Print_NOFLOOD_Writer (EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String) is
	begin
		Debug.Put("    NOFLOOD Writer ", Pantalla.Amarillo);
		Debug.Put_Line(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " & GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
	end Print_NOFLOOD_Writer;

	procedure Print_NOFLOOD_Confirm (EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String) is
	begin
		Debug.Put("    NOFLOOD Confirm ", Pantalla.Amarillo);
		Debug.Put_Line(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " & GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
	end Print_NOFLOOD_Confirm;

	procedure Print_NOFLOOD_Logout (EP_H_Creat, EP_H: LLU.End_Point_Type; Seq_N: Seq_N_T; Nick: ASU.Unbounded_String) is
	begin
		Debug.Put("    NOFLOOD Logout ", Pantalla.Amarillo);
		Debug.Put_Line(GFT.Print_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " & GFT.Print_EP(EP_H) & " " & ASU.To_String(Nick));
	end Print_NOFLOOD_Logout;

end Debug_Message;
