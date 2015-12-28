-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Ada.Strings.Unbounded;
with Gnat.Calendar.Time_IO;

package body Generic_Funct_Types is
	package ASU renames Ada.Strings.Unbounded;

	-- Print an End_Point_Type like => IP:Port => XX.XX.XX.XX : XXXX --
	function Print_EP (EP: LLU.End_Point_Type) return String is
		S, S_IP, S_Port: ASU.Unbounded_String;
        N: Natural;
	begin
		S := ASU.To_Unbounded_String(LLU.Image(EP));
        
        N := ASU.Index(S, ":");
		S := ASU.Tail(S, ASU.Length(S)-N-1);

		N := ASU.Index(S, ",");
		S_IP := ASU.Head(S, N-1);

		N := ASU.Index(S, "  ");
		S_Port := ASU.Tail(S, ASU.Length(S)-N-1);

		return (ASU.To_String(S_IP) & ":" & ASU.To_String(S_Port));
	end Print_EP;

    -- Functions Latest_Messages --
    -------------------------------
    function "=" (Left, Right : New_Mssg_Type) return Boolean is
    begin
        return Left.Session_Id = Right.Session_Id and then LLU.Image(Left.EP) = LLU.Image(Right.EP);
    end "=";

    function Print_Message(Message: New_Mssg_Type) return String is
    begin
        return ("Session" & Natural'Image(Message.Session_Id) & " => " & Print_EP(Message.EP));
    end Print_Message;

	-- Functions Print Sender Dest --
	---------------------------------
	function Print_Mess_Id (Mess: Mess_Id_T) return String is
	begin
		return ("Sesion" & Natural'Image(Mess.Session_Id) & " => [" & Print_EP(Mess.EP) & "," & Seq_N_T'Image(Mess.Seq_N) & "]");		
	end Print_Mess_Id;

	function Print_Destination (Destination: Destination_T) return String is
		use type LLU.End_Point_Type;
	begin
		if Destination.EP = null then
			return ("[-]");
		else
			return ("[" & Print_EP(Destination.EP) & ", ret =" & Integer'Image(Destination.Retries) & "]");
		end if;
	end Print_Destination;


	function Print_Destinations_T (Destinations: Destinations_T) return String is
		S: ASU.Unbounded_String := ASU.To_Unbounded_String("( ");	
	begin
		for I in Destinations'Range loop
			S := ASU.To_Unbounded_String(ASU.To_String(S) & Print_Destination(Destinations(I)));
			if I /= Destinations'Length then
				S := ASU.To_Unbounded_String(ASU.To_String(S) &", ");
			end if;
		end loop;
		return (ASU.To_String(S) & " )");
	end Print_Destinations_T;


	function "=" (Left, Right : Mess_Id_T) return Boolean is
	begin
		return Left.Session_Id = Right.Session_Id and then LLU.Image(Left.EP) = LLU.Image(Right.EP) and then Left.Seq_N = Right.Seq_N;
	end "=";


	function "<" (Left, Right : Mess_Id_T) return Boolean is
	begin	
		return Left.Session_Id < Right.Session_Id or LLU.Image(Left.EP) < LLU.Image(Right.EP) or Left.Seq_N < Right.Seq_N;
	end "<";


	function ">" (Left, Right : Mess_Id_T) return Boolean is
	begin
		return Left.Session_Id > Right.Session_Id or LLU.Image(Left.EP) > LLU.Image(Right.EP) or Left.Seq_N > Right.Seq_N;
	end ">";

	-- Functions Print Sender Buffering --
	--------------------------------------
	-- We can not print a buffer, so it just print [EP, Seq_N] --
	function Print_Value_T (Value: Value_T) return String is
	begin
		return ("Sesion" & Positive'Image(Value.Session_Id) &" => [" & Print_EP(Value.EP_H_Creat) & "," & Seq_N_T'Image(Value.Seq_N) & "]");	
	end Print_Value_T;


	-- Convert Time to String --
	----------------------------
	function Time_To_String (T: Ada.Calendar.Time) return String is
		package C_IO renames Gnat.Calendar.Time_IO;
	begin
		return C_IO.Image(T, "%T.%i");
	end Time_To_String;

end Generic_Funct_Types;
