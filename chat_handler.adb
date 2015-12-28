-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Ada.Text_IO;
with Ada.Command_Line;

with Users;
with Debug;
with Pantalla;
with Debug_Message;
with Retransmissions;
with Timed_Handlers;

package body Chat_Handler is
    package ASU renames Ada.Strings.Unbounded;
    package DM renames Debug_Message;
    use type Seq_N_T;
    use type Ada.Calendar.Time;

	--------------------------
	--- Procedures Handler ---
	--------------------------

    procedure Send_ACk (EP_H_ACK, EP_H_Creat: LLU.End_Point_Type; Seq_N: Seq_N_T; Session_ID: Positive) is
        use type LLU.End_Point_Type;
    begin
		-- Send ACK --
		if EP_H_ACK /= EP_H_Creat then
			Debug.Put("    SEND Ack ", Pantalla.Amarillo);
   			Debug.Put_Line(GFT.Print_EP(EP_H_ACK) & " to " & GFT.Print_EP(EP_H_Creat));
	
   			P_Buffer_Handler := new LLU.Buffer_Type(1024);
   			Users.Introduce_ACK(P_Buffer_Handler, EP_H_ACK, EP_H_Creat, Seq_N, Session_ID);
			LLU.Send(EP_H_Creat, P_Buffer_Handler);
		end if;
	end Send_ACK;

	procedure Get_Out_Init(P_Buffer: Access LLU.Buffer_Type; To: in LLU.End_Point_Type) is
		package ACL renames Ada.Command_Line;
		use type ASU.Unbounded_String;
		use type LLU.End_Point_Type;

		EP_H_Creat, EP_R_Creat, EP_H_Rsnd: LLU.End_Point_Type;
		Nick_Name: ASU.Unbounded_String;
        Hour: Ada.Calendar.Time;
		Seq_N, Value: Seq_N_T;
        Session_Id: Positive;
        List_Message: GFT.New_Mssg_Type;
		Success: Boolean;
	begin
		--Get out from P_Buffer--
        Session_Id := Positive'Input(P_Buffer);           --Number of Session
		EP_H_Creat := LLU.End_Point_Type'Input(P_Buffer); --EP_H_Creat: node that created the message
		Seq_N := Seq_N_T'Input(P_Buffer); 				  --Seq_N assigned by the node EP_H_Creat
		EP_H_Rsnd := LLU.End_Point_Type'Input(P_Buffer);  --EP_H: node that has forwarded the message
		EP_R_Creat := LLU.End_Point_Type'Input(P_Buffer); --EP_R: node that created the message. For reject message
		Nick_Name := ASU.Unbounded_String'Input(P_Buffer);
		
		DM.Print_RCV_Init(EP_H_Creat, EP_H_Rsnd, Seq_N, Nick_Name, Session_Id);
		if ASU.To_String(Nick_Name) = ACL.Argument(2) then
            Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);
			
            -- Send Reject Message to that who has the same Nick_Name --
            Debug.Put("    ");
			Users.Add_LM(EP_H_Creat, Seq_N, Session_Id);
			DM.Print_NOFLOOD_Init(EP_H_Creat, To, Seq_N, Nick_Name);
			Debug.Put_Line("");

            -- Send Reject --
			Debug.Put("    SEND Reject ", Pantalla.Amarillo);
			Debug.Put_Line(GFT.Print_EP(To) & " to " & GFT.Print_EP(EP_H_Creat) & " " & ASU.To_String(Nick_Name));

            Session_Id := Session_Id+1;
            Seq_N := 1; --All rejects messages will have seq_n = 1

		    P_Buffer_Handler := new LLU.Buffer_Type(1024);
			Users.Introduce_Reject(P_Buffer_Handler, Session_Id, To, Seq_N, Nick_Name);
			LLU.Send(EP_R_Creat, P_Buffer_Handler);

            Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
            Debug.Put("    ");
    		Users.Add_Sender_Buffering(To, Seq_N, Hour, Session_Id, P_Buffer_Handler);
            Debug.Put("    ");
            Users.Add_Reject(EP_H_Creat, To, Seq_N, Session_Id);
            Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);

            Debug.Put_Line("");

		else
			-- Check if we must add a Neighbor
			if EP_H_Creat = EP_H_Rsnd then
				Debug.Put("    ");
				Users.Add_Neighbor(EP_H_Creat);
			end if;

            -- Check Seq_N received --
            List_Message.EP := EP_H_Creat;
            List_Message.Session_Id := Session_Id;
            Latest_Msgs.Get(L_M_List, List_Message, Value, Success);
			if Seq_N > Value or (not Success) then
                if Seq_N = Value + 1 or (not Success) then
                    Debug.Put("    ");
					Users.Add_LM(EP_H_Creat, Seq_N, Session_Id);
					Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);

					-- Print Init --
					Debug.Put("    ");
					DM.Print_Flood_Init(EP_H_Creat, To, Seq_N, Nick_Name);
                end if;

                -- Send Message to neighbors --
			    P_Buffer_Handler := new LLU.Buffer_Type(1024);
			    Users.Introduce_Init(P_Buffer_Handler, Session_Id, EP_H_Creat, To, EP_R_Creat, Nick_Name, Seq_N);
			    Users.Send_Except_One_Neighbor(P_Buffer_Handler, EP_H_Rsnd, Success);
			    -- Retransmission --
			    if Success then
                    Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
                    Debug.Put("    ");
	    			Users.Add_Sender_Buffering(To, Seq_N, Hour, Session_Id, P_Buffer_Handler);
                    Debug.Put("    ");
                    Users.Add_Dest_Except_Neighbor(EP_H_Rsnd, To, Seq_N, Session_Id);

				    Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);
			    end if;
			else -- Seq_N <= Value
                Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);

				-- Print NOFLOOD Init --
				DM.Print_NOFLOOD_Init(EP_H_Creat, To, Seq_N, Nick_Name);
			end if;
		Debug.Put_Line("");
		end if;
	end Get_Out_Init;



	procedure Get_Out_Confirm(P_Buffer: access LLU.Buffer_Type; To: LLU.End_Point_Type) is
		EP_H_Creat, EP_H_Rsnd: LLU.End_Point_Type;
		Nick_Name: ASU.Unbounded_String;
		Seq_N, Value: Seq_N_T;
		Hour: Ada.Calendar.Time;
        Session_Id: Positive;
        List_Message: GFT.New_Mssg_Type;
		Success: Boolean;
	begin
		--Get out from P_Buffer--
        Session_Id := Positive'Input(P_Buffer);           --Number of session
		EP_H_Creat := LLU.End_Point_Type'Input(P_Buffer); --EP_H_Creat: node that created the message
		Seq_N := Seq_N_T'Input(P_Buffer); 				  --Seq_N assigned by the node EP_H_Creat
		EP_H_Rsnd := LLU.End_Point_Type'Input(P_Buffer);  --EP_H: node that has forwarded the message
		Nick_Name := ASU.Unbounded_String'Input(P_Buffer);

		DM.Print_RCV_Confirm(EP_H_Creat, EP_H_Rsnd, Seq_N, Nick_Name, Session_Id);		

        -- Check Seq_N received --
        List_Message.EP := EP_H_Creat;
        List_Message.Session_Id := Session_Id;
        Latest_Msgs.Get(L_M_List, List_Message, Value, Success);
		if Seq_N > Value or (not Success) then
            if Seq_N = Value + 1 or (not Success) then
				Debug.Put("    ");
				Users.Add_LM(EP_H_Creat, Seq_N, Session_Id);
				Ada.Text_IO.Put_Line(ASU.To_String(Nick_Name) & " ha entrado en el chat");

				Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);

				-- Print Flood Confirm --
				Debug.Put("    ");
				DM.Print_Flood_Confirm(EP_H_Creat, To, Seq_N, Nick_Name);

				Supernode.Put(Super_list, Nick_Name, EP_H_Creat, Success); --Add client with the name
			end if;

			-- Send to neighbors --
			P_Buffer_Handler := new LLU.Buffer_Type(1024);
			Users.Introduce_Confirm(P_Buffer_Handler, Session_Id, EP_H_Creat, To, Nick_Name, Seq_N);
			Users.Send_Except_One_Neighbor(P_Buffer_Handler, EP_H_Rsnd, Success);
			-- Retransmission --
			if Success then
                Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
                Debug.Put("    ");
				Users.Add_Sender_Buffering(To, Seq_N, Hour, Session_Id, P_Buffer_Handler);
                Debug.Put("    ");
                Users.Add_Dest_Except_Neighbor(EP_H_Rsnd, To, Seq_N, Session_Id);

				Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);
			end if;

		else -- Seq_N <= Value
			Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);

			-- Print NOFLOOD Confirm --
			DM.Print_NOFLOOD_Confirm(EP_H_Creat, EP_H_Rsnd, Seq_N, Nick_Name);
		end if;
		Debug.Put_Line("");
	end Get_Out_Confirm;



	procedure Get_Out_Writer(P_Buffer: access LLU.Buffer_Type; To: LLU.End_Point_Type) is
		EP_H_Creat, EP_H_Rsnd: LLU.End_Point_Type;
		Message, Nick_Name: ASU.Unbounded_String;
		Seq_N, Value: Seq_N_T;
		Hour: Ada.Calendar.Time;
        Session_Id: Positive;
        List_Message: GFT.New_Mssg_Type;
		Success: Boolean;
	begin
		--Get out from P_Buffer--
        Session_Id := Positive'Input(P_Buffer);           -- Number of session
		EP_H_Creat := LLU.End_Point_Type'Input(P_Buffer); -- EP_H_Creat: node that created the message
		Seq_N := Seq_N_T'Input(P_Buffer);				  -- Seq_N assigned by the node EP_H_Creat
		EP_H_Rsnd := LLU.End_Point_Type'Input(P_Buffer);  -- EP_H: node that has forwarded the message
		Nick_Name := ASU.Unbounded_String'Input(P_Buffer);
		Message := Asu.Unbounded_String'Input(P_Buffer);

		DM.Print_RCV_Writer(EP_H_Creat, EP_H_Rsnd, Seq_N, Nick_Name, Message, Session_Id);
		-- Check Seq_N received --
        List_Message.EP := EP_H_Creat;
        List_Message.Session_Id := Session_Id;
        Latest_Msgs.Get(L_M_List, List_Message, Value, Success);
		if Seq_N > Value or (not Success) then
            if Seq_N = Value + 1 or (not Success) then
				ADA.Text_IO.Put_Line(ASU.To_String(Nick_Name) & ": " & ASU.To_String(Message));

				Debug.Put("    ");
				Users.Add_LM(EP_H_Creat, Seq_N, Session_Id);
				Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);
				-- Print Flood Writer --
				Debug.Put("    ");
				DM.Print_Flood_Writer(EP_H_Creat, To, Seq_N, Nick_Name, Message);

				Supernode.Put(Super_list, Nick_Name, EP_H_Creat, Success); --Add Client with the name
			end if;
			
			-- Send To neighbors --
			P_Buffer_Handler := new LLU.Buffer_Type(1024);
			Users.Introduce_Writer(P_Buffer_Handler, Session_Id, EP_H_Creat, To, Nick_Name, Message, Seq_N);
			Users.Send_Except_One_Neighbor(P_Buffer_Handler, EP_H_Rsnd, Success);
			-- Retransmission --
			if Success then
                Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
                Debug.Put("    ");
				Users.Add_Sender_Buffering(To, Seq_N, Hour, Session_Id, P_Buffer_Handler);
                Debug.Put("    ");
                Users.Add_Dest_Except_Neighbor(EP_H_Rsnd, To, Seq_N, Session_Id);

				Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);
            end if;
			Debug.Put_Line("");

		else -- Seq_N <= Value
			Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);

			-- Print NOFLOOD Writer --
			DM.Print_NOFLOOD_Writer(EP_H_Creat, EP_H_Rsnd, Seq_N, Nick_Name);
		end if;
		Debug.Put_Line("");
	end Get_Out_Writer;


	-- Para que no haga caso a mensajes de fuera --
	function Has_Latest_Mssg(EP_Creat: LLU.End_Point_Type; Session_Id: Positive) return Boolean is
		Value: Seq_N_T;
        List_Message: GFT.New_Mssg_Type;
		Success: Boolean;
	begin
        List_Message.EP := EP_Creat;
        List_Message.Session_Id := Session_Id;
        Latest_Msgs.Get(L_M_List, List_Message, Value, Success);
		return Success;	
	end Has_Latest_Mssg;

	procedure Get_Out_Logout(P_Buffer: access LLU.Buffer_Type; To: LLU.End_Point_Type) is
		use type LLU.End_Point_Type;
		EP_H_Creat, EP_H_Rsnd: LLU.End_Point_Type;
		Seq_N, Value: Seq_N_T;
		Nick_Name: ASU.Unbounded_String;
		Confirm_Sent, Success: Boolean;
		Hour: Ada.Calendar.Time;
        List_Message: GFT.New_Mssg_Type;
        Session_Id: Positive;
	begin
		--Get out from P_Buffer--
        Session_Id := Positive'Input(P_Buffer);           --Number of Session
		EP_H_Creat := LLU.End_Point_Type'Input(P_Buffer); --EP_H_Creat: node that created the message
		Seq_N := Seq_N_T'Input(P_Buffer);				  --Seq_N assigned by the node EP_H_Creat
		EP_H_Rsnd := LLU.End_Point_Type'Input(P_Buffer);  --EP_H: node that has forwarded the message
		Nick_Name := Asu.Unbounded_String'Input(P_Buffer);
		Confirm_Sent := Boolean'Input(P_Buffer);

		DM.Print_RCV_Logout(EP_H_Creat, EP_H_Rsnd, Seq_N, Nick_Name, Confirm_Sent, Session_Id);

		-- Check Seq_N received --
        List_Message.EP := EP_H_Creat;
        List_Message.Session_Id := Session_Id;
        Latest_Msgs.Get(L_M_List, List_Message, Value, Success);
		if (Seq_N > Value or (not Success)) and Has_Latest_Mssg(EP_H_Creat, Session_Id) then
            if Seq_N = Value + 1 or (not Success) then
				EP_H_Logout := EP_H_Creat;
				Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
				Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Logout'Access);

				ADA.Text_IO.Put_Line(ASU.To_String(Nick_Name) & " ha abandonado el chat");

                Debug.Put("    ");
				Users.Add_LM(EP_H_Creat, Seq_N, Session_Id);
				Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);
				-- Print Flood Logout --
				Debug.Put("    ");
				DM.Print_Flood_Logout(EP_H_Creat, To, Seq_N, Nick_Name, Confirm_Sent);
                
				Supernode.Delete(Super_list, Nick_Name, Confirm_Sent);
			end if;

			-- Send message to Neighbors --
 			P_Buffer_Handler := new LLU.Buffer_Type(1024);
			Users.Introduce_Logout(P_Buffer_Handler, Session_Id, EP_H_Creat, To, Nick_Name, Seq_N, Confirm_Sent);
			Users.Send_Except_One_Neighbor(P_Buffer_Handler, EP_H_Rsnd, Success);
			-- Retransmission --
			if Success then
                Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
                Debug.Put("    ");
				Users.Add_Sender_Buffering(To, Seq_N, Hour, Session_Id, P_Buffer_Handler);
                Debug.Put("    ");
                Users.Add_Dest_Except_Neighbor(EP_H_Rsnd, To, Seq_N, Session_Id);

				Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);
			end if;

		else -- Seq_N <= Value
			Send_ACK(To, EP_H_Rsnd, Seq_N, Session_Id);

			-- Print NOFLOOD Logout --
			DM.Print_NOFLOOD_Logout(EP_H_Creat, EP_H_Rsnd, Seq_N, Nick_Name);
		end if;
		Debug.Put_Line("");
	end Get_Out_Logout;


	procedure Get_Out_Ack(P_Buffer: access LLU.Buffer_Type) is
		use type LLU.End_Point_Type;
		EP_H_Ack, EP_H_Creat: LLU.End_Point_Type;
		Seq_N: Seq_N_T;
		Counter: Natural := 0;
        Session_Id: Positive;

		Mess_Id: GFT.Mess_Id_T;
		Destinations: GFT.Destinations_T;
		Success: Boolean;
	begin
		EP_H_Ack := LLU.End_Point_Type'Input(P_Buffer);    --EP_H_Ack: node that send ack message
        Session_Id := Positive'Input(P_Buffer);            --Number of session
		EP_H_Creat := LLU.End_Point_Type'Input(P_Buffer);  --EP_H_Creat: node that created the message
		Seq_N := Seq_N_T'Input(P_Buffer);				   --Seq_N assigned by the node EP_H_Creat

		Mess_Id.EP := EP_H_Creat;
		Mess_Id.Seq_N := Seq_N;
        Mess_Id.Session_Id := Session_Id;
        DM.Print_RCV_Ack(EP_H_ACK, Mess_Id);

		Sender_Dest.Get(Sender_Dest_List, Mess_Id, Destinations, Success);
		if Success then
			for I in 1..Destinations'Length loop
				if Destinations(I).Ep = EP_H_Ack then
					Destinations(I).Ep := null;
					Counter := Counter + 1;
				elsif Destinations(I).EP = null then
					Counter := Counter + 1;
				end if;
			end loop;

			if Counter = Destinations'Length then
				Users.Delete_Sender_Dest(EP_H_Creat, Seq_N, Session_Id);
			else
				Debug.Put_Line("Modificamos Sender_Dest de " & GFT.Print_Mess_Id(Mess_Id));
				Sender_Dest.Put(Sender_Dest_List, Mess_Id, Destinations);
			end if;
			Debug.Put("     ");
		else
			Debug.Put_Line("");
		end if; 

	end Get_Out_Ack;

    procedure Get_Out_Reject (P_Buffer: access LLU.Buffer_Type; To: LLU.End_Point_Type) is
        Session_Id: Positive;
		EP_H_Logout: LLU.End_Point_Type;
		Nick_Name: ASU.Unbounded_String;
        Seq_N_Reject: Seq_N_T;
	begin    
        Session_Id := Positive'Input(P_Buffer);
		EP_H_Logout := LLU.End_Point_Type'Input(P_Buffer);
        Seq_N_Reject := Seq_N_T'Input(P_Buffer);
		Nick_Name := ASU.Unbounded_String'Input(P_Buffer);

		DM.Print_RCV_Reject(EP_H_Logout, Nick_Name);
        Debug.Put(" ");
        Send_Ack(To, EP_H_Logout, Seq_N_Reject, Session_Id);
		Must_Logout := True;
     end Get_Out_Reject;

    procedure Get_Out_Priv (P_Buffer: access LLU.Buffer_Type; To: LLU.End_Point_Type) is
        Session_Id: Positive;
		EP_H_Creat: LLU.End_Point_Type;
		Name_Client, Name_Object, Message: ASU.Unbounded_String;
        Seq_N_Priv, Value: Seq_N_T;
        List_Message: GFT.New_Mssg_Type;
		Success: Boolean;
	begin    
        Session_Id := Positive'Input(P_Buffer);
		EP_H_Creat := LLU.End_Point_Type'Input(P_Buffer);
        Seq_N_Priv := Seq_N_T'Input(P_Buffer);
		Name_Object := ASU.Unbounded_String'Input(P_Buffer);
		Name_Client := ASU.Unbounded_String'Input(P_Buffer);
		Message := ASU.Unbounded_String'Input(P_Buffer);

		Debug.Put("RCV Priv ", Pantalla.Amarillo);
		Debug.Put(GFT.Print_EP(EP_H_Creat) & " Session:" & Positive'Image(Session_Id) & " => " & 
				  Seq_N_T'Image(Seq_N_Priv) & " " & GFT.Print_EP(To) & " " & ASU.To_String(Name_Object));
        Debug.Put_Line(" ");
		Supernode.Put(Super_list, Name_Client, EP_H_Creat, Success); --Add Client with the name

        List_Message.EP := EP_H_Creat;
        List_Message.Session_Id := Session_Id;
        Latest_Msgs.Get(L_M_List, List_Message, Value, Success);
		if Seq_N_Priv > Value or (not Success) then
            if Seq_N_Priv = Value + 1 or (not Success) then
			
				Debug.Put("    ");
				Users.Add_LM(EP_H_Creat, Seq_N_Priv, Session_Id);
				Send_ACK(To, EP_H_Creat, Seq_N_Priv, Session_Id);

				Debug.put_Line("");
				ADA.Text_IO.Put_Line("Mensaje privado");
				ADA.Text_IO.Put_Line(ASU.To_String(Name_Object) & ": " & ASU.To_String(Message));
			end if;				
		else -- Seq_N <= Value
			Send_ACK(To, EP_H_Creat, Seq_N_Priv, Session_Id);
		end if;
		Debug.Put_Line("");
     end Get_Out_Priv;



	procedure Users_Handler (From: in LLU.End_Point_Type; To: in LLU.End_Point_Type; P_Buffer: access LLU.Buffer_Type) is
		use type Message_Type;
		Mess: Message_Type;
	begin
		Mess := Message_Type'Input(P_Buffer);
		if Mess = Init then
			Get_out_Init(P_Buffer, To);
		elsif Mess = Confirm then
			Get_Out_Confirm(P_Buffer, To);
		elsif Mess = Writer then
			Get_Out_Writer(P_Buffer, To);
		elsif Mess = Logout then
			Get_Out_Logout(P_Buffer, To);
		elsif Mess = Ack then
			Get_Out_Ack(P_Buffer);
        elsif Mess = Reject then
            Get_Out_Reject(P_Buffer, To);
		elsif Mess = Priv then
			Get_Out_Priv(P_Buffer, To);
		end if;
	end Users_Handler;

end Chat_Handler;
