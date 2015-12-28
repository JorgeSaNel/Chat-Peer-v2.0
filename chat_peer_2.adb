-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE
-- jorgesantosneila@hotmail.com
-- @jorgesanel

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Calendar;
with Gnat.Ctrl_C;
with Ctrl_C_Handler;

with Lower_Layer_UDP;
with Chat_Handler;
with Timed_Handlers;
with Retransmissions;
with Generic_Funct_Types;
with Users;

with Help;
with Debug_Message;
with Pantalla;
with Debug;
with Private_Message;

procedure Chat_Peer_2 is
	package ATI renames ADA.Text_IO;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package CH renames Chat_Handler;
	package DM renames Debug_Message;
	use type CH.Seq_N_T;
    use type Ada.Calendar.Time;

	Usage_Error: Exception;
	Delay_Error:Exception;
	Fault_Error: Exception;
	Port_Error: Exception;
	Port_Error_Neighbor_1: Exception;
	Port_Error_Neighbor_2: Exception;


	procedure Get_Neighbors is
		Neighbor_Host_1, Neighbor_Host_2: ASU.Unbounded_String; 
		Neighbor_Port_1, Neighbor_Port_2: Natural;
		Neighbor_EP: LLU.End_Point_Type;
	begin
		Neighbor_Host_1 := ASU.To_Unbounded_String(ACL.Argument(6));
		Neighbor_Host_1 := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Neighbor_Host_1)));
		Neighbor_Port_1 := Integer'Value(ACL.Argument(7));
		if Neighbor_Port_1 < 1024 or Neighbor_Port_1 > 65535 then
			raise Port_Error_Neighbor_1;
		end if;

		ATI.New_Line;
		Neighbor_EP := LLU.Build(ASU.To_String(Neighbor_Host_1), Neighbor_Port_1);
		Users.Add_Neighbor(Neighbor_EP);

		if ACL.Argument_Count = 9 then
			Neighbor_Host_2 := ASU.To_Unbounded_String(ACL.Argument(8));
			Neighbor_Host_2 := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Neighbor_Host_2)));
			Neighbor_Port_2 := Integer'Value(ACL.Argument(9));
			if Neighbor_Port_2 < 1024 or Neighbor_Port_2 > 65535 then
				raise Port_Error_Neighbor_2;
			end if;

			Neighbor_EP := LLU.Build(ASU.To_String(Neighbor_Host_2), Neighbor_Port_2);
			Users.Add_Neighbor(Neighbor_EP);
		end if;
	end Get_Neighbors;

	-- This procedure reads input arguments --
	-- It makes transmission failures and delays --
	-----------------------------------------------
	procedure Arguments_Input(Port: out Natural; Nick_Name: out ASU.Unbounded_String;
							  Min_Delay, Max_Delay, Fault_Pct: out Natural) is
	begin
		if ACL.Argument_Count = 5 or ACL.Argument_Count = 7 or ACL.Argument_Count = 9 then
			Port := Integer'Value(ACL.Argument(1));
			if Port < 1024 or Port > 65535 then
				raise Port_Error;
			end if;
			Nick_Name := ASU.To_Unbounded_String(ACL.Argument(2));

			Min_Delay := Natural'Value(ACL.Argument(3));
			Max_Delay := Natural'Value(ACL.Argument(4));
			if Min_Delay > Max_Delay then
				raise Delay_Error;
			else
				LLU.Set_Random_Propagation_Delay (Min_Delay, Max_Delay);
				Debug.Put_Line("Simulación de retardos entre [" & Integer'Image(Min_Delay) & "," & 
								Integer'Image(Max_Delay) & "] milisegundos");
			end if;

			Fault_Pct := Integer'Value(ACL.Argument(5));
			if Fault_Pct < 0 or Fault_Pct > 100 then
				raise Fault_Error;
			else
				LLU.Set_Faults_Percent(Fault_Pct);
				Debug.Put_Line("El porcentaje de perdidas será de un" & Integer'Image(Fault_Pct) & "%");
			end if;

			if ACL.Argument_Count >= 7 then
				--Just if there are neighbors--
				Get_Neighbors;
			end if;
		else
			raise Usage_Error;
		end if;
	end Arguments_Input;

	procedure Read_String (Strings : out ASU.Unbounded_String; Nick: ASU.Unbounded_String) is
		package ASU_IO renames Ada.Strings.Unbounded.Text_IO;
		Prompt_Status: Boolean;
	begin
		Prompt_Status := Help.Get_Prompt_Status;
		if Prompt_Status then
			ATI.Put(ASU.To_String(Nick) & " >> ");
		end if;
		Strings := ASU_IO.Get_Line;
	end Read_String;

	procedure Logout (EP_H: LLU.End_Point_Type; Nick_Name: ASU.Unbounded_String; Seq_N: in out CH.Seq_N_T; Bol: Boolean) is
		Hour: Ada.Calendar.Time;
	begin
		Seq_N := Seq_N + 1;
		Users.Add_LM(EP_H, Seq_N, 1);

		CH.P_Buffer_Main := new LLU.Buffer_Type(1024);
		Users.Introduce_Logout(CH.P_Buffer_Main, 1, EP_H, EP_H, Nick_Name, Seq_N, Bol); --Always in chat_peer it will be Session 1 
		DM.Print_Flood_Logout(EP_H, EP_H, Seq_N, Nick_Name, Bol); -- True becouse it has been admitted on admission protocol
		Users.Send_ALL_Neighbors(CH.P_Buffer_Main);

        Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
        Debug.Put("     ");
		Users.Add_Sender_Buffering(EP_H, Seq_N, Hour, 1, CH.P_Buffer_Main);
        Debug.Put("     ");
        Users.Add_Sender_Dest(EP_H, Seq_N, 1);

        Debug.Put_Line("");
		Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);
		loop
			exit when CH.Sender_Dest.Map_Length(CH.Sender_Dest_List) = 0;
			delay 0.5 * Retransmissions.Relay_Time;
		end loop;	
	end Logout;

	procedure Writer (EP_H: LLU.End_Point_Type; Nick_Name: ASU.Unbounded_String; Seq_N: in out CH.Seq_N_T;
					  Message: ASU.Unbounded_String; Session_Id: Positive) is 
		Hour: Ada.Calendar.Time;
	begin
		Seq_N := Seq_N + 1;
		Users.Add_LM(EP_H, Seq_N, Session_Id);

		CH.P_Buffer_Main := new LLU.Buffer_Type(1024);
		Users.Introduce_Writer(CH.P_Buffer_Main, Session_Id, EP_H, EP_H, Nick_Name, Message, Seq_N);
		DM.Print_Flood_Writer(EP_H, EP_H, Seq_N, Nick_Name, Message);
		Users.Send_ALL_Neighbors(CH.P_Buffer_Main);

        Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
        Debug.Put("     ");
        Users.Add_Sender_Buffering(EP_H, Seq_N, Hour, Session_Id, CH.P_Buffer_Main);
        Debug.Put("     ");
        Users.Add_Sender_Dest(EP_H, Seq_N, Session_Id);

        Debug.Put_Line("");
	    Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);
	end Writer;

	-- This procedure read Strings and send it to all nodes --
	----------------------------------------------------------
	procedure Welcome_Message(EP_H, EP_R: LLU.End_Point_Type; Nick_Name:ASU.Unbounded_String; Seq_N: in out CH.Seq_N_T) is
		Message, Mess: ASU.Unbounded_String;
		N: Natural;
	begin
		ATI.Put_Line("Peer-Chat v1.0");
		ATI.Put_Line("==============");
		ATI.New_Line;
		ATI.Put_Line("Entramos en el chat con el Nick: " & ASU.To_String(Nick_Name));
		ATI.Put_Line(".h para help");

		while ASU.To_String(Message) /= ".salir" loop
    	   	Read_String(Message, Nick_Name);
			if ASU.To_String(Message) = ".h" or ASU.To_String(Message) = ".help" then
				Help.Message_h;
			elsif ASU.To_String(Message) = ".nb" or ASU.To_String(Message) = ".neighbors" then
				Help.Message_NB;
			elsif ASU.To_String(Message) = ".lm" or ASU.To_String(Message) = ".latest_msgs" then
				Help.Message_LM;
			elsif ASU.To_String(Message) = ".sb" or ASU.To_String(Message) = ".sender_buff" then
				Help.Message_SB;
			elsif ASU.To_String(Message) = ".sd" or ASU.To_String(Message) = ".sender_dest" then
                Help.Message_SD;
			elsif ASU.To_String(Message) = ".super" or ASU.To_String(Message) = ".super_node" then
				Help.Super_Node;
			elsif ASU.To_String(Message) = ".debug" then
				Help.Change_debug;
			elsif ASU.To_String(Message) = ".wai" or ASU.To_String(Message) = ".whoami" then
				Help.Message_WAI(Nick_Name, EP_H, EP_R);
            elsif ASU.To_String(Message) = ".fault" then
                Help.Message_Fault(Natural'Value(ACL.Argument(5)));
            elsif ASU.To_String(Message) = ".delay" then
                Help.Message_Delay(Natural'Value(ACL.Argument(3)), Natural'Value(ACL.Argument(4)));
			elsif ASU.To_String(Message) = ".prompt" then
				Help.Change_Prompt;
			elsif ASU.To_String(Message) = ".salir" then
				Logout(EP_H, Nick_Name, Seq_N, True); -- True becouse it has been admitted on admission protocol
			else
			  	begin
  	                N := ASU.Index(Message, " ");
	                Mess := ASU.Head(Message, N-1);
	                if(ASU.To_String(Mess) = ".priv") then
		                Private_Message.Analyze_Message(Message, N, EP_H);
	                else 
    					Writer(EP_H, Nick_Name, Seq_N, Message, 1);
                    end if;
                exception
                    when Constraint_Error =>
					    Writer(EP_H, Nick_Name, Seq_N, Message, 1);
                end;
			end if;
		end loop;
	end Welcome_Message;

	procedure Reject_Message(P_Buffer: access LLU.Buffer_Type; EP_H: LLU.End_Point_Type; Seq_N: in out CH.Seq_N_T;
							 Hour: Ada.Calendar.Time) is
		subtype Message_Type is CH.Message_Type;
		Mess: Message_Type;
        Session_Id: Positive;
		EP_H_Logout: LLU.End_Point_Type;
		Nick_Name: ASU.Unbounded_String;
        Seq_N_Reject: CH.Seq_N_T;
	begin
		Mess := Message_Type'Input(P_Buffer);
        Session_Id := Positive'Input(P_Buffer);
		EP_H_Logout := LLU.End_Point_Type'Input(P_Buffer);
        Seq_N_Reject := CH.Seq_N_T'Input(P_Buffer);
		Nick_Name := ASU.Unbounded_String'Input(P_Buffer);

		DM.Print_RCV_Reject(EP_H_Logout, Nick_Name);
        Debug.Put(" ");
        CH.Send_Ack(EP_H, EP_H_Logout, Seq_N_Reject, Session_Id);

        ATI.Put_Line("Usuario rechazado porque " & Generic_Funct_Types.Print_EP(EP_H_Logout) & " está usando el mismo nick");
        ATI.New_Line;
          
        Logout(EP_H, Nick_Name, Seq_N, False); -- False becouse it has not been admitted on admission protocol

		delay 10.0;
       	ATI.Put_Line("Terminamos el Programa");
	end Reject_Message;


	--Variables --
	Port: Natural;
	Nick_Name: ASU.Unbounded_String;
	Min_Delay, Max_Delay, Fault_Pct: Natural;
    Message: Generic_Funct_Types.New_Mssg_Type;

	EP_R, EP_H: LLU.End_Point_Type;
	Buffer: aliased LLU.Buffer_Type(1024);
	Seq_N: CH.Seq_N_T := 0;
	Hour: Ada.Calendar.Time;
	Expired, Success: Boolean;
begin
	begin
		Arguments_Input(Port, Nick_Name, Min_Delay, Max_Delay, Fault_Pct);

		EP_H := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
		LLU.Bind(EP_H, CH.Users_Handler'Access); --Build a free Handler.End_Point
		LLU.Bind_Any(EP_R); --Build a free End_Point

		-- Control_C program --
		-----------------------
		--Gnat.Ctrl_C.Install_Handler(Ctrl_C_Handler.Ctrl_C'Access);

		if ACL.Argument_Count >= 7 then
			ATI.New_Line;
			Debug.Put_Line("Iniciando Protocolo de Admisión ...");
			
			Seq_N := Seq_N + 1;
			Users.Add_LM(EP_H, Seq_N, 1);		

			CH.P_Buffer_Main := new LLU.Buffer_Type(1024);
			Users.Introduce_Init(CH.P_Buffer_Main, 1, EP_H, EP_H, EP_R, Nick_Name, Seq_N);
			DM.Print_Flood_Init(EP_H, EP_H, Seq_N, Nick_Name);
			Users.Send_ALL_Neighbors(CH.P_Buffer_Main); --Send to neighbors;

			Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
            Debug.Put("     ");
			Users.Add_Sender_Buffering(EP_H, Seq_N, Hour, 1, CH.P_Buffer_Main); --Always in chat_peer it will be Session 1
            Debug.Put("     ");
            Users.Add_Sender_Dest(EP_H, Seq_N, 1);
            
            Debug.Put_Line("");
			Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);

			LLU.Reset(Buffer);
			LLU.Receive(EP_R, Buffer'Access, 10.0 * Retransmissions.Relay_Time, Expired);

			-- It exits just when sender dest list is empty --
			loop
				exit when (not expired) or CH.Sender_Dest.Map_Length(CH.Sender_Dest_List) = 0;
				delay 1.5 * Retransmissions.Relay_Time;
			end loop;

			if Expired and (not CH.Must_Logout) then
				ATI.New_Line;
				Seq_N := Seq_N + 1;
				Users.Add_LM(EP_H, Seq_N, 1);

			    CH.P_Buffer_Main := new LLU.Buffer_Type(1024);
				Users.Introduce_Confirm(CH.P_Buffer_Main, 1, EP_H, EP_H, Nick_Name, Seq_N); --Always in chat_peer it will be Session 1
				DM.Print_Flood_Confirm(EP_H, EP_H, Seq_N, Nick_Name);
				Users.Send_ALL_Neighbors(CH.P_Buffer_Main);

                Hour := Ada.Calendar.Clock + Retransmissions.Relay_Time;
                Debug.Put("     ");
			    Users.Add_Sender_Buffering(EP_H, Seq_N, Hour, 1, CH.P_Buffer_Main);
                Debug.Put("     ");
                Users.Add_Sender_Dest(EP_H, Seq_N, 1);

			    Timed_Handlers.Set_Timed_Handler (Hour, Retransmissions.Timer_Handler'Access);

				ATI.New_Line;
				Debug.Put_Line("Fin del Protocolo de Admisión.");

				ATI.New_Line;
               	Welcome_Message(EP_H, EP_R, Nick_Name, Seq_N);
            else
                Users.Delete_Sender_Dest(Ep_H, Seq_N, 1); --For not send Init Message if it still on retransmission
            	Reject_Message(Buffer'Access, EP_H, Seq_N, Hour);

				ATI.New_Line;
				Debug.Put_Line("Fin del Protocolo de Admisión");
			end if;
		else
			ATI.New_Line;
			Debug.Put_Line("No hacemos protocolo de admisión pues no tenemos contactos iniciales ...");
			Welcome_Message(EP_H, EP_R, Nick_Name, Seq_N);
		end if;

	exception
		when Usage_Error =>
			Debug.Put_Line("./chat_peer_2 port nick MinDelay Maxdelay FaultPct " &
							"[[nb_host nb_port] [nb_host nb_port]]", Pantalla.Rojo);
		when Port_Error =>
			Debug.Put_Line("Puerto incorrecto [1024 < Port < 65535]", Pantalla.Rojo);
		when Delay_Error =>
			Debug.Put_Line("[Max_Delay >= Min_Delay]", Pantalla.Rojo);
		when Fault_Error =>
			Debug.Put_Line("[0 < Fault_Pct < 100]", Pantalla.Rojo);
		when Port_Error_Neighbor_1 =>
			Debug.Put_Line("Puerto Neighbor_1 incorrecto [1024 < Port < 65535]", Pantalla.Rojo);
		when Port_Error_Neighbor_2 =>
			Debug.Put_Line("Puerto Neighbor_2 incorrecto [1024 < Port < 65535]", Pantalla.Rojo);
		when Program_Error =>
            Message.EP := EP_H;
            Message.Session_Id := 1;
			CH.Latest_Msgs.Get(CH.L_M_List, Message, Seq_N, Success);
			Logout(EP_H, Nick_Name, Seq_N, True); -- True becouse it has been admitted on admission protocol
		when Ex:others =>
			Debug.Put_Line("Excepción imprevista: " & Ada.Exceptions.Exception_Name(Ex) & " en: " &
							Ada.Exceptions.Exception_Message(Ex), Pantalla.Rojo);
	end;
    LLU.Finalize;
	Timed_Handlers.Finalize;
end;
