-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Ada.Strings.Unbounded;
with Ada.Calendar;

with Maps_G;
with Maps_Protector_G;

with Ordered_Maps_G;
with Ordered_Maps_Protector_G;
with Generic_Funct_Types;

with Lower_Layer_UDP;

package Chat_Handler is
	package LLU renames Lower_Layer_UDP;
	package GFT renames Generic_Funct_Types;

	type Message_Type is (Init, Reject, Confirm, Writer, Logout, Ack, Priv);
	P_Buffer_Main: GFT.Buffer_A_T;
	P_Buffer_Handler: GFT.Buffer_A_T;

	EP_H_Logout: LLU.End_Point_Type;

    Must_Logout: Boolean := False;

	-- Instantiation Neighbors Package --
	-------------------------------------
	package NP_Neighbors is new Maps_G (Key_Type   => LLU.End_Point_Type,
								  		Value_Type => Ada.Calendar.Time,
								  		Null_Key   => Null,
								  		Null_Value => Ada.Calendar.Time_Of(2000,1,1),
								  		Max_Length => 10,
								  		"="		 => LLU."=",
								  		Key_To_String   => GFT.Print_EP,
								  		Value_To_String => GFT.Time_To_String);

	package Neighbors is new Maps_Protector_G (NP_Neighbors);


	-- Instantiation Latest_Messages Package --
	-------------------------------------------
	subtype Seq_N_T is GFT.Seq_N_T;
	package NP_Latest_Msgs is new Maps_G (Key_Type 	=> GFT.New_Mssg_Type,
									Value_Type 	=> Seq_N_T,
									Null_Key 	=> (1, null),
									Null_Value 	=> 0,
									Max_Length 	=> 50,
									"=" 			=> GFT."=",
									Key_To_String 	=> GFT.Print_Message,
									Value_To_String => Seq_N_T'Image);

	package Latest_Msgs is new Maps_Protector_G (NP_Latest_Msgs);

	
	-- Instantiation Sender_Dest Package --
	---------------------------------------
	package NP_Sender_Dest is new Ordered_Maps_G (Key_Type   => GFT.Mess_Id_T,
								  			Value_Type => GFT.Destinations_T,
								  			"="		 => GFT."=",
											"<"		 => GFT."<",
											">"		 => GFT.">",
								  			Key_To_String   => GFT.Print_Mess_Id,
								  			Value_To_String => GFT.Print_Destinations_T);

	package Sender_Dest is new Ordered_Maps_Protector_G (NP_Sender_Dest);

	
	-- Instantiation Sender_Buffering Package --
	--------------------------------------------
	package NP_Sender_Buffering is new Ordered_Maps_G (Key_Type   => Ada.Calendar.Time,
								  				Value_Type => GFT.Value_T,
								  				"="		 => Ada.Calendar."=",
												"<"		 => Ada.Calendar."<",
												">"		 => Ada.Calendar.">",
								  				Key_To_String   => GFT.Time_To_String,
								  				Value_To_String => GFT.Print_Value_T);

	package Sender_Buffering is new Ordered_Maps_Protector_G (NP_Sender_Buffering);

	package NP_Supernode is new Maps_G (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
								  		Value_Type => LLU.End_Point_Type,
								  		Null_Key   => Ada.Strings.Unbounded.To_Unbounded_String(""),
								  		Null_Value => Null,
								  		Max_Length => 100,
								  		"="		 => Ada.Strings.Unbounded."=",
								  		Key_To_String   => Ada.Strings.Unbounded.To_String,
								  		Value_To_String => GFT.Print_EP);

	package Supernode is new Maps_Protector_G (NP_Supernode);


	Super_List: Supernode.Prot_Map;
	N_List: Neighbors.Prot_Map;
	L_M_List: Latest_Msgs.Prot_Map;

	Sender_Dest_List: Sender_Dest.Prot_Map;
	Sender_Buff_List: Sender_Buffering.Prot_Map;


	-- Handler para utilizar como parámetro en LLU.Bind en el cliente
	-- Este procedimiento NO debe llamarse explícitamente
	procedure Users_Handler (From: in LLU.End_Point_Type; To: in LLU.End_Point_Type; P_Buffer: access LLU.Buffer_Type);

    -- Send ACK --
    procedure Send_ACk (EP_H_ACK, EP_H_Creat: LLU.End_Point_Type; Seq_N: Seq_N_T; Session_ID: Positive);
end Chat_Handler;
