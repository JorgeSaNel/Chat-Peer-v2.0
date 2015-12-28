-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Ada.Calendar;
with Lower_Layer_UDP;

package Generic_Funct_Types is
	package LLU renames Lower_Layer_UDP;

	function Print_EP(EP: LLU.End_Point_Type) return String;

	function Time_To_String(T: Ada.Calendar.Time) return String;

	type Seq_N_T is mod Integer'Last;

    -- Latest_Messages Types --
    ---------------------------
    
    -- Key Type --
    type New_Mssg_Type is record
        Session_Id: Positive;
        EP: LLU.End_Point_Type;
    end record;

    function "=" (Left, Right : New_Mssg_Type) return Boolean;
    
    function Print_Message(Message: New_Mssg_Type) return String;

	-- Sender Dest Types --
	-----------------------
	
	-- Key Type --
	type Mess_Id_T is record
		EP: LLU.End_Point_Type;
		Seq_N: Seq_N_T;
        Session_Id: Positive := 1;
	end record;

	-- Value Type --
	type Destination_T is record
		EP: LLU.End_Point_Type := null;
		Retries : Natural := 0;
	end record;

	type Destinations_T is array (1..10) of Destination_T;

	-- Functions of Sender Dest --
	function Print_Mess_Id (Mess: Mess_Id_T) return String;
	function Print_Destinations_T (Destinations: Destinations_T) return String;

	function "=" (Left, Right : Mess_Id_T) return Boolean;
	function "<" (Left, Right : Mess_Id_T) return Boolean;
	function ">" (Left, Right : Mess_Id_T) return Boolean;


	-- Sender Buffering Types --
	----------------------------

	type Buffer_A_T is access LLU.Buffer_Type;

	-- Value Type --
	type Value_T is record
		EP_H_Creat : LLU.End_Point_Type;
		Seq_N : Seq_N_T;
		P_Buffer : Buffer_A_T;
        Session_Id: Positive := 1;
	end record;
	
	function Print_Value_T (Value: Value_T) return String;

end Generic_Funct_Types;
