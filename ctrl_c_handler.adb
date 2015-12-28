-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Ada.Text_IO;

package body Ctrl_C_Handler is

	procedure Ctrl_C is
		C: Character;
	begin
		Ada.Text_IO.New_Line;		
		Ada.Text_IO.Put_Line ("Has pulsado CTRL-C. ¿Realmente quieres terminar? [Y/N]");
		loop
			Ada.Text_IO.Get_Immediate (C);
    	 	if C = 'Y' or C = 'y' then
				raise Program_Error;
			elsif C = 'N' or C = 'n' then
				Ada.Text_IO.Put_Line("Seguimos con el programa...");
				exit;
			else
				Ada.Text_IO.Put_Line("[Y/N]");
			end if;
		end loop;
	end Ctrl_C;

end Ctrl_C_Handler;

