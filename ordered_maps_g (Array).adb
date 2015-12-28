-- Jorge Santos Neila
-- Doble Grado en Sist. Telecomunicación + ADE

with Debug;
with Pantalla;

package body Ordered_Maps_G is

   Counter: Natural := 0;

   function Return_Position (M: Map; Key: Key_Type) return Natural is
       Center : Natural;
       First : Natural := 1;
       Last : Natural := Counter;
   begin
      while (First <= Last) and Counter /= 0 loop
         Center := (First + Last) / 2;
         if M(Center).Key = Key then
            exit;
          elsif Key < M(center).Key then
	        Last := center - 1;
          else 
            First := center + 1; 
          end if;
      end loop;
      return Center;
   end Return_Position;


   -- Ordenar array por el metodo de la sacudida --
   procedure Ordination (M: in out Map) is
      j, k, de: Integer := Counter;
      iz: Integer := 2;
      x: Node;
   begin
      if counter > 1 then
         loop
            while j >= iz loop
               if M(j-1).Key > M(j).Key then
                  x := M(j-1);
                  M(j-1) := M(j);
                  M(j) := x;
                  k := j;
               end if;
               j := j - 1;
         end loop;

         iz := k + 1;
         for j in iz..de loop
            if M(j-1).Key > M(j).Key then
               x := M(j-1);
               M(j-1) := M(j);
               M(j) := x;
               k := j;
            end if;
         end loop;
         de := k - 1;

         exit when iz > de;
         end loop;
      end if;
   end Ordination;

   procedure Get (M: Map; Key: in Key_Type; Value: out Value_Type; Success: out Boolean) is
       N : Natural;
   begin
       Success := False;
       if Counter /= 0 then
          N := Return_Position(M, Key);
          if M(N).Key = Key then
             Value := M(N).Value;
             Success := True;
          end if;
       end if;
   end Get;


   procedure Put (M: in out Map; Key: Key_Type; Value: Value_Type) is
      Success: Boolean := False;
      I: Natural;
   begin
      if Counter /= 0 then
          I := Return_Position(M, Key);
          if M(I).Key = Key then
             M(I).Value := Value;
             Success := True;
          end if;
      end if;
      if not Success then
          Counter := Counter + 1;
          M(Counter).Key := Key;
          M(Counter).Value := Value;

          Ordination(M);
      end if;
   end Put;

   procedure Delete (M: in out Map; Key: in  Key_Type; Success: out Boolean) is
      I: Natural;
   begin
      Success := False;
      I := Return_Position(M, Key);
      if M(I).Key = Key then
          M(I).Key := M(Counter).Key;
          M(I).Value := M(Counter).Value;

          Counter := Counter - 1;
          Success := True;

          Ordination(M);
      end if;   
   end Delete;

   function Map_Length (M : Map) return Natural is
   begin
      return Counter;
   end Map_Length;

   procedure Print_Map (M : Map) is
   begin
      Debug.Put_Line ("                      --------------------", Pantalla.Rojo);
      for I in 1..Counter loop
         Debug.Put_Line ("                      " & 
                         "(" & Key_To_String(M(I).Key) & " => " & Value_To_String(M(I).Value) & ")", Pantalla.Rojo);
      end loop;
   end Print_Map;

end Ordered_Maps_G;
