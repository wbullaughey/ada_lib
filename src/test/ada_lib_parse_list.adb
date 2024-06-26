with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Parse_List;
with Ada_Lib.Set;

procedure Ada_Lib.Parse_List is

   type Id_Type            is range 1 .. 1000;

   package Set is new Ada_Lib.Set (Id_Type);

   package Parser is new Ada_Lib.Parse_List (
      Set_Package    => Set);

   procedure Test (
      Data              : in   String;
      Valid             : in   Boolean) is

   begin
      declare
         ID_Set                  : Set.Set_Type;

      begin
         Parser.To_Set (Data, ID_Set);

         if Valid then
            Put_Line ("Set parse of '" & Data & "' = " & Set.Image (ID_Set));
         else
            Put_Line ("Invalid data " & Data & " set parsed ok");
         end if;

      exception
         when Fault: Parser.Invalid =>
            if Valid then
               Put_Line ("Invalid exception for good data " & Data);
            else
               Put_Line ("Expected Invalid for " & Data);
            end if;

         when Fault: others =>
            Put_Line ("Unexpected exception " &
               Ada.Exceptions.Exception_Name (Fault) &
               " for " & Data);
            Put_Line (Ada.Exceptions.Exception_Message (Fault));
      end;

      declare
         ID_Vector               : Set.Vector_Type (1 .. 100);
         Count                : Natural;

      begin
         Parser.To_Vector (Data, ID_Vector, Count);

         if Valid then
            Put_Line ("Vector parse of '" & Data & "' = " & Set.Image (ID_Vector (1 .. Count)));
         else
            Put_Line ("Invalid data " & Data & " vector parsed ok");
         end if;

      exception
         when Fault: Parser.Invalid =>
            if Valid then
               Put_Line ("Invalid exception for good data " & Data);
            else
               Put_Line ("Expected Invalid for " & Data);
            end if;

         when Fault: others =>
            Put_Line ("Unexpected exception " &
               Ada.Exceptions.Exception_Name (Fault) &
               " for " & Data);
            Put_Line (Ada.Exceptions.Exception_Message (Fault));
      end;
   end Test;

begin
   Test ("1,2,100", True);
   Test ("10-20", True);
   Test ("1,10-20,100", True);

   Test (" 1, 2 ,100 ", True);
   Test ("10 -20", True);
   Test ("10- 20", True);
   Test ("10 - 20", True);

   Test ("1,,2", False);
   Test (",2", False);
   Test ("2,", False);
   Test ("2-", False);
   Test ("-2", False);

   Test ("x", False);
   Test ("1,x", False);
   Test ("1-x", False);
   Test ("x-10", False);
   Test ("1.0", False);
end Ada_Lib.Parse_List;