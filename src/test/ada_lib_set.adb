with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.SET;

procedure Ada_Lib.Set is

   type Index_Type            is range 1 .. 80;

   package Set is new Ada_Lib.Set (Index_Type);
   use type Set.Unconstrained_Type;

   Set_1                : Set.Set_Type;

begin
   Set_1 := Set.Null_Set;
   Set_1 := Set_1 or Set.Singleton (5);

   Put_Line (Set.Image (Set_1));

   declare
      Set_2             : constant Set.Set_Type := Set.Null_Set;

   begin
      put_line ("set size " & integer'image (Set_2'size));
   end;

end Ada_Lib.Set;