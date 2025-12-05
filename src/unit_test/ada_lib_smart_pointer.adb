with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Smart_Pointer_Object;

procedure Ada_Lib.Smart_Pointer is

   package Object          renames Ada_Lib.Smart_Pointer_Object;
   package Pointer            renames Ada_Lib.Smart_Pointer_Object.Pointer;

-- procedure X (
--    O                 : in out Object.Object_Type) is
--
-- begin
--    O.Field := O.Field + 1;
-- end X;

   P                    : Pointer.Pointer_Type;
   I                    : Object.Object_Type;

begin
   I.Field := 99;

   Pointer.Allocate (
      Object   => I,
      Pointer  => P);

   Put_Line ("enter block");
   declare
      V                 : Object.Object_Type renames Pointer.Reference (P);
--    N                 : Object.Object_Type renames Pointer.Reference (P);

   begin
      Put_Line ("v" & V.Field'img);
--    Put_Line ("n" & N.Field'img);

--    Put_Line ("increment v");
--    X (Pointer.Reference (P));
--
--    Put_Line ("v" & V.Field'img);
--    Put_Line ("n" & N.Field'img);
--
--    Put_Line ("increment n");
--    N.Field := N.Field + 1;
--
--    Put_Line ("v" & V.Field'img);
--    Put_Line ("n" & N.Field'img);
      Put_Line ("exit block");
   end;
   Put_Line ("exit program");
end Ada_Lib.Smart_Pointer;