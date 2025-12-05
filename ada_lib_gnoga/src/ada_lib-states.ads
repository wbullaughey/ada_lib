with Ada.Containers;
with Gnoga.Types;

package Ada_Lib.States is

   type State_ID_Type   is tagged private;
   Null_State_ID        : constant State_ID_Type;

   type Window_Connection_Class_Access
                     is access all Gnoga.Types.Connection_Data_Type'class;
-- type Camera_Names_Type
--                   is array (Positive range <>) of
--                      Ada_Lib.Strings.Unlimited.String_Type;
   type State_Type                  is tagged private;
   type State_Access                is access State_Type;
   type State_Constant_Access       is access all State_Type;
   type State_Class_Access          is access State_Type'class;
   type State_Constant_Class_Access is access constant State_Type'class;


   function Has_State_ID
   return Boolean;

   function Has_State_ID (
      State_ID            : in        State_ID_Type
   ) return Boolean;

   function Get_Window_Connection_Data (
      State_ID             : in     State_ID_Type'class := Null_State_ID
   ) return Window_Connection_Class_Access
   with Pre => Has_Window_Connection_Data;

   function Has_Window_Connection_Data (
      State_ID            : in        State_ID_Type'class := Null_State_ID
   ) return Boolean;

   function Image (
      State_ID             : in     State_ID_Type'class := Null_State_ID
   ) return String;

   procedure Set_Current_State_ID (
      State_ID             : in     State_ID_Type'class := Null_State_ID
   ) with Pre  => not Has_Window_Connection_Data,
          Post => Has_Window_Connection_Data;

   procedure Set_Window_Connection (
      State_ID             : in out State_ID_Type;
      Window_Connection    : in     Window_Connection_Class_Access);

   Debug                   : Boolean := False;

private

   type State_ID_Type            is tagged record
      Set                        : Boolean := False;
      Value                      : Ada.Containers.Hash_Type;
   end record;

   Null_State_ID                 : constant State_ID_Type := (
      Set   => False,
      Value => 0);

   type State_Type         is tagged record
      Window_Connection    : Window_Connection_Class_Access := Null;
   end record;

end Ada_Lib.States;
