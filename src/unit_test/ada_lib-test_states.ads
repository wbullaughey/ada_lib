with Ada_Lib.Lock;
with Ada_Lib.Trace;
with Gnoga.Gui.Window;
with Gnoga.Types;

package Ada_Lib.Test_States is

   Failed                        : exception;

   use type Gnoga.Gui.Window.Pointer_To_Window_Class;

   subtype Window_Type           is Gnoga.Gui.Window.Window_Type;
   subtype Window_Class_Access   is Gnoga.Gui.Window.Pointer_To_Window_Class;
   type Window_Constant_Class_Access
                                 is access constant Window_Type'class;
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
   type Window_Lock_Type            is new Ada_Lib.Lock.Lock_Type with private;

   procedure Clear_Window (
      Lock        : in out Window_Lock_Type
   ) with Pre    => Lock.Has_Window;

   procedure Clear_Window_Connection_Data (
      Window               : in     Window_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here);

   function Get_Window (
      Lock        : in     Window_Lock_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class
   with Pre    => Lock.Has_Window;

   function Has_Window (
      Lock        : in     Window_Lock_Type
   ) return Boolean;

   procedure Set_Window (
      Lock        : in out Window_Lock_Type;
      Window      : in     Gnoga.Gui.Window.Pointer_To_Window_Class
   ) with Pre  => not Lock.Has_Window;

   procedure Allocate_State (
      Window               : in     Window_Class_Access;
      Window_Connection    : in     Window_Connection_Class_Access := Null
   ) with Pre  => Window /= Null and then
                  Window_Connection /= Null;

   function Get_Window_Connection_Data (
      Window               : in     Window_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Window_Connection_Class_Access
   with Pre  => Window /= Null;

   function Get_Window_Connection_Data (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Window_Connection_Class_Access
   with Pre    => Window_Connection /= Null;

   function Has_Window_Connection_Data (
      Window               : in     Window_Constant_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with Pre  => Window /= Null;

   function Has_Window_Connection_Data (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with Pre    => Window_Connection /= Null;

   procedure Set_Window_Connection (
      Window               : in     Window_Constant_Class_Access;
      Window_Connection    : in     Window_Connection_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here
   ) with Pre  => Window /= Null and then
                  Window_Connection /= Null;

   Pre_Window_State  : constant Window_Class_Access;
                        -- from its creation until its saved in the window state

private

-- Null_State_ID                 : constant State_ID_Type := (
--    Set   => False,
--    Value => 0);

   -- associates the main window with the connection data
   type State_Type         is tagged record
      Window               : Window_Constant_Class_Access := Null;
      Window_Connection    : Window_Connection_Class_Access := Null;
   end record;

   type Window_Lock_Type   is new Ada_Lib.Lock.Lock_Type with record
      Window               : Gnoga.Gui.Window.Pointer_To_Window_Class := Null;
   end record;

   Pre_Window_State        : constant Window_Class_Access := Null;

end Ada_Lib.Test_States;
