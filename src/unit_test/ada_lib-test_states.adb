with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Strings;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with AUnit.Test_Suites;
with Hex_IO;
with System;

package body Ada_Lib.Test_States is

   use type System.Address;

   function Image (
      Window           : in      Window_Constant_Class_Access
   ) return String;

   function Image (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class
   ) return String;

   function Get_State (
      Window               : in     Window_Constant_Class_Access;
      From                 : in     String :=  Standard.Ada_Lib.Trace.Here
   ) return State_Access;

   function Get_State (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class;
      From                 : in     String :=  Standard.Ada_Lib.Trace.Here
   ) return State_Access;

   function Hash_Equivalent (
      Left, Right          : in     System.Address
   ) return Boolean;

   function Window_Hash (
      Key                  : in     System.Address
   ) return Ada.Containers.Hash_Type
   with Pre => Key /= System.Null_Address;

   package State_Package  is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type       => System.Address,
      Element_Type   => State_Access,
      Hash           => Window_Hash,
      Equivalent_Keys=> Hash_Equivalent);

   procedure Dump;

   Debug             : Boolean renames Options.Unit_Test.Ada_Lib_Test_States.Debug;
   States            : State_Package.Map;

   ----------------------------------------------------------------
   procedure Allocate_State (
      Window               : in     Window_Class_Access;
      Window_Connection    : in     Window_Connection_Class_Access) is
   ----------------------------------------------------------------

      State                         : constant State_Access := new State_Type;

   begin
      Log_In (Debug, "window " & Image (Window_Constant_Class_Access (Window))  &
         " address " & Ada_Lib.Strings.Image (Window.all'address) &
         " connection " & Image (Window_Connection));
      State.Window := Window_Constant_Class_Access (Window);
      State.Window_Connection := Window_Connection;
      State_Package.Insert (States, Window.all'address, State);
      if Debug then
         Dump;
      end if;
      Log_Out (Debug);
   end Allocate_State;

   ----------------------------------------------------------------
   procedure Clear_Window (
      Lock        : in out Window_Lock_Type) is
   ----------------------------------------------------------------

   begin
      Lock.Window := Null;
   end Clear_Window;

   ----------------------------------------------------------------
   procedure Clear_Window_Connection_Data (
      Window               : in     Window_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "called from " & From);
      if Window /= Null then
         State_Package.Delete (States, Window.all'address);
      end if;
   end Clear_Window_Connection_Data;

   ----------------------------------------------------------------
   procedure Dump  is
   ----------------------------------------------------------------

      -------------------------------------------------------------
      procedure Process (
         Position    : in State_Package.Cursor) is
      -------------------------------------------------------------

         Element     : constant State_Access :=
                        State_Package.Constant_Reference (States, Position);

      begin
         Put_Line ("key: " &
            Ada_Lib.Strings.Image (State_Package.Key (Position)) &
            " hash" & Window_Hash (Element.all'address)'img &
            " element: " & Ada_Lib.Strings.Image (Element.all'address));
      end Process;
      -------------------------------------------------------------

   begin
      State_Package.Iterate (States, Process'access);
   end Dump;

   ----------------------------------------------------------------
   function Get_State (
      Window               : in     Window_Constant_Class_Access;
      From                 : in     String :=  Standard.Ada_Lib.Trace.Here
   ) return State_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "window: " & Image (Window) & " address " &
         Ada_Lib.Strings.Image (Window.all'address) &
         " from " & From);
      if Debug then
         Dump;
      end if;

      if State_Package.Contains (States, Window.all'address) then
log_here;
         return State_Access (State_Package.Element (
            States, Window.all'address));
      else
         raise Failed with "window " &
            Ada_Lib.Strings.Image (Window.all'address) &
            " not in states";
      end if;

   exception
      when Fault: others =>
         Log_Exception (True, Fault);
         raise;

   end Get_State;

   ----------------------------------------------------------------
   function Get_State (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class;
      From                 : in     String :=  Standard.Ada_Lib.Trace.Here
   ) return State_Access is
   ----------------------------------------------------------------

      Result         : State_Access := Null;

      -------------------------------------------------------------
      procedure Process (
         Position    : in     State_Package.Cursor) is
      -------------------------------------------------------------

         Element     : constant State_Access := State_Package.Element (Position);

      begin
         if Element.Window_Connection = Window_Connection then
            Result := Element;
         end if;
      end Process;
      -------------------------------------------------------------

   begin
      Log_In (Debug, "called from " & From);
      State_Package.Iterate (States, Process'access);
      if Result = Null then
         Log_Exception (Debug, "connection not found");
         raise Failed with Image (Window_Connection) & " not found";
      end if;
      Log_Out (Debug);
      return Result;
   end Get_State;

   ----------------------------------------------------------------
   function Get_Window (
      Lock        : in     Window_Lock_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class is
   ----------------------------------------------------------------

   begin
      return Lock.Window;
   end Get_Window;

   ----------------------------------------------------------------
   function Get_Window_Connection_Data (
      Window               : in     Window_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Window_Connection_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);

      declare
         State             : constant State_Access :=
                              Get_State (Window_Constant_Class_Access (Window));
      begin
         return State.Window_Connection;
      end;
   end Get_Window_Connection_Data;

   ----------------------------------------------------------------
   function Get_Window_Connection_Data (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Window_Connection_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);

      declare
         State             : constant State_Access := Get_State (Window_Connection);

      begin
         return State.Window_Connection;
      end;
   end Get_Window_Connection_Data;

   ----------------------------------------------------------------
   function Has_Window (
      Lock        : in     Window_Lock_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Lock.Window /= Null;
   end Has_Window;

   ----------------------------------------------------------------
   function Hash_Equivalent (
      Left, Right          : in     System.Address
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end Hash_Equivalent;

   ----------------------------------------------------------------
   function Window_Hash (
      Key                  : in     System.Address
   ) return Ada.Containers.Hash_Type is
   ----------------------------------------------------------------

      Result               : Ada.Containers.Hash_Type;
      for Result'Address use Key'Address;  -- Overlay Result on Key's storage
      pragma Import (Ada, Result);       -- Optional: suppresses initialization of Y

   begin
log_here ("hash " & Result'img);
      return Result;
   end Window_Hash;

   ----------------------------------------------------------------
   function Has_Window_Connection_Data (
      Window               : in     Window_Constant_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "window: " & Image (Window) & " from " & From);
      declare
         State    : constant State_Access := Get_State (Window);
         Result   : constant Boolean := State.Window_Connection /= Null;

      begin
         return Log_Here (Result,
            Debug or else Trace_Pre_Post_Conditions or else not Result,
            "Window Connection for " & Image (Window) &
            " not allocated called from ", From);
      end;
   end Has_Window_Connection_Data;

   ----------------------------------------------------------------
   function Has_Window_Connection_Data (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------


   begin
      Log_Here (Debug, "window connection: " &
         Image (Window_Connection) & " from " & From);
      declare
         State    : constant State_Access := Get_State (Window_Connection);
         Result   : constant Boolean := State.Window_Connection /= Null;

      begin
         return Log_Here (Result,
            Debug or else Trace_Pre_Post_Conditions or else not Result,
            "Window Connection for " & Image (Window_Connection) &
            " not allocated called from ", From);
      end;
   end Has_Window_Connection_Data;

-- ----------------------------------------------------------------
-- function Image (
--    Window           : in      State_Constant_Class_Access
-- ) return String is
-- ----------------------------------------------------------------
--
-- begin
--    return Hex_IO.Modular_Hex_Address (Window.all'address, 8);
-- end Image;

   ----------------------------------------------------------------
   function Image (
      Window           : in      Window_Constant_Class_Access
   ) return String is
   ----------------------------------------------------------------

   begin
      return Hex_IO.Modular_Hex_Address (Window.all'address, 8);
   end Image;

   ----------------------------------------------------------------
   function Image (
      Window_Connection    : not null access Gnoga.Types.Connection_Data_Type'class
   ) return String is
   ----------------------------------------------------------------

   begin
      return Hex_IO.Modular_Hex_Address (Window_Connection.all'address, 64);
   end Image;

   ----------------------------------------------------------------
   procedure Set_Window (
      Lock        : in out Window_Lock_Type;
      Window      : in     Gnoga.Gui.Window.Pointer_To_Window_Class) is
   ----------------------------------------------------------------

   begin
      Lock.Window := Window;
   end Set_Window;

-- ----------------------------------------------------------------
-- procedure Set_Window (
--    State_ID             : in out State_ID_Type;
--    Window               : in     Window_Class_Access;
--    From                 : in     String := Ada_Lib.Trace.Here) is
-- ----------------------------------------------------------------
--
-- begin
--    Log_Here (Debug, "called from " & From);
-- end Set_Window;
--
   ----------------------------------------------------------------
   procedure Set_Window_Connection (
      Window               : in     Window_Constant_Class_Access;
      Window_Connection    : in     Window_Connection_Class_Access;
      From                 : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "Window:" & Image (Window) & " called from " & From);
      declare
         State             : constant State_Access := Get_State (Window);

      begin
         State.Window_Connection := Window_Connection;
         Log_Out (Debug);
      end;
   end Set_Window_Connection;

-- ----------------------------------------------------------------
-- function State_ID_Equal (
--    Left, Right       : State_ID_Type
-- ) return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return Left = Right;
-- end State_ID_Equal;

-- ----------------------------------------------------------------
-- function State_ID_Hash (
--    ID                      : State_ID_Type
-- ) return Ada.Containers.Hash_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return ID.Value;
-- end State_ID_Hash;

begin
   --Debug := False;
   Log_Here (Debug);
end Ada_Lib.Test_States;
