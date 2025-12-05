with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with AUnit.Test_Suites;

package body Ada_Lib.States is

-- use type Camera_Base_State_Class_Access;
-- use type Camera_Configuration_State_Class_Access;
-- use type Camera_Main_Window_Connection_Class_Access;

   package Unit_Test is

      function Suite return AUnit.Test_Suites.Access_Test_Suite;

   end Unit_Test;

   package body Unit_Test is separate;

   function State_ID_Equal (
      Left, Right       : State_ID_Type
   ) return Boolean;

   function State_ID_Hash (
      ID                : State_ID_Type
   ) return Ada.Containers.Hash_Type;

   package State_Package  is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type       => State_ID_Type,
      Element_Type   => State_Access,
      Hash           => State_ID_Hash,
      Equivalent_Keys=> State_ID_Equal);

   Current_State_ID        : State_ID_Type;

   States                  : State_Package.Map;

-- ----------------------------------------------------------------
-- procedure Allocate_Connection_Data (
--    State_ID   : State_ID_Type := Null_State_ID) is
-- ----------------------------------------------------------------
--
--    State       : constant Camera_State_Access :=
--                   Allocate_State (State_ID);
--    pragma Unreferenced (State);
--
-- begin
--    null;
-- end Allocate_Connection_Data;

   ----------------------------------------------------------------
   function Allocate_State (
      State_ID            : in        State_ID_Type'class := Null_State_ID
   ) return State_Access is
   ----------------------------------------------------------------

      Lookup_State_ID     : constant State_ID_Type := (if State_ID.Set then
                                 State_ID_Type (State_ID)
                              else
                                 Current_State_ID);
   begin
      Log_In (Debug, "ID:" & Lookup_State_ID.Image);
      if States.Contains (Lookup_State_ID) then
         Log_Out (Debug, "current state");
         return State_Access (States.Element (Lookup_State_ID));
      else
         declare
            State    : constant State_Access := new State_Type;

         begin
            States.Insert (Lookup_State_ID, State);
            Log_Out (Debug, "new state");
            return State;
         end;
      end if;

   end Allocate_State;

   ----------------------------------------------------------------
   function Get_Window_Connection_Data (
      State_ID             : in     State_ID_Type'class := Null_State_ID
   ) return Window_Connection_Class_Access is
   ----------------------------------------------------------------

      State                : constant State_Access := Allocate_State (State_ID);

   begin
      return State.Window_Connection;
   end Get_Window_Connection_Data;

   ----------------------------------------------------------------
   function Has_State_ID
   return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Current_State_ID.Set,
         Debug or else Trace_Pre_Post_Conditions, Current_State_ID.Image);
   end Has_State_ID;

   ----------------------------------------------------------------
   function Has_State_ID (
      State_ID            : in        State_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State_ID.Set or else Current_State_ID.Set,
         Debug or else Trace_Pre_Post_Conditions,
         "current: " &Current_State_ID.Image &
         " parameter " & State_ID.Image);
   end Has_State_ID;

   ----------------------------------------------------------------
   function Has_State (
      State_ID            : in        State_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

      State          : constant State_Access := Allocate_State (State_ID);

   begin
      return Log_Here (State.Window_Connection /= Null,
         Debug or else Trace_Pre_Post_Conditions,
         "state for" & State_ID.Image & " not allocated");
   end Has_State;

   ----------------------------------------------------------------
   function Has_Window_Connection_Data (
      State_ID            : in        State_ID_Type'class := Null_State_ID
   ) return Boolean is
   ----------------------------------------------------------------

      State          : constant State_Access := Allocate_State (State_ID);

   begin
      return Log_Here (State.Window_Connection /= Null,
         Debug or else Trace_Pre_Post_Conditions,
         "Window Connection for " & State_ID.Image & " not allocated");
   end Has_Window_Connection_Data;

   ----------------------------------------------------------------
   function Image (
      State_ID             : in     State_ID_Type'class := Null_State_ID
   ) return String is
   ----------------------------------------------------------------

   begin
      return "set: " &State_ID.Set'img & " value:" & State_ID.Value'img;
   end Image;

   ----------------------------------------------------------------
   procedure Set_Current_State_ID (
      State_ID : in     State_ID_Type'class := Null_State_ID) is
   ----------------------------------------------------------------

   begin
      Current_State_ID := State_ID_Type (State_ID);
   end Set_Current_State_ID;

   ----------------------------------------------------------------
   procedure Set_Window_Connection (
      State_ID             : in out State_ID_Type;
      Window_Connection    : in     Window_Connection_Class_Access) is
   ----------------------------------------------------------------

      State                : constant State_Access := Allocate_State (State_ID);

   begin
      Log_In (Debug, "ID:" & State_ID.Image);
      State.Window_Connection := Window_Connection;
      Log_Out (Debug);
   end Set_Window_Connection;

   ----------------------------------------------------------------
   function State_ID_Equal (
      Left, Right       : State_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end State_ID_Equal;

   ----------------------------------------------------------------
   function State_ID_Hash (
      ID                      : State_ID_Type
   ) return Ada.Containers.Hash_Type is
   ----------------------------------------------------------------

   begin
      return ID.Value;
   end State_ID_Hash;

begin
   --Debug := False;
   Log_Here (Debug);
end Ada_Lib.States;
