--with Ada.Exceptions;
--with Ada.Text_IO; use Ada.Text_IO;
--with Aunit.Assertions; use Aunit.Assertions;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.Test is

-- use type Ada.Exceptions.Exception_Id;

--   ----------------------------------------------------------------------------
--   procedure Exception_Handler (
--      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
--      Where                      : in   String := Ada_Lib.Trace.Here;
--      Who                        : in   String := Ada_Lib.Trace.Who;
--      Message                    : in   String := "") is
--   ----------------------------------------------------------------------------
--
--   begin
--      Trace_Exception (Fault, Where, Who);
--
--      if Ada.Exceptions.Exception_Identity (Fault) =
--            Aunit.Assertions.Assertion_Error'identity then
--         Put ("assertion caught at " & Where & ". ");
--         if Message'length > 0 then
--            Put (Quote ("Message", Message));
--         end if;
--         New_Line;
----       Ada.Exceptions.Raise_Exception (Ada.Exceptions.Exception_Identity (Fault), Message);
--      else
--         Assert (False, "Exception " & Ada.Exceptions.Exception_Name (Fault) &
--            " message " & Ada.Exceptions.Exception_Message (Fault) &
--            " caught at " & Where & " in " & Who & " " &
--            (if Message'length = 0 then "" else " message " & Message));
--      end if;
--end Exception_Handler;

-- ----------------------------------------------------------------------------
-- procedure Help (
--    Option                     : in     Character) is
-- ----------------------------------------------------------------------------
--
-- begin
--    Put_Line ("Ada_Lib AUnit test options (-" & Option & ")");
--    Put_Line ("      a               all");
--    Put_Line ("      l               Ada_Lib.Test");
--    Put_Line ("      u               Ada_Lib.Unit_Test");
-- end Help;
--
   ----------------------------------------------------------------------------
   function Near (
      Actual                     : in     Value_Type;
      Expected                   : in     Value_Type;
      Tolerance                  : in     Value_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      return abs (Actual - Expected) < Tolerance;
   end Near;

   ----------------------------------------------------------------------------
   procedure Raise_Assert_Failed (
      Message                    : in     String;
      Called_From                : in     String := Ada_Lib.Trace.Who;
      Raised_From                : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------------------

   begin
      raise Ada_Lib.Test.Assert_Failed with Message &
         " called from " & Called_From & " raised at " & Raised_From;
   end Raise_Assert_Failed;

-- ----------------------------------------------------------------------------
-- procedure Set_All_Traces is
-- ----------------------------------------------------------------------------
--
-- begin
--    Ada_Lib.Test.Debug := True;
--    Ada_Lib.Unit_Test.Debug := True;
-- end Set_All_Traces;

begin
--Debug := True;
   Log_Here (Elaborate);
end Ada_Lib.Test;
