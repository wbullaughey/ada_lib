with Ada_Lib.Options_Interface;
--with Ada_Lib.Options.Unit_Test;

package body Ada_Lib.Unit_Test.Tests is

   use type Ada_Lib.Options.Program_Options_Constant_Class_Access;
-- use type Ada_Lib.Options.Unit_Test.Unit_Test_Options_Constant_Class_Access;
   use type Ada_Lib.Options_Interface.Interface_Options_Constant_Class_Access;

   ----------------------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Test_Case_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Test.Options := Ada_Lib.Options.
         Program_Options_Constant_Class_Access (
            Ada_Lib.Options.Get_Modifiable_Options);
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up);
   end Set_Up;

   ----------------------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Test_Case_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Test.Options := Null;
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      Log_Out (Debug or Trace_Set_Up);
   end Tear_Down;

   ---------------------------------------------------------------
   function Verify_Pre_Setup (
      Test                       : in     Test_Case_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      if Test.Options /= Null then
         Log_Here ("Test.Options is not null");
         return False;
      else
         Log_Here (Debug, "Test.Options is null");
         return True;
      end if;

   end Verify_Pre_Setup;

   ---------------------------------------------------------------
   function Verify_Post_Setup (
      Test                       : in     Test_Case_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      return Log_Out (Ada_Lib.Options_Interface.Read_Only_Options /= Null,
         Debug, "Test.Options is null");
   end Verify_Post_Setup;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Ada_Lib.Unit_Test.Tests;
