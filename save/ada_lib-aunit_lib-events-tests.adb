with Ada_Lib.Trace; use Ada_Lib.Trace;


package body Ada_Lib.Unit_Test.Events.Tests is

   ---------------------------------------------------------------
   procedure Mouse_Click(
      Test                       : in out Standard.AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Mouse_Click;

   ---------------------------------------------------------------
   function Name (
      Test                       : in     Test_Type
   ) return Standard.AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return Standard.AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   procedure Register_Tests (Test : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Test.Add_Routine (Standard.AUnit.Test_Cases.Routine_Spec'(
         Routine        => Mouse_Click'access,
         Routine_Name   => Standard.AUnit.Format ("Mouse_Click")));
      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   procedure Set_Up (Test : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Set_Up;

   ---------------------------------------------------------------
   function Suite return Standard.AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant Standard.AUnit.Test_Suites.Access_Test_Suite :=
                                    new Standard.AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Suite;

-- ---------------------------------------------------------------
-- procedure Tear_Down (Test : in out Test_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log (Debug, Here, Who);
--    Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
-- end Tear_Down;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Ada_Lib.Unit_Test.Events.Tests;
