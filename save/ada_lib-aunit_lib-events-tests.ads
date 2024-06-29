with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Ada_Lib.Unit_Test.Tests;

package Ada_Lib.Unit_Test.Events.Tests is

   type Test_Type is new Ada_Lib.Unit_Test.Tests.Test_Case_Type with null record;

   type Test_Access is access Test_Type;

   procedure Mouse_Click (
      Test                       : in out Standard.AUnit.Test_Cases.Test_Case'class);

   function Name (Test : Test_Type) return Standard.AUnit.Message_String;

   procedure Register_Tests (Test : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type
   ) with pre => Test.Verify_Presetup,
          post => Test.Verify_Postsetup;

   function Suite return Standard.AUnit.Test_Suites.Access_Test_Suite;

-- overriding
-- procedure Tear_Down (Test : in out Test_Type)

   Suite_Name                    : constant String := "AUnit_Events";

end Ada_Lib.Unit_Test.Events.Tests;
