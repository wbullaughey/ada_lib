with Ada_Lib.Unit_Test.Tests;
with AUnit.Test_Cases;
with AUnit.Test_Suites;

package Ada_Lib.JSON.Tests is

   Suite_Name                    : constant String := "JSON";

   type Test_Type                is new Ada_Lib.Unit_Test.Tests.
                                    Test_Case_Type with null record;

   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

   procedure Test_Load (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- overriding
-- procedure Set_Up (Test : in out Test_Type)
-- with post => Test.Was_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

-- overriding
-- procedure Tear_Down (Test : in out Test_Type)
-- with post => Was_Torn_Down (Test);

end Ada_Lib.JSON.Tests;
