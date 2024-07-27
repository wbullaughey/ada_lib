with AUnit.Test_Suites;
with Ada_Lib.Unit_Test.Tests;

package Ada_Lib.Options.Help.Tests is

   type Test_Type is new Ada_Lib.Unit_Test.Tests.Test_Case_Type with null record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (Test : in out Test_Type);

-- overriding
-- procedure Set_Up (Test : in out Test_Type)

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (Test : in out Test_Type);

   Debug                         : Boolean := False;
private

   Suite_Name                    : constant String := "Help";

end Ada_Lib.Options.Help.Tests;

