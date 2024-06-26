with AUnit.Test_Suites;
with Ada_Lib.Unit_Test.Tests;

package Ada_Lib.Configuration.Tests is

   type Test_Type is new Ada_Lib.Unit_Test.Tests.Test_Case_Type with record
      Configuration              : Configuration_Type;
   end record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (Test : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type
   ) with Pre => Test.Verify_Presetup,
          Post => Test.Verify_Postsetup;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (Test : in out Test_Type);

   Debug                         : Boolean := False;
private

   Suite_Name                    : constant String := "Configuration";

end Ada_Lib.Configuration.Tests;

