with Ada_Lib.Unit_Test.Tests;
--with AUnit.Test_Cases;
with AUnit.Test_Suites;

package Ada_Lib.Lock.Tests is

Suite_Name                    : constant String := "Lock";

   type Test_Type                is new Ada_Lib.Unit_Test.Tests.
                                    Test_Case_Type with null record;

   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type)
   with Pre => not Test.Verify_Set_Up,
        Post => Test.Verify_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

-- overriding
-- procedure Tear_Down (Test : in out Test_Type)
-- with post => Verify_Set_Up (Test);

   Debug                         : Boolean := False;

end Ada_Lib.Lock.Tests;