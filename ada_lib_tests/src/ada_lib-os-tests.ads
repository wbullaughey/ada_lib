--with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Ada_Lib.Unit_Test.Tests;

package Ada_Lib.OS.Tests is

   Suite_Name                    : constant String := "OS";

   type Test_Type is new Ada_Lib.Unit_Test.Tests.Test_Case_Type with null record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (Test : in out Test_Type);

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Ada_Lib.OS.Tests;
