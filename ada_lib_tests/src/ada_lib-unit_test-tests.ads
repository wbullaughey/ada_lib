with Ada_Lib.Options.Actual;
with Ada_Lib.Unit_Test.Test_Cases;

package Ada_Lib.Unit_Test.Tests is

   type Test_Case_Type           is abstract new Ada_Lib.Unit_Test.Test_Cases.
                                    Test_Case_Type with record
      Options                    : Ada_Lib.Options.Actual.
                                    Program_Options_Constant_Class_Access := Null;
      Test_Failed                : Boolean := False;
   end record;

   overriding
   procedure Set_Up (
      Test                       : in out Test_Case_Type);

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Case_Type);

   function Verify_Presetup (
      Test                       : in     Test_Case_Type
   ) return Boolean;

   function Verify_Postsetup (
      Test                       : in     Test_Case_Type
   ) return Boolean;

end Ada_Lib.Unit_Test.Tests;
