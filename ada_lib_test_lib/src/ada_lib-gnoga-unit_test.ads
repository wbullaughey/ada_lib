----with Ada_Lib.Socket_IO;
with Ada_Lib.Unit_Test.Test_Cases;

package Ada_Lib.GNOGA.Unit_Test is

   Failed                        : exception;

   type GNOGA_Tests_Interface    is limited interface;

   type GNOGA_Tests_Type (
      Initialize_GNOGA  : Boolean;
      Test_Driver       : Boolean) is abstract limited new
                  Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type with null record;

   type GNOGA_Tests_Access       is access GNOGA_Tests_Type;
   type GNOGA_Tests_Class_Access is access GNOGA_Tests_Type'class;

   overriding
   procedure Tear_Down (Test : in out GNOGA_Tests_Type);

-- overriding
-- function Verify_Set_Up (
--    Test                       : in     GNOGA_Tests_Type
-- )  return Boolean;

   overriding
   procedure Set_Up (
      Test                       : in out GNOGA_Tests_Type
   ) with Post => Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type'class (
      Test).Verify_Set_Up;

   Debug                         : aliased Boolean := False;


end Ada_Lib.GNOGA.Unit_Test;
