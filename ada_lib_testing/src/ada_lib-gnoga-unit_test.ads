--with Ada_Lib.Command_Line_Iterator;
--with Ada_Lib.Socket_IO;
with Ada_Lib.Unit_Test.Tests;

package Ada_Lib.GNOGA.Unit_Test is

   Failed                        : exception;

   -- type to be used by all specific tests that use GNOGA
   type GNOGA_Tests_Type (
      Initialize_GNOGA           : Boolean;
      Test_Driver                : Boolean) is abstract new Ada_Lib.Unit_Test.
                                    Tests.Test_Case_Type with null record;

   type GNOGA_Tests_Access       is access GNOGA_Tests_Type;
   type GNOGA_Tests_Class_Access is access GNOGA_Tests_Type'class;

   overriding
   procedure Set_Up (
      Test                       : in out GNOGA_Tests_Type);

   overriding
   procedure Tear_Down (Test : in out GNOGA_Tests_Type);

   overriding
   function Verify_Set_Up (
      Test                       : in     GNOGA_Tests_Type
   )  return Boolean;

   Debug                         : aliased Boolean := False;

end Ada_Lib.GNOGA.Unit_Test;
