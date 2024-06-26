--with Ada_Lib.GNOGA;
--with Ada_Lib.GNOGA.Unit_Test;

package Ada_Lib.GNOGA.Unit_Test.Tests is

   Failed                        : exception;

   type GNOGA_Test_Type          is abstract new Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type
                                    with null record;

   type GNOGA_Test_Access is access GNOGA_Test_Type;

   overriding
   procedure Set_Up (
      Test                       : in out GNOGA_Test_Type
   ) with Pre => Ada_Lib.GNOGA.Has_Connection_Data;

   Debug                         : aliased Boolean := False;

end Ada_Lib.GNOGA.Unit_Test.Tests;
