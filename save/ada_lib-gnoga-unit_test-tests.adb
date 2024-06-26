--with Ada_Lib.GNOGA;
with ADA_LIB.Trace; use ADA_LIB.Trace;

package body Ada_Lib.GNOGA.Unit_Test.Tests is

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out GNOGA_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Test.Connection_Data := Ada_Lib.GNOGA.Get_Connection_Data;
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Set_Up;
      Log_Out (Debug);
   end Set_Up;
end Ada_Lib.GNOGA.Unit_Test.Tests;
