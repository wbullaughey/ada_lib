--with Ada_Lib.Test.Tests;
--with AUnit.Assertions; use AUnit.Assertions;
--with Ada_Lib.Database.Subscription.Tests;
--with Ada_Lib.Database.Updater.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Unit_Test.Test_Cases;
--with Ada_Lib.Test; -- .Tests;

package body Ada_Lib.Database.Subscribe_Tests is

   ---------------------------------------------------------------
   overriding
   function Allocate (
      Table                      : in     Test_Table_Type
   ) return Ada_Lib.Database.Subscribe.Entry_Class_Access is
   ---------------------------------------------------------------

   begin
      return new Updater_Type;
   end Allocate;

begin

   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Ada_Lib.Database.Subscribe_Tests;

