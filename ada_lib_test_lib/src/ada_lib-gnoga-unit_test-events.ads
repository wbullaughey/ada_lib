with AUnit.Test_Suites;
--with Ada_Lib.GNOGA;
-- with Ada_Lib.Unit_Test.Tests;
--with Ada_Lib.GNOGA.Unit_Test;

package Ada_Lib.GNOGA.Unit_Test.Events is

   function Suite return Standard.AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;

end Ada_Lib.GNOGA.Unit_Test.Events;
