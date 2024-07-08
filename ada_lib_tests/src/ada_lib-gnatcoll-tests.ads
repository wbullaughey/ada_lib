with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Ada_Lib.Unit_Test.Test_Cases;

package Ada_Lib.GNATCOLL.Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;

end Ada_Lib.GNATCOLL.Tests;
