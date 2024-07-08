with AUnit.Test_Suites;

package Ada_Lib.Database.Get_Put_Tests is

   -- creates a test suite for all get_put tests that use DBDaemon
   -- adds a test list for local and Remote test to the test suite
   function Database_Suite (
      Which_Host              : in    Ada_Lib.Database.Which_Host_Type
   ) return AUnit.Test_Suites.Access_Test_Suite;

   -- creates a test suite for all get_put tests that no not use DBDaemon
   function No_Database_Suite return AUnit.Test_Suites.Access_Test_Suite;

end Ada_Lib.Database.Get_Put_Tests;
