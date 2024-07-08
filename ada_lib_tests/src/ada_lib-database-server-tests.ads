with AUnit.Test_Suites;

package Ada_Lib.Database.Server.Tests is

   -- creates a test suite for all get_put tests that use DBDaemon
   -- adds a test list for local and Remote test to the test suite
   function Server_Suite (
      Which_Host              : in    Ada_Lib.Database.Which_Host_Type
   ) return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;

end Ada_Lib.Database.Server.Tests;
