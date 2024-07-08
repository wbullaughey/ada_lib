with AUnit.Test_Suites;

package Ada_Lib.Trace.Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;
   Debug_Test                    : Boolean := False;
   Debug_Tests                   : Boolean := False;

end Ada_Lib.Trace.Tests;