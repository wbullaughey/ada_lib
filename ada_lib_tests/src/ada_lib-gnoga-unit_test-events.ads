with AUnit.Test_Suites;

-- only use in Ada_Lib tests
package Ada_Lib.GNOGA.Unit_Test.Events is

   function Suite return Standard.AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;

end Ada_Lib.GNOGA.Unit_Test.Events;
