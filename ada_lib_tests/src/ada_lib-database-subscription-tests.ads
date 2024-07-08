with AUnit.Test_Suites;

package Ada_Lib.Database.Subscription.Tests is

   function Subscription_Suite (
      Which_Host              : in    Ada_Lib.Database.Which_Host_Type
   ) return AUnit.Test_Suites.Access_Test_Suite;

end Ada_Lib.Database.Subscription.Tests;
