with Ada_Lib.Database.Subscribe;
with Ada_Lib.Database.Updater;
with Ada_Lib.Unit_Test.Test_Cases;

package Ada_Lib.Database.Subscribe_Tests is

   type Test_Table_Type    is new Ada_Lib.Database.Subscribe.Table_Type
                              with null record;
   type Test_Table_Access  is access Test_Table_Type;
   type Test_Table_Class_Access
                           is access Test_Table_Type'class;
   type Updater_Type       is new Ada_Lib.Database.Updater.
                              Abstract_Updater_Type with null record;
   type Updater_Class_Access
                           is access Updater_Type;
   overriding
   function Allocate (
      Table             : in     Test_Table_Type
   ) return Ada_Lib.Database.Subscribe.Entry_Class_Access;

   type Test_Type       is abstract new Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type
                           with record
      Subscribed        : Boolean := False;
      Table             : Test_Table_Type;
   end record;

end Ada_Lib.Database.Subscribe_Tests;
