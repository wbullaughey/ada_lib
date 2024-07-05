with AUnit.Test_Cases;
with AUnit.Test_Suites;
--with Ada_Lib.GNOGA;
-- with Ada_Lib.Unit_Test.Tests;
--with Ada_Lib.GNOGA.Unit_Test;
with Gnoga.Gui.Base;

package Ada_Lib.GNOGA.Unit_Test.Events is

   type Event_Connection_Data_Type
                                 is new Ada_Lib.GNOGA.Connection_Data_Type with record
      Down_Key                   : Standard.GNOGA.Gui.Base.Keyboard_Event_Record;
      Got_Click                  : Boolean := False;
      Got_Key                    : Boolean := False;
      Key                        : Character := ' ';
      Last_X                     : Integer := 0;
      Last_Y                     : Integer := 0;
      Mouse_Move_Count           : Natural := 0;
      Press_Key                  : Standard.GNOGA.Gui.Base.Keyboard_Event_Record;
      Delta_X                    : Integer := 0;
      Delta_Y                    : Integer := 0;
      Up_Key                     : Standard.GNOGA.Gui.Base.Keyboard_Event_Record;
   end record;

   type Event_Connection_Data_Access
                                 is access all Event_Connection_Data_Type;

   type Event_Test_Type          is new Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (
                                    Initialize_GNOGA  => True,
                                    Test_Driver       => False) with null record;

   type Test_Access is access Event_Test_Type;

   procedure Mouse_Click (
      Test                       : in out Standard.AUnit.Test_Cases.Test_Case'class);

   overriding
   function Name (Test : Event_Test_Type) return Standard.AUnit.Message_String;

   overriding
   procedure Register_Tests (Test : in out Event_Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Event_Test_Type
   ) with Pre => Test.Verify_Pre_Setup,
          Post => Test.Verify_Post_Setup;

   function Suite return Standard.AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (Test : in out Event_Test_Type);

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "GNOGA_Events";

end Ada_Lib.GNOGA.Unit_Test.Events;
