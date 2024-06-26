with Ada_Lib.Time;
with Ada_Lib.Unit_Test.Tests;
--with AUnit.Test_Cases;
with AUnit.Test_Suites;

package Ada_Lib.Timer.Tests is

Suite_Name                    : constant String := "Timer";

   type Test_Type                is new Ada_Lib.Unit_Test.Tests.
                                    Test_Case_Type with null record;

   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

-- overriding
-- procedure Set_Up (Test : in out Test_Type)
-- with post => Test.Verify_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

-- overriding
-- procedure Tear_Down (Test : in out Test_Type)
-- with post => Verify_Set_Up (Test);

   Debug                         : Boolean := False;

private

   type Test_Timer_Type          is new Event_Type with record
      Occurred                   : Boolean := False;
      Occured_At                 : Ada_Lib.Time.Time_Type;
   end record;

   type Test_Timer_Access        is access all Test_Timer_Type;

   function Allocate_Event (
      Offset                     : in     Duration;
      Dynamic                    : in     Boolean;
      Description                : in     String := ""
   ) return Test_Timer_Access;

   type Event_Pointers_Type      is array (Positive range <>) of Test_Timer_Access;

   overriding
   procedure Callback (
      Event             : in out Test_Timer_Type);

end Ada_Lib.Timer.Tests;
