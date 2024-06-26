with AUnit.Test_Suites;
--with Ada_Lib.Unit_Test.Tests;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View;

package Ada_Lib.GNOGA.Unit_Test.Base is

-- type Test_Base_Type is new Base_Type with null record;

-- procedure Start (
--    Base                       : in out Test_Base_Type);
--
-- procedure Terminated (
--    Base                       : in out Test_Base_Type);

   type Connection_Type is new Ada_Lib.GNOGA.Connection_Data_Type with  record
--    Base                       : Test_Base_Type;
      Button                     : Standard.Gnoga.Gui.Element.Common.Button_Type;
      Display_Window             : Standard.Gnoga.Gui.View.View_Type;
      Form                       : Standard.Gnoga.Gui.Element.Form.Form_Type;
   end record;

   type Connection_Access        is access all Connection_Type;
   type Connection_Class_Access  is access all Connection_Type'class;

   type Test_Type is new Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (
      Initialize_GNOGA  => False,
      Test_Driver       => False) with null record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (Test : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type
   ) with Pre => Test.Verify_Presetup,
          Post => Test.Verify_Postsetup;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (Test : in out Test_Type);

   Suite_Name                    : constant String := "Main";

end Ada_Lib.GNOGA.Unit_Test.Base;

