with AUnit.Test_Cases;
with AUnit.Test_Suites;
--with Ada_Lib.GNOGA;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View;

package Ada_Lib.GNOGA.Unit_Test.Window_Events is

   Fault                         : exception;

   type Window_Connection_Data_Type
                                 is new Ada_Lib.GNOGA.Connection_Data_Type with record
      Button                     : Standard.gnoga.Gui.Element.Common.Button_Type;
      Form                       : Standard.gnoga.Gui.Element.Form.Form_Type;
      Delta_X                    : Integer := 0;
      Delta_Y                    : Integer := 0;
      Drag_Source_View           : Standard.gnoga.Gui.Element.Common.Div_Type;
      Drag_Target_View           : Standard.gnoga.Gui.Element.Common.Div_Type;
      Key_Pressed                : Boolean := False;
      Last_X                     : Integer;
      Last_Y                     : Integer;
      Mouse_Location             : Standard.gnoga.Gui.Element.Form.Text_Type;
      Mouse_X                    : Integer;
      Mouse_Y                    : Integer;
      Left_Position              : Integer;
--    Left_View                  : Standard.gnoga.Gui.Element.Common.Div_Type;
      Move_Count                 : Natural := 0;
      Moving                     : Boolean := False;
      Move_Started               : Boolean := False;
      Moved                      : Boolean := False;
      Move_Stopped               : Boolean := False;
--    Selection                  : Standard.gnoga.Gui.Element.Form.Selection_Type;
      Start_Left                    : Integer;
      Start_Top                    : Integer;
      Stop                       : Boolean := False;
      Text                       : Standard.gnoga.Gui.Element.Form.Text_Area_Type;
      Top_Position               : Integer;
      Top_View                   : Standard.gnoga.Gui.View.View_Type;
   end record;

   type Window_Connection_Data_Access
                                 is access all Window_Connection_Data_Type;

   type Window_Event_Test_Type   is new Ada_Lib.GNOGA.Unit_Test.
                                    GNOGA_Tests_Type (True, False) with null record;

   type Window_Evemt_Test_Access is access Window_Event_Test_Type;

   procedure Mouse_Move (
      Test                       : in out Standard.AUnit.Test_Cases.Test_Case'class);

   overriding
   function Name (Test : Window_Event_Test_Type) return Standard.AUnit.Message_String;

   overriding
   procedure Register_Tests (Test : in out Window_Event_Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Window_Event_Test_Type
   ) with Pre => Test.Verify_Pre_Setup,
          Post => Test.Verify_Post_Setup;

   function Suite return Standard.AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (Test : in out Window_Event_Test_Type);

   Suite_Name                    : constant String := "GNOGA_Window_Events";

end Ada_Lib.GNOGA.Unit_Test.Window_Events;
