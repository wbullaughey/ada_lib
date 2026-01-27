with Ada_Lib.Test_States;
with Ada_Lib.Unit_Test.Test_Cases;
with Gnoga.Gui.Window;
with GNOGA_Ada_Lib;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View;

package Ada_Lib.GNOGA.Unit_Test is

   Failed                        : exception;

   type GNOGA_Tests_Interface    is limited interface;

   type Connection_Type is new GNOGA_Ada_Lib.Connection_Data_Type with  record
--    Base                       : Test_Base_Type;
      Button                     : Standard.Gnoga.Gui.Element.Common.Button_Type;
      Display_Window             : Standard.Gnoga.Gui.View.View_Type;
      Form                       : Standard.Gnoga.Gui.Element.Form.Form_Type;
   end record;

   type Connection_Access        is access all Connection_Type;
   type Connection_Class_Access  is access all Connection_Type'class;

   type GNOGA_Tests_Type (
      Initialize_GNOGA
                  : Boolean;
      Test_Driver : Boolean) is abstract limited new
                     Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type with record
      Main_Window : aliased Standard.Gnoga.Gui.Window.Pointer_To_Window_Class := Null;
   end record;

   type GNOGA_Tests_Access       is access GNOGA_Tests_Type;
   type GNOGA_Tests_Class_Access is access GNOGA_Tests_Type'class;

   procedure Set_Up_With_Handler (
      Test           : in out GNOGA_Tests_Type'class;
      Test_Handler   : in     Standard.Gnoga.Application.Multi_Connect.
                                 Application_Connect_Event;
      Wait_For_Initialization
                     : in     Boolean
   ) with Post => Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type'class (
                     Test).Verify_Set_Up;

   overriding
   procedure Tear_Down (
      Test : in out GNOGA_Tests_Type
   ) with post => Test.Verify_Tear_Down;

-- procedure Test_Handler (
--    Main_Window    : in out Standard.Gnoga.Gui.Window.Window_Type'Class;
--    Connection     : access Standard.Gnoga.Application.Multi_Connect.
--                      Connection_Holder_Type);

   Main_Window_Name  : constant String := "main window";
   Window_Lock_Description
                     : aliased constant String := "test states window lock";
   Window_Lock       : Ada_Lib.Test_States.Window_Lock_Type (
                        Window_Lock_Description'access);
                        -- used to save pointer to main window

end Ada_Lib.GNOGA.Unit_Test;
