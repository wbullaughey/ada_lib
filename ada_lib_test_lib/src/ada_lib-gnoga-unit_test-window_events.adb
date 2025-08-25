with Ada.Text_IO;use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with Ada_Lib.Options.Actual;
with Ada_Lib.Options.AUnit_Lib;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Unit_Test;
with GNOGA_Ada_Lib.Interfaces;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Types.Colors;

package body Ada_Lib.GNOGA.Unit_Test.Window_Events is

   procedure Keyboard_Down_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event                : in     Standard.Gnoga.Gui.Base.Keyboard_Event_Record);

   procedure Mouse_Drag_End_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class);

   procedure Mouse_Drag_Enter_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class);

   procedure Mouse_Drag_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class);

   procedure Mouse_Drop_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      X, Y                       : in     Integer;
      Drag_Text                  : in     String);

   procedure Mouse_Drag_Leave_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class);

   procedure Mouse_Drag_Start_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class);

   procedure Mouse_Move_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event                : in     Standard.Gnoga.Gui.Base.Mouse_Event_Record);

   Left_View_Height              : constant := 100;
   Left_View_Width               : constant := 200;
   Top_View_Height              : constant := 400;
   Top_View_Width               : constant := 800;

   ---------------------------------------------------------------
   procedure Button_Click_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

      Connection_Data            : constant Window_Connection_Data_Access :=
                                    Window_Connection_Data_Access (
                                       Object.Connection_Data);
   begin
      Log_In (Debug);
      Connection_Data.Move_Started := not Connection_Data.Move_Started;
      Connection_Data.Button.Border (Color => (
         if Connection_Data.Move_Started then
               Standard.Gnoga.Types.Colors.Red
            else
               Standard.Gnoga.Types.Colors.Black));
      Log_Out (Debug, "move started " & Connection_Data.Move_Started'img);
   end Button_Click_Handler;

   ---------------------------------------------------------------
   procedure Keyboard_Press (
      Test                    : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Options           : Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type'class
                           renames Ada_Lib.Options.AUnit_Lib.
                              Aunit_Options_Constant_Class_Access (
                                 Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options).all;
--    Local_Test              : Window_Event_Test_Type renames
--                               Window_Event_Test_Type (Test);
      Connection_Data         : Window_Connection_Data_Type renames
                                 Window_Connection_Data_Access (
                                    GNOGA_Ada_Lib.Get_Connection_Data).all;
      Press_Event             : constant Standard.Gnoga.Gui.Base.
                                 Keyboard_Event_Record := (
                                    Message     => Standard.Gnoga.
                                                   Gui.Base.Key_Down,
                                    Key_Code    => GNOGA_Ada_Lib.Interfaces.
                                                      Right_Arrow,
                                    Key_Char    => '>',
                                    Alt         => False,
                                    Control     => False,
                                    Shift       => False,
                                    Meta        => False
                                 );
   begin
      Log_In (Debug);
      Pause_On_Flag ("start of test");

      Connection_Data.Form.Create (Parent => Connection_Data.Top_View);
      Connection_Data.Form.Text_Alignment (Value => Standard.Gnoga.Gui.Element.Center);
      Connection_Data.Form.Put_Line (Message => "type 'x' in the text field to stop test");
      Connection_Data.Text.Create (
         Form  => Connection_Data.Form,
         ID    => "Text_ID");
      Connection_Data.Text.On_Key_Down_Handler (
         Handler => Keyboard_Down_Handler'Unrestricted_Access);
      Connection_Data.Left_Position := Connection_Data.Top_View.Position_Left;
      Connection_Data.Top_Position := Connection_Data.Top_View.Position_Top;

      if Options.Manual then
         Put_Line ("enter key directions for 20 seconds");
         Log_Here (Debug, "stop " & Connection_Data.Stop'img);
         while not Connection_Data.Stop loop
            delay 0.2;
         end loop;
         Log_Here (Debug);
      else
         Log_Here (Debug);

         for Count in 1 .. 10 loop
            delay 0.25;
            Connection_Data.Text.Fire_On_Key_Down (Press_Event);
         end loop;

         Pause_On_Flag ("press fired");

      end if;
      Assert (Connection_Data.Key_Pressed, "Key Down Handler not moved");
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Keyboard_Press;

   ---------------------------------------------------------------
   procedure Keyboard_Down_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event                : in     Standard.Gnoga.Gui.Base.Keyboard_Event_Record) is
   ---------------------------------------------------------------

      Options                    : Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type'class renames
                                       Ada_Lib.Options.AUnit_Lib.
                                          Aunit_Options_Constant_Class_Access (
                                             Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options).all;
      Connection_Data            : constant Window_Connection_Data_Access :=
                                    Window_Connection_Data_Access (
                                       Object.Connection_Data);
      Stop_Code                  : constant := 88;

   begin
      Log_In (Debug, GNOGA_Ada_Lib.Interfaces.Keyboard_Event_Image (Keyboard_Event));

      if Options.Verbose then
         Put_Line (GNOGA_Ada_Lib.Interfaces.Keyboard_Event_Image (Keyboard_Event));
      end if;

      if Debug or else Options.Verbose then
         GNOGA_Ada_Lib.Interfaces.Dump_Keyboard_Event (Keyboard_Event);
      end if;

      case Keyboard_Event.Message is

         when Standard.Gnoga.Gui.Base.Key_Down =>
            Connection_Data.Key_Pressed := True;

            case Keyboard_Event.Key_Code is

               when GNOGA_Ada_Lib.Interfaces.Down_Arrow =>
                  Connection_Data.Top_Position := Connection_Data.Top_Position + 1;
                  Connection_Data.Top_View.Top (Connection_Data.Top_Position, "px");

               when GNOGA_Ada_Lib.Interfaces.Left_Arrow =>
                  Connection_Data.Left_Position := Connection_Data.Left_Position - 1;
                  Connection_Data.Top_View.Left (Connection_Data.Left_Position, "px");

               when GNOGA_Ada_Lib.Interfaces.Right_Arrow =>
                  Connection_Data.Left_Position := Connection_Data.Left_Position + 1;
                  Connection_Data.Top_View.Left (Connection_Data.Left_Position, "px");

               when GNOGA_Ada_Lib.Interfaces.Up_Arrow =>
                  Connection_Data.Top_Position := Connection_Data.Top_Position - 1;
                  Connection_Data.Top_View.Top (Connection_Data.Top_Position, "px");

               when others =>
                  Log_Here (Debug, "code" & Keyboard_Event.Key_Code'img);

                  if Keyboard_Event.Key_Code = Stop_Code then
                     Connection_Data.Stop := True;
                  end if;

            end case;

         when others =>
            Log_Here (Debug, "code" & Keyboard_Event.Key_Code'img);

      end case;

      Log_Out (Debug);
   end Keyboard_Down_Handler;

   ---------------------------------------------------------------
   procedure Mouse_Drag (
      Test                    : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Options           : Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type'class
                           renames Ada_Lib.Options.AUnit_Lib.
                              Aunit_Options_Constant_Class_Access (
                                 Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options).all;
--    Local_Test        : Window_Event_Test_Type renames Window_Event_Test_Type (Test);
      Connection_Data   : Window_Connection_Data_Type renames
                           Window_Connection_Data_Access (
                              GNOGA_Ada_Lib.Get_Connection_Data).all;
   begin
      Log_In (Debug);
      Pause_On_Flag ("start of test");

      Connection_Data.Drag_Source_View.Create (Connection_Data.Top_View, ID => "Source_ID");
         Connection_Data.Drag_Source_View.Border (
            Width       => "2px",
            Style       => Standard.Gnoga.Gui.Element.Double,
            Color       => Standard.Gnoga.Types.Colors.Blue);
      Connection_Data.Drag_Source_View.Position (Standard.Gnoga.Gui.Element.Fixed);
      Connection_Data.Drag_Source_View.Top (20);
      Connection_Data.Drag_Source_View.Left (20);
      Connection_Data.Drag_Source_View.Height (50);
      Connection_Data.Drag_Source_View.Width (50);
      Connection_Data.Drag_Source_View.Draggable;
      Connection_Data.Drag_Target_View.Create (Connection_Data.Top_View, ID => "Target_ID");
      Connection_Data.Drag_Target_View.Border (
         Width       => "10px",
         Style       => Standard.Gnoga.Gui.Element.Dashed,
         Color       => Standard.Gnoga.Types.Colors.Red);
      Connection_Data.Drag_Target_View.Position (Standard.Gnoga.Gui.Element.Fixed);
      Connection_Data.Drag_Target_View.Top (50);
      Connection_Data.Drag_Target_View.Height (Left_View_Height);
      Connection_Data.Drag_Target_View.Width (Left_View_Width);
      Connection_Data.Drag_Target_View.Left (100);
      Connection_Data.Left_Position := Connection_Data.Drag_Target_View.Position_Left;
      Connection_Data.Top_Position := Connection_Data.Drag_Target_View.Position_Top;
      Log_Here (Debug, "Initial left: " & Connection_Data.Left_Position'img &
         " top: " & Connection_Data.Top_Position'img);
      Connection_Data.Drag_Target_View.On_Drag_End_Handler (Mouse_Drag_End_Handler'access);
      Connection_Data.Drag_Source_View.On_Drag_End_Handler (Mouse_Drag_End_Handler'access);
      Connection_Data.Drag_Target_View.On_Drag_Enter_Handler (Mouse_Drag_Enter_Handler'access);
      Connection_Data.Drag_Target_View.On_Drag_Handler (Mouse_Drag_Handler'access);
      Connection_Data.Drag_Target_View.On_Drop_Handler (Mouse_Drop_Handler'access);
      Connection_Data.Drag_Target_View.On_Drag_Leave_Handler (Mouse_Drag_Leave_Handler'access);
      Connection_Data.Drag_Source_View.On_Drag_Start_Handler (Mouse_Drag_Start_Handler'access, "test drag");

      if Options.Manual then
         Pause ("drag the object");
      else
         Log_Here (Debug);
         Connection_Data.Drag_Source_View.Fire_On_Drag_Start;

         for Count in 1 .. 10 loop
            Connection_Data.Drag_Source_View.Fire_On_Drag;
            delay 0.2;
         end loop;

         Connection_Data.Drag_Target_View.Fire_On_Drop (50, 75, "dropped");
         Connection_Data.Drag_Source_View.Fire_On_Drag_End;
         Connection_Data.Drag_Target_View.Fire_On_Drag_Enter;
         Connection_Data.Drag_Target_View.Fire_On_Drag_Leave;
         Pause_On_Flag ("Drag fired");

      end if;
--    Assert (Connection_Data.Drag_Started, "Drag Handler not started");
--    Assert (Connection_Data.Dragd, "Drag Handler not Dragd");
--    Assert (Connection_Data.Drag_Stopped, "Drag Handler not stopped");
      Log_Out (Debug);
   end Mouse_Drag;

   ---------------------------------------------------------------
   procedure Mouse_Drag_End_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class) is
   pragma Unreferenced (Object);
   ---------------------------------------------------------------

--    Connection_Data            : constant Window_Connection_Data_Access :=
--                                  Window_Connection_Data_Access (
--                                     Object.Connection_Data);
   begin
      Log_In (Debug);

      Log_Out (Debug);
   end Mouse_Drag_End_Handler;

   ---------------------------------------------------------------
   procedure Mouse_Drag_Enter_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class) is
   pragma Unreferenced (Object);
   ---------------------------------------------------------------

--    Connection_Data            : constant Window_Connection_Data_Access :=
--                                  Window_Connection_Data_Access (
--                                     Object.Connection_Data);
   begin
      Log_In (Debug);

      Log_Out (Debug);
   end Mouse_Drag_Enter_Handler;

   ---------------------------------------------------------------
   procedure Mouse_Drag_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class) is
   pragma Unreferenced (Object);
   ---------------------------------------------------------------

--    Connection_Data            : constant Window_Connection_Data_Access :=
--                                  Window_Connection_Data_Access (
--                                     Object.Connection_Data);
   begin
      Log_In (Debug);

      Log_Out (Debug);
   end Mouse_Drag_Handler;

   ---------------------------------------------------------------
   procedure Mouse_Drop_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      X, Y                       : in     Integer;
      Drag_Text                  : in     String) is
   pragma Unreferenced (Object);
   ---------------------------------------------------------------

--    Connection_Data            : constant Window_Connection_Data_Access :=
--                                  Window_Connection_Data_Access (
--                                     Object.Connection_Data);
   begin
      Log_In (Debug, "x " & X'img & " y " & Y'img & Quote (" text", Drag_Text));

      Log_Out (Debug);
   end Mouse_Drop_Handler;

   ---------------------------------------------------------------
   procedure Mouse_Drag_Leave_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class) is
   pragma Unreferenced (Object);
   ---------------------------------------------------------------

--    Connection_Data            : constant Window_Connection_Data_Access :=
--                                  Window_Connection_Data_Access (
--                                     Object.Connection_Data);
   begin
      Log_In (Debug);

      Log_Out (Debug);
   end Mouse_Drag_Leave_Handler;

   ---------------------------------------------------------------
   procedure Mouse_Drag_Start_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class) is
   pragma Unreferenced (Object);
   ---------------------------------------------------------------

--    Connection_Data            : constant Window_Connection_Data_Access :=
--                                  Window_Connection_Data_Access (
--                                     Object.Connection_Data);
   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Mouse_Drag_Start_Handler;

   ---------------------------------------------------------------
   function Mouse_Location (
      Connection_Data            : in        Window_Connection_Data_Type
   ) return String is
   ---------------------------------------------------------------

      Result                     : constant String :=
                                    "pan " & Connection_Data.Mouse_X'img &
                                    " tilt " & Connection_Data.Mouse_y'img &
                                    " delta x " & Connection_Data.Delta_X'img &
                                    " y " & Connection_Data.Delta_Y'img;

   begin
      Log_Here (Debug, Result);
      return Result;
   end Mouse_Location;

   ---------------------------------------------------------------
   procedure Mouse_Move (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Options           : Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type'class
                           renames Ada_Lib.Options.AUnit_Lib.
                              Aunit_Options_Constant_Class_Access (
                                 Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options).all;
--    Local_Test              : Window_Event_Test_Type renames Window_Event_Test_Type (Test);
      Connection_Data         : Window_Connection_Data_Type renames
                                 Window_Connection_Data_Access (
                                    GNOGA_Ada_Lib.Get_Connection_Data).all;
      Move_Event              : Standard.Gnoga.Gui.Base.Mouse_Event_Record := (
                                 Message       => Standard.Gnoga.Gui.Base.Mouse_Move,
                                 X             => 0,
                                 Y             => 0,
                                 Screen_X      => 30,
                                 Screen_Y      => 40,
                                 Left_Button   => True,
                                 Middle_Button => False,
                                 Right_Button  => False,
                                 Alt           => True,
                                 Control       => False,
                                 Shift         => True,
                                 Meta          => False
                              );
      Steps                   : constant := 100;
      Move_Steps              : constant := Steps - 1;

   begin
      Log_In (Debug);
      Pause_On_Flag ("start of test");
      Connection_Data.Button.Create (Connection_Data.Top_View, "Move",
         ID => "Button_ID");
      Connection_Data.Button.On_Click_Handler (
         Button_Click_Handler'Unrestricted_Access);

      Connection_Data.Form.Create (Connection_Data.Top_View,
         Action => "form",
         ID => "Form_ID",
         Target => "target");
      Connection_Data.Form.Position (Standard.Gnoga.Gui.Element.Relative);
      Connection_Data.Form.Top (0);
      Connection_Data.Form.Left (0);
      Connection_Data.Form.Border (Color => Standard.GNOGA.Types.Colors.Blue);
      Connection_Data.Text.Create (
         Form     => Connection_Data.Form,
         ID       => "Text_ID");
      Connection_Data.Top_View.On_Mouse_Down_Handler (Mouse_Move_Handler'access);
      Connection_Data.Top_View.On_Mouse_Move_Handler (Mouse_Move_Handler'access);

      if Options.Manual then
         Connection_Data.Moving := true;
         Pause ("move the mouse");
      else
         Log_Here (Debug);
         Move_Event.Message  := Standard.Gnoga.Gui.Base.Mouse_Down;
         Connection_Data.Top_View.Fire_On_Mouse_Down (Move_Event);
         Move_Event.Message  := Standard.Gnoga.Gui.Base.Mouse_Move;

         for Count in 1 .. Steps loop
            Move_Event.X := Move_Event.X + 1;
            Move_Event.y := Move_Event.y + 1;
            Connection_Data.Top_View.Fire_On_Mouse_Move (Move_Event);
            delay 0.2;
         end loop;

         Move_Event.Message  := Standard.Gnoga.Gui.Base.Mouse_Down;
         Connection_Data.Top_View.Fire_On_Mouse_Down (Move_Event);

         Assert (Connection_Data.Delta_X = Move_Steps and then
            Connection_Data.Delta_Y = Move_Steps,
            "wrong deleta x or y expected " & Move_Steps'img & "," &
            Move_Steps'img &
            " got " & Connection_Data.Delta_X'img & "," &
               Connection_Data.Delta_Y'img);
         Pause_On_Flag ("move fired");

      end if;
      Assert (Connection_Data.Move_Started, "Move Handler not started");
      Assert (Connection_Data.Moved, "Move Handler not moved");
      Assert (Connection_Data.Move_Stopped, "Move Handler not stopped");
      Log_Out (Debug);
   end Mouse_Move;

   ---------------------------------------------------------------
   procedure Mouse_Move_Handler (
      Object                  : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event             : in     Standard.Gnoga.Gui.Base.Mouse_Event_Record) is
   ---------------------------------------------------------------

      Options                    : Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type'class renames
                                       Ada_Lib.Options.AUnit_Lib.
                                          Aunit_Options_Constant_Class_Access (
                                             Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options).all;
      Connection_Data         : constant Window_Connection_Data_Access :=
                                 Window_Connection_Data_Access (
                                    Object.Connection_Data);
   begin
      Log_In (Debug, "moveing " & Connection_Data.Moving'img & " event " & Mouse_Event.Message'img);

      if Options.Verbose then
         Put_Line (GNOGA_Ada_Lib.Interfaces.Mouse_Event_Image (Mouse_Event));
      end if;

      if Debug then
         GNOGA_Ada_Lib.Interfaces.Dump_Mouse_Event (Mouse_Event);
      end if;

     case Mouse_Event.Message is

        when Standard.Gnoga.Gui.Base.Mouse_Down =>
           Connection_Data.Moving := not Connection_Data.Moving;

           if Connection_Data.Moving then
              Connection_Data.Move_Started := True;
           else
              Connection_Data.Move_Stopped := True;
           end if;

        when Standard.Gnoga.Gui.Base.Mouse_Move =>
            if Connection_Data.Moving then
               Connection_Data.Move_Count := Connection_Data.Move_Count + 1;
               declare
                  X              : constant Integer :=  Mouse_Event.X;
                  Y              : constant Integer :=  Mouse_Event.Y;
                  Delta_X        : constant Integer :=
                                    X - Connection_Data.Mouse_X;
                  Delta_Y        : constant Integer :=
                                    Y - Connection_Data.Mouse_Y;

               begin
                  Log_Here (Debug,
                     " current left " & Connection_Data.Left_Position'img &
                        " top" & Connection_Data.Top_Position'img &
                     " x: " & X'img & " y: " & Y'img &
                     " last x: " & Connection_Data.Mouse_X'img & " y: " & Connection_Data.Mouse_Y'img &
                     " delta x:" & Delta_X'img & " y:" & Delta_Y'img &
                     Mouse_Location (Connection_Data.all));

                  if Connection_Data.Move_Count > 1 then
                     Connection_Data.Delta_X := Connection_Data.Delta_X + Delta_X;
                     Connection_Data.Delta_Y := Connection_Data.Delta_Y + Delta_Y;
                     Connection_Data.Text.Text (Mouse_Location (Connection_Data.all));
                  end if;

                  Connection_Data.Moved := True;
                  Connection_Data.Mouse_X := X;
                  Connection_Data.Mouse_Y := Y;
               end;
            end if;

         when others =>
            raise Fault with "unexpected mouse event " & Mouse_Event.Message'img;

      end case;

      Log_Out (Debug);
   end Mouse_Move_Handler;

   ---------------------------------------------------------------
   overriding
   function Name (Test : Window_Event_Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (Test : in out Window_Event_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Mouse_Move'access,
         Routine_Name   => AUnit.Format ("Mouse_Move")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Mouse_Drag'access,
         Routine_Name   => AUnit.Format ("Mouse_Drag")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Keyboard_Press'access,
         Routine_Name   => AUnit.Format ("Keyboard_Press")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Window_Event_Test_Type) is
   ---------------------------------------------------------------

      Connection_Data            : constant Window_Connection_Data_Access :=
                                    new Window_Connection_Data_Type;
   begin
      Log_In (Debug or Trace_Set_Up);
      GNOGA_Ada_Lib.Set_Connection_Data (
         GNOGA_Ada_Lib.Connection_Data_Class_Access (Connection_Data));
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Set_Up;

      Connection_Data.Top_View.Create (
         Connection_Data.Main_Window.all, "Top_View_ID");
      Connection_Data.Top_View.Border (
         Width       => "3px",
         Style       => Standard.Gnoga.Gui.Element.Solid,
         Color       => Standard.Gnoga.Types.Colors.Black);
      Connection_Data.Top_View.Position (Standard.Gnoga.Gui.Element.Absolute);
      Connection_Data.Top_View.Top (20);
      Connection_Data.Top_View.Height (Top_View_Height);
      Connection_Data.Top_View.Left (20);
      Connection_Data.Top_View.Width (Top_View_Width);
      Log_Out (Debug or Trace_Set_Up);

   exception

      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Window_Evemt_Test_Access :=
                                    new Window_Event_Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (Test : in out Window_Event_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log (Debug or Trace_Set_Up, Here, Who);
--    GNOGA_Ada_Lib.Clear_Connection_Data;
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Tear_Down;
   end Tear_Down;

begin
--Debug := True;
--Elaborate := True;
if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Trace_Options := True;
   Log_Here (Trace_Options or Elaborate);
end Ada_Lib.GNOGA.Unit_Test.Window_Events;
