with Ada.Characters.Handling;
with Ada.Text_IO;use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with Ada_Lib.Interfaces;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Options_Interface;
with Ada_Lib.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Gnoga.GUI.Window;
with Gnoga.Types;

package body Ada_Lib.GNOGA.Unit_Test.Events is

   use type Standard.Gnoga.Gui.Base.Keyboard_Event_Record;
   use type Standard.Gnoga.Gui.Base.Mouse_Event_Record;
   use type Standard.Gnoga.GUI.Window.Pointer_To_Window_Class;
   use type Standard.Gnoga.Types.Pointer_to_Connection_Data_Class;

   procedure Mouse_Move_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event             :  in     Standard.Gnoga.Gui.Base.Mouse_Event_Record);

-- package Mouse_Move is
--    generic
--
--       Kind                    : String;
--
--    procedure Move_Handler (
--       Object                     : in out Gnoga.Gui.Base.Base_Type'Class;
--       Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record);
--
-- end Mouse_Move;

   Auto_Mouse_Event_1              : constant Standard.Gnoga.Gui.Base.Mouse_Event_Record := (
                                    Message        => Standard.Gnoga.Gui.Base.Click,
                                    X              => 0,
                                    Y              => 0,
                                    Screen_X       => -1,
                                    Screen_Y       => -2,
                                    Left_Button    => True,
                                    Middle_Button  => False,
                                    Right_Button   => False,
                                    Alt            => True,
                                    Control        => False,
                                    Shift          => True,
                                    Meta           => False
                                 );
   Auto_Mouse_Event_2              : constant Standard.Gnoga.Gui.Base.Mouse_Event_Record := (
                                    Message        => Standard.Gnoga.Gui.Base.Click,
                                    X              => 100,
                                    Y              => 200,
                                    Screen_X       => -1,
                                    Screen_Y       => -2,
                                    Left_Button    => True,
                                    Middle_Button  => False,
                                    Right_Button   => False,
                                    Alt            => True,
                                    Control        => False,
                                    Shift          => True,
                                    Meta           => False
                                 );

-- package body Mouse_Move is
--
--    ---------------------------------------------------------------
--    procedure Move_Handler (
--       Object                  : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
--       Mouse_Event             : in     Standard.Gnoga.Gui.Base.Mouse_Event_Record) is
--    ---------------------------------------------------------------
--
--    begin
--       Log_In (Debug, "move type " & Kind & " object type " & Tag_Name (Object'tag) &
--          " connection data " & (if Object.Connection_Data = Null then
--             "null"
--          else
--             Tag_Name (Object.Connection_Data'tag)));
--
--       declare
--          Data                    : constant Event_Connection_Data_Access :=
--                                     Event_Connection_Data_Access (
--                                        Object.Connection_Data);
--       begin
--          Data.Mouse_Move_Count := Data.Mouse_Move_Count + 1;
--          Put_Line ("mouse moved");
--          Ada_Lib.Interfaces.Dump_Mouse_Event (Mouse_Event);
--       end;
--       Log_Out (Debug);
--    end Move_Handler;
--
-- end Mouse_Move;
--
-- procedure Mouse_Move_Handler is new Mouse_Move.Move_Handler ("move");

   ---------------------------------------------------------------
   procedure Character_Event_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Key                        : in     Character) is
   ---------------------------------------------------------------

      Data                       : constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (
                                       Object.Connection_Data);
   begin
      Log_In (Debug, "key " & Key'img);
      Put_Line ("key " & Key);
      Data.Got_Key := True;
      Data.Key := Key;
      Log_Out (Debug);
   end Character_Event_Handler;

   ---------------------------------------------------------------
   procedure Click_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "object type " & Tag_Name (Object'tag) &
         " connection data " & (if Object.Connection_Data = Null then
            "null"
         else
            Tag_Name (Object.Connection_Data'tag)));

      declare
         Data                    : constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (
                                       Object.Connection_Data);
      begin
         Data.Got_Click := True;
         Put_Line ("mouse click occured");
      end;
      Log_Out (Debug);
   end Click_Handler;

   ---------------------------------------------------------------
   procedure Click_Event_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event                : in     Standard.Gnoga.Gui.Base.Mouse_Event_Record) is
   ---------------------------------------------------------------

      Options                 : Ada_Lib.Options.Unit_Test.
                                 Unit_Test_Options_Type'class renames
                                    Ada_Lib.Options.Unit_Test.
                                       Unit_Test_Options_Constant.all;
   begin
      Log_In (Debug, "object type " & Tag_Name (Object'tag) &
         " connection data " & (if Object.Connection_Data = Null then
            "null"
         else
            Tag_Name (Object.Connection_Data'tag)));

      declare
         Data                    : constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (
                                       Object.Connection_Data);
      begin
         if Options.Manual then
            Data.Got_Click := Mouse_Event.Left_Button and then
                              not Mouse_Event.Right_Button and then
                              not Mouse_Event.Middle_Button;
         else
            Data.Got_Click := Mouse_Event = Auto_Mouse_Event_1;
         end if;

         Log_Out (Debug, "got click " & Data.Got_Click'img);
      end;
      Put_Line ("mouse event occured");
      Ada_Lib.Interfaces.Dump_Mouse_Event (Mouse_Event);
   end Click_Event_Handler;

   ---------------------------------------------------------------
   procedure Keyboard_Event_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event             : in     Standard.Gnoga.Gui.Base.Keyboard_Event_Record) is
   ---------------------------------------------------------------

      Data                       : constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (
                                       Object.Connection_Data);
   begin
      Log_In (Debug);
      case Keyboard_Event.Message is

         when Standard.Gnoga.Gui.Base.Unknown =>
            Log_Here (Debug, "unknown key type");

         when Standard.Gnoga.Gui.Base.Key_Down =>
            Data.Down_Key := Keyboard_Event;

         when Standard.Gnoga.Gui.Base.Key_Press =>
            Data.Press_Key := Keyboard_Event;

         when Standard.Gnoga.Gui.Base.Key_Up =>
            Data.Up_Key := Keyboard_Event;

      end case;

      if Debug or else Ada_Lib.Options.Program_Options_Constant_Class_Access (
            Ada_Lib.Options_Interface.Read_Only_Options).Verbose then
         Ada_Lib.Interfaces.Dump_Keyboard_Event (Keyboard_Event);
      end if;
      Log_Out (Debug);
   end Keyboard_Event_Handler;

   ---------------------------------------------------------------
   procedure Keyboard_Test (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => Ada_Lib.GNOGA.Has_Connection_Data is
   ---------------------------------------------------------------

      Options                 : Ada_Lib.Options.Unit_Test.
                                 Unit_Test_Options_Type'class renames
                                    Ada_Lib.Options.Unit_Test.
                                       Unit_Test_Options_Constant.all;
      Local_Test                 : Event_Test_Type renames Event_Test_Type (Test);
      Data                       : constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (Local_Test.Connection_Data);
      Key                        : constant Character := 'A';
      Down_Key_Event             : constant Standard.Gnoga.Gui.Base.Keyboard_Event_Record := (
                                    Message     => Standard.Gnoga.Gui.Base.Key_Down,
                                    Key_Code    => 100,
                                    Key_Char    => Ada.Characters.Handling.To_Wide_Character ('B'),
                                    Alt         => False,
                                    Control     => False,
                                    Shift       => False,
                                    Meta        => False
                                 );
      Press_Key_Event            : constant Standard.Gnoga.Gui.Base.Keyboard_Event_Record := (
                                    Message     => Standard.Gnoga.Gui.Base.Key_Press,
                                    Key_Code    => 100,
                                    Key_Char    => Ada.Characters.Handling.To_Wide_Character ('C'),
                                    Alt         => False,
                                    Control     => False,
                                    Shift       => False,
                                    Meta        => False
                                 );
      Up_Key_Event               : constant Standard.Gnoga.Gui.Base.Keyboard_Event_Record := (
                                    Message     => Standard.Gnoga.Gui.Base.Key_Up,
                                    Key_Code    => 100,
                                    Key_Char    => Ada.Characters.Handling.To_Wide_Character ('D'),
                                    Alt         => False,
                                    Control     => False,
                                    Shift       => False,
                                    Meta        => False
                                 );

   begin
      Log_In (Debug, (if Data = Null then "null data" else "data address " & Image (Data'address) &
         (if Data.Main_Window = Null then " null main window" else "have main window")));
      Data.Main_Window.On_Character_Handler (Character_Event_Handler'access);
      Data.Main_Window.On_Key_Down_Handler (Keyboard_Event_Handler'access);
      Data.Main_Window.On_Key_Up_Handler (Keyboard_Event_Handler'access);
      Data.Main_Window.On_Key_Press_Handler (Keyboard_Event_Handler'access);

      if Options.Manual then
         Pause ("Press enter on keyboard and then click a mouse while button");
         while not Data.Got_Click loop
            delay 0.1;
         end loop;
      else
         Local_Test.Connection_Data.Main_Window.Fire_On_Character (Key);
         Local_Test.Connection_Data.Main_Window.Fire_On_Key_Down (Down_Key_Event);
         Local_Test.Connection_Data.Main_Window.Fire_On_Key_Press (Press_Key_Event);
         Local_Test.Connection_Data.Main_Window.Fire_On_Key_Up (Up_Key_Event);
      end if;

      Assert (Data.Got_Key, "did not get Key");
      if not Options.Manual then
         Assert (Data.Down_Key = Down_Key_Event, "did not get expected Down_Key " &
            Ada.Characters.Handling.To_Character (Down_Key_Event.Key_Char) & " got '" &
            Ada.Characters.Handling.To_Character (Data.Down_Key.Key_Char) & "'");
         Assert (Data.Key = Key, "did not get expected Key " & Key &
            " got '" & Data.Key & "'");
         Assert (Data.Press_Key = Press_Key_Event, "did not get expected Press_Key " &
            Ada.Characters.Handling.To_Character (Press_Key_Event.Key_Char) & " got '" &
            Ada.Characters.Handling.To_Character (Data.Press_Key.Key_Char) & "'");
         Assert (Data.Up_Key = Up_Key_Event, "did not get expected Up_Key " &
            Ada.Characters.Handling.To_Character (Up_Key_Event.Key_Char) & " got '" &
            Ada.Characters.Handling.To_Character (Data.Up_Key.Key_Char) & "'");
      end if;

      Log_Out (Debug);

   exception
      when Fault: others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);

   end Keyboard_Test;

   ---------------------------------------------------------------
   procedure Mouse_Click(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Options                 : Ada_Lib.Options.Unit_Test.
                                 Unit_Test_Options_Type'class renames
                                    Ada_Lib.Options.Unit_Test.
                                       Unit_Test_Options_Constant.all;
      Local_Test                 : Event_Test_Type renames Event_Test_Type (Test);
      Data                      : constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (Local_Test.Connection_Data);
   begin
      Log_In (Debug, (if Local_Test.Connection_Data = Null then
            "data null"
         else
            (if Local_Test.Connection_Data.Main_Window = Null then
                  "main window null"
               else
                  "have main window")));
      Local_Test.Connection_Data.Main_Window.On_Click_Handler (Click_Handler'access);
      if Options.Manual then
         Pause ("Press enter on keyboard and then click a mouse while button");
         while not Data.Got_Click loop
            delay 0.1;
         end loop;
      else
         Local_Test.Connection_Data.Main_Window.Fire_On_Click;
      end if;

      Assert (Data.Got_Click, "did not get click");
      Event_Connection_Data_Access (Local_Test.Connection_Data).Got_Click := False; -- clear for next event

      Local_Test.Connection_Data.Main_Window.On_Mouse_Click_Handler (Click_Event_Handler'access);
      if Options.Manual then
         Pause ("Click the left mouse while holding the shift key");
         while not Data.Got_Click loop
            delay 0.1;
         end loop;
      else
         Local_Test.Connection_Data.Main_Window.Fire_On_Mouse_Click (Auto_Mouse_Event_1);
      end if;
      Log_Out (Debug);
   end Mouse_Click;

-- ---------------------------------------------------------------
-- procedure Mouse_Drag (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
-- ---------------------------------------------------------------
--
--    Local_Test                 : Event_Test_Type renames Event_Test_Type (Test);
--    Data                      : Constant Event_Connection_Data_Access :=
--                                  Event_Connection_Data_Access (Local_Test.Connection_Data);
--
-- begin
--    Log_In (Debug);
--    Local_Test.Connection_Data.Main_Window.On_Click_Handler (Click_Handler'access);
--    Local_Test.Connection_Data.Main_Window.On_Mouse_Drag_Handler (Drag_Handler'access);
--    if Local_Test.Connection_Data.Manual then
--       Pause ("Press enter on keyboard and then Drag the mouse over the window " &
--          "and then click the mouse");
--       while not Data.Got_Click loop
--          delay 0.1;
--       end loop;
--    else
--       Local_Test.Connection_Data.Main_Window.Fire_On_Mouse_Drag (Auto_Mouse_Event_1);
--       Local_Test.Connection_Data.Main_Window.Fire_On_Click;
--    end if;
--
--    Assert (Data.Got_Click, "did not get click");
--    Assert (Data.Mouse_Drag_Count > 0, "zero mouse Drag count");
--    Put_Line ("mouse Drag count" & Data.Mouse_Drag_Count'img);
-- end Mouse_Drag;

   ---------------------------------------------------------------
   procedure Test_Mouse_Move (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Options                 : Ada_Lib.Options.Unit_Test.
                                 Unit_Test_Options_Type'class renames
                                    Ada_Lib.Options.Unit_Test.
                                       Unit_Test_Options_Constant.all;
      Local_Test                 : Event_Test_Type renames Event_Test_Type (Test);
      Connection_Data            : Event_Connection_Data_Type renames
                                    Event_Connection_Data_Access (
                                       Local_Test.Connection_Data).all;
      Data                       : Constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (Local_Test.
                                       Connection_Data);

   begin
      Log_In (Debug, "manual " & Options.Manual'img);
      Local_Test.Connection_Data.Main_Window.On_Click_Handler (Click_Handler'access);
      Local_Test.Connection_Data.Main_Window.On_Mouse_Move_Handler (Mouse_Move_Handler'access);
      if Options.Manual then
         Pause ("Press enter on keyboard and then move the mouse over the window " &
            "and then click the mouse");
         while not Data.Got_Click loop
            delay 0.1;
         end loop;
      else
         Local_Test.Connection_Data.Main_Window.Fire_On_Mouse_Move (Auto_Mouse_Event_1);
         Local_Test.Connection_Data.Main_Window.Fire_On_Mouse_Move (Auto_Mouse_Event_2);
         Local_Test.Connection_Data.Main_Window.Fire_On_Click;
         Assert (Connection_Data.Delta_X = Auto_Mouse_Event_2.X and then
            Connection_Data.Delta_Y = Auto_Mouse_Event_2.Y,
            "wrong deleta x or y expected " & Auto_Mouse_Event_2.X'img & "," &
               Auto_Mouse_Event_2.Y'img &
            " got " & Connection_Data.Delta_X'img & "," &
               Connection_Data.Delta_Y'img);
      end if;

      Assert (Data.Got_Click, "did not get click");
      Assert (Data.Mouse_Move_Count > 0, "zero mouse move count");
      Put_Line ("mouse move count" & Data.Mouse_Move_Count'img);
   end Test_Mouse_Move;

   ---------------------------------------------------------------
   procedure Mouse_Move_Handler (
      Object                     : in out Standard.Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event             :  in     Standard.Gnoga.Gui.Base.Mouse_Event_Record) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, Ada_Lib.Interfaces.Mouse_Event_Image (Mouse_Event));

      declare
         Data                    : constant Event_Connection_Data_Access :=
                                    Event_Connection_Data_Access (
                                       Object.Connection_Data);
         Delta_X                 : constant Integer := Mouse_Event.X - Data.Last_X;
         Delta_Y                 : constant Integer := Mouse_Event.Y - Data.Last_Y;

      begin
         Data.Mouse_Move_Count := Data.Mouse_Move_Count + 1;
         Ada_Lib.Interfaces.Dump_Mouse_Event (Mouse_Event);
         Put_Line ("mouse moved delta X" & Delta_X'img & " Y" & Delta_Y'img);
         Data.Last_X := Mouse_Event.X;
         Data.Last_Y := Mouse_Event.Y;
         Data.Delta_X := Data.Delta_X + Delta_X;
         Data.Delta_Y := Data.Delta_Y + Delta_Y;
      end;
      Log_Out (Debug);
   end Mouse_Move_Handler;

   ---------------------------------------------------------------
   overriding
   function Name (Test : Event_Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (Test : in out Event_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Keyboard_Test'access,
         Routine_Name   => AUnit.Format ("Keyboard_Test")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Mouse_Click'access,
         Routine_Name   => AUnit.Format ("Mouse_Click")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Mouse_Move'access,
         Routine_Name   => AUnit.Format ("Test_Mouse_Move")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Event_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Test.Connection_Data := new Event_Connection_Data_Type;
      Ada_Lib.GNOGA.Set_Connection_Data (Test.Connection_Data);
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Set_Up;

-- main window not opened during set_up
--    if Test.Connection_Data.Main_Window = Null then
--       raise Failed with "Null main window raised at " & Here;
--    end if;
      Log_Out (Debug);
   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Event_Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (Test : in out Event_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log (Debug, Here, Who);
      Ada_Lib.GNOGA.Clear_Connection_Data;
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Tear_Down;
   end Tear_Down;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Trace_Options or Elaborate);
end Ada_Lib.GNOGA.Unit_Test.Events;
