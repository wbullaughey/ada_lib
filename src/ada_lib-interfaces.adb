with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Strings;
-- with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Gnoga.Gui.Base;

package body Ada_Lib.Interfaces is

   use type Ada_Lib.Strings.String_Access;

   Graphic_Labels                  : constant array (Natural range 0 .. 255) of
                                    Ada_Lib.Strings.String_Access := (
                                       37       => new String'("Left_Arrow"),
                                       91       => new String'("Left_Command"),
                                       40       => new String'("Down_Arrow"),
                                       18       => new String'("Option"),
                                       39       => new String'("Right_Arrow"),
                                       93       => new String'("Right_Command"),
                                       16       => new String'("Shift"),
                                       38       => new String'("Up_Arrow"),
                                       others   => Null
                                    );
   ---------------------------------------------------------------
   procedure Dump_Keyboard_Event (
      Keyboard_Event                : in     Gnoga.Gui.Base.Keyboard_Event_Record) is
   ---------------------------------------------------------------

      Code                          : constant Natural := Keyboard_Event.Key_Code;
      Key                           : constant Character :=
                                       Ada.Characters.Conversions.To_Character (
                                          Keyboard_Event.Key_Char);
   begin
      Put ("message type " & Keyboard_Event.Message'img &
                " key code " & Code'img);
      if Ada.Characters.Handling.Is_Graphic (Key) then
         if Graphic_Labels (Code) = Null then
            Put (" unknown " & Key'img);
         else
            Put (Graphic_Labels (Code).all);
         end if;
      elsif not Ada.Characters.Handling.Is_Control (Key) then
         Put (Key);
      end if;

--    Put_Line (  "Left_Button " & Keyboard_Event.Left_Button'img &
--                " Middle_Button " & Keyboard_Event.Middle_Button'img &
--                " Right_Button " & Keyboard_Event.Right_Button'img);
      Put_Line (  " Alt " & Keyboard_Event.Alt'img &
                  " Control " & Keyboard_Event.Control'img &
                  " Shift " & Keyboard_Event.Shift'img &
                  " Meta " & Keyboard_Event.Meta'img );

   end Dump_Keyboard_Event;

   ---------------------------------------------------------------
   function Image (
      Code                       : in     Natural
   ) return String is
   ---------------------------------------------------------------

   begin
      return Image (Character'val (Code));
   end Image;

   ---------------------------------------------------------------
   function Image (
      Key                        : in     Character
   ) return String is
   ---------------------------------------------------------------

      Result                     : constant String (1 .. 1) := (
         1 => Key);

   begin
      return
         (if Ada.Characters.Handling.Is_Graphic (Key) then
            (if Graphic_Labels (Character'Pos (Key)) = Null then
               "unknown graphic" & Key'img
            else
               Graphic_Labels (Character'Pos (Key)).all
            )
         else
            (if Ada.Characters.Handling.Is_Control (Key) then
               "control"
            else
               Result
            )
         );
   end Image;

   ---------------------------------------------------------------
   function Keyboard_Event_Image (
      Keyboard_Event                : in     Gnoga.Gui.Base.Keyboard_Event_Record
   ) return String is
   ---------------------------------------------------------------

   begin
      return "message type " & Keyboard_Event.Message'img &
             " key code " & Keyboard_Event.Key_Code'img &
             " Key_Char " & Ada.Characters.Conversions.To_Character (
                  Keyboard_Event.Key_Char);
   end Keyboard_Event_Image;

   ---------------------------------------------------------------
   procedure Dump_Mouse_Event (
      Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record) is
   ---------------------------------------------------------------

   begin
      Put_Line ( "message type " & Mouse_Event.Message'img &
                 " X " & Mouse_Event.X'img &
                 " Y " & Mouse_Event.Y'img &
                 " Screen_X " & Mouse_Event.Screen_X'img &
                 " Screen_Y " & Mouse_Event.Screen_Y'img);

      Put_Line (  "Left_Button " & Mouse_Event.Left_Button'img &
                  " Middle_Button " & Mouse_Event.Middle_Button'img &
                  " Right_Button " & Mouse_Event.Right_Button'img);
      Put_Line (  "Alt " & Mouse_Event.Alt'img &
                  " Control " & Mouse_Event.Control'img &
                  " Shift " & Mouse_Event.Shift'img &
                  " Meta " & Mouse_Event.Meta'img );

   end Dump_Mouse_Event;

   ---------------------------------------------------------------
   function Mouse_Event_Image (
      Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record
   ) return String is
   ---------------------------------------------------------------

   begin
      return "x" & Mouse_Event.X'img & " " & Mouse_Event.Screen_X'img &
            " y" & Mouse_Event.Y'img & " " & Mouse_Event.Screen_Y'img &
            " " & Mouse_Event.Message'img;
   end Mouse_Event_Image;

end Ada_Lib.Interfaces;
