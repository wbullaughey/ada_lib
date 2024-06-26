with Gnoga.Gui.Base;
-- with Gnoga.Gui.Element.Common;
-- with Gnoga.Types;

package Ada_Lib.Interfaces is

   Fault                         : exception;

   Down_Arrow                    : constant := 40;
   Left_Arrow                    : constant := 37;
   Right_Arrow                   : constant := 39;
   Up_Arrow                      : constant := 38;

   procedure Dump_Keyboard_Event (
      Keyboard_Event                : in     Gnoga.Gui.Base.Keyboard_Event_Record);

   procedure Dump_Mouse_Event (
      Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record);

   function Image (
      Code                       : in     Natural
   ) return String;

   function Image (
      Key                        : in     Character
   ) return String;

   function Keyboard_Event_Image (
      Keyboard_Event                : in     Gnoga.Gui.Base.Keyboard_Event_Record
   ) return String;

   function Mouse_Event_Image (
      Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record
   ) return String;

end Ada_Lib.Interfaces;
