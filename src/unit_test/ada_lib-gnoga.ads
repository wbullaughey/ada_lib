with Ada_Lib.Lock;
with Gnoga.Gui.Window;

package Ada_Lib.GNOGA is

   -- use for all types that create a GNOGA object
   type GNOGA_Interface    is limited interface;

   package Window_Lock_Package is
      type Window_Lock_Type   is new Ada_Lib.Lock.Lock_Type with private;

      function Get_Window (
         Lock     : in     Window_Lock_Type
      ) return Standard.Gnoga.Gui.Window.Pointer_To_Window_Class;

      procedure Set_Window (
         Lock     : in out Window_Lock_Type;
         Window   : in     Standard.Gnoga.Gui.Window.Pointer_To_Window_Class);

   private

      type Window_Lock_Type            is new Ada_Lib.Lock.Lock_Type with record
         Window   : Standard.Gnoga.Gui.Window.Pointer_To_Window_Class;
      end record;

   end Window_Lock_Package;

   Window_Lock_Description
                     : aliased constant String := "test states window lock";

   procedure Clear_Window
   with Pre    => Has_Window;

   function Has_Window
   return Boolean;

   function Get_Window
   return Standard.Gnoga.Gui.Window.Pointer_To_Window_Class;

   procedure Set_Window (
      Window   : in     Standard.Gnoga.Gui.Window.Pointer_To_Window_Class);

private

   type Window_Lock_Type   is new Ada_Lib.Lock.Lock_Type with record
      Window   : Standard.Gnoga.Gui.Window.Pointer_To_Window_Class := Null;
   end record;

end Ada_Lib.GNOGA;
