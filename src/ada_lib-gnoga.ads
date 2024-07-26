with Ada_Lib.Options;
--with Ada_Lib.Socket_IO;
with GNAT.Source_Info;
with Gnoga.Gui.Window;
with Gnoga.Types;

package Ada_Lib.GNOGA is

   type Connection_Data_Type is new Standard.Gnoga.Types.Connection_Data_Type with
                                    record
         Main_Window             : Standard.Gnoga.Gui.Window.
                                    Pointer_To_Window_Class := Null;
         Options                 : Ada_Lib.Options.Interface_Options_Class_Access := Null;
      end record;

   type Connection_Data_Access is access all Connection_Data_Type;
   type Connection_Data_Class_Access is access all Connection_Data_Type'class;

   procedure Set_Main_Window (
      Connection_Data         : in out Connection_Data_Type;
      Main_Window             : in     Standard.Gnoga.Gui.Window.
                                          Pointer_To_Window_Class);
   procedure Clear_Connection_Data
   with Pre => Has_Connection_Data;

   function Get_Connection_Data return Connection_Data_Class_Access
   with Pre => Has_Connection_Data;

   function Has_Connection_Data return Boolean;

   procedure Set_Connection_Data (
      Connection_Data            : in     Connection_Data_Class_Access;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) with Pre => Connection_Data /= Null and then
                 not Has_Connection_Data;

   Debug                         : Boolean := False;

private
   Program_Connection_Data       : Connection_Data_Class_Access := Null;

end Ada_Lib.GNOGA;

