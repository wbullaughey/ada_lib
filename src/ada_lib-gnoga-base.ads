with Ada_Lib.Socket_IO;
with Gnoga.Application.Multi_Connect;

package Ada_Lib.GNOGA.Base is

   Failed                        : exception;

   protected Message_Loop_Signal is

      procedure Completed;
      entry Wait;

   private
      Done                       : Boolean := False;

   end Message_Loop_Signal;

   procedure Initialize_GNOGA (   -- makes sure only done once
      Handler                    : in     Standard.Gnoga.Application.Multi_Connect.Application_Connect_Event;
      Application_Title          : in     String;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type;
      Wait_For_Completion        : in     Boolean;
      Handler_Path               : in     String := "default";
      Verbose                    : in     Boolean := False);

   procedure Set_Main_Created (
      Value                      : in     Boolean);

end Ada_Lib.GNOGA.Base;

