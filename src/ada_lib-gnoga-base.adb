with Ada.Task_Identification;
--with Ada.Unchecked_Deallocation;
--with Ada_Lib.Options.GNOGA;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
-- with Gnoga.Application.Multi_Connect;

package body Ada_Lib.GNOGA.Base is

   task type Message_Loop_Task;

   type Message_Loop_Access      is access Message_Loop_Task;

   use type Ada.Task_Identification.Task_Id;

-- Base_State                    : Base_Class_Access := Null;
   GNOGA_Initialized             : Boolean := False;  -- can only be done once per program
   Main_Created                  : Boolean := False;
   Message_Loop                  : Message_Loop_Access := Null;
   Task_Id                       : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;

--   ---------------------------------------------------------------
--   procedure Clear_Base (
--      Source                     : in     String := GNAT.Source_Info.Source_Location) is -- Ada_Lib.Trace.Here) is
--   pragma Unreferenced (Source);
--   ---------------------------------------------------------------
--
--   begin
----    Log_In (Debug, "called from " & Source);
--      Base_State := Null;
--   end Clear_Base;
--
--   ---------------------------------------------------------------
--   function Get_Base (
--      Source                     : in     String := GNAT.Source_Info.Source_Location -- Here
--   ) return Base_Class_Access is
--   ---------------------------------------------------------------
--
--   begin
--      if Base_State = Null then
--         raise Failed with "Base_State not set called from " & Source;
--      else
--         return Base_State;
--      end if;
--   end Get_Base;

   ------------------------------------------------------------------------------------------------
   procedure Initialize_GNOGA (
      Handler                    : in     Standard.Gnoga.Application.Multi_Connect.Application_Connect_Event;
      Application_Title          : in     String;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type;
      Wait_For_Completion        : in     Boolean;
      Handler_Path               : in     String := "default";
      Verbose                    : in     Boolean := False) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "GNOGA_Initialized " & GNOGA_Initialized'img &
         " Wait_For_Completion " & Wait_For_Completion'img &
         " port" & Port'img &
         " verbose " & Verbose'img);

      if not GNOGA_Initialized then
         GNOGA_Initialized := True;

         Standard.GNOGA.Application.Title (Application_Title);
         Standard.GNOGA.Application.HTML_On_Close
           ("<b>Connection to Application has been terminated</b>");

         Standard.Gnoga.Application.Multi_Connect.Initialize (
--          Event=> Handler,
            Port => Integer (Port),
            Boot => "boot_jqueryui.html",
            Verbose => Verbose);
      end if;


      Standard.Gnoga.Application.Multi_Connect.On_Connect_Handler
        (Event => Handler,
         Path  => Handler_Path);


      if Message_Loop = Null then      -- only one per program
         Log_Here (Debug);
         Message_Loop := new Message_Loop_Task;

         while Task_ID = Ada.Task_Identification.Null_Task_Id loop
--          Log (Debug, Here, Who & " wait for task to initialize");
            delay 0.1;  -- let task initialize
            Task_ID := Ada.Task_Identification.Current_Task;
         end loop;
         Log_Here (Debug);
         delay 0.1;        -- let message loop initialize
      end if;
      Log_Here (Debug, "Main_Created " & Main_Created'img &
         " Wait_For_Completion " & Wait_For_Completion'img);

      while not Main_Created loop     -- wait for On_Connect to complete
         delay 0.1;
      end loop;

      if Wait_For_Completion then
         Message_Loop_Signal.Wait;
      end if;
      Log_Out (Debug);

   exception
      when Fault: others =>
         Log_Exception (Debug);
         Trace_Exception (Debug, Fault);
         raise Failed with "could not Initialize_GNOGA";
   end Initialize_GNOGA;

--   ---------------------------------------------------------------
--   procedure Set_Base (
--      Base                       : in     Base_Class_Access;
--      Source                     : in     String := GNAT.Source_Info.Source_Location
--   ) is
--   ---------------------------------------------------------------
--
--   begin
--      if Base_State = Null then
--         Base_State := Base;
--      else
--         raise Failed with "Base_State already set called from " & Source;
--      end if;
--   end Set_Base;

   ---------------------------------------------------------------
   procedure Set_Main_Created (
      Value                      : in     Boolean) is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img);
      Main_Created := Value;
   end Set_Main_Created;
--
--   ---------------------------------------------------------------
--   procedure Stop_GNOGA is
--   ---------------------------------------------------------------
--
--   begin
----    Log_In (Debug);
--      Standard.Gnoga.Application.Multi_Connect.End_Application;
--      GNOGA_Initialized := False;
--      Free (Message_Loop);
--      delay 0.2;        -- let GNOGA server stop
----    Log_Out (Debug);
--   end Stop_GNOGA;
--
   ---------------------------------------------------------------
   protected body Message_Loop_Signal is
   ---------------------------------------------------------------

      ------------------------------------------------------------
      procedure Completed is
      ------------------------------------------------------------

      begin
         Log_Here (Debug);
         Message_Loop := Null;
         Done := True;
      end Completed;

      ------------------------------------------------------------
      entry Wait when Done is
      ------------------------------------------------------------

      begin
         Log_Here (Debug);
      end Wait;

   end Message_Loop_Signal;

   ---------------------------------------------------------------
   task body Message_Loop_Task is
   begin
      Log_In (Debug);
      Ada_Lib.Trace_Tasks.Start ("message loop task", Here);
      Task_ID := Ada.Task_Identification.Current_Task;
      Standard.Gnoga.Application.Multi_Connect.Message_Loop;
      Log_Here (Debug);
      Task_ID := Ada.Task_Identification.Null_Task_Id;
      Message_Loop_Signal.Completed;
--    Message_Loop := Null;
      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Debug);

   exception
      when Fault: others =>
         Trace_Message_Exception (Fault, Who, Here);
         raise;

   end Message_Loop_Task;

--begin
--log_here;
end Ada_Lib.GNOGA.Base;


