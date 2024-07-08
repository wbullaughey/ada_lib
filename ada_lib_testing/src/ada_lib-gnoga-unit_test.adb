--with Ada.Unchecked_Deallocation;
--with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Command_Line_Iterator; -- needed for pragma Elaborate_All
with Ada_Lib.GNOGA.Base;
--with Ada_Lib.GNOGA.Unit_Test;
--with Ada_Lib.Help;
with Ada_Lib.Options;
with Ada_Lib.Options.GNOGA;
--with Ada_Lib.Options.Unit_Test;
--with Ada_Lib.Runstring_Options;
--with Ada_Lib.Options_Interface;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Gnoga.Application.Multi_Connect;

--pragma Elaborate_All (Ada_Lib.Command_Line_Iterator);

package body Ada_Lib.GNOGA.Unit_Test is

-- procedure Free is new Ada.Unchecked_Deallocation (
--    Object         => Ada_Lib.GNOGA.Connection_Data_Type'class,
--    Name           => Ada_Lib.GNOGA.Connection_Data_Class_Access);

   procedure Test_Handler (
      Main_Window                : in out Standard.Gnoga.Gui.Window.Window_Type'Class;
      Connection                 : access Standard.Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   Main_Window_Name              : constant String := "main window";

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                    : in out GNOGA_Tests_Type) is
   ---------------------------------------------------------------

      Options                 : Ada_Lib.Options.GNOGA.GNOGA_Options_Type'class
                                 renames Ada_Lib.Options.GNOGA.GNOGA_Options.all;
   begin
      Log_In (Debug or Trace_Set_Up, "Initialize_GNOGA " & Test.Initialize_GNOGA'img &
         " test driver " & Test.Test_Driver'img);
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;
      if not Test.Test_Driver then
         Log_Here (Debug or Trace_Set_Up, -- "URL_Opened " & URL_Opened'img &
            " Initialize_GNOGA " & Test.Initialize_GNOGA'img);
         if Test.Initialize_GNOGA then
            Log_Here (Debug);
            Standard.GNOGA.Application.Open_URL;
            Ada_Lib.GNOGA.Base.Initialize_GNOGA (Test_Handler'access,
               Application_Title    => "Unit_Test",
   --          Start_Message_Loop   => True,
               Port                 => Options.HTTP_Port,
               Verbose              => True, -- Ada_Lib.Options.GNOGA.Verbose);
               Wait_For_Completion  => False);
            Log_Here (Debug or Trace_Set_Up);
         end if;
      end if;
      Log_Out (Debug or Trace_Set_Up);
   end Set_Up;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out GNOGA_Tests_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up, "Initialize_GNOGA " & Test.Initialize_GNOGA'img);
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      if Test.Initialize_GNOGA then
         Standard.Gnoga.Application.Multi_Connect.End_Application;
         delay 0.2;  -- let server stop
      end if;

--    if Test.Connection_Data /= Null then      -- causes memory corruption
--       Free (Test.Connection_Data);
--    end if;

      Ada_Lib.GNOGA.Base.Set_Main_Created (False);
--    Ada_Lib.GNOGA.Base.Message_Loop_Signal.Wait;
                                 -- it does not end until end of program
      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Out (Debug);

   end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Handler (
      Main_Window                : in out Standard.Gnoga.Gui.Window.Window_Type'Class;
      Connection                 : access Standard.Gnoga.Application.Multi_Connect.Connection_Holder_Type) is
   pragma Unreferenced (Connection);
   ---------------------------------------------------------------

      Connection_Data            : constant Ada_Lib.GNOGA.Connection_Data_Class_Access :=
                                       Ada_Lib.GNOGA.Get_Connection_Data;
      URL                        : constant String := Main_Window.Document.URL;

   begin
      Log_In (Debug, Quote ("URL", URL));
      Main_Window.Document.Title (Main_Window_Name);
      Connection_Data.Main_Window := Main_Window'unchecked_access;
      Main_Window.Connection_Data (Connection_Data);
      Pause_On_Flag ("exit handler");
      Ada_Lib.GNOGA.Base.Set_Main_Created (True);
      Log_Out (Debug);
   end Test_Handler;

   ---------------------------------------------------------------
   overriding
   function Verify_Set_Up (
      Test                       : in     GNOGA_Tests_Type
   )  return Boolean is
   ---------------------------------------------------------------

      Has_Connection_Data        : constant Boolean :=
                                    Ada_Lib.GNOGA.Has_Connection_Data;
   begin
      return Log_Here (Has_Connection_Data and then
             Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Verify_Set_Up,
         Debug, (
            if not Has_Connection_Data then
               " Test.Connection_Data is Null"
            else
               ""
            ) &
            (if Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Verify_Set_Up then
               ""
            else
               " Verify_Set_Up failed"
            ));
   end Verify_Set_Up;

begin
--Trace_Tests := True;
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
--Debug_Options := True;
--Trace_Options := True;
   Log_Here (Trace_Options or Elaborate);
end Ada_Lib.GNOGA.Unit_Test;
