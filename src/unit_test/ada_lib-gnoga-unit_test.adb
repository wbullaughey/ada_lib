with Ada_Lib.Options.Unit_Test;
--with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with GNOGA_Ada_Lib.Base;
--with Gnoga.Gui.Window;
with GNOGA_Options;

--pragma Elaborate_All (Ada_Lib.Command_Line_Iterator);

package body Ada_Lib.GNOGA.Unit_Test is

-- procedure Free is new Ada.Unchecked_Deallocation (
--    Object         => GNOGA_Ada_Lib.Connection_Data_Type'class,
--    Name           => GNOGA_Ada_Lib.Connection_Data_Class_Access);

   Debug                   : Boolean renames Options.Unit_Test.
                              Ada_Lib_GNOGA_Unit_Test.Debug;

   ---------------------------------------------------------------
   procedure Set_Up_With_Handler (
      Test           : in out GNOGA_Tests_Type'class;
      Test_Handler   : in     Standard.Gnoga.Application.Multi_Connect.
                                 Application_Connect_Event;
      Wait_For_Initialization
                     : in     Boolean) is
   ---------------------------------------------------------------

      Options                 : GNOGA_Options.GNOGA_Options_Type'class
                                 renames GNOGA_Options.GNOGA_Options.all;
   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down,
         "Initialize_GNOGA " & Test.Initialize_GNOGA'img &
         " test driver " & Test.Test_Driver'img &
         " Wait_For_Initialization " & Wait_For_Initialization'img);
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;
      if not Test.Test_Driver then
         Log_Here (Debug, -- "URL_Opened " & URL_Opened'img &
            " Initialize_GNOGA " & Test.Initialize_GNOGA'img);
         if Test.Initialize_GNOGA then
            Log_Here (Debug);
            Standard.GNOGA.Application.Open_URL;
Log_Here (Debug);
            GNOGA_Ada_Lib.Base.Initialize_GNOGA (Test_Handler,
               Application_Title    => "Unit_Test",
   --          Start_Message_Loop   => True,
               Port                 => Options.HTTP_Port,
               Verbose              => True, -- GNOGA_Options.Verbose);
               Wait_For_Completion  => False);
            Log_Here (Debug);
         end if;
      end if;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);
   end Set_Up_With_Handler;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out GNOGA_Tests_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down,
         "Initialize_GNOGA " & Test.Initialize_GNOGA'img);
      if Test.Initialize_GNOGA then
         Standard.Gnoga.Application.Multi_Connect.End_Application;
         delay 0.2;  -- let server stop
      end if;

      GNOGA_Ada_Lib.Base.Set_Main_Created (False);
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);

   exception
      when Fault: others =>
         Trace_Exception (Debug or Trace_Set_Up_Tear_Down, Fault);
         Log_Out (Debug or Trace_Set_Up_Tear_Down);

   end Tear_Down;

-- ---------------------------------------------------------------
-- overriding
-- function Verify_Set_Up (
--    Test                       : in     GNOGA_Tests_Type
-- )  return Boolean is
-- ---------------------------------------------------------------
--
-- begin
--    return Log_Here (Test.Connection_Data /= Null and then
--           Ada_Lib.Unit_Test.Tests.Test_Case_Type (Test).Verify_Set_Up,
--       Debug, (
--          if Test.Connection_Data = Null then
--             " Test.Connection_Data is Null"
--          else
--             ""
--          ) &
--          (if Ada_Lib.Unit_Test.Tests.Test_Case_Type (Test).Verify_Set_Up then
--             ""
--          else
--             " Verify_Set_Up failed"
--          ));
-- end Verify_Set_Up;

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
