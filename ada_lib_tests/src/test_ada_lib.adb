--
--  Copyright (C) 2008, AdaCore
--
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Help;
with Ada_Lib.Options.Actual;
with Ada_Lib.Options.AUnit_Lib;
with Ada_Lib.OS;
with Ada_Lib.Test.Run_Suite;
--with Ada_Lib.Timer;
with Ada_lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Gnoga.Application.Multi_Connect;

procedure Test_Ada_Lib is

   Result   : Ada_Lib.OS.OS_Exit_Code_Type := Ada_Lib.OS.No_Error;

begin
   Put_Line ("test_ada_lib");

   declare
      Aunit_Options  : aliased Ada_Lib.Options.AUnit_Lib.
                        Aunit_Program_Options_Type (
         Multi_Test        => True,
         Options_Selection => Ada_Lib.Options.AUnit_Lib.
                                 Ada_Lib_Unit_Test_With_Database);
      Debug          : Boolean renames AUnit_Options.Tester_Debug;

   begin
      Debug := True;
      Ada_Lib.Options.Actual.Set_Ada_Lib_Program_Options (
         Aunit_Options'unchecked_access);
      if Aunit_Options.Initialize then

         if Aunit_Options.Process (
            Include_Options      => True,
            Include_Non_Options  => False,
            Modifiers            => Ada_Lib.Help.Modifiers) then
            Log_Here (Debug, "help test " & Ada_Lib.Help_Test'img);

            Aunit_Options.Post_Process;
            if Ada_Lib.Help_Test then
               Put_Line ("help test " & (if Ada_Lib.Exception_Occured then
                     "failed"
                  else
                     "completed"));
               if Ada_Lib.Exception_Occured then
                  Result := Ada_Lib.OS.Assertion_Exit;
               end if;
            else
               Ada_Lib.Trace_Tasks.Start ("main");
               Ada_Lib.Test.Run_Suite (Aunit_Options);
               Gnoga.Application.Multi_Connect.End_Application;
               Log_Here (Debug);
               if Aunit_Options.Exit_On_Done then
                  Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
               end if;

               Ada_Lib.Trace_Tasks.Stop;
               Ada_Lib.Trace_Tasks.Report;
            end if;
         else  -- Options.Process false
            Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);
         end if;
      else
         Put_Line ("initialiation failed");
      end if;
   end;
   Ada_Lib.OS.Immediate_Halt (Result);

exception
      when Fault: Ada_Lib.Command_Line_Iterator.Not_Option =>
         Trace_Exception (True, Fault, Here);
         Put_Line ("could not process command line options");
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Exception_Exit);

   when Fault: others =>
      Trace_Exception (True, Fault, Here);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Exception_Exit);

end Test_Ada_Lib;
