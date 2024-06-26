--
--  Copyright (C) 2008, AdaCore
--
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Options.AUnit_Lib;
with Ada_Lib.OS;
with Ada_Lib.Test.Run_Suite;
--with Ada_Lib.Timer;
with Ada_lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Gnoga.Application.Multi_Connect;

procedure Test_Ada_Lib is

begin
   Put_Line ("test_ada_lib");
   Ada_Lib.Options.AUnit_Lib.Set_Options;
   declare
      Aunit_Options                 : constant Ada_Lib.Options.AUnit_Lib.
                                          Aunit_Options_Class_Access :=
                                             Ada_Lib.Options.AUnit_Lib.
                                                Get_Modifiable_AUnit_Options;
      Debug                         : Boolean renames
                                       AUnit_Options.Tester_Debug;
   begin
--Debug := True;
      if Aunit_Options.Initialize then
         Log_Here (Debug);
         Ada_Lib.Trace_Tasks.Start ("main");
         Ada_Lib.Test.Run_Suite (Aunit_Options.all);
         Gnoga.Application.Multi_Connect.End_Application;
         Log_Here (Debug);
         if Aunit_Options.Unit_Test.Exit_On_Done then
            Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
         end if;

         Ada_Lib.Trace_Tasks.Stop;
         Ada_Lib.Trace_Tasks.Report;
      else
         Put_Line ("initialiation failed");
      end if;
   end;
   Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

exception
      when Fault: Ada_Lib.Command_Line_Iterator.Not_Option =>
         Trace_Exception (True, Fault, Here);
         Put_Line ("could not process command line options");

   when Fault: others =>
      Trace_Exception (True, Fault, Here);

end Test_Ada_Lib;
