with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.Unit_Test is

   Failed                        : Boolean := False;

   ----------------------------------------------------------------------------
   function Did_Fail return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug, " Failed " & Failed'img);
      return Failed;
   end Did_Fail;

   ----------------------------------------------------------------------------
   procedure Set_Failed (
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug,  " called from " & From);
      Failed := True;
   end Set_Failed;

   ----------------------------------------------------------------------------
   function Test_Name (
      Suite_Name                 : in     String;
      Routine_Name               : in     String
   ) return String is
   ----------------------------------------------------------------------------

      Result                     : constant String := Suite_Name & ":" & Routine_Name;

   begin
      Log_Here (Debug, " Suite '" & Suite_Name & "' Routine '" & Routine_Name &
         "' filter '" & Result & "'");
      return Result;
   end Test_Name;

begin
-- Debug := True;
   Log_Here (Debug or Elaborate);
end Ada_Lib.Unit_Test;
