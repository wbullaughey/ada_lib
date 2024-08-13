--with AUnit.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Options.Unit_Test;
--with Ada_Lib.Test.Suites;
--with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.Unit_Test.Test_Cases is

   use type Ada_Lib.Options.Mode_Type;

   ----------------------------------------------------------------------------
   overriding
   procedure Add_Routine (
      Test                    : in out Test_Case_Type;
      Val                     : AUnit.Test_Cases.Routine_Spec) is
   ----------------------------------------------------------------------------

      Options     : Ada_Lib.Options.Unit_Test.
                     Ada_Lib_Unit_Test_Options_Type'class renames
                           Ada_Lib.Options.Unit_Test.
                        Ada_Lib_Unit_Test_Options_Type'class (
                           Ada_Lib.Options.Get_Ada_Lib_Read_Only_Options.all);
   begin
      Log_In (Debug, Quote ("routine", Val.Routine_Name.all) &
         " mode " & Options.Mode'img);

      if Options.Mode = Ada_Lib.Options.Run_Tests then
         AUnit.Test_Cases.Test_Case (Test).Add_Routine (Val);
      end if;

      Routine (Test_Case_Type'class (Test).Name.all, Val.Routine_Name.all);
      Log_Out (Debug);
   end Add_Routine;

   ----------------------------------------------------------------------------
   procedure Set_Up_Exception (
      Test                       : in out Test_Case_Type;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who) is
   ----------------------------------------------------------------------------

   begin
      Put_Line ("------ exception trace --------");
      Put ("Exception in Set_Up " & Ada.Exceptions.Exception_Name (Fault) &
         ": " & Ada.Exceptions.Exception_Message (Fault) &
         "' called from " & Who & " at " & Here);
      New_Line;
      Put_Line ("-------------------------------------------------");
      Flush;
      Test.Set_Up_Failed;
   end Set_Up_Exception;

   ----------------------------------------------------------------------------
   procedure Set_Up_Message_Exception (
      Test                       : in out Test_Case_Type;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who) is
   ----------------------------------------------------------------------------

   begin
      Put ("Exception in Set_Up " & Ada.Exceptions.Exception_Name (Fault) &
         ": " & Ada.Exceptions.Exception_Message (Fault) &
         Quote (" message", Message) &
         "' called from " & Who & " at " & Here);
      New_Line;
      Flush;
      Test.Set_Up_Failed;
   end Set_Up_Message_Exception;

   ----------------------------------------------------------------------------
   procedure Set_Up_Failure (
      Test                       : in     Test_Case_Type;
      Condition                  : in     Boolean;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who;
      Message                    : in     String := "") is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------------------

   begin
      if not Condition then
         Put ("Failure in Set_Up called from " & Who & " at " & Here);
         if Message'length > 0 then
            Put (" " & Message);
         end if;
         New_Line;
         raise Failed with "Failure in Set_Up " & Message &
            " called from " & Who & " at " & Here;
--       Ada_Lib.OS.Immediate_Halt (-1);
      end if;
   end Set_Up_Failure;

--   ----------------------------------------------------------------------------
--   procedure Tear_Down (
--      Test                       : in out Test_Case_Type) is
--   ----------------------------------------------------------------------------
--
--   begin
--      Log ( Debug, Here, Who & " enter");
--
----    if Ada_Lib.Database.Unit_Test.Is_DBDaemon_Running then
----       raise Ada_Lib.Unit_Test.Test_Cases.Failed with "dbdaemon not closed at " & Here & " " & Who;
----    end if;
--
--      AUnit.Test_Cases.Test_Case (Test).Tear_Down;
--      Log ( Debug, Here, Who & " exit");
--   end Tear_Down;
--
     ----------------------------------------------------------------------------
     procedure Tear_Down_Exception (
        Test                       : in out Test_Case_Type;
        Fault                      : Ada.Exceptions.Exception_Occurrence;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who;
        Message                    : in     String := "") is
     ----------------------------------------------------------------------------

     begin
        Put ("Exception in Tear_Down " & Ada.Exceptions.Exception_Name (Fault) &
           ": " & Ada.Exceptions.Exception_Message (Fault) &
           " called from " & Who & " at " & Here);
        if Message'length > 0 then
           Put (" " & Message);
        end if;
        New_Line;
--      Ada_Lib.OS.Immediate_Halt (-1);
        Test.Tear_Down_Failed;
     end Tear_Down_Exception;

     ----------------------------------------------------------------------------
     procedure Tear_Down_Failure (
        Test                       : in out Test_Case_Type;
        Condition                  : in     Boolean;
        Here                       : in     String := Ada_Lib.Trace.Here;
        Who                        : in     String := Ada_Lib.Trace.Who;
        Message                    : in     String := "") is
     ----------------------------------------------------------------------------

     begin
        if not Condition then
           Put ("Failure in Tear_Down called from " & Who & " at " & Here);
           if Message'length > 0 then
              Put (" " & Message);
           end if;
           New_Line;
--         Ada_Lib.OS.Immediate_Halt (-1);
           Test.Tear_Down_Failed;
        end if;
     end Tear_Down_Failure;

--   ----------------------------------------------------------------------------
--   function Was_There_An_Async_Failure
--   return Boolean is
--   ----------------------------------------------------------------------------
--
--   begin
--      if Length (Current_Fixture.Async_Failure_Message) > 0 then
--         Put_Line (To_String (Current_Fixture.Async_Failure_Message) & " called from " &
--            To_String (Current_Fixture.Failure_From));
--         return True;
--      else
--         return False;
--      end if;
--   end Was_There_An_Async_Failure;

   package body Root_Test is

      ----------------------------------------------------------------------------
      function Did_Set_Up_Fail (
         Test                    : in     Test_Type
      ) return Boolean is
      ----------------------------------------------------------------------------

      begin
         return Test.Set_Up_Failed;
      end Did_Set_Up_Fail;

--    ----------------------------------------------------------------------------
--    overriding
--    procedure Register_Tests (
--       Test                    : in out Test_Type) is
--    ----------------------------------------------------------------------------
--
--    begin
--       Log_In (Debug);
--       AUnit.Test_Cases.Test_Case'class (Test).Register_Tests;
--       Log_Out (Debug);
--    end Register_Tests;

      ----------------------------------------------------------------------------
      overriding
      procedure Set_Up (
         Test                    : in out Test_Type) is
      ----------------------------------------------------------------------------

      begin
         Log_In (Debug);
         Ada_Lib.Unit_Testing := True;
         Test.Set_Up := True;
         Log_Out (Debug);
      end Set_Up;

      ----------------------------------------------------------------------------
      procedure Set_Up_Failed (
         Test                       : in out Test_Type;
         Here                       : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------------------

      begin
         Test.Set_Up_Failed := True;
         Log_Here;
      end Set_Up_Failed;

      ----------------------------------------------------------------------------
      overriding
      procedure Tear_Down (
         Test                       : in out Test_Type) is
      ----------------------------------------------------------------------------

      begin
         Log_Here (Debug);
         Test.Set_Up := False;
         Test.Torn_Down := True;
      end Tear_Down;

      ----------------------------------------------------------------------------
      procedure Tear_Down_Failed (
         Test                       : in out Test_Type;
         Here                       : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------------------

      begin
         Test.Tear_Down_Failed := True;
         Log_Here;
      end Tear_Down_Failed;

      ----------------------------------------------------------------------------
      function Verify_Set_Up (
         Test                       : in     Test_Type
      )  return Boolean is
      ----------------------------------------------------------------------------

      begin
         Log_Here (Debug, "Set_Up " & Test.Set_Up'img &
            " Set_Up_FAiled " & Test.Set_Up_FAiled'img);

         return Test.Set_Up and then not Test.Set_Up_FAiled;
      end Verify_Set_Up;

      ----------------------------------------------------------------------------
      function Verify_Torn_Down (
         Test                       : in     Test_Type
      ) return Boolean is
      ----------------------------------------------------------------------------

      begin
         Log_Here (Debug, "Torn_Down " & Test.Torn_Down'img &
            " Tear_Down_Failed " & Test.Tear_Down_Failed'img);

         return Test.Torn_Down and then not Test.Tear_Down_Failed;

      end Verify_Torn_Down;

   end Root_Test;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Trace_Options or Elaborate);

end Ada_Lib.Unit_Test.Test_Cases;
