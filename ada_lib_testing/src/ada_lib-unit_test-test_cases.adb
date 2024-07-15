with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Options.Unit_Test;

package body Ada_Lib.Unit_Test.Test_Cases is

-- use type Ada_Lib.Options_Interface.Interface_Options_Constant_Class_Access;
   use type Ada_Lib.Options.Mode_Type;
   use type Ada_Lib.Options.Program_Options_Constant_Class_Access;


   ----------------------------------------------------------------------------
   overriding
   procedure Add_Routine (
      Test                    : in out Test_Case_Type;
      Val                     : AUnit.Test_Cases.Routine_Spec) is
   ----------------------------------------------------------------------------

      Options                 : Ada_Lib.Options.Unit_Test.
                                 Unit_Test_Options_Type'class renames
                                    Ada_Lib.Options.Unit_Test.
                                       Unit_Test_Options_Constant.all;
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
   overriding
   procedure Set_Up (
      Test                       : in out Test_Case_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Test.Options := Ada_Lib.Options.
         Program_Options_Constant_Class_Access (
            Ada_Lib.Options.Get_Modifiable_Options);
      Root_Test.Test_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up);
   end Set_Up;

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

   ----------------------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Test_Case_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Test.Options := Null;
      Root_Test.Test_Type (Test).Tear_Down;
      Log_Out (Debug or Trace_Set_Up);
   end Tear_Down;

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

   package body Root_Test is

      ----------------------------------------------------------------------------
      function Did_Set_Up_Fail (
         Test                    : in     Test_Type
      ) return Boolean is
      ----------------------------------------------------------------------------

      begin
         return Test.Set_Up_Failed;
      end Did_Set_Up_Fail;

      ----------------------------------------------------------------------------
      overriding
      procedure Set_Up (
         Test                    : in out Test_Type) is
      ----------------------------------------------------------------------------

      begin
         Log_In (Debug or Trace_Set_Up);
         Test.Set_Up := True;
         Log_Out (Debug or Trace_Set_Up);
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

   ---------------------------------------------------------------
   function Verify_Pre_Setup (
      Test                       : in     Test_Case_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      if Test.Options /= Null then
         Log_Here ("Test.Options is not null");
         return False;
      else
         Log_Here (Debug, "Test.Options is null");
         return True;
      end if;

   end Verify_Pre_Setup;

   ---------------------------------------------------------------
   function Verify_Post_Setup (
      Test                       : in     Test_Case_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      return Log_Out (Ada_Lib.Options_Interface.Read_Only_Options /= Null,
         Debug, "Test.Options is null");
   end Verify_Post_Setup;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Trace_Options or Elaborate);

end Ada_Lib.Unit_Test.Test_Cases;
