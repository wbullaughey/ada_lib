with Ada.Numerics.Float_Random;
--with Ada.Text_IO; use  Ada.Text_IO;
--with Ada_Lib.Unit_Test;
with Ada_Lib.Time;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Cases;

package body Ada_Lib.Lock.Tests is

   use type Ada_Lib.Time.Time_Type;

   procedure Test_Async_Lock (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Lock (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Relock (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   task type Test_Task_Type is

      entry Start;

   end Test_Task_Type;

   Async_Test_Length             : constant Duration := 1.0;
   Random_Generator              : Ada.Numerics.Float_Random.Generator;

 ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   -------------------------------------------------------------- Test_Task;;
   function Random_Duration return Duration is
   -------------------------------------------------------------- Test_Task;;

   begin
      return Duration (Ada.Numerics.Float_Random.Random (Random_Generator));
   end Random_Duration;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Lock'access,
         Routine_Name   => AUnit.Format ("Test_Lock")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Relock'access,
         Routine_Name   => AUnit.Format ("Test_Relock")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Async_Lock'access,
         Routine_Name   => AUnit.Format ("Test_Async_Lock")));

   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Ada_Lib.Unit_Test.Tests.Test_Case_Type (Test).Set_Up;
      Ada.Numerics.Float_Random.Reset (Random_Generator);
      Log_Out (Debug or Trace_Set_Up);
   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (AUnit.Simple_Test_Cases.Test_Case_Access (Tests));
      return Test_Suite;
   end Suite;

   -------------------------------------------------------------- Test_Task;;
   procedure Test_Async_Lock (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Description                : aliased constant String := "test lock";
      Lock                       : Lock_Type (Description'unchecked_access);
      Stop_Time                  : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + Async_Test_Length;
      Test_Task                  : Test_Task_Type;

   begin
      Log_In (Debug);
      Test_Task.Start;
      while Ada_Lib.Time.Now < Stop_Time loop
         Lock.Lock;
         Log_Here (Debug);
         delay Random_Duration;
         Lock.Unlock;
         delay Random_Duration;
      end loop;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Test_Async_Lock;

   -------------------------------------------------------------- Test_Task;;
   procedure Test_Lock (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Description                : aliased constant String := "test lock";
      Lock                       : Lock_Type (Description'unchecked_access);

   begin
      Log_In (Debug);
      Lock.Lock;
      Log_Here (Debug);
      Lock.Unlock;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Test_Lock;

   -------------------------------------------------------------- Test_Task;;
   procedure Test_Relock (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Description                : aliased constant String := "test lock";
      Lock                       : Lock_Type (Description'unchecked_access);

   begin
      Log_In (Debug);
      Lock.Lock;
      Log_Here (Debug);
      Lock.Lock;
      Log_Here (Debug);
      Lock.Unlock;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Test_Relock;

   ---------------------------------------------------------------
   task body Test_Task_Type is
   ---------------------------------------------------------------

      Description                : aliased constant String := "task lock";

   begin
      Log_In (Debug);
      accept Start;

      declare
         Lock                    : Lock_Type (Description'unchecked_access);
         Stop_Time               : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + Async_Test_Length;
      begin
         while Ada_Lib.Time.Now < Stop_Time loop
            Lock.Lock;
            Log_Here (Debug);
            delay Random_Duration;
            Lock.Unlock;
            delay Random_Duration;
         end loop;
      end;

      Log_Out (Debug);
   end Test_Task_Type;

begin
if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Elaborate or Trace_Options);
end Ada_Lib.Lock.Tests;