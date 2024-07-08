with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Time;
with Ada_Lib.Unit_Test.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Cases;

package body Ada_Lib.Timer.Tests is

   use type Ada_Lib.Strings.String_Access;
   use type Ada_Lib.Time.Time_Type;

   type Test_Type                is new Ada_Lib.Unit_Test.Test_Cases.
                                    Test_Case_Type with null record;
   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

   generic
      with procedure Timed_Out;

   package Timeout_Package is

      type Timeout_Timer_Type    is new Event_Type with null record;

      overriding
      procedure Callback (
         Event             : in out Timeout_Timer_Type);

   end Timeout_Package;

   type Test_Timer_Type          is new Event_Type with record
      Occurred                   : Boolean := False;
      Occured_At                 : Ada_Lib.Time.Time_Type;
   end record;

   type Test_Timer_Access        is access all Test_Timer_Type;

   function Allocate_Event (
      Offset                     : in     Duration;
      Description                : in     String := ""
   ) return Test_Timer_Access;

   type Event_Pointers_Type      is array (Positive range <>) of Test_Timer_Access;

   overriding
   procedure Callback (
      Event             : in out Test_Timer_Type);

   procedure Single_Event (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Wait_For_Event (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Criteria                      : constant Duration := 0.2;
                                    -- limit of actual event from expected
   Suite_Name                    : constant String := "Timer";


   ---------------------------------------------------------------------------
   function Allocate_Event (
      Offset                     : in     Duration;
      Description                : in     String := ""
   ) return Test_Timer_Access is
   ---------------------------------------------------------------------------

      Result                     : constant Test_Timer_Access :=
                                     new Test_Timer_Type;
   begin
      Log_Here (Debug, Quote ("description", Description) &
         " offset " & Offset'img);
      Result.Initialize (
            Description    => Description,
            Dynamic        => True,
            Wait           => Offset);
      return Result;
   end Allocate_Event;

 ---------------------------------------------------------------
   overriding
   procedure Callback (
      Event             : in out Test_Timer_Type) is
 ---------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("description", Event.Description));
      Event.Occurred := True;
      Event.Occured_At := Ada_Lib.Time.Now;
      Log_Out (Debug, "occured at " & From_Start (Event.Occured_At, True));
   end Callback;

   ---------------------------------------------------------------
   procedure Cancel_Event (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Number_Events              : constant := 3;
      Cancel_Event_Index         : constant Natural := 3;   -- event to cancel
      Expect_Active              : constant Array (1 .. Number_Events) of Boolean := (
                                    True,       -- last event should not be canceled,
                                    False,      -- event shoud occure before cancel,
                                    False);     -- event canceled
      Event_Times                : constant Array (1 .. Number_Events) of Duration := (
                                       1.5, 0.5, 1.0);
   begin
      Log_In (Debug);
      for Dynamic_Index in 2 .. 3 loop -- select which event is dynamic
         Log_Here (Debug, "dynamic" & Dynamic_Index'img);
         declare
            Event_1                    : aliased Test_Timer_Type;
            Event_2                    : aliased Test_Timer_Type;
            Event_3                    : aliased Test_Timer_Type;

         begin
            Log_Here (Debug, "Dynamic_Index" & Dynamic_Index'img);
            declare
               Active_Events        : Event_Pointers_Type (1 .. Number_Events);

            begin
            for Index in Active_Events'range loop   -- create events
               declare
                     Event          : Test_Timer_Access renames
                                       Active_Events (Index);
               begin
                  Log_Here (Debug, "index" & Index'img);

                  if Index = Dynamic_Index then
                     Event := Allocate_Event (
                              Description    => "dynamic event",
                              Offset         => Event_Times (Dynamic_Index));
                  else
                     Event := (case Index is

                        when 1 =>
                           Event_1'unchecked_access,
                        when 2 =>
                           Event_2'unchecked_access,
                        when 3 =>
                           Event_3'unchecked_access);

                     Event.Initialize (
                           Description    => "static" & Index'img,
                           Wait           => Event_Times (Index));
                  end if;

                  Log_Here (Debug, "index" & Index'img &
                     " wait " & Event.Wait'img &
                     Quote (" description", Event.Description));

               end;
            end loop;
            declare
               All_Occured       : Boolean := False;
                  Cancel_Event   : Test_Timer_Type renames
                                    Active_Events (Cancel_Event_Index).all;
               Canceled          : Boolean := False;
               Schedule_Time     : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now;
            begin
               delay 0.75; -- after 1st event and before end of cancel event;
               Log_Here (Debug, "cancel Index" & Cancel_Event_Index'img);
               Canceled := Cancel_Event.Cancel; -- cancel middle event
               Log_Here (Debug, " canceled " & Canceled'img);

               Assert (Canceled, "event" & Cancel_Event_Index'img & " not canceled");

               for Index in Active_Events'range loop
                  Log_Here (Debug, "index" & Index'img &
                     " dynamic" & Dynamic_Index'img);
                  if    Index /= Dynamic_Index and then  -- cancled dynamic was freed
                        Index /= Cancel_Event_Index then
                     declare
                        Event          : Test_Timer_Type renames
                                          Active_Events (Index).all;
                        Active         : constant Boolean := Event.Active;
                        Expected       : constant Boolean := Expect_Active (Index);

                     begin
                        Log_Here (Debug, "index" & Index'img &
                           Quote ( " description", Event.Description) &
                           " event active " & Active'img &
                           " expected " & Expected'img);
                        Assert (Active = Expected,
                           Quote ("description", Event.Description) &
                           " index" & Index'img & " expected to be " &
                           (if Expected then
                              "active but was not."
                           else
                              "in active but was active."));
                     end;
                  end if;
               end loop;

               Log_Here (Debug);
               loop     -- wait for all events to occure
                  All_Occured := True;
                  for Index in 1 .. Number_Events loop
                     Log_Here (Debug, "index" & Index'img &
                        " dynamic" & Dynamic_Index'img);
                     declare
                        Event    : Test_Timer_Type renames
                                    Active_Events (Index).all;
                        Fault    : constant Ada_Lib.Strings.String_Access :=
                                    Event.Get_Exception;
                     begin
                        if Fault /= Null then
                           Assert (False, "exception " &
                              Fault.all & " in timer task");

                        end if;
                        if Index /= Cancel_Event_Index and then
                              not Event.Occurred then
                           All_Occured := False;
                           Log_Here (Debug, "not occured yet");
                        end if;
                     end;
                  end loop;

                  if All_Occured then
                     Log_Here (Debug, "all Occurred");
                     exit;
                  end if;

                  delay 0.1;
               end loop;
                                          -- all events completed or canceled
               Log_Here (Debug);
               for Index in 1 .. Number_Events loop
                  if    Index /= Dynamic_Index then
                     declare
                        Event       : Test_Timer_Type renames
                                       Active_Events (Index).all;
                        Expected    : constant Ada_Lib.Time.Time_Type :=
                                       Schedule_Time + Event_Times (Index);
                        Offset      : constant Duration := (if Event.Occurred then
                                          abs (Expected - Event.Occured_At)
                                       else
                                          0.0);
                     begin
                        Log_Here (Debug, "index" & Index'img &
                           " expected " & From_Start (Expected, True) &
                           (if Event.Occurred then
                                 " occured at " & From_Start (Event.Occured_At, True) &
                                 " offset " & Offset'img
                              else
                                 " not occured") &
                           " Schedule time " & From_Start (Schedule_Time, True) &
                           " event time " & Event_Times (Index)'img);

                        Assert (Offset < Criteria,
                           "event" & Index'img & " Occurred at wrong time. Offset " &
                              Offset'img);
                     end;
                  end if;
               end loop;
               Log_Here (Debug);

            exception
               when Fault: AUNIT.ASSERTIONS.ASSERTION_ERROR =>
                  Trace_Message_Exception (Fault, "assert Occurred");
                  return;
            end;

         exception
            when Fault: others =>
               Trace_Message_Exception (Fault, "error in library");
               Assert (False, "library failed");

         end;
         end;
      end loop;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Cancel_Event;

   ---------------------------------------------------------------
   procedure Finalize_Event (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------


   begin
      Log_In (Debug);

      declare
         Static_Event            : aliased Test_Timer_Type;

      begin
         Static_Event.Initialize (
            Description    => "static",
            Wait           => 0.2);
         Log_Here (Debug, "state " & Static_Event.State'img);
      end;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Finalize_Event;

   ---------------------------------------------------------------
   procedure Multiple_Events (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Dynamic_Index              : constant := 2;
      Number_Events              : constant := 3;

      Event_Times                : constant Array (1 .. Number_Events) of
                                    Duration := (1.5, 0.5, 1.0);
      Event_1                    : aliased Test_Timer_Type;
      Event_3                    : aliased Test_Timer_Type;

   begin

      Log_In (Debug);
      Event_1.Initialize (
         Description    => "static 1",
         Wait           => Event_Times (1));
      Event_3.Initialize (
         Description    => "static 3",
         Wait           => Event_Times (3));
      declare
         Events                  : Event_Pointers_Type (1 .. Number_Events);
         Schedule_Time           : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now;

      begin
         for Index in 1 .. Number_Events loop
            declare
               Event             : Test_Timer_Access renames Events (Index);

            begin
               case Index is

                  when 1 =>
                     Event := Event_1'unchecked_access;

                  when Dynamic_Index =>
                     Event := Allocate_Event (
                        Description    => "dynamic",
                        Offset         => Event_Times (Dynamic_Index));

                  when 3 =>
                     Event := Event_3'unchecked_access;

               end case;
            end;
         end loop;

         declare
            All_Occured          : Boolean := False;
--          Start_Time           : constant Ada_Lib.Time.Time_Type := Ada_Lib.Time.Now;

         begin
            Log_Here (Debug);
            loop
               All_Occured := True;
               for Event of Events loop
                  if not Event.Occurred then
                     All_Occured := False;
                  end if;
               end loop;

               if All_Occured then
                  Log_Here (Debug, "all Occurred");
                  exit;
               end if;

               delay 0.1;
            end loop;
            Log_Here (Debug);

            for Index in 1 .. Number_Events loop
log_here (index'img);
if Events (Index) = Null then
   raise failed with "at " & here;
end if;
log_here;
log_here ("time " & Event_Times (Index)'img);
log_here;
log_here (" Schedule time " & From_Start (Schedule_Time, True));
log_here;
               declare
                  Event          : Test_Timer_Type renames Events (Index).all;
                  Expected       : constant Ada_Lib.Time.Time_Type :=
                                    Schedule_Time + Event_Times (Index);
                  Offset         : constant Duration :=
                                    abs (Expected - Event.Occured_At);
               begin
                  Log_Here (Debug, "test"& Index'img &
                     " expected " & From_Start (Expected, True) &
                     " occured at " & From_Start (Event.Occured_At, True) &
                     " Schedule time " & From_Start (Schedule_Time, True) &
                     " event time " & Event_Times (Index)'img &
                     " offset " & Offset'img);

                  Assert (Offset < Criteria,
                     "event" & Index'img & " Occurred at wrong time. Offset " &
                        Offset'img);
log_here;
               end;
            end loop;
            Log_Here (Debug);

         exception
            when Fault: AUNIT.ASSERTIONS.ASSERTION_ERROR =>
               Trace_Message_Exception (Fault, "assert Occurred");
               return;
         end;

      exception
         when Fault: others =>
            Trace_Message_Exception (Fault, "error in library");
            Assert (False, "library failed");

      end;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Multiple_Events;

 ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Finalize_Event'access,
         Routine_Name   => AUnit.Format ("Finalize_Event")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Cancel_Event'access,
         Routine_Name   => AUnit.Format ("Cancel_Event")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Single_Event'access,
         Routine_Name   => AUnit.Format ("Single_Event")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Multiple_Events'access,
         Routine_Name   => AUnit.Format ("Multiple_Events")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Wait_For_Event'access,
         Routine_Name   => AUnit.Format ("Wait_For_Event")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   procedure Single_Event (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Wait_Time                  : constant Duration := 0.5;
      Static_Event               : aliased Test_Timer_Type;

   begin
      Log_In (Debug);
      Static_Event.Initialize (
         Description    => "static",
         Wait           => Wait_Time);

      for Dynamic in Boolean'range loop
         declare
            Event                   : constant Test_Timer_Access := (if Dynamic then
                                          Allocate_EVent (
                                             Description    => "dynamic",
                                             Offset         => Wait_Time)
                                       else
                                          Static_Event'unchecked_access);
         begin
            Log_Here (Debug);
            while not Event.Occurred loop
               delay 0.1;
            end loop;

            declare
               Elapsed_Time      : constant Duration := Ada_Lib.Time.Now -
                                    Event.Occured_At;
            begin
               Log_Here (Debug, "elapse time " & Elapsed_Time'img);
               Assert (Abs (Elapsed_Time) < Criteria, "elapse time " &
                  Elapsed_Time'img & " too long");

            exception
               when Fault: AUNIT.ASSERTIONS.ASSERTION_ERROR =>
                  Trace_Message_Exception (Fault, "assert Occurred");
                  return;
            end;

         exception
            when Fault: others =>
               Trace_Message_Exception (Fault, "error in library");
               Assert (False, "library failed");

         end;
      end loop;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Single_Event;

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

   ---------------------------------------------------------------
   procedure Wait_For_Event (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Wait_Canceled              : Boolean := False;
      Wait_Event                 : Test_Timer_Type;

      procedure Timed_Out;

      package Timeout is new Timeout_Package (Timed_Out);

      Timeout_Event              : Timeout.Timeout_Timer_Type;

      ---------------------------------------------------------------
      procedure Timed_Out is
      ---------------------------------------------------------------

      begin
         Log_Here (Debug);
         Wait_Canceled := True;
         Assert (Wait_Event.Cancel, "could not cancel wait event");
      end Timed_Out;
      ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Timeout_Event.Initialize (
         Description    => "timeout event",
         Wait           => 0.5);
      Wait_Event.Initialize (
         Description    => "wait event",
         Wait           => 0.2);

      Wait_Event.Wait_For_Event;
      Log_Here (Debug);
      Assert (Timeout_Event.Cancel, "could not cancel timeout event");
      Assert (not Wait_Canceled, "wait event got canceled");

      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Wait_For_Event;

   ---------------------------------------------------------------
   package body Timeout_Package is

      ---------------------------------------------------------------
      overriding
      procedure Callback (
         Event             : in out Timeout_Timer_Type) is
      pragma Unreferenced (Event);
      ---------------------------------------------------------------

      begin
         Timed_Out;
      end Callback;

   end Timeout_Package;
   ---------------------------------------------------------------

begin
--Debug := True;
   Log_Here (Elaborate or Trace_Options);
end Ada_Lib.Timer.Tests;