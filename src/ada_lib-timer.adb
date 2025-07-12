with Ada.Exceptions;
--with Ada.Text_IO;use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--with Ada_Lib.Time;
--with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks; use Ada_Lib.Trace_Tasks;

package body Ada_Lib.Timer is

-- use type Ada.Calendar.Time;
   use type Ada_Lib.Strings.String_Access;
   use type Ada_Lib.Strings.String_Access_All;

   procedure Free_It is new Ada.Unchecked_Deallocation (
      Name     => Event_Class_Access,
      Object   => Event_Type'class);

   procedure Free is new Ada.Unchecked_Deallocation (
      Name     => Ada_Lib.Strings.String_Access_All,
      Object   => String);

   Trace                         : Boolean := False;

   ---------------------------------------------------------------------------
   function Active (
      Event                      : in   Event_Type
   ) return Boolean is
   ---------------------------------------------------------------------------

      Running                    : constant Boolean := not Event.Timer_Task'terminated;

   begin
      Log_Here (Trace, "active " & Running'img);
      return Running;
   end Active;

   ---------------------------------------------------------------------------
   function Cancel (
      Event                      : in out Event_Type;
      From                       : in     String := Here
   ) return Boolean is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Quote ("description", Event.Description) &
            " state " & Event.State'img);

      case Event.State is

         when Waiting =>
            Event.Timer_Task.Cancel;
            return Log_Out (True, Trace);

         when Uninitialized =>
            return Log_Out (False, Trace);

--       when others =>
--          Event.State := Canceled;
--          return Log_Out (False, Trace);

         when others =>
            declare
               Message  : constant String := "could not cancel event " &
                           Quote ("description", Event.Description) &
                           " state " & Event.State'img;
            begin
               Event.State := Canceled;
               Log_Exception (Trace, Message);
               raise Failed with Message;
            end;
      end case;
   end Cancel;

   ---------------------------------------------------------------------------
   procedure Cancel (
      Event                      : in out Event_Type;
      From                       : in     String := Here) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Quote ("description", Event.Description) &
            " state " & Event.State'img);

      case Event.State is

         when Waiting =>
            Event.Timer_Task.Cancel;
            Log_Out (Trace);

         when Uninitialized =>
            Log_Out (Trace);

         when others =>
            declare
               Message  : constant String := "could not cancel event " &
                           Quote ("description", Event.Description) &
                           " state " & Event.State'img;
            begin
               Event.State := Canceled;
               Log_Exception (Trace, Message);
               raise Failed with Message;
            end;
      end case;
   end Cancel;

   ---------------------------------------------------------------------------
   function Description (
      Event             : in     Event_Type
   ) return String is
   ---------------------------------------------------------------------------

   begin
      return Event.Description.all;
   end Description;

   ---------------------------------------------------------------------------
   overriding
   procedure Finalize (
      Event             : in out Event_Type) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Quote ("description", Event.Description) &
         " state " & Event.State'img &
         " address " & Image (Event'address));

      case Event.State is
         when Canceled | Completed | Finalized | Uninitialized =>
            Null;

         when Waiting =>
            Event.Timer_Task.Cancel;

      end case;

      if Event.Description /=
            Uninitialized_Event_Description'unchecked_access then
         Log_Here (Trace);
         Free (Event.Description);
      end if;
      Log_Out (Trace);

exception
   when FAult: others =>
      Trace_Exception (Fault, Here);
      Log_Exception (Trace, Fault);
      raise;

   end Finalize;

   ---------------------------------------------------------------------------
   procedure Free (
      Event             : in out Event_Type) is
   ---------------------------------------------------------------------------

      Pointer           : Event_Class_Access := Event'unchecked_access;

   begin
      Log_Here (Trace, "dynamic " & Event.Dynamic'img);
      if not Event.Dynamic then
         raise Failed with "event " & Event.Description.all &
            " is not dynamically allocated";
      end if;
      Free_It (Pointer);
   end Free;

   ---------------------------------------------------------------------------
   function Get_Exception (
      Event             : in     Event_Type
   ) return Ada_Lib.Strings.String_Access is
   ---------------------------------------------------------------------------

   begin
      return (if Event.Exception_Name = Null then
            Null
         else
            new String'("name " & Event.Exception_Name.all &
               " message " & Event.Exception_Message.all));
   end Get_Exception;

-- ---------------------------------------------------------------------------
-- overriding
-- procedure Initialize (
--    Event             : in out Event_Type) is
-- ---------------------------------------------------------------------------
--
-- begin
--    Log_Here (Trace, "state " & Event.State'img);
-- end Initialize;

   ---------------------------------------------------------------------------
   procedure Initialize (
      Event                      : in out Event_Type;
      Wait                       : in     Duration;
      Description                : in     String := "";
      Dynamic                    : in     Boolean := False;
      Repeating                  : in     Boolean := False) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Quote ("description", Description) &
         " wait " & Wait'img);
      Event.Dynamic := Dynamic;
      Event.Repeating := Repeating;
      Event.Set_Description (Description);
      Event.State := Waiting;
      Event.Wait := Wait;
      Event.Timer_Task := new Timer_Task_Type (Event'unchecked_access);
      Event.Initialized := True;
      while not Event.Started loop
         delay 0.1;
         Log_Here (Trace);
      end loop;
      delay 0.1;  -- make sure task gets into select
      Log_Out (Trace, "address " & Image (Event'address));
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Set_Description (
      Event                      : in out Event_Type;
      Description                : in     String) is
   ---------------------------------------------------------------------------

   begin
      if    Event.Description /= Null and then
            Event.Description /= Uninitialized_Event_Description'
               unchecked_access then
         Free (Event.Description);
      end if;
      Event.Description := new String'(Description);
   end Set_Description;

   ---------------------------------------------------------------------------
   procedure Set_Trace (
      State             : in   Boolean) is
   ---------------------------------------------------------------------------

   begin
      Trace := State;
   end Set_Trace;

   ---------------------------------------------------------------------------
   procedure Set_Wait (
      Event                      : in out Event_Type;
      Wait                       : in     Duration;
      Description                : in     String := "") is
   ---------------------------------------------------------------------------

   begin
      Log_Here (Trace, "wait " & Wait'img &
         " initialized " & Event.Initialized'img);
      if not Event.Initialized then
         Event.Initialize (Wait, Description, False, False);
      end if;
      Event.Wait := Wait;
   end Set_Wait;

   ---------------------------------------------------------------------------
   function State (
      Event                : in     Event_Type
   ) return State_Type is
   ---------------------------------------------------------------------------

   begin
      return Event.State;
   end State;

   ---------------------------------------------------------------------------
   task body Timer_Task_Type is

--    Event          : Event_Class_Access := Null;
   begin
      Log_In (Trace, "task started ");
      Ada_Lib.Trace_Tasks.Start ("timer task", Here);
      Event.Started := True;

      Log_Here (Trace, "wait " & Event.Wait'img &
         Quote ( " description", Event.Description) &
         " repeating " & Event.Repeating'img &
         " state " & Event.State'img);

      case Event.State is

         when Canceled | Completed | Finalized =>
            Log_Here (Trace, "state " & Event.State'img);

         when Uninitialized =>
            raise Failed with "unexpected state " & Event.State'img &
               " at " & Here;

         when Waiting =>
            while Event.State = Waiting loop -- if null then canceled before set
               Log_Here (Trace, Quote ("description", Event.Description) &
                  " start loop delay time " & Event.Wait'img &
                  " address " & Image (Event.all'address));
               select
                  accept Cancel do
                     Log_Here (Trace, Quote ("description", Event.Description));
                     Event.State := Canceled;
                  end Cancel;
               or
                  delay Event.Wait;          -- delay until timeout
                  Log_Here (Trace, Quote ("description", Event.Description) &
                     " initialized " & Event.Initialized'img);
                  if not Event.Initialized then
                     raise Failed with Quote ("event", Event.Description) &
                        " not initialized";
                  end if;
                  Event.Callback;
                  if not Event.Repeating then
                     Event.State := Completed;
                  end if;
                  Log_Here (Trace, Quote ("description", Event.Description) &
                     " repeating " & Event.Repeating'img);
               end select;
            end loop;

      end case;
      Log_Here (Trace, "event state " & Event.State'img
         & Quote (" description", Event.Description) & " completed");

      Ada_Lib.Trace_Tasks.Stop;
--    may cause memory exception
--    Log_Out (Trace, Quote (if Event = Null then
--             "uninitialized"
--          else
--             Quote ("description", Event.Description)));

   exception
      when Fault: others =>
         Log_Exception (Trace, Fault);
         if Event /= Null then
            Event.Exception_Name := new String'(
               Ada.Exceptions.Exception_Name (Fault));
            Event.Exception_Message := new String'(
               Ada.Exceptions.Exception_Message (Fault));
         end if;
         Log_Here (Trace);

   end Timer_Task_Type;

begin
--Trace := True;
   Log_Here (Elaborate or Trace);
end Ada_Lib.Timer;
