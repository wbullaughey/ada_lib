with Ada.Exceptions;
--with Ada.Text_IO;use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--with Ada_Lib.Time;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks; use Ada_Lib.Trace_Tasks;

package body Ada_Lib.Timer is

-- use type Ada.Calendar.Time;
   use type Ada_Lib.Strings.String_Access;

   procedure Free is new Ada.Unchecked_Deallocation (
      Name     => Event_Class_Access,
      Object   => Event_Type'class);

   procedure Free is new Ada.Unchecked_Deallocation (
      Name     => Ada_Lib.Strings.String_Access,
      Object   => String);

   procedure Free is new Ada.Unchecked_Deallocation (
      Name     => Ada_Lib.Event.Event_Access,
      Object   => Ada_Lib.Event.Event_Type);

   ---------------------------------------------------------------------------
   function Active (
      Event                      : in   Event_Type
   ) return Boolean is
   ---------------------------------------------------------------------------

      Running                    : constant Boolean := (if Event.Timer_Task = Null then
                                       False
                                    else
                                       not Event.Timer_Task.all'terminated);
   begin
      Log_Here (Trace, "active " & Running'img);
      return Running;
   end Active;

   ---------------------------------------------------------------------------
   function Cancel (
      Event                      : in out Event_Type
   ) return Boolean is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Quote ("description", Event.Description) &
         " state " & Event.State'img);
      if Event.State = Waiting then
         Event.Timer_Task.Cancel;
         return Log_Out (True, Trace);
      else
         return Log_Out (False, Trace);
      end if;
   end Cancel;

   ---------------------------------------------------------------------------
   overriding
   procedure Finalize (
      Event             : in out Event_Type) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Quote ("description", Event.Description) &
         " address " & Image (Event'address));
      if Event.Timer_Task /= Null then
         Log_Here (Trace, "state " & Event.State'img);

         case Event.State is
            when Canceled | Completed | Finalized =>
               Null;

            when Waiting =>
               Event.Timer_Task.Finalizing;

         end case;
      end if;
      Free (Event.Description);
      Free (Event.Wait_Event);
      Log_Out (Trace);

exception
   when FAult: others =>
      Trace_Exception (Fault, Here);
      Log_Exception (Trace, Fault);
      raise;

   end Finalize;

   ---------------------------------------------------------------------------
   function Description (
      Event             : in     Event_Type
   ) return String is
   ---------------------------------------------------------------------------

   begin
      return Event.Description.all;
   end Description;

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
--    Log_In (Trace, "state " & Event.State'img);
--    Event.Timer_Task := new Timer_Task_Type (Event'unchecked_access);
--    Log_Out (Trace,;
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
      Event.Set_Description (Description);
      Event.Dynamic := Dynamic;
      Event.Repeating := Repeating;
      Event.Wait := Wait;
      Event.Wait_Event := new Ada_Lib.Event.Event_Type (
         Ada_Lib.Strings.String_Constant_Access (Event.Description));
      Event.Timer_Task := new Timer_Task_Type (Event'unchecked_access);
      Log_Out (Trace, "address " & Image (Event'address));
   end Initialize;

-- ---------------------------------------------------------------------------
-- function Offset (
--    Event             : in   Event_Type
-- ) return Duration is
-- ---------------------------------------------------------------------------
--
-- begin
--    return To_Duration (Event.Offset);
-- end Offset;
--
   ---------------------------------------------------------------------------
   procedure Set_Description (
      Event                      : in out Event_Type;
      Description                : in     String) is
   ---------------------------------------------------------------------------

   begin
      if Event.Description /= Null then
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
      Wait                       : in     Duration) is
   ---------------------------------------------------------------------------

   begin
      Event.Wait := Wait;
   end Set_Wait;

   ---------------------------------------------------------------------------
   function State (
      Event                : in     Event_Type
   ) return State_Type is
   ---------------------------------------------------------------------------

   begin
      if Event.Timer_Task = Null then
         return Waiting;
      else
         declare
            Result               : State_Type;

         begin
            Event.Timer_Task.Get_State (Result);
            return Result;
         end;
      end if;
   end State;

-- ---------------------------------------------------------------------------
-- function To_Duration (
--    Time                       : in     Duration
-- ) return Duration is
-- ---------------------------------------------------------------------------
--
-- begin
--    return Duration (Time) / Duration (Ratio);
-- end To_Duration;

   ---------------------------------------------------------------------------
   procedure Wait_For_Event (
      Event                   : in out Event_Type;
      From                    : in     String :=
                                          GNAT.Source_Info.Source_Location) is
   ---------------------------------------------------------------------------

   begin
      Event.Wait_Event.Wait_For_Event;
   end Wait_For_Event;

   ---------------------------------------------------------------------------
   task body Timer_Task_Type is

   begin
      Log_In (Trace, "task started");
      Ada_Lib.Trace_Tasks.Start ("timer task", Here);


      Log_Here (Trace, "wait " & Event.Wait'img &
         Quote ("description", Event.Description) &
         "state " & Event.State'img);

      while Event.State = Waiting loop -- if null then canceled before set
         Log_Here (Trace, Quote ("description", Event.Description) &
            " start loop delay time " & Event.Wait'img,
            " address " & Image (Event.all'address));
         select
            accept Cancel do
               Event.State := Canceled;
               Log_Here (Trace, Quote ("description", Event.Description));
            end Cancel;
         or
            accept Finalizing do
               Event.State := Finalized;
               Log_Here (Trace, Quote ("description", Event.Description));
            end Finalizing;
         or
            accept Get_State (
               Return_State         :   out State_Type) do

               Return_State := Event.State;
               Log_Here (Trace, Quote ("description", Event.Description));
            end Get_State;
         or
            delay Event.Wait;          -- delay until even Finalizing;;
            Log_Here (Trace, Quote ("description", Event.Description));
            Event.Callback;
            if not Event.Repeating then
               Event.State := Completed;
            end if;
            Log_Here (Trace, Quote ("description", Event.Description) &
               " repeating " & Event.Repeating'img);
         end select;
      end loop;
      Log_Here (Trace, "event " & Quote (" description", Event.Description) &
         " completed");
      Event.Wait_Event.Set_Event;

      Log_Here (Trace, "state " & Event.State'img &
         Quote (" description", Event.Description) &
         " dynamic " & Event.Dynamic'img);

      if Event.Dynamic and then Event.State/= Finalized then
         declare
            Event_Pointer        : Event_Class_Access := Event;

         begin
            Free (Event_Pointer);
         end;
      end if;

      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Trace);

   exception
      when Fault: others =>
         Log_Exception (Trace, Fault);
         Event.Exception_Name := new String'(
            Ada.Exceptions.Exception_Name (Fault));
         Event.Exception_Message := new String'(
            Ada.Exceptions.Exception_Message (Fault));
         Log_Out (Trace);

   end Timer_Task_Type;

begin
--Trace := True;
   Log_Here (Elaborate or Trace);
end Ada_Lib.Timer;
