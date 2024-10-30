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

   procedure Free is new Ada.Unchecked_Deallocation (
      Name     => Event_Class_Access,
      Object   => Event_Type'class);

   procedure Free is new Ada.Unchecked_Deallocation (
      Name     => Ada_Lib.Strings.String_Access_All,
      Object   => String);

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
      Event                      : in out Event_Type;
      From                       : in     String := Here
   ) return Boolean is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Quote ("description", Event.Description) &
            " state " & Event.State'img & (
            if Event.Timer_Task = Null then
               " null task"
            else
               " have task") &
            " called from " & From);
      if    Event.State = Waiting and then
            Event.Timer_Task /= Null then
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
            when Canceled | Completed | Finalized | Uninitialized =>
               Null;

            when Waiting =>
               Event.Timer_Task.Finalizing;
               Event.Timer_Task := Null;

         end case;
      end if;
      if Event.Description /= Uninitialized_Event_Description'unchecked_access then
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

   ---------------------------------------------------------------------------
   overriding
   procedure Initialize (
      Event             : in out Event_Type) is
   ---------------------------------------------------------------------------

   begin
      Log_Here (Trace, "state " & Event.State'img);
   end Initialize;

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
      Event.Initialized := True;
      Event.Repeating := Repeating;
      Event.Set_Description (Description);
      Event.State := Waiting;
      Event.Wait := Wait;
      Event.Timer_Task := new Timer_Task_Type (Event'unchecked_access);
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

   ---------------------------------------------------------------------------
   task body Timer_Task_Type is

   begin
      Log_In (Trace, "task started dynamic " & Event.Dynamic'img);
      Ada_Lib.Trace_Tasks.Start ("timer task", Here);


      Log_Here (Trace, "wait " & Event.Wait'img &
         Quote ( " description", Event.Description) &
         " repeating " & Event.Repeating'img &
         " state " & Event.State'img);
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
            accept Finalizing do
               Log_Here (Trace, Quote ("description", Event.Description));
               Event.State := Finalized;
            end Finalizing;
         or
            accept Get_State (
               Return_State         :   out State_Type) do

               Log_Here (Trace, Quote ("description", Event.Description));
               Return_State := Event.State;
            end Get_State;
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

      Log_Here (Trace, "event state " & Event.State'img
         & Quote (" description", Event.Description) & " completed" &
         " dynamic " & Event.Dynamic'img);

      if Event.Dynamic and then Event.State/= Finalized then
         declare
            Event_Pointer        : Event_Class_Access := Event;

         begin
            Free (Event_Pointer);
         end;
      else
         Log_Here (Trace, "dynamic already finalized");
      end if;

      Event.Timer_Task := Null;
      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Trace, Quote (" description", Event.Description));

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
Trace := True;
   Log_Here (Elaborate or Trace);
end Ada_Lib.Timer;
