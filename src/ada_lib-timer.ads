with Ada.Calendar;
with Ada_Lib.Event;
with Ada.Finalization;
with Ada_Lib.Strings;
with GNAT.Source_Info;

package Ada_Lib.Timer is

   Failed                  : exception;   -- invalid request
   Overflow             : exception;   -- too many events

   Null_Time               : constant Ada.Calendar.Time :=
                           Ada.Calendar.Time_Of (
                              Year  => Ada.Calendar.Year_Number'last,
                              Month => Ada.Calendar.Month_Number'last,
                              Day      => Ada.Calendar.Day_Number'last,
                              Seconds  => 0.0);

   type Duration_Access    is access all Duration;

   type Event_Type         is abstract new Ada.Finalization.Limited_Controlled
                                 with private;

   type Event_Access          is access all Event_Type;
   type Event_Class_Access    is access all Event_Type'class;

   type State_Type               is (Canceled, Completed, Finalized, Waiting);

   function Active (
      Event             : in   Event_Type
   ) return Boolean;

   -- called when event occurs
   -- called from seperate task
   procedure Callback (
      Event             : in out Event_Type
   ) is abstract;

   -- cancel a scheduled event
   function Cancel (
      Event             : in out Event_Type
   ) return Boolean;

   function Get_Exception (
      Event             : in     Event_Type
   ) return Ada_Lib.Strings.String_Access;

-- function Create_Event (
--    Offset                  : in     Duration;
--    Dynamic                 : in     Boolean;
--    Repeating               : in     Boolean;
--    Description             : in  String
-- ) return Event_Type is abstract;

   function Description (
      Event             : in     Event_Type
   ) return String;

   overriding
   procedure Finalize (
      Event             : in out Event_Type);

   procedure Initialize (
      Event                      : in out Event_Type;
      Wait                       : in     Duration;
      Description                : in     String := "";
      Dynamic                    : in     Boolean := False;
      Repeating                  : in     Boolean := False
   ) with Pre => Wait > 0.0;

   procedure Set_Description (
      Event                      : in out Event_Type;
      Description                : in     String);

   procedure Set_Wait (
      Event                      : in out Event_Type;
      Wait                       : in     Duration);

   function State (
      Event                : in     Event_Type
   ) return State_Type;

   procedure Wait_For_Event (
      Event                      : in out Event_Type;
      From                       : in     String :=
                                             GNAT.Source_Info.Source_Location);

   procedure Set_Trace (
      State             : in   Boolean);

   No_Timeout                    : constant Duration := Duration'last;
   Trace                         : Boolean := False;

private

   task type Timer_Task_Type (
      Event                      : Event_Class_Access) is

      entry Cancel;

      entry Finalizing;

      entry Get_State (
         Return_State            :   out State_Type);

   end Timer_Task_Type;

   type Timer_Task_Access  is access Timer_Task_Type;

   Wait_Event_Description  : aliased constant String := "wait event";
   type Event_Type         is abstract new Ada.Finalization.Limited_Controlled
                                 with record
      Description          : Ada_Lib.Strings.String_Access := Null;
      Dynamic              : Boolean := False;
      Exception_Message    : Ada_Lib.Strings.String_Access := Null;
      Exception_Name       : Ada_Lib.Strings.String_Access := Null;
      Repeating            : Boolean := False;
      State                : State_Type := Waiting;
      Time                 : Ada.Calendar.Time := Null_Time;
      Timer_Task           : Timer_Task_Access := Null;
      Wait                 : Duration;
      Wait_Event           : Ada_Lib.Event.Event_Access := Null;
   end record;

end Ada_Lib.Timer;
