--  Package that provides basic tracing facilities.

with Ada.Tags;
with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada_Lib.Specifications;
with Ada_Lib.Strings.Unlimited;
with GNAT.Source_Info;
with System;

package Ada_Lib.Trace is

   Recursive_Failure             : exception;
   Trace_Failure                 : exception;

   type Dump_Width_Type          is (Width_8, Width_16, Width_32, Width_64);

   type Priority_Type            is range 0 .. 5;
   subtype Level_Type            is Natural;
   type Task_Data_Type           is record
      Buffer                     : Ada_Lib.Strings.Unlimited.String_Type;
      Last_level                 : Level_Type := 0;
      Priority                      : Level_Type := 0;
      Locked                     : Boolean := False;
      Task_Name                  : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   Off                           : constant Priority_Type := Priority_Type'first;
   Low                           : constant Priority_Type := Off + 1;
   High                          : constant Priority_Type := Priority_Type'last;
   Medium                        : constant Priority_Type := (Low + High) / 2;

   function Ask_Pause (
      Manual                     : in     Boolean;
      Prompt                     : in     String
   ) return Boolean;

   procedure Dump (
      Address                 : in     System.Address;
      Length                  : in     Natural;
      Width                   : in     Positive;
      Dump_Width              : in     Dump_Width_Type;
      Description             : in     String;
      From                    : in     String);

   function Exception_Info (
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String := "";
      Whom                       : in     String := GNAT.Source_Info.Enclosing_Entity;
      Where                      : in     String := GNAT.Source_Info.Source_Location
   ) return String;

   function From_Start (
      Time                 : in   Ada.Calendar.Time;
      Hundreds            : in   Boolean := False;
      Show_Days            : in   Boolean := False
   ) return String;

   function From_Start (
      Hundreds            : in   Boolean := False;
      Show_Days            : in   Boolean := False
   ) return String;

   function Here
   return String renames GNAT.Source_Info.Source_Location;

   function Who
   return String renames GNAT.Source_Info.Enclosing_Entity;

   function Current_Task
   return String;

   function Image (
      Time                 : in   Ada.Calendar.Time;
      Hundreds            : in   Boolean := False
   ) return String;

   function Image (
      Time                       : in   Duration;
      Hundreds                  : in   Boolean := False;
      Show_Days                  : in   Boolean := False
   ) return String;

   function Image (
      Address                    : in   System.Address
   ) return String;

   function Image (
      Fault                      : Ada.Exceptions.Exception_Occurrence
   ) return String;

   function Image_Pointer (                     -- print content of pointer with checking for constraint error
      Address              : in     System.Address;   -- address of pointer
      Bits                 : in     Natural := 32          -- in bits
   ) return String;

   function Line return Positive renames GNAT.Source_Info.Line;

   procedure Log (
      Enable                     : in     Boolean := True;
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Message                    : in     String := "");

   -- use before raising an exception when not in an exception handler
   -- and in a routine that has a log_in
   procedure Log_Exception (
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   -- use before raising an exception when in an exception handler
   -- and in a routine that has a log_in
   procedure Log_Exception (
      Enable                     : in     Boolean := True;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   procedure Log_Here (
      Enable                     : in     Boolean;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   procedure Log_Here (
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   function Log_Here (
      Result                     : in     Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity
   ) return Boolean;

   procedure Log_In (
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   procedure Log_In_Checked (
      Recursed                   : in out Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   procedure Log_Out (
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   function Log_Out (
      Result                     : in     Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity
   ) return Boolean;

   procedure Log_Out_Checked (
      Recursed                   : in out Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   function Log_Out_Checked (
      Recursed                   : in out Boolean;
      Result                     : in     Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity
   ) return Boolean;

   -- aborts after printing message
   procedure Not_Implemented (
      Why                        : in     String := "";
      Here                       : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

   procedure Override_Level (
      Level                      : in     Level_Type);

   function Pad (
      Source                     : in   String
   ) return String;

   procedure Pause (
      Prompt                     : in     String := "";
      From                       : in     String := Here;
      Trace                      : in    Boolean := False);

   procedure Pause (
      Condition                  : in     Boolean;
      Prompt                     : in     String := "";
      From                       : in     String := Here;
      Trace                      : in    Boolean := False);

   procedure Pause_On_Flag (
      Prompt                     : in     String;
      From                       : in     String := Here;
      Trace                      : in     Boolean := False);

   function Quote (
      Value                : in   Character
   ) return String;

   function Quote (
      Value                : in   String
   ) return String;

   function Quote (
      Value                : in   Ada_Lib.Strings.Unlimited.String_Type
   ) return String;

   function Quote (
      Value                : in   Ada.Strings.Unbounded.Unbounded_String
   ) return String;

   function Quote (
      Variable             : in   String;
      Value                : in   Character
   ) return String;

   function Quote (
      Variable             : in   String;
      Value                : in   String
   ) return String;

   function Quote (
      Variable             : in   String;
      Value                : access constant String
   ) return String;

   function Quote (
      Variable             : in   String;
      Value                : in   Ada.Strings.Unbounded.Unbounded_String
   ) return String;

   function Quote (
      Variable             : in   String;
      Value                : in   Ada_Lib.Strings.Unlimited.String_Type
   ) return String;

      procedure Set_Check_Address (
         Address              : in     System.Address
      );

   procedure T (
      Enable                     : in     Boolean := True;
      What                       : in     String := "";
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity;
      Where                      : in     String :=  GNAT.Source_Info.Source_Location);

   procedure Tag_History (
      Tag_Value                  : in     Ada.Tags.Tag;
      From                       : in     String := GNAT.Source_Info.
                                             Source_Location
   );

   function Tag_Name (
      Tag_Value                  : Ada.Tags.Tag
   ) return String renames Ada.Tags.Expanded_Name ;

   procedure Trace_Exception (
      Debug                      : in   Boolean;
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Where                      : in   String := GNAT.Source_Info.Source_Location);

   procedure Trace_Exception (
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Where                      : in   String := GNAT.Source_Info.Source_Location);

   procedure Trace_Message_Exception (
      Debug                      : in   Boolean;
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Message                    : in   String;
      Where                      : in   String := GNAT.Source_Info.Source_Location);

   procedure Trace_Message_Exception (
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Message                    : in   String;
      Where                      : in   String := GNAT.Source_Info.Source_Location);

   function File
   return String renames GNAT.Source_Info.File;

   subtype String_Constant_Access
                                 is Ada_Lib.Strings.String_Constant_Access;

   Absolute                      : constant Boolean := False;
   Ada_Lib_Lib_Verbose           : Boolean := False;
   Ada_Lib_Trace_Trace           : aliased Boolean := False;
   Debug                         : Boolean := False;
   Debug_Trace                   : Boolean := False;
   Detail                        : Boolean := False;
   Elaborate                     : Boolean := False;
   Include_Hundreds             : Boolean := False;
   Include_Program               : Boolean := False;
   Include_Task                  : Boolean := False;
   Include_Time                  : Boolean := True;
   Indent_Trace                  : Boolean := False;
   Inhibit_Trace                 : Boolean := False;
   No_Time                       : constant Ada.Calendar.Time :=
                                       Ada.Calendar.Time_Of (
                                          Year => Ada.Calendar.Year_Number'last,
                                          Month => Ada.Calendar.Month_Number'last,
                                          Day => Ada.Calendar.Day_Number'last,
                                          Seconds => Ada.Calendar.Day_Duration'last);
   Pause_Flag                    : Boolean := False;
   Test_Condition                : Boolean := False;
   Trace_Levels                  : Boolean := False;
   Trace_Tests                   : Boolean := False;
   Trace_Options                 : Boolean := False;

   type Traces_Type        is (
      Containers,
      None,
      Finalization
   );

   package Selection_Package is new Ada_Lib.Specifications.Selection_Package (
      Priority_Type              => Priority_Type,
      Selection_Type             => Traces_Type);

   Specification           : constant Selection_Package.Specification_Array := (
      (
         Priority       => Off,
         Option      => ' ',
         Prompt      => Null ),
      (
         Priority       => Medium,
         Option      => 'c',
         Prompt      => new String'("containers") ),
      (
         Priority       => High,
         Option      => 'f',
         Prompt      => new String'("finalization") )
   );

   package Specification_Package is new Ada_Lib.Specifications.Specification_Package (
      Priority_Type           => Priority_Type,
      Selection_Type       => Traces_Type,
      Specification_Type   => Selection_Package.Specification_Level_Type,
      Specifications_Array => Selection_Package.Specification_Array,
      Specifications       => Specification);

   function Test (
      Which             : in   String;
      Priority             : in   Priority_Type := Priority_Type'first
   ) return Boolean renames Specification_Package.Test;

   procedure Set (
      Which             : in   String;
      Priority             : in   Priority_Type := Priority_Type'first
   ) renames Specification_Package.Set;

   procedure Set (
      Options              : in   String
   ) renames Specification_Package.Set;

private

   type File_Type                is abstract tagged limited null record;

   procedure Flush (
      File                       : in     File_Type) is abstract;

   procedure Output (
      File                       : in out File_Type;
      Data                       : in     String) is abstract;

   type File_Access              is access File_Type;
   type File_Class_Access        is access all File_Type'class;

   type Default_File_Type        is new File_Type with record
      File                       : Ada.Text_IO.File_Type;
   end record;

   overriding
   procedure Flush (
      File                       : in     Default_File_Type);

   overriding
   procedure Output (
      File                       : in out Default_File_Type;
      Data                       : in     String);

   procedure Replace_Output_File (
      New_File                   : in     File_Class_Access;
      Previous_File              :    out File_Class_Access);

end Ada_Lib.Trace;
