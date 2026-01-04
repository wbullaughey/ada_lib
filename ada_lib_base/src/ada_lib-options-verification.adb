with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Options.Program;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with System;

package body Ada_Lib.Options.Verification is

   Debug : Boolean renames Ada_Lib_Options_Verification.Debug;

   ----------------------------------------------------------------------------
   overriding
   procedure Bad_Option (              -- aborts program
      Options                    : in     Verification_Options_Type;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, "what " & What & " where " & Where);
      Parsing_Failed;
      raise Failed with
         (if Message'length > 0 then
            Quote (Message) & " "
         else "") &
         Quote ("Processing option ", What) & (if Debug or Trace_Options then
            " From " & Where
         else
            "");
   end Bad_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Bad_Option (              -- aborts program
      Options                    : in     Verification_Options_Type;
      What                       : in     String;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, "what " & What & " where " & Where);
      Parsing_Failed;
      raise Failed with
         (if Message'length > 0 then
            Quote (Message) & " "
         else "") &
         Quote ("Processing option ", What) &
         (if Debug or Trace_Options then
            " From " & Where
         else
            "");
   end Bad_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Verification_Options_Type;
      Option                     : in     Base_Flag_Option_Type'class;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, Quote ("message", Message) &
         " what " & Option.Image &
         " where " & Where);
      Parsing_Failed;
      raise Failed with (
         (if Message'length > 0 then
            Quote (Message) & " "
         else "") &
         "Processing " & Option.Image) &
         (if Debug or Trace_Options then
            " From " & Where
         else
            "");
   end Bad_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Bad_Trace_Option (              -- aborts program
      Options           : in     Verification_Options_Type;
      Trace_Option      : in     Character;
      What              : in     Character;
      Modifier          : in     Character := Ada.Characters.Latin_1.Nul;
      Message           : in     String := "";
      Where             : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options,
         Quote ("trace option", Trace_Option) &
         Quote (" what ", What) &
         Quote (" modifier ", Modifier) &
         " where " & Where);
      Parsing_Failed;
      raise Failed with
         (if Message'length > 0 then
            Quote (Message) & " "
         else "") &
         (if Modifier /= Ada.Characters.Latin_1.Nul then
            " modifier " & Modifier
         else
            "") &
         Quote (" Trace option", What) &
         Quote (" not defined for", Trace_Option) &
         (if Debug or Trace_Options then
            " From " & Where
         else
            "");
   end Bad_Trace_Option;

   ---------------------------------------------------------------
   overriding
   function Initialize (
      Options                 : in out Verification_Options_Type;
      From                    : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In_Checked (Options.Initialized, Debug or Trace_Options,
         "options address " &
         Ada_Lib.Strings.Image (Options'address) &" options tag " &
         Tag_Name (Verification_Options_Type'class (Options)'tag));
      if Debug or Trace_Options then
         Tag_History (Verification_Options_Type'class (Options)'tag);
      end if;
      Options.Initialized := True;
      return Log_Out_Checked (Options.Initialized, True,
         Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options              : in out Verification_Options_Type;
      Iterator             : in out Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Extended                   : Boolean := False;
      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      Log_In (Debug or Ada_Lib_Trace_Trace or Trace_Options,
         Quote ("parameter", Parameter));
not_implemented;
   end Trace_Parse;

   ----------------------------------------------------------------------------
   overriding
   procedure Update_Filter (
      Options                    : in out Verification_Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_Here ("options tag " & Tag_Name (
         Verification_Options_Type'class (Options)'tag));
      Not_Implemented;
   end Update_Filter;

   ---------------------------------------------------------------
   overriding
   function Process_Argument (  -- process one argument
     Options                     : in out Verification_Options_Type;
     Iterator                    : in out Command_Line_Iterator_Interface'
                                             class;
     Argument                    : in     String
   ) return Boolean is
   pragma Unreferenced (Options, Iterator, Argument);
   ---------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, "no argument");
      return False;
   end Process_Argument;

   ---------------------------------------------------------------
   overriding
   function Verify_Initialized (
      Options                    : in     Verification_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   ---------------------------------------------------------------

      ---------------------------------------------------------------
      procedure Failed (
         Text                    : in     String) is
      ---------------------------------------------------------------

         Message  : constant String := Text &  " called from " & From;

      begin
         Log_Here (Message);
         Put_Line (Message);
      end Failed;
      ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options or Trace_Pre_Post_Conditions,
         "Initialized " & Options.Initialized'img &
         " options tag " &
         Tag_Name (Verification_Options_Type'class (Options)'tag));

      if not Have_Ada_Lib_Program_Options then
         Failed ("Get_Modifiable_Program_Options not initialized at " & Here);
      else
         if Options.Initialized then
            return Log_Out (True, Debug or Trace_Options or
               Trace_Pre_Post_Conditions, "Ada options not initialized");
         else
            Failed ("Options.Initialized not initialized at " & Here);
         end if;
      end if;

      if Debug or Trace_Options then
         Tag_History (Verification_Options_Type'class (Options)'tag);
      end if;
      return Log_Out (False, True, "Verify_Initialized failed");
   end Verify_Initialized;

   ---------------------------------------------------------------
   overriding
   function Verify_Preinitialize (
      Options                    : in     Verification_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options,
         "Initialized " & Options.Initialized'img &
         " options address " &
         Ada_Lib.Strings.Image (Options'address) &
         " options tag " &
         Tag_Name (Verification_Options_Type'class (Options)'tag) &
         " Get_Ada_Lib_Read_Only_Program_Options " &
         Ada_Lib.Strings.Image (
            Get_Ada_Lib_Read_Only_Program_Options.all'address));
      if Debug or Trace_Options then
         Tag_History (Verification_Options_Type'class (Options)'tag);
      end if;

      if Get_Ada_Lib_Read_Only_Program_Options = Null then
         Put_Line ("Get_Ada_Lib_Read_Only_Program_Options null " & Here);
      else
         if Options.Initialized then
            Put_Line ("Options.Initialized should be false");
         else
            return Log_Out (True, Debug or Trace_Options);
         end if;
      end if;
      Put_Line (Who & " failed at " & Here);
      return Log_Out (False, Debug or Trace_Options);

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         return False;

   end Verify_Preinitialize;

   ----------------------------------------------------------------------------
   function Was_Initialized (
      Options                 : in     Verification_Options_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Tag_History (Debug or Trace_Options,
         Verification_Options_Type'class (Options)'tag);
      return Log_Here (Options.Initialized,
         Debug or Trace_Options or Trace_Pre_Post_Conditions or
            not Options.Initialized);
   end Was_Initialized;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Ada_Lib.Options.Verification;

