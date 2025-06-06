with Ada.Assertions;
with Ada.Exceptions;
--with Ada.Strings.Maps;
with Ada.Tags;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Configuration;
with Ada_Lib.Database.Connection;
with Ada_Lib.Directory;
with Ada_Lib.EMail;
with Ada_Lib.Event;
with Ada_Lib.Help;
with Ada_Lib.Interrupt;
with Ada_Lib.Lock;
with Ada_Lib.Mail;
with Ada_Lib.Options.Runstring;
with Ada_Lib.OS;
with Ada_Lib.Parser;
with Ada_Lib.OS.Run;
with Ada_Lib.Socket_IO;
with Ada_Lib.Strings;
with Ada_Lib.Template;
with Ada_Lib.Text;
with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;

pragma Elaborate_All (Ada_Lib.Lock);

package body Ada_Lib.Options.Actual is

   use type Ada.Tags.Tag;

   Initialize_Recursed           : Boolean := False;
   Modifiable_Nested_Options     : Nested_Options_Class_Access := Null;
   Modifiable_Program_Options    : Program_Options_Class_Access := Null;
   Test_Condition_Flag           : constant Character := 'c';
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                          Create_Options ('a', Unmodified);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             "hPv",
                                             Unmodified) &
                                          Ada_Lib.Options.Create_Options (
                                             "iptx" & Test_Condition_Flag,
                                             Ada_Lib.Help.Modifier);
-- Parameter_Parsing_Failed      : Boolean := False;


   procedure Set_All;

   ----------------------------------------------------------------------------
   overriding
   procedure Bad_Option (              -- aborts program
      Options                    : in     Abstract_Options_Type;
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
      Options                    : in     Abstract_Options_Type;
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
      Options                    : in     Abstract_Options_Type;
      Option                     : in     Option_Type'class;
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
      Options                    : in     Abstract_Options_Type;
      Trace_Option               : in     Character;
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
         Quote ("Trace option ", What) &
         Quote (" not defined for", Trace_Option) &
         (if Debug or Trace_Options then
            " From " & Where
         else
            "");
   end Bad_Trace_Option;

   ----------------------------------------------------------------
   function Get_Ada_Lib_Modifiable_Nested_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Nested_Options_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, "from " & From);
      if Debug or Trace_Options then
         Tag_History (Modifiable_Nested_Options.all'tag, From);
      end if;
      return Modifiable_Nested_Options;

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Get_Ada_Lib_Modifiable_Nested_Options;

   ----------------------------------------------------------------------------
   function Get_Ada_Lib_Read_Only_Nested_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Nested_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options,
         "modifiable options tag " & Tag_Name (Modifiable_Nested_Options.all'tag) &
         " from " & From);
Tag_History (Modifiable_Nested_Options.all'tag);
      return Nested_Options_Constant_Class_Access (
         Modifiable_Nested_Options);
   end Get_Ada_Lib_Read_Only_Nested_Options;

   ----------------------------------------------------------------
   function Get_Ada_Lib_Modifiable_Program_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Program_Options_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options,
         (if Modifiable_Program_Options = Null then
            "Modifiable_Program_Options not set "
         else
            "") &
         "from " & From);

      Ada.Assertions.Assert (Modifiable_Program_Options /= Null,
         "Modifiable_Program_Options not set");

      if Debug or Trace_Options then
         Tag_History (Modifiable_Program_Options.all'tag, From);
      end if;
      return Modifiable_Program_Options;

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Get_Ada_Lib_Modifiable_Program_Options;

   ----------------------------------------------------------------------------
   function Get_Ada_Lib_Read_Only_Program_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Program_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options,
         "modifiable options tag " & Tag_Name (Modifiable_Program_Options.all'tag) &
         " from " & From);
      if Debug then
         Tag_History (Modifiable_Program_Options.all'tag);
      end if;

      return Program_Options_Constant_Class_Access (
         Modifiable_Program_Options);
   end Get_Ada_Lib_Read_Only_Program_Options;

   ----------------------------------------------------------------------------
   overriding
   procedure Display_Help (            -- common for all programs that use GNOGA_Options
                              -- prints full help, aborts program
     Options                     : in     Nested_Options_Type;  -- only used for dispatch
     Message                     : in     String := "";   -- leave blank no error help
     Halt                        : in     Boolean := True) is
   ----------------------------------------------------------------------------

   begin
      raise Failed with "should not be called";
   end Display_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Display_Help (            -- common for all programs that use GNOGA_Options
                              -- prints full help
     Options                     : in     Program_Options_Type;     -- only used for dispatch
     Message                     : in     String := "";  -- leave blank no error help
     Halt                        : in     Boolean := True) is
   ----------------------------------------------------------------------------

      -------------------------------------------------------------------------
      procedure Print_Help (
         Line                    : in     String) is
      -------------------------------------------------------------------------

      begin
         Put ("    ");
         Put_Line (Line);
      end Print_Help;
      -------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, Quote ("message", Message) &
         " halt " & Halt'img &
         " options tag " & Tag_Name (Interface_Options_Type'class (
            Options)'tag) &
         " in help " & Options.In_Help'img);
      if Options.In_Help then
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
      end if;

      if Message'length > 0 then
         Put_Line (Message);
      end if;
      Get_Ada_Lib_Read_Only_Program_Options.Program_Help (Program);

      Ada_Lib.Help.Display (Print_Help'access);
      New_Line;

      Interface_Options_Type'class (Options).Program_Help (Traces);
      if Halt then
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
      end if;
   end Display_Help;

   ----------------------------------------------------------------------------
   function Have_Ada_Lib_Nested_Options
   return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Modifiable_Nested_Options /= Null;
   end Have_Ada_Lib_Nested_Options;

   ----------------------------------------------------------------------------
   function Have_Ada_Lib_Program_Options
   return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Modifiable_Program_Options /= Null;
   end Have_Ada_Lib_Program_Options;

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

      Message        : constant String := " from " & From &
         " options with parameters " & Image (Options_With_Parameters) &
         " with out " & Image (Options_Without_Parameters);

   begin
     Log_In_Checked (Initialize_Recursed, Debug or Trace_Options, Message);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);

      return Log_Out_Checked (Initialize_Recursed,
         Verification_Options_Type (Options).Initialize,
         Debug or Trace_Options, Message);
   end Initialize;

-- ----------------------------------------------------------------------------
-- function Parsing_Failed return Boolean is
-- ----------------------------------------------------------------------------
--
-- begin
--    Log_Here (Debug or Trace_Options, "Parameter_Parsing_Failed " & Parameter_Parsing_Failed'img);
--    return Parameter_Parsing_Failed;
-- end Parsing_Failed;

   ----------------------------------------------------------------------------
   procedure Post_Process (      -- final initialization
     Options                    : in out Program_Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, "options post processing completed");
   end Post_Process;

   ----------------------------------------------------------------------------
-- overriding
   function Process (     -- processes whole command line calling Process_Option for each option
     Options                     : in out Program_Options_Type;
     Include_Options             : in     Boolean;
     Include_Non_Options         : in     Boolean;
     Option_Prefix               : in     Character := '-';
     Modifiers                   : in     String := ""
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, "Include_Options " & Include_Options'img &
         " Include_Non_Options " & Include_Non_Options'img &
         Ada_Lib.Trace.Quote (" modifiers", Modifiers) &
         " " & Tag_Name (Program_Options_Type'class (Options)'tag));

      declare
         Iterator                : Ada_Lib.Command_Line_Iterator.Run_String.
                                    Runstring_Iterator_Type;

      begin
         Log_Here (Debug or Trace_Options);
         Iterator.Initialize (Include_Options, Include_Non_Options,
            Option_Prefix, Modifiers);
         Options.Process (Iterator);

      exception
         when Fault: others =>
            Trace_Exception (Debug or Trace_Options, Fault);
            Get_Ada_Lib_Read_Only_Program_Options.Display_Help (
               Ada.Exceptions.Exception_Message (Fault));
      end;

      Options.Processed := True;
      return Log_Out (True, Debug or Trace_Options);

   exception
      when Fault: Ada_Lib.Options.Failed =>
         Trace_Exception (Debug or Trace_Options, Fault);
         Get_Ada_Lib_Read_Only_Program_Options.Display_Help (Ada.Exceptions.Exception_Message (Fault), True);
         raise;

      when Fault: others =>
         Trace_Exception (Debug or Trace_Options, Fault);
         raise;

   end Process;

   ----------------------------------------------------------------------------
-- overriding
   procedure Process (
      Options                    : in out Program_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options,
         Tag_Name (Program_Options_Type'class (Options)'tag));

      while not Iterator.At_End loop
         begin
            if Iterator.Is_Option then
               declare
                  Option         : constant Ada_Lib.Options.Option_Type'class :=
                                    Iterator.Get_Option;
                  Message        : constant String := Option.Image & " not defined";

               begin
                  Log_Here (Debug or Trace_Options, Option.Image);
                  if Ada_Lib.Options.Interface_Options_Type'class (
                        Options).Process_Option (Iterator, Option) then
                     Log_Here (Debug or Trace_Options, Option.Image);
                  else
                     Log_Here (Debug or Trace_Options, Message);
                     Options.Bad_Option (Option, Message);     -- aborts program
                     exit;
                  end if;
               end;
            else
               declare
                  Argument          : constant String :=
                                       Iterator.Get_Argument;
               begin
                  if not Program_Options_Type'class (Options).Process_Argument (
                        Iterator, Argument) then
                     Log_Out (Debug or Trace_Options);
                     Options.Bad_Option ("unexpected '" & Argument & "' on run string" &
                        " from " & Here);
                        -- raises exception
                  end if;
               end;
            end if;

         exception

            when Fault: others =>
               Trace_Exception (Debug or Trace_Options, Fault);
               if not Ada_Lib.Help_Test then
                  raise;
               end if;

         end;
         if not Iterator.At_End then
            Iterator.Advance;
         end if;
      end loop;

      Log_Out (Debug or Trace_Options, "processed");

   exception

      when Fault: others =>
         Trace_Exception (Debug or Trace_Options, Fault);
         raise;

   end Process;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options                    : in out Program_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class;
      Option                     : in     Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug, "option '" & Option.Image &
         " kind " & Option.Kind'img &
         " Help_Test " & Ada_Lib.Help_Test'img);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         if Option.Kind = Ada_Lib.Options.Plain then
            case Option.Option is

               when 'a' =>
                  Options.Trace_Parse (Iterator);

               when 'h' =>
                  if not Ada_Lib.Help_Test then
                     Get_Ada_Lib_Read_Only_Program_Options.Display_Help;
                  end if;

               when 'P' =>
                  Ada_Lib.Trace.Pause_Flag := True;

               when 'v' =>
                  Options.Verbose := True; -- Set_Verbose (True);

               when Others =>
                  Log_Exception (Debug or Trace_Options);
                  raise Failed with "Has_Option incorrectly passed " &
                     Option.Image;

            end case;
         else
            case Option.Option is

               when Test_Condition_Flag =>      -- c
                  Test_Condition := True;

               when 'i' =>
                  Indent_Trace := True;

               when 'p' =>
                  Include_Program := True;

               when 't' =>
                  Include_Task := True;

               when 'x' =>
                  Include_Time := False;

               when others =>
                  return Log_Out (False, Debug or Trace_Options,
                     " option not handled" & Option.Image);

            end case;
         end if;
      else
         return Log_Out (False, Debug or Trace_Options,
            Quote (" option not handled", Option.Image));
      end if;
      return Log_Out (True, Debug or Trace_Options,
         " option" & Option.Image & " handled");
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options                    : in out Nested_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class;
      Option                     : in     Option_Type'class
   ) return Boolean is
   pragma Unreferenced (Iterator);
   ----------------------------------------------------------------------------

   begin
      return False;  -- no options for parent
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in      Nested_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      null; -- no more help
   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Ada_Lib";

   begin
      Log_In (Debug or Trace_Options, "mode " & Help_Mode'img);
      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Help.Add_Option ('a', "trace options",
            "Ada_Lib library trace options", Component);
         Ada_Lib.Help.Add_Option ('h', "", "this message", Component);
         Ada_Lib.Help.Add_Option ('P', "", "pause", Component);
         Ada_Lib.Help.Add_Option ('v', "", "verbose", Component);
         Ada_Lib.Help.Add_Option (Test_Condition_Flag, "", "trace test condition",
            Component, Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('i', "", "indent trace", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('p', "", "include program in trace", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('t', "", "include task in trace", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('x', "", "exclude time in trace", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('?', "", "this message", Component);

      when Ada_Lib.Options.Traces =>
         Put_Line ("CAC ada_lib trace library options (-a)");
         Put_Line ("      a               all");
         Put_Line ("      b               database subscribe");
         Put_Line ("      c               Ada_Lib.Command_Line Trace");
         Put_Line ("      C               Ada_Lib.Configuration Trace");
         Put_Line ("      e               Event");
--       Put_Line ("      g               GNOGA.Debug");
--       Put_Line ("      G               GNOGA_Options.Debug");
         Put_Line ("      h               Help");
         Put_Line ("      i               interrupt");
         Put_Line ("      I               Ada_Lib.interface");
         Put_Line ("      l               lock");
         Put_Line ("      m               timer");
         Put_Line ("      M               mail");
         Put_Line ("      o               os");
         Put_Line ("      O               Ada_Lib.Options");
         Put_Line ("      p               parser");
         Put_Line ("      P               database post");
         Put_Line ("      r               run remote, database connect");
         Put_Line ("      R               Ada_Lib.Options.Runstring.Debug");
         Put_Line ("      s               socket");
         Put_Line ("      S               socket Stream");
         Put_Line ("      t               Ada_Lib.Trace");
         Put_Line ("      T               Ada_Lib.Trace_Tasks");
--       Put_Line ("      x               Ada_Lib.Trace.Detail");
--       Put_Line ("      @               Ada_Lib.Strings");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "c              Template Compile");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "d              Template Detail");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "D              Ada_Lib.Directory trace");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "e              Template Evaluate");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "E              Template Expand");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "l              Template Load");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "o              Trace_Options");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "s              Strings");
         Put_Line ("      " & Ada_Lib.Help.Modifier &
                           "t              Ada_Lib.Text");

      end case;
      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
-- function Get_Ada_Lib_Read_Only_Program_Options
-- return Program_Options_Constant_Class_Access is
-- ----------------------------------------------------------------------------
--
-- begin
--    return Program_Options_Constant_Class_Access (
--       Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options);
-- exception
--    when Fault: others =>
--       Trace_Message_Exception (Fault, "Get_Ada_Lib_Read_Only_Program_Options " & Tag_Name (
--          Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options.all'tag));
--       Tag_History (Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options.all'tag);
--       raise;
-- end Get_Ada_Lib_Read_Only_Program_Options;

   ----------------------------------------------------------------
   procedure Set_Ada_Lib_Nested_Options (
      Options                    : in     Nested_Options_Class_Access) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, Tag_Name (Options.all'tag));
      if Debug or Trace_Options then
         Tag_History (Options.all'tag);
      end if;
      Modifiable_Nested_Options := Options;
      Log_Out (Debug or Trace_Options); --, Modifiable_Options_Address);

   end Set_Ada_Lib_Nested_Options;

   ----------------------------------------------------------------
   procedure Set_Ada_Lib_Program_Options (
      Options                    : in     Program_Options_Class_Access) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, Tag_Name (Options.all'tag));
      if Debug or Trace_Options then
         Tag_History (Options.all'tag);
      end if;
      Modifiable_Program_Options := Options;
      Log_Out (Debug or Trace_Options); --, Modifiable_Options_Address);

   end Set_Ada_Lib_Program_Options;

   ----------------------------------------------------------------------------
   procedure Set_All is
   ----------------------------------------------------------------------------

   begin
      Ada_Lib.Command_Line_Iterator.Debug := True;
      Ada_Lib.Configuration.Trace := True;
      Ada_Lib.Interrupt.Debug := True;
      Ada_Lib.Database.Connection.Debug := True;
      Ada_Lib.Database.Set_Trace (True, True);
      Ada_Lib.Database.Trace_Get_Post := True;
      Ada_Lib.Help.Debug := True;
      Ada_Lib.Lock.Debug := True;
      Ada_Lib.EMail.Debug := True;
      Ada_Lib.Mail.Debug := True;
--    GNOGA_Options.Debug := True;
      Ada_Lib.Options.Runstring.Debug := True;
      Ada_Lib.Options.Debug := True;
      Ada_Lib.OS.Trace := True;
      Ada_Lib.OS.Run.Debug := True;
      Ada_Lib.Parser.Debug := True;
      Ada_Lib.Socket_IO.Trace := True;
      Ada_Lib.Strings.Debug := True;
      Ada_Lib.Timer.Set_Trace (True);
      Ada_Lib.Trace_Tasks.Debug := True;
      Debug := True;

   end Set_All;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options              : in out Program_Options_Type;
      Iterator             : in out Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Extended                   : Boolean := False;
      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      Log_In (Debug or Ada_Lib_Trace_Trace or Trace_Options,
         Quote ("parameter", Parameter));
      for Trace of Parameter loop
         Log_Here (Debug or Ada_Lib_Trace_Trace or Trace_Options, " Extended " &
            Extended'img & Quote (" trace", Trace));

         case Extended is

            when False =>
               case Trace is

                  when 'a' =>
                     Set_All;

                  when 'b' =>
                     Ada_Lib.Database.Debug_Subscribe := True;

                  when 'c' =>
                     Ada_Lib.Command_Line_Iterator.Debug := True;

                  when 'C' =>
                     Ada_Lib.Configuration.Trace := True;

                  when 'e' =>
                     Ada_Lib.Event.Debug := True;

--                when 'g' =>
--                   GNOGA_Options.Debug := True;

--                when 'G' =>
--                   GNOGA_Options.Debug := True;

                  when 'h' =>
                     Ada_Lib.Help.Debug := True;

                  when 'i' =>
                     Ada_Lib.Interrupt.Debug := True;

                  when 'I' =>
                     Ada_Lib.Options.Debug := True;

                  when 'l' =>
                     Ada_Lib.Lock.Debug := True;

                  when 'm' =>
                     Ada_Lib.Timer.Set_Trace (True);

                  when 'M' =>
                     Ada_Lib.EMail.Debug := True;
                     Ada_Lib.Mail.Debug := True;

                  when 'o' =>
                     Ada_Lib.OS.Trace := True;

                  when 'O' =>
                     Debug := True;

                  when 'p' =>
                     Ada_Lib.Parser.Debug := True;

                  when 'P' =>
                     Ada_Lib.Database.Trace_Get_Post := True;

                  when 'r' =>
                     Ada_Lib.OS.Run.Debug := True;
                     Ada_Lib.Database.Connection.Debug := True;

                  when 'R' =>
                     Ada_Lib.Options.Runstring.Debug := True;

                  when 's' =>
                     Ada_Lib.Socket_IO.Trace := True;

                  when 'S' =>
                     Ada_Lib.Socket_IO.Tracing := True;

                  when 't' =>
                     Ada_Lib_Trace_Trace := True;

                  when 'T' =>
                     Ada_Lib.Trace_Tasks.Debug := True;

                  when Ada_Lib.Help.Modifier =>
                     Extended := True;

                  when others =>
                     Options.Bad_Option (Quote ("unexpected Ada_Lib trace option",
                        Trace));

               end case;

            when True =>

               case Trace is

                  when 'c' =>
                     Ada_Lib.Template.Trace_Compile := True;

                  when 'd' =>
                     Ada_Lib.Trace.Detail := True;

                  when 'D' =>
                     Ada_Lib.Directory.Debug := True;

                  when 'e' =>
                     Ada_Lib.Template.Trace_Evaluate := True;

                  when 'E' =>
                     Ada_Lib.Template.Trace_Expand := True;

                  when 'l' =>
                     Ada_Lib.Template.Trace_Load := True;

                  when 'o' =>
                     Trace_Options := True;

                  when 's' =>
                     Ada_Lib.Strings.Debug := True;

                  when 't' =>
                     Ada_Lib.Text.Debug := True;

                  when others =>
                     Options.Bad_Option (Quote ("unexpected Ada_Lib trace option",
                        Trace));    -- aborts program

               end case;
               Extended := False;

         end case;
      end loop;
      Log_Out (Debug or Ada_Lib_Trace_Trace or Trace_Options);
   end Trace_Parse;

      ---------------------------------------------------------------
      overriding
      function Verify_Initialized (
         Options     : in     Program_Options_Type;
         From        : in     String := GNAT.Source_Info.Source_Location
      ) return Boolean is
      ---------------------------------------------------------------

         Verify_Initialized   : constant Boolean := Verification_Options_Type (
                                 Options).Verify_Initialized;

      begin
         Log_In (Debug, "Verify_Initialized " & Verify_Initialized'img &
            " Processed " & Options.Processed'img);
         if Verify_Initialized then
            if Options.Processed then
               declare
                  Message  : constant String :=
                              "Options.Processed befor inialization called from " & From;
               begin
                  Log_Here (Message);
                  Put_Line (Message);
               end;
            else
               return Log_Out (True, Debug);
            end if;
         end if;

         return Log_Out (False, Debug);
      end Verify_Initialized;

   ---------------------------------------------------------------
   function Verify_Postprocess (
      Options                    : in     Program_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, "options tag " &
         Tag_Name (Program_Options_Type'class (Options)'tag) & " called from " & From);
      if Options.Processed then
            return Log_Out (True, Debug or Trace_Options);
      else
         Put_Line ("not Options.Processed called from " & From);
      end if;
      Put_Line (Who & " " & Here);
      return Log_Out (False, Debug or Trace_Options);

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         return False;

   end Verify_Postprocess;

   ---------------------------------------------------------------
   overriding
   function Verify_Preinitialize (
      Options                    : in     Program_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, "called from " & From);
      if Verification_Options_Type (
            Options).Verify_Preinitialize then
         if Options.Processed then
            Put_Line ("Options.Processed");
         else
            return Log_Out (True, Debug or Trace_Options);
         end if;
      else
         Put_Line ("Ada_Lib_Options not initialized at " & Here & " called from " & From);
      end if;
      Put_Line (Who & " " & Here);
      return Log_Out (False, Debug);

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         return False;

   end Verify_Preinitialize;

   ---------------------------------------------------------------
   function Verify_Preprocess (
      Options                    : in     Program_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options,  "called from " & From);
      if Options.Verify_Initialized then
         if Options.Processed then
            Put_Line ("Options.Processed already called " & " called from " & From);
         else
            return Log_Out (True, Debug or Trace_Options);
         end if;
      else
         Put_Line ("Ada_Lib_Options not initialized at " & Here & " called from " & From);
      end if;
      return Log_Out (False, Debug or Trace_Options);
   end Verify_Preprocess;

   ----------------------------------------------------------------------------
   overriding
   procedure Update_Filter (
      Options                    : in out Abstract_Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_Here ("options tag " & Tag_Name (
         Abstract_Options_Type'class (Options)'tag));
      Not_Implemented;
   end Update_Filter;

   ----------------------------------------------------------------

-- package body Verification_Package is

      ---------------------------------------------------------------
      overriding
      function Initialize (
         Options                 : in out Verification_Options_Type;
         From                    : in     String := Standard.Ada_Lib.Trace.Here
      ) return Boolean is
      ---------------------------------------------------------------

      begin
         Log_In_Checked (Options.Initialized, Debug or Trace_Options,
            "options address " &
            Image (Options'address) &" options tag " &
            Tag_Name (Verification_Options_Type'class (Options)'tag));
         if Debug or Trace_Options then
            Tag_History (Verification_Options_Type'class (Options)'tag);
         end if;
         Options.Initialized := True;
         return Log_Out_Checked (Options.Initialized, True,
            Debug or Trace_Options);
      end Initialize;

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
         Log_In (Debug or Trace_Options, "options tag " &
            Tag_Name (Verification_Options_Type'class (Options)'tag));

         if Modifiable_Program_Options = Null then
            Failed ("Modifiable_Program_Options not initialized at " & Here);
         else
            if Options.Initialized then
               return Log_Out (True, Debug or Trace_Options);
            else
               Failed ("Options.Initialized not initialized at " & Here);
            end if;
         end if;

         if Debug or Trace_Options then
            Tag_History (Verification_Options_Type'class (Options)'tag);
         end if;
         return Log_Out (False, Debug or Trace_Options);
      end Verify_Initialized;

      ---------------------------------------------------------------
      overriding
      function Verify_Preinitialize (
         Options                    : in     Verification_Options_Type;
         From                       : in     String := GNAT.Source_Info.Source_Location
      ) return Boolean is
      ---------------------------------------------------------------

      begin
         Log_In (Debug or Trace_Options, "options tag " &
            Tag_Name (Verification_Options_Type'class (Options)'tag) &
            " Get_Ada_Lib_Read_Only_Program_Options " & Image (Get_Ada_Lib_Read_Only_Program_Options.all'address));
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

-- end Verification_Package;

begin
--Debug := True;
--Trace_Options := True;
--Elaborate := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Ada_Lib.Options.Actual;

