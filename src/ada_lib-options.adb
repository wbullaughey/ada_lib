with Ada.Characters.Handling;
--with Ada.Exceptions;
--with Ada.Strings.Maps;
--with Ada.Tags;
with Ada.Text_IO;use Ada.Text_IO;
----with Ada_Lib.Configuration;
--with Ada_Lib.Database.Connection;
--with Ada_Lib.Directory;
--with Ada_Lib.EMail;
--with Ada_Lib.Event;
--with Ada_Lib.GNOGA;
--with Ada_Lib.Help;
--with Ada_Lib.Interrupt;
--with Ada_Lib.Lock;
--with Ada_Lib.Mail;
--with Ada_Lib.Options.Runstring;
--with Ada_Lib.OS;
--with Ada_Lib.Parser;
--with Ada_Lib.OS.Run;
--with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.Strings.Unlimited;
--with Ada_Lib.Template;
--with Ada_Lib.Text;
--with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Trace_Tasks;
with Debug_Options;

--pragma Elaborate_All (Ada_Lib.Lock);
--pragma Elaborate_All (Ada_Lib.Command_Line_Iterator);

package body Ada_Lib.Options is

   use type Ada_Lib.Strings.Unlimited.String_Type;

   Modifiable_Options            : Interface_Options_Class_Access := Null;
   Parameter_Parsing_Failed      : Boolean := False;

   ----------------------------------------------------------------------------
   function Create_Option (
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Option_Type is
   ----------------------------------------------------------------------------

      Result                     : constant Option_Type := Option_Type'(
                                    Kind     => (if Modifier = Unmodified then
                                                   Plain
                                                else
                                                   Modified),
                                    Modifier => Modifier,
                                    Option   => Option);
   begin
      Log_Here (Debug or Trace_Options, Quote ("option", Option) &
         (if Modifier = Unmodified then
            " no modifier"
         else
            Quote (" modifier", Modifier)) & " " &
         Result.Image & " from " & From);

      return Result;
   end Create_Option;

   ----------------------------------------------------------------------------
   function Create_Options (
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Options_Access is
   ----------------------------------------------------------------------------

      Result                     : constant Options_Access :=
                                    New Options_Type (1 .. 1);

   begin
      Result.all := Create_Options (Option, Modifier, From);
      return Result;
   end Create_Options;

   ----------------------------------------------------------------------------
   function Create_Options (    -- create a single options
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Options_Type is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, Quote ("option", Option) & (if Modifier = Unmodified then
            " no modifier"
         else
            Quote (" modifier", Modifier) &
         " from " & From));

      return Options_Type'(
         1 => Option_Type' (
            Kind     => (if Modifier = Unmodified then
                           Plain
                        else
                           Modified),
            Modifier => Modifier,
            Option   => Option));
   end Create_Options;

   ----------------------------------------------------------------------------
   function Create_Options (
      Source                     : in     String;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Ada_Lib.Options.Options_Type is
   ----------------------------------------------------------------------------

      Count                      : Natural := 0;
      Options                    : Options_Type (1 .. 100);

   begin
      Log_In (Debug or Trace_Options, Quote ("source", Source) & (if Modifier = Unmodified then
            " no modifier"
         else
            Quote (" modifier", Modifier)) &
         " from " & From);
      for Option of Source loop
         Count := Count + 1;
            Options (Count) := Create_Option (Option, Modifier, From);
      end loop;

      Log_Out (Debug or Trace_Options, "count" & Count'img);
      return Options (1 .. Count);
   end Create_Options;

   ----------------------------------------------------------------------------
   function Create_Options (
      Source                     : in     String;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Ada_Lib.Options.Options_Access is
   ----------------------------------------------------------------------------

      Options                    : constant Ada_Lib.Options.Options_Type :=
                                    Create_Options (Source, Modifier, From);
      Result                     : constant Ada_Lib.Options.Options_Access :=
                                    new Options_Type (1 .. Options'last);
   begin
      Result.all := Options;
      return Result;
   end Create_Options;

   ----------------------------------------------------------------
   function Get_Ada_Lib_Modifiable_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Interface_Options_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, "from " & From);
      if Debug or Trace_Options then
         Tag_History (Modifiable_Options.all'tag, From);
      end if;
      return Modifiable_Options;

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Get_Ada_Lib_Modifiable_Options;

   ----------------------------------------------------------------------------
   function Get_Ada_Lib_Read_Only_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Interface_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, "from " & From);
      return Interface_Options_Constant_Class_Access (
         Modifiable_Options);
   end Get_Ada_Lib_Read_Only_Options;

   ----------------------------------------------------------------------------
   function Has_Option (
      Option                     : in     Option_Type;
      Options_With_Parameters    : in     Options_Type;
      Options_Without_Parameters : in     Options_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, Option.Image);

      for Element of Options_With_Parameters loop
         if Element = Option then
            return Log_Out (True, Debug or Trace_Options,
               "options address " & Image (Option'address));
         end if;
      end loop;

      for Element of Options_Without_Parameters loop
         if Element = Option then
            return Log_Out (True, Debug or Trace_Options);
         end if;
      end loop;
      return Log_Out (False, Debug or Trace_Options);
   end Has_Option;

   ----------------------------------------------------------------------------
   function Have_Options return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Modifiable_Options /= Null;
   end Have_Options;

   ----------------------------------------------------------------------------
   function Image (
      Option                     : in     Option_Type;
      Quote                      : in     Boolean := True
   ) return String is
   ----------------------------------------------------------------------------

      Text                       : constant String := (case Option.Kind is
                                    when Modified => String'(
                                       Option.Modifier, Option.Option),
                                    when Plain    => String'(1 => Option.Option),
                                    when Nil_Option   => (
                                       if Quote then "" else "Null"));
   begin
      return (if Quote then
            Ada_Lib.Trace.Quote ("option", Text)
         else
            Text);
   end Image;

   ----------------------------------------------------------------------------
   function Image (
      Options                    : in     Options_Type;
      Quote                      : in     Boolean := True
   ) return String is
   ----------------------------------------------------------------------------

      Result                     : Ada_Lib.Strings.Unlimited.String_Type;

   begin
      for Option of Options loop
         Result := Result & " " & Option.Image (False);
      end loop;

      return (if Quote then
         Ada_Lib.Trace.Quote ("options", Result)
      else
         Result.Coerce);
   end Image;

   ----------------------------------------------------------------------------
   function Less (
      Left, Right                : in     Option_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

      use Ada.Characters.Handling;

      Left_Letter                : constant Character :=
                                    To_Upper (Left.Option);
      Right_Letter               : constant Character :=
                                    To_Upper (Right.Option);
      Left_Upper                 : constant Boolean :=
                                    Is_Upper (Left.Option);
      Right_Upper                : constant Boolean :=
                                    Is_Upper (Right.Option);

   begin
      return (if Left.Kind = Right.Kind then
            (if Left_Letter = Right_Letter then
               (if Left_Upper = Right_Upper then
                  True
               else
                  Right_Upper)
            else
               Left_Letter < Right_Letter)
         else
            Left.Kind < Right.Kind);
   end Less;

   ----------------------------------------------------------------------------
   function Modifiable_Options_Address
   return String is
   ----------------------------------------------------------------------------

   begin
      return "Modifiable_Options address is " &
         (if Modifiable_Options = Null then
            "null "
         else
            Image (Modifiable_Options.all'address));

   end Modifiable_Options_Address;

   ----------------------------------------------------------------------------
   function Modified (
      Option                     : in     Option_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Option.Kind = Modified;
   end Modified;

   ----------------------------------------------------------------------------
   function Modifier (
      Option                     : in     Option_Type
   ) return Character is
   ----------------------------------------------------------------------------

   begin
      return Option.Modifier;
   end Modifier;

   ----------------------------------------------------------------------------
   procedure Parsing_Failed is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options);
      Parameter_Parsing_Failed := True;
   end Parsing_Failed;

   ----------------------------------------------------------------
   function Parsing_Failed return Boolean is
   ----------------------------------------------------------------

   begin
      return Parameter_Parsing_Failed;
   end Parsing_Failed;

   ----------------------------------------------------------------
   procedure Set_Ada_Lib_Options (
      Options                    : in     Interface_Options_Class_Access) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, Tag_Name (Options.all'tag));
      if Debug or Trace_Options then
         Tag_History (Options.all'tag);
      end if;
      Modifiable_Options := Options;
      Log_Out (Debug or Trace_Options, Modifiable_Options_Address);

   end Set_Ada_Lib_Options;

   ----------------------------------------------------------------

   package body Verification_Package is

      ---------------------------------------------------------------
      overriding
      function Initialize (
         Options                 : in out Options_Type;
         From                        : in     String := Standard.Ada_Lib.Trace.Here
      ) return Boolean is
      ---------------------------------------------------------------

      begin
         Log_In_Checked (Options.Initialized, Debug or Trace_Options,
            "options address " &
            Image (Options'address) &" options tag " &
            Tag_Name (Options_Type'class (Options)'tag));
         if Debug or Trace_Options then
            Tag_History (Options_Type'class (Options)'tag);
         end if;
         Options.Initialized := True;
         return Log_Out_Checked (Options.Initialized, True,
            Debug or Trace_Options);
      end Initialize;

      ---------------------------------------------------------------
      overriding
      function Process_Argument (  -- process one argument
        Options                     : in out Options_Type;
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
         Options                    : in     Options_Type;
         From                       : in     String := GNAT.Source_Info.Source_Location
      ) return Boolean is
      ---------------------------------------------------------------

      begin
         Log_In (Debug or Trace_Options, "options tag " &
            Tag_Name (Options_Type'class (Options)'tag) &
            Modifiable_Options_Address);

         if Modifiable_Options = Null then
            Put_Line ("Modifiable_Options not initialized at " & Here &
            " called from " & From);
         else
            if not Options.Initialized then
               Put_Line ("Options.Initialized not initialized at " & Here &
                  " called from " & From);
            else
               return Log_Out (True, Debug or Trace_Options);
            end if;
         end if;

         if Debug or Trace_Options then
            Tag_History (Options_Type'class (Options)'tag);
         end if;
         return Log_Out (False, Debug or Trace_Options);
      end Verify_Initialized;

      ---------------------------------------------------------------
      overriding
      function Verify_Preinitialize (
         Options                    : in     Options_Type;
         From                       : in     String := GNAT.Source_Info.Source_Location
      ) return Boolean is
      ---------------------------------------------------------------

      begin
         Log_In (Debug or Trace_Options, "options tag " &
            Tag_Name (Options_Type'class (Options)'tag) &
            " Get_Ada_Lib_Read_Only_Options " & Image (Get_Ada_Lib_Read_Only_Options.all'address));
         if Debug or Trace_Options then
            Tag_History (Options_Type'class (Options)'tag);
         end if;

         if Get_Ada_Lib_Read_Only_Options = Null then
            Put_Line ("Get_Ada_Lib_Read_Only_Options null " & Here);
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

   end Verification_Package;

begin
     Debug := Debug or Debug_Options.Debug_All;
--Debug := true;
--Elaborate := True;
--Trace_Options := True;

   Indent_Trace := True;
--log_here ("Indent_Trace address " & Image (Indent_Trace'address));
   Log_Here (Debug or Elaborate or Trace_Options);

--declare
--options : aliased Options_Type;
--begin
--Log_Here;
--Program_Options_Package.set_ada_lib_options (options'unchecked_access);
--if not options.verify_preinitialize then
--put_line ("verify_preinitialize failed");
--end if;
--Log_Here (if options.initialize then "initialized" else "failed");
--if not options.verify_initialized then
--put_line ("verify_initialize failed");
--end if;
--if not options.Verify_Preprocess then
--put_line ("Verify_Preprocess failed");
--end if;
--if not options.process (
--Include_Options      => True,
--Include_Non_Options  => False) then
--put_line ("process failed");
--end if;
--Log_Here;
--if not options.Verify_Postprocess then
--put_line ("Verify_Postprocess failed");
--end if;
--Log_Here;
--
--exception
--when Fault: others =>
--Trace_Exception (Fault);
--end;
end Ada_Lib.Options;
