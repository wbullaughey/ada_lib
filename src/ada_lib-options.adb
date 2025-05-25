with Ada.Characters.Handling;
--with Ada.Exceptions;
--with Ada.Strings.Maps;
--with Ada.Tags;
--with Ada.Text_IO;use Ada.Text_IO;
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

-- ----------------------------------------------------------------------------
-- function Have_Options return Boolean is
-- ----------------------------------------------------------------------------
--
-- begin
--    return Modifiable_Options /= Null;
-- end Have_Options;

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

-- ----------------------------------------------------------------------------
-- function Modifiable_Options_Address
-- return String is
-- ----------------------------------------------------------------------------
--
-- begin
--    return "Modifiable_Options address is " &
--       (if Modifiable_Options = Null then
--          "null "
--       else
--          Image (Modifiable_Options.all'address));
--
-- end Modifiable_Options_Address;

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

begin
--Debug := true;
--Trace_Options := True;
--Elaborate := True;

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
