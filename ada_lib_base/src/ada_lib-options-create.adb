--with Ada.Assertions;
with Ada.Characters.Handling;
--with Ada.Exceptions;
--with Ada.Strings.Maps;
--with Ada.Tags;
--with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Command_Line_Iterator;
--with Ada_Lib.Configuration;
--with Ada_Lib.Database.Connection;
--with Ada_Lib.Directory;
--with Ada_Lib.EMail;
--with Ada_Lib.Event;
--with Ada_Lib.Help;
--with Ada_Lib.Interrupt;
--with Ada_Lib.Lock;
--with Ada_Lib.Mail;
with Ada_Lib.Options.Actual;
--with Ada_Lib.Options.Runstring;
--with Ada_Lib.OS;
--with Ada_Lib.Parser;
--with Ada_Lib.OS.Run;
--with Ada_Lib.Socket_IO;
with Ada_Lib.Strings.Unlimited;
--with Ada_Lib.Template;
--with Ada_Lib.Text;
--with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Trace_Tasks;

--pragma Elaborate_All (Ada_Lib.Lock);

package body Ada_Lib.Options.Create is

   use type Ada_Lib.Strings.Unlimited.String_Type;
-- use type Root_Option_Type;

   Parameter_Parsing_Failed      : Boolean := False;

--   ----------------------------------------------------------------------------
---- overriding
--   function Create_Option (
--      Option                     : in     Character;
--      Modifier                   : in     Character;
--      From                       : in     String := Ada_Lib.Trace.Here
--   ) return Root_Option_Type is
--   ----------------------------------------------------------------------------
--
--      Result                     : constant Root_Option_Type := Root_Option_Type'(
--                                    Kind     => (if Modifier = Unmodified then
--                                                   Plain
--                                                else
--                                                   Modified),
--                                    Modifier => Modifier,
--                                    Option   => Option);
--   begin
--      Log_Here (Debug or Trace_Options, Quote ("option", Option) &
--         (if Modifier = Unmodified then
--            " no modifier"
--         else
--            Quote (" modifier", Modifier)) & " " &
--         Result.Image & " from " & From);
--
--      return Result;
--   end Create_Option;

   ----------------------------------------------------------------------------
   function Create_Options (
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access is
   ----------------------------------------------------------------------------

      Result                     : constant Root_Option_Class_Access :=
                                    New Root_Options_Type (1 .. 1);

   begin
      Result.all := Create_Options (Option, Modifier, From);
      return Result;
   end Create_Options;

   ----------------------------------------------------------------------------
   function Create_Options (    -- create a single options
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options, Quote ("option", Option) & (if Modifier = Unmodified then
            " no modifier"
         else
            Quote (" modifier", Modifier) &
         " from " & From));

      return Options_Type'(
         1 =>  Ada_Lib.Options.Actual.Create_Option (
            Modifier => Modifier,
            Option   => Option));
   end Create_Options;

   ----------------------------------------------------------------------------
   function Create_Options (
      Source                     : in     String;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access is
   ----------------------------------------------------------------------------

      Count                      : Natural := 0;
      Options                    : Options_Type (1 .. 100);

   begin
      Log_In (Debug or Trace_Options, Quote ("source", Source) & (
         if Modifier = Unmodified then
            " no modifier"
         else
            Quote (" modifier", Modifier)) &
         " from " & From);
      for Option of Source loop
         Count := Count + 1;
            Options (Count) := Ada_Lib.Options.Actual.Create_Option (
               Option, Modifier, From);
      end loop;

      Log_Out (Debug or Trace_Options, "count" & Count'img);
      return Options (1 .. Count);
   end Create_Options;

   ----------------------------------------------------------------------------
   function Create_Options (
      Source                     : in     String;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access is
   ----------------------------------------------------------------------------

      Options                    : constant Options_Type :=
                                    Create_Options (Source, Modifier, From);
      Result                     : constant Root_Option_Class_Access :=
                                    new Options_Type (1 .. Options'last);
   begin
      Result.all := Options;
      return Result;
   end Create_Options;

   ----------------------------------------------------------------------------
   function Has_Option (
      Option                     : in     Root_Option_Type'class;
      Options_With_Parameters    : in     Options_Type;
      Options_Without_Parameters : in     Options_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, Option.Image &
         " Options_With_Parameters length " &
            Options_With_Parameters'length'img &
         " Options_Without_Parameters length " &
            Options_Without_Parameters'length'img);

      for Element of Options_With_Parameters loop
         if Element.all = Option then
            return Log_Out (True, Debug or Trace_Options,
               "options address " & Image (Option'address));
         end if;
      end loop;

      for Element of Options_Without_Parameters loop
         if Element.all = Option then
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
      Option                     : in     Root_Option_Type;
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
      Left, Right                : in     Root_Option_Type
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
      Option                     : in     Root_Option_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Option.Kind = Modified;
   end Modified;

   ----------------------------------------------------------------------------
   function Modifier (
      Option                     : in     Root_Option_Type
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
--Debug := True;
--Trace_Options := True;
--Elaborate := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Ada_Lib.Options.Create;

