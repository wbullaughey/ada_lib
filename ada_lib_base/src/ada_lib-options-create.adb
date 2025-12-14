--with Ada.Assertions;
--with Ada.Characters.Handling;
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
--with Ada_Lib.Strings.Unlimited;
--with Ada_Lib.Template;
--with Ada_Lib.Text;
--with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Trace_Tasks;

--pragma Elaborate_All (Ada_Lib.Lock);

package body Ada_Lib.Options.Create is

-- use type Ada_Lib.Strings.Unlimited.String_Type;
-- use type Actual.Flag_Option_Type;

   ----------------------------------------------------------------------------
   function Create_One (-- create a single option
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Flag_List_Type is
   ----------------------------------------------------------------------------

      Flag                       : constant Actual.Flag_Option_Access :=
                                    Actual.Allocate_Option (Option, Modifier, From);
      Options                    : Base_Options_Array (1 .. 1);
      Result                     : Flag_List_Type;

   begin
      Options (1) := Base_Flag_Option_Class_Access (Flag);
      Result.Create_Options (Options, From);
      return Result;
   end Create_One;

-- ----------------------------------------------------------------------------
-- function Create_Option (
--    Option                     : in     Character;
--    Modifier                   : in     Character;
--    From                       : in     String := Ada_Lib.Trace.Here
-- ) return Base_Flag_Option_Class_Access is
-- ----------------------------------------------------------------------------
--
--    Result                     : constant Base_Flag_Option_Class_Access :=
--                                  new Flag_Option_Type;
-- begin
--    Result.Create_Option (Option, Modifier, From);
--    return Result;
-- end Create_Option;

   ----------------------------------------------------------------------------
   function Create_Multiple (   -- create multiple option
      Source                     : in     String;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Flag_List_Type is
   ----------------------------------------------------------------------------

      Count                      : Natural := 0;
      Options                    : Base_Options_Array (1 .. Source'length);
      Result                     : Flag_List_Type;

   begin
      Log_In (Debug or Trace_Options, Quote ("source", Source) & (if Modifier = Unmodified_flag then
            " no modifier"
         else
            Quote (" modifier", Modifier)) &
         " from " & From);
      for Option of Source loop
         declare
            Flag                 : constant Actual.Flag_Option_Access :=
                                    Actual.Allocate_Option (Option, Modifier, From);
         begin
            Count := Count + 1;
            Options (Count) := Base_Flag_Option_Class_Access (Flag);
         end;
      end loop;

      Result.Create_Options (Options);
      Log_Out (Debug or Trace_Options, "count" & Count'img);
      return Result;
   end Create_Multiple;

-- ----------------------------------------------------------------------------
-- function Create_Options (
--    Source                     : in     String;
--    Modifier                   : in     Character;
--    From                       : in     String := Ada_Lib.Trace.Here
-- ) return Ada_Lib.Options.Options_Access is
-- ----------------------------------------------------------------------------
--
--    Options                    : constant Flag_Option_Type :=
--                                  Create_Options (Source, Modifier, From);
--    Result                     : constant Ada_Lib.Options.Options_Access :=
--                                  new Options_Type (1 .. Options'last);
-- begin
--    Result.all := Options;
--    return Result;
-- end Create_Options;


begin
--Debug := True;
--Trace_Options := True;
--Elaborate := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Ada_Lib.Options.Create;

