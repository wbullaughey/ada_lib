with Ada.Command_Line;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Command_Line_Iterator.Tests;
with Ada_Lib.Configuration.Tests;
with Ada_Lib.Database.Server.Tests;
with Ada_Lib.Directory.Test;
with Ada_Lib.Help.Tests;
with Ada_Lib.Lock.Tests;
with Ada_Lib.Mail.Tests;
with Ada_Lib.Help;
--with Ada_Lib.Options; -- .Actual;
with Ada_Lib.Options.Runstring;
--with Ada_Lib.Options.GNOGA;
--with Ada_Lib.Options.Template;
with Ada_Lib.Socket_IO.Client.Unit_Test;
with Ada_Lib.Socket_IO.Stream_IO.Unit_Test;
--with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Template;
--with Ada_Lib.Test;
with Ada_Lib.Timer.Tests;
with Ada_Lib.Trace.Tests; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test; --.GNOGA;
--with AUnit.Ada_Lib.Options;
with Debug_Options;

--pragma Elaborate_All (Ada_Lib.Command_Line_Iterator);

package body Ada_Lib.Options.AUnit_Lib is

   Trace_Option                  : constant Character := 't';
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             Trace_Option, Unmodified);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Null_Options;

   -------------------------------------------------------------------------
   function Has_Database return Boolean is
   -------------------------------------------------------------------------

      Options           : Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type'class
                           renames Ada_Lib.Options.AUnit_Lib.
                              Aunit_Options_Constant_Class_Access (
                                 Ada_Lib.Options.Get_Ada_Lib_Read_Only_Options).all;
   begin
      Log_Here (Debug or Trace_Options);
      return Options.Database_Options.Has_Database;
   end Has_Database;

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Aunit_Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters, Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters, Options_Without_Parameters);
      return Log_Out (
--       Options.AUnit_Options.Initialize and then
         Options.Database_Options.Initialize and then
         Options.GNOGA_Unit_Test_Options.Initialize and then
         Options.Template.Initialize and then
--       Options.Initialize and then
         Ada_Lib.Options.Unit_Test.Ada_Lib_Unit_Test_Program_Options_Type (
            Options).Initialize and then
         Options.Process (
            Include_Options      => True,
            Include_Non_Options  => False,
            Modifiers            => String'(
               1 => Ada_Lib.Help.Modifier)),
         Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   function New_Suite return DBDamon_Test_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug);
      return new DBDamon_Test_Suite;
   end New_Suite;

   ----------------------------------------------------------------------------
   function New_Suite return Non_DBDamon_Test_Access is
   ----------------------------------------------------------------------------


   begin
      Log_Here (Debug);
      return new Non_DBDamon_Test_Suite;
   end New_Suite;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options                    : in out Aunit_Program_Options_Type;
      Iterator                   : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      Has_Option                 : constant Boolean :=
                                    Ada_Lib.Options.Has_Option (Option,
                                       Options_With_Parameters,
                                       Options_Without_Parameters);
   begin
      Log_In (Trace_Options or Debug, Option.Image &
         " has options " & Has_Option'img &
         " Options address " & Image (Options'address));

      if Has_Option then
         if Option.Modified then
            return Log_Out (False, Trace_Options or Debug);
         end if;

         case Option.Option is

            when 'A' => -- ada_lib trace options
                Options.Trace_Parse (Iterator);

            when Trace_Option =>    -- t
               Options.Trace_Parse (Iterator);

            when Others =>
               Log_Exception (Trace_Options or Debug, " other option" & Option.Image);
               raise Failed with "Has_Option incorrectly passed " & Option.Image;
         end case;

         return Log_Out (True, Trace_Options or Debug);

      else
         return Log_Out(
--          Options.AUnit_Options.Process_Option (Iterator, Option) or else
            Options.Database_Options.Process_Option (Iterator, Option) or else
            Options.GNOGA_Unit_Test_Options.Process_Option (Iterator, Option) or else
            Options.Template.Process_Option (Iterator, Option) or else
--          Options.Unit_Test.Process_Option (Iterator, Option) or else
            Ada_Lib.Options.Unit_Test.Ada_Lib_Unit_Test_Program_Options_Type (
               Options).Process_Option (Iterator, Option),
            Trace_Options or Debug, Option.Image & " processed");
      end if;
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in      Aunit_Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, "mode " & Help_Mode'img);
      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Help.Add_Option ('t', "", "ada_lib unit tests");

      when Ada_Lib.Options.Traces =>
         Put_Line (Ada.Command_Line.Command_Name & " trace options (-" &
            Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      c               configuration");
         Put_Line ("      C               command line iterator");
         Put_Line ("      d               directory compare and copy");
         Put_Line ("      h               help test");
         Put_Line ("      i               Socket_IO.Clent trace");
         Put_Line ("      l               Lock Test");
         Put_Line ("      m               Mail Test");
         Put_Line ("      o               Ada_Lib.Options.AUnit_Lib options");
         Put_Line ("      r               suites");
         Put_Line ("      R               Test program trace");
         Put_Line ("      s               Socket Stream Test");
         Put_Line ("      S               Database server Test");
         Put_Line ("      t               Template Test");
         Put_Line ("      T               Timer Test");
         Put_Line ("      @d              Debug Test");
         Put_Line ("      @T              Debug Tests");
         Put_Line ("      @t              Debug Test routines");
         New_Line;

      end case;
--    Options.AUnit_Options.Program_Help (Help_Mode);
      Options.Database_Options.Program_Help (Help_Mode);
      Options.GNOGA_Unit_Test_Options.Program_Help (Help_Mode);
      Options.Template.Program_Help (Help_Mode);
--    Options.Unit_Test.Program_Help (Help_Mode);
      Ada_Lib.Options.Unit_Test.Ada_Lib_Unit_Test_Program_Options_Type (
         Options).Program_Help (Help_Mode);
      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   procedure Register_Tests (
      Options                    : in     Aunit_Program_Options_Type;
      Suite_Name                 : in     String;
      Test                       : in out Ada_Lib.Unit_Test.Tests.
                                             Test_Case_Type'class) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test.Register_Tests;
      Log_Out (Debug);
   end Register_Tests;

-- ----------------------------------------------------------------------------
-- procedure Set_Options is
-- ----------------------------------------------------------------------------
--
-- begin
--    Log_Here (Debug, Tag_Name (Aunit_Program_Options_Type'class (Protected_Options)'tag));
--
--    Ada_Lib.Options.Set_Ada_Lib_Options (
--       Protected_Options'access);
-- end Set_Options;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options     : in out Aunit_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.
                              Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;
      Extended                   : Boolean := False;

   begin
      Log (Trace_Options or Debug, Here, Who & Quote (" Parameter", Parameter));
      for Index in Parameter'range  loop
         declare
            Trace    : constant Character := Parameter (Index);

         begin
            Log_Here (Trace_Options or Debug, Quote ("trace", Trace) &
               " extended " & Extended'img);
            case Extended is

               when False =>
                  case Trace is

                     when 'a' =>
                        Debug := True;
                        Ada_Lib.Command_Line_Iterator.Tests.Debug := True;
                        Ada_Lib.Configuration.Tests.Debug := True;
                        Ada_Lib.Database.Server.Tests.Debug := True;
                        Ada_Lib.Help.Tests.Debug := True;
                        Ada_Lib.Lock.Tests.Debug := True;
                        Ada_Lib.Mail.Tests.Debug := True;
                        Ada_Lib.Socket_IO.Client.Unit_Test.Debug := True;
                        Ada_Lib.Socket_IO.Stream_IO.Unit_Test.Debug := True;
                        Ada_Lib.Template.Trace_Compile := True;
                        Ada_Lib.Template.Trace_Evaluate := True;
                        Ada_Lib.Template.Trace_Expand := True;
                        Ada_Lib.Template.Trace_Load := True;
                        Ada_Lib.Template.Trace_Test := True;
                        Ada_Lib.Timer.Tests.Debug := True;
                        Ada_Lib.Trace.Tests.Debug := True;
                        Ada_Lib.Trace.Tests.Debug_Test := True;
                        Ada_Lib.Trace.Tests.Debug_Tests := True;
                        Options.Tester_Debug := True;

                     when 'c' =>
                        Ada_Lib.Configuration.Tests.Debug := True;

                     when 'C' =>
                        Ada_Lib.Command_Line_Iterator.Tests.Debug := True;

                     when 'd' =>
                        Ada_Lib.Directory.Test.Debug := True;

                     when 'h' =>
                        Ada_Lib.Help.Tests.Debug := True;

                     when 'i' =>
                        Ada_Lib.Socket_IO.Client.Unit_Test.Debug := True;

                     when 'l' =>
                        Ada_Lib.Lock.Tests.Debug := True;

                     when 'm' =>
                        Ada_Lib.Mail.Tests.Debug := True;

                     when 'o' =>
                        Debug := True;

                     when 'r' =>
                        Debug := True;

                     when 'R' =>
                        Options.Tester_Debug := True;

                     when 's' =>
                        Ada_Lib.Socket_IO.Stream_IO.Unit_Test.Debug := True;

                     when 'S' =>
                        Ada_Lib.Database.Server.Tests.Debug := True;

                     when 't' =>
                        Ada_Lib.Template.Trace_Test := True;

                     when 'T' =>
                        Ada_Lib.Timer.Tests.Debug := True;

      --             when 'u' =>
      --                Ada_Lib.Unit_Test.Debug := True;

                     when '@' =>
                        Extended := True;

                     when others =>
                        Options.Bad_Trace_Option (Trace_Option, Trace);

                  end case;

               when True =>
                  case Trace is

                     when 'd' =>
                        Ada_Lib.Trace.Tests.Debug_Test := True;

                     when 't' =>
                        Ada_Lib.Trace.Tests.Debug := True;

                     when 'T' =>
                        Ada_Lib.Trace.Tests.Debug_Tests := True;

                     when others =>
                        Options.Bad_Trace_Option (Trace_Option, Trace);

                  end case;
                  Extended := False;

            end case;
         end;
      end loop;
   end Trace_Parse;

begin
-- AUnit_Lib_Options := Protected_Options'access;
-- Elaborate := True;
   Debug := Debug_Options.Debug_All;
--Trace_Options := True;
--debug := True;
--Protected_Options.Tester_Debug := True;
   Log_Here (Elaborate or Debug);

exception
   when Fault: others =>
      Trace_Exception (Fault);
-- Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
end Ada_Lib.Options.AUnit_Lib;

