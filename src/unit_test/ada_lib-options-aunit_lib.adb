--with Ada.Command_Line;
with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Command_Line_Iterator.Tests;
with Ada_Lib.Configuration.Tests;
with Ada_Lib.Database.Server.Tests;
with Ada_Lib.Directory.Test;
with Ada_Lib.Help;
with Ada_Lib.Lock.Tests;
with Ada_Lib.Mail.Tests;
--with Ada_Lib.Options.Flags;
with Ada_Lib.Options.Create;
with Ada_Lib.Options.Runstring;
--with Ada_Lib.Options.Unit_Test;
--with GNOGA_Options;
--with Ada_Lib.Options.Template;
with Ada_Lib.Socket_IO.Client.Unit_Test;
with Ada_Lib.Socket_IO.Stream_IO.Unit_Test;
with Ada_Lib.Strings;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
--with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Template;
--with Ada_Lib.Unit_Test;
--with Ada_Lib.Timer.Tests;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test; --.GNOGA;
--with AUnit.Ada_Lib.Options;
--with Command_Name;
--with Debug_Options;
with GNOGA_Ada_Lib;

--pragma Elaborate_All (Ada_Lib.Command_Line_Iterator);

-- tests for the Ada_Lib unit tests
package body Ada_Lib.Options.AUnit_Lib is

   Debug          : Boolean renames AUnit.Debug;
   Trace_Option                  : constant Character := 't';
   Options_With_Parameters       : aliased constant
                                    Flag_List_Type :=
                                       Create.Create_One (
                                          Trace_Option, Unmodified_flag);
   Options_Without_Parameters    : aliased constant
                                    Flag_List_Type :=
                                       Create.Create_Multiple (
                                          "dtT", Ada_Lib.Help.Trace_Modifier);
   Trace_Modifier             : character renames Ada_Lib.Help.Trace_Modifier;

   -------------------------------------------------------------------------
   function Has_Database return Boolean is
   -------------------------------------------------------------------------

      Options  : AUnit_Lib.Aunit_Program_Options_Type'class
                  renames AUnit_Lib.
                     Aunit_Options_Constant_Class_Access (
                        Get_Ada_Lib_Read_Only_Program_Options).all;
      Options_Selection
               : AUnit_Lib.Options_Selection_Type renames
                  Options.Options_Selection;
      Result   : constant Boolean := (case Options_Selection is

            when Ada_Lib_Unit_Test_Only =>
               False,

            when Ada_Lib_Unit_Test_With_Database =>
               Options.Database_Options.Has_Database,

            when Not_Ada_Lib_Unit_Test =>
               False,

            when With_Database_Only =>
               Options.Database_Only.Has_Database);
   begin
      return Log_Here (Result,
         Debug or Trace_Options or Trace_Pre_Post_Conditions or not Result,
         "Options_Selection " & Options.Options_Selection'img);
   end Has_Database;

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Aunit_Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options,
         "Options_Selection " & Options.Options_Selection'img);

      Runstring.Options.Register (
         Runstring.With_Parameters, Options_With_Parameters);
      Runstring.Options.Register (
         Runstring.Without_Parameters, Options_Without_Parameters);

      return Log_Out (
         (case Options.Options_Selection is

            when Ada_Lib_Unit_Test_Only =>
               Options.Template_Only.Initialize,

            when Not_Ada_Lib_Unit_Test =>
               True,

            when Ada_Lib_Unit_Test_With_Database =>
               Options.Database_Options.Initialize and then
               Options.Template.Initialize,

            when With_Database_Only =>
               Options.Database_Only.Initialize

         ) and then
         Options.GNOGA_Unit_Test_Options.Initialize and then
         Unit_Test.Ada_Lib_Unit_Test_Program_Options_Type (
            Options).Initialize,
         Debug or Trace_Options,
            "Options_Selection " & Options.Options_Selection'img);
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
      Iterator                   : in out Command_Line_Iterator_Interface'class;
      Option                     : in     Base_Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      Has_It                     : constant Boolean :=
                                    Has_Option (Option,
                                       Options_With_Parameters,
                                       Options_Without_Parameters);
   begin
      Log_In (Trace_Options or Debug, Option.Image &
         " has options " & Has_It'img &
         " option selection " & Options.Options_Selection'img &
         " Options address " & Ada_Lib.Strings.Image (Options'address));

      if Has_It then
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
         return Log_Out (
            (case Options.Options_Selection is

               when Ada_Lib_Unit_Test_Only =>
                  Options.Template_Only.Process_Option (Iterator, Option),

               when Not_Ada_Lib_Unit_Test =>
                  False,

               when Ada_Lib_Unit_Test_With_Database =>
                  Options.Database_Options.Process_Option (
                     Iterator, Option) or else
                  Options.Template.Process_Option (Iterator, Option),

               when With_Database_Only =>
                  Options.Database_Only.Process_Option (Iterator, Option)

            ) or else
            Options.GNOGA_Unit_Test_Options.Process_Option (Iterator, Option) or else
            Unit_Test.Ada_Lib_Unit_Test_Program_Options_Type (
               Options).Process_Option (Iterator, Option),
            Trace_Options or Debug, Option.Image & " processed");
      end if;
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in      Aunit_Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Ada_Lib Unit Test";

   begin
      Log_In (Debug or Trace_Options, "mode " & Help_Mode'img);
      case Help_Mode is

      when Program_Mode =>
         Ada_Lib.Help.Create_Option (Trace_Option, "trace options",
            "ada_lib trace options", Component, Ada_Lib.Help.Unmodified_Flag);

      when Trace_Mode =>
         Put_Line (Ada_Lib.Trace.Who & " trace options (-" &
            Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      A               AUnit debug");
         Put_Line ("      c               configuration");
         Put_Line ("      C               command line iterator");
         Put_Line ("      d               directory compare and copy");
         Put_Line ("      h               help test");
         Put_Line ("      i               Socket_IO.Clent trace");
         Put_Line ("      l               Lock Test");
         Put_Line ("      m               Mail Test");
         Put_Line ("      o               Ada_Lib.Options.AUnit_Lib options");
--       Put_Line ("      r               suites");
         Put_Line ("      R               Test program trace");
         Put_Line ("      s               Socket Stream Test");
         Put_Line ("      S               Database server Test");
         Put_Line ("      t               Template Test");
         Put_Line ("      T               Timer Test");
         Put_Line ("      " & Trace_Modifier &
                          "c              Camera Commands Unit Test");
         Put_Line ("      " & Trace_Modifier &
                          "d              Debug Test");
         Put_Line ("      " & Trace_Modifier &
                          "T              Debug Tests");
         Put_Line ("      " & Trace_Modifier &
                          "t              Debug Test routines");
         New_Line;

      end case;
--    Options.AUnit_Options.Program_Help (Help_Mode);
      case Options.Options_Selection is

         when Ada_Lib_Unit_Test_Only =>
            Options.Template_Only.Program_Help (Help_Mode);

         when Not_Ada_Lib_Unit_Test =>
            null;

         when Ada_Lib_Unit_Test_With_Database =>
            Options.Database_Options.Program_Help (Help_Mode);
            Options.Template.Program_Help (Help_Mode);

         when With_Database_Only =>
            Options.Database_Only.Program_Help (Help_Mode);

      end case;
      Options.GNOGA_Unit_Test_Options.Program_Help (Help_Mode);
      GNOGA_Ada_Lib.Program_Help (Help_Mode);
      Unit_Test.Ada_Lib_Unit_Test_Program_Options_Type (
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
      Iterator    : in out Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Trace_Tests_Debug       : Boolean renames
                                 Unit_Test.Ada_Lib_Options_Trace_Tests.Debug;
      Trace_Tests_Debug_Test  : Boolean renames
                                 Unit_Test.Ada_Lib_Options_Trace_Tests.Debug_Test;
      Trace_Tests_Debug_Tests : Boolean renames
                                 Unit_Test.Ada_Lib_Options_Trace_Tests.Debug_Tests;
      Extended                : Boolean := False;
      Parameter               : constant String := Iterator.Get_Parameter;

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
                        Ada_Lib_Command_Line_Iterator.Tests_Debug := True;
                        Ada_Lib.Configuration.Tests.Debug := True;
                        Ada_Lib.Database.Server.Tests.Debug := True;
                        Unit_Test.Ada_Lib_Help_Unit_Test.Debug := True;
                        Ada_Lib.Lock.Tests.Debug := True;
                        Ada_Lib.Mail.Tests.Debug := True;
                        Ada_Lib.Socket_IO.Client.Unit_Test.Debug := True;
                        Ada_Lib.Socket_IO.Stream_IO.Unit_Test.Debug := True;
                        Ada_Lib.Template.Trace_Compile := True;
                        Ada_Lib.Template.Trace_Evaluate := True;
                        Ada_Lib.Template.Trace_Expand := True;
                        Ada_Lib.Template.Trace_Load := True;
                        Ada_Lib.Template.Trace_Test := True;
                        Trace_Tests_Debug := True;
                        Trace_Tests_Debug_Test := True;
                        Trace_Tests_Debug_Tests := True;
                        Debug := True;
                        Options.Tester_Debug := True;

                     when 'A' =>
                        AUnit.Debug := True;

                     when 'c' =>
                        Ada_Lib.Configuration.Tests.Debug := True;

                     when 'C' =>
                        Ada_Lib_Command_Line_Iterator.Tests_Debug := True;

                     when 'd' =>
                        Ada_Lib.Directory.Test.Debug := True;

                     when 'h' =>
                        Unit_Test.Ada_Lib_Help_Unit_Test.Debug := True;

                     when 'i' =>
                        Ada_Lib.Socket_IO.Client.Unit_Test.Debug := True;

                     when 'l' =>
                        Ada_Lib.Lock.Tests.Debug := True;

                     when 'm' =>
                        Ada_Lib.Mail.Tests.Debug := True;

                     when 'o' =>
                        Debug := True;

--                   when 'r' =>
--                      Debug := True;

                     when 'R' =>
                        Options.Tester_Debug := True;

                     when 's' =>
                        Ada_Lib.Socket_IO.Stream_IO.Unit_Test.Debug := True;

                     when 'S' =>
                        Ada_Lib.Database.Server.Tests.Debug := True;

                     when 't' =>
                        Ada_Lib.Template.Trace_Test := True;

                     when 'T' =>
                        Trace_Tests_Debug := True;

      --             when 'u' =>
      --                Ada_Lib.Unit_Test.Debug := True;

                     when Trace_Modifier =>
                        Extended := True;

                     when others =>
                        Options.Bad_Trace_Option (Trace_Option, Trace);

                  end case;

               when True =>
                  case Trace is

                     when 'd' =>
                        Trace_Tests_Debug_Test := True;

                     when 't' =>
                        Trace_Tests_Debug := True;

                     when 'T' =>
                        Trace_Tests_Debug_Tests := True;

                     when others =>
                        Options.Bad_Trace_Option (Trace_Option, Trace,
                           Ada_Lib.Help.Trace_Modifier);

                  end case;
                  Extended := False;

            end case;
         end;
      end loop;
--    GNOGA_Iterator.Trace_Parse (Iterator);
   end Trace_Parse;

begin
-- AUnit_Lib_Options := Protected_Options'access;
-- Elaborate := True;
   Debug := Debug or Debug_All;
--Trace_Options := True;
--debug := True;
--Protected_Options.Tester_Debug := True;
   Log_Here (Elaborate or Trace_Options or Debug);

exception
   when Fault: others =>
      Trace_Exception (Fault);
-- Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
end Ada_Lib.Options.AUnit_Lib;

