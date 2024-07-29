with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.GNOGA.Unit_Test;
with Ada_Lib.Help;
--with Ada_Lib.Options.GNOGA;
with Ada_Lib.OS;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test;
with Debug_Options;

package body Ada_Lib.Options.Unit_Test is

-- use type Ada_Lib.Options.Options_Type;
-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;

   Driver_List_Option            : constant Character := 'd';

   Options_With_Parameters
                                    : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             "esSU") &
                                          Ada_Lib.Options.Create_Options (
                                             "RS", Ada_Lib.Help.Modifier);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             "x") &
                                          Ada_Lib.Options.Create_Options (
                                             Driver_List_Option & "lmP",
                                             Ada_Lib.Help.Modifier);

   Recursed                      : Boolean := False;

   ---------------------------------------------------------------ada   -------------
   procedure Check_Test_Suite_And_Routine (
      Options                    : in     Ada_Lib_Unit_Test_Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "Multi_Test " & Options.Multi_Test'img &
            " mode " & Options.Mode'img);

      if not (Options.Multi_Test or else
            Options.Mode = Ada_Lib.Options.Run_Tests) then
         if    Options.Routine.Length = 0 or else
               Options.Suite_Name.Length = 0 then
            raise Failed with "single unit test must have both " &
               "suite name and routine name";
         end if;
      end if;
      Log_Out (Debug);
   end Check_Test_Suite_And_Routine;

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Ada_Lib_Unit_Test_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
     Log_In_Checked (Recursed, Debug or Trace_Options);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
            Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);
--    Unit_Test_Options := Options'unchecked_access;

      return Log_Out_Checked (Recursed,
         Options.GNOGA_Options.Initialize and then
         Actual.Program_Options_Type (Options).Initialize,
         Debug or Trace_Options);
   end Initialize;

-- ----------------------------------------------------------------------------
-- function Options (
--    From                       : in     String :=
--                                           Standard.GNAT.Source_Info.Source_Location
-- ) return Unit_Test_Options_Constant_Class_Access is
-- ----------------------------------------------------------------------------
--
-- begin
--    if Ada_Lib.Options.Get_Read_Only_Options = Null then
--       raise Failed with "Get_Read_Only_Options not set called from " & From;
--    end if;
--
--    Log_Here (Debug, "from " & From &
--       " Get_Read_Only_Options tag " & Tag_Name (
--          Ada_Lib.Options.Get_Read_Only_Options.all'tag));
--
--    return Unit_Test_Options_Constant_Class_Access (
--       Ada_Lib.Options.Get_Read_Only_Options);
-- end Options;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options                    : in out Ada_Lib_Unit_Test_Options_Type;
      Iterator                   : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      ----------------------------------------------------------------------------
      procedure Bad_Options (
         From                    : in     String := Here) is
      ----------------------------------------------------------------------------

      begin
         Options.Bad_Option ("bad options: Routine (-e) or Suite Name (-s) " &
            "cannot be combined" &
            " with List_Suites (-@l) or Driver_Suites (-@d) called from " & From);
      end Bad_Options;
      ----------------------------------------------------------------------------

--    use Standard.Ada_Lib.Options;

   begin
      Log_In_Checked (Recursed, Trace_Options or Debug, Option.Image &
         " mode " & Options.Mode'img);

      if Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Kind is

            when Plain =>

               case Option.Option is

                  when 'e' =>    -- routine name
                     if Options.Mode /= Run_Tests then
                        Bad_Options;
                     end if;

                     if Options.Routine.Length = 0 then
                        Options.Routine.Set (Iterator.Get_Parameter);
                     else
                        Options.Bad_Option ("Only one routine is allowed");
                     end if;
                     Options.Update_Filter;
                     Log_Here (Trace_Options or Debug, Quote ("filter Routine",
                        Options.Routine));

                  when 's' =>    -- suites to include
                     if Options.Mode /= Run_Tests then
                        Bad_Options;
                     end if;
                     if Options.Suite_Name.Length = 0 then
                        Options.Suite_Name.Set (Iterator.Get_Parameter);
                     else
                        Options.Bad_Option ("Only one suite is allowed");
                     end if;
                     Options.Update_Filter;
                     Log_Here (Trace_Options or Debug, Quote ("filter Suite_Name",
                        Options.Suite_Name));

                  when 'S' =>       -- enable default disabled tests
                     declare
                        Parameter: constant String := Iterator.Get_Parameter;

                     begin
                        for Letter of Parameter loop
                           case Letter is

                              when 'a' =>
                                 for Index in Options.Suite_Set'range loop
                                    Options.Suite_Set (Index) := True;
                                 end loop;

                              when 'd' =>
                                 Options.Suite_Set (Database_Server) := True;

                              when 't' =>
                                 Options.Suite_Set (Textbelt) := True;

                              when others =>
                                 Options.Bad_Option (Quote (
                                    "unexpected test suite selection", Letter));

                           end case;
                        end loop;
                     end;

                  when 'U' =>       -- trace options
                     Options.Trace_Parse (Iterator);

                  when 'x' =>
                     Options.Exit_On_Done := True;

                  when others =>
                     Log_Exception (Trace_Options or Debug, " other option" & Option.Image);
                     raise Failed with "Has_Option incorrectly passed " & Option.Image;

               end case;

            when Modified =>
               case Option.Option is

                  when 'd' =>
                     if Options.Routine.Length > 0 or else
                           Options.Suite_Name.Length > 0 then
                        Bad_Options;
                     end if;
                     Options.Mode := Driver_Suites;

                  when 'l' =>
                     if Options.Routine.Length > 0 or else
                           Options.Suite_Name.Length > 0 then
                        Bad_Options;
                     end if;
                     Options.Mode := List_Suites;

                  when 'm' =>    -- manual operation
                     Options.Manual := True;

                  when 'P' => -- print test suites
                     if Options.Routine.Length > 0 or else
                           Options.Suite_Name.Length > 0 then
                        Bad_Options;
                     end if;
                     Options.Mode := Print_Suites;
--                   Options.Update_Filter;   -- sets filter name

                  when 'r' =>
                     Options.Report_Random := True;

                  when 'R' =>
                     Options.Set_Random_Seed := True;
                     Options.Random_Seed := Iterator.Get_Integer;

                  when 'S' =>
                     Options.Report_Random := True;

                  when 'x' =>
                     Options.Exit_On_Done := True;

                  when others =>
                     Log_Exception (Trace_Options or Debug, " other option" &
                        Option.Image);
                     raise Failed with "Has_Option incorrectly passed " &
                        Option.Image;
               end case;

            when Nil_Option =>
               pragma Assert (False, "unexpected kind");

         end case;

         return Log_Out_Checked (Recursed, True, Trace_Options or Debug,
            " option" & Option.Image & " handled mode " & Options.Mode'img);
      else
         return Log_Out_Checked (Recursed,
            Options.GNOGA_Options.Process_Option (Iterator, Option),
            Trace_Options or Debug,
            "other option" & " Option" & Option.Image);
      end if;
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in      Ada_Lib_Unit_Test_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "test_ada_lib";

   begin
      Log_In (Debug or Trace_Options, "mode " & Help_Mode'img);
      Options.GNOGA_Options.Program_Help (Help_Mode);
      case Help_Mode is

      when Ada_Lib.Options.Program =>
         -- options without modifier
         Ada_Lib.Help.Add_Option ('e', "routine", "test routine.", Component);
         Ada_Lib.Help.Add_Option ('s', "test suite", "select test suite to run.",
            Component);
         Ada_Lib.Help.Add_Option ('S', "suites", "enable default disabled suites.",
            Component);
         Ada_Lib.Help.Add_Option ('U', "unit test trace Ada_Lib.",
            "select trace", Component);
         Ada_Lib.Help.Add_Option ('x', "", "exit on tests complete", Component);
         -- options with modifier
         Ada_Lib.Help.Add_Option ('d', "", "driver suites", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('l', "", "List test suites", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('m', "", "manual operations.", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('P', "", "Print test suites.", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('S', "", "report random seed", Component,
            Ada_Lib.Help.Modifier);
         Ada_Lib.Help.Add_Option ('R', "seed", "set random seed", Component,
            Ada_Lib.Help.Modifier);

      when Ada_Lib.Options.Traces =>
         Put_Line ("Ada_Lib unit test library trace options (-U)");
         Put_Line ("      a               all");
         Put_Line ("      A               all unit tests");
         Put_Line ("      g               Ada_Lib.GNOGA.Unit_Test.Debug");
         Put_Line ("      l               Ada_Lib.Unit_Test.Debug Library");
         Put_Line ("      p               test programs");
         Put_Line ("      r               Runtime_Options");
         Put_Line ("      t               Ada_Lib.Test.Debug");
         Put_Line ("      T               Ada_Lib.Trace.Debug_Trace");
         New_Line;
         Put_Line ("Enable suites disabled by default (-S)");
         Put_Line ("      a               enable all");
         Put_Line ("      d               database server");
         Put_Line ("      t               text");
         New_Line;

      end case;

      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ------------------------------------------------------------
   procedure Routine_Action (
      Suite                      : in     String;
      Routine                    : in     String;
      Mode                       : in     Mode_Type) is
   ------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("Suite", Suite) & Quote (" Routine", Routine) &
         " Suites_Mode " & Mode'img);

      case Mode is

         when Ada_Lib.Options.Driver_Suites =>
            Put_Line ("suite: " & Suite & " routine: " & Routine);

         when Ada_Lib.Options.List_Suites =>
            Put_Line (Suite & " " & Routine);

         when Ada_Lib.Options.Print_Suites =>
            Put_Line ("      " & Routine);

         when Ada_Lib.Options.Run_Tests =>
            null;

      end case;
      Log_Out (Debug);
   end Routine_Action;

   ------------------------------------------------------------
   procedure Suite_Action (
      Suite                      : in     String;
      First                      : in out Boolean;
      Mode                       : in     Mode_Type) is
   ------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("Suite", Suite) & " first " & First'img &
         " Mode " & Mode'img);

      case Mode is

         when Ada_Lib.Options.Driver_Suites |
              Ada_Lib.Options.List_Suites |
              Ada_Lib.Options.Run_Tests =>
            null;

         when Ada_Lib.Options.Print_Suites =>
            if First then
               Put_Line ("test suites: ");
               First := False;
            end if;
            Put_Line ("   " & Suite);

      end case;
      Log_Out (Debug);
   end Suite_Action;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Ada_Lib_Unit_Test_Options_Type;
      Iterator                   : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      Log_In (Trace_Options or Debug,  Quote ("parameter", Parameter));
      for Trace of Parameter loop
         Log_Here (Trace_Options or Debug, Quote ("trace", Trace));

         case Trace is

            when 'a' =>
               Debug := True;
               Ada_Lib.GNOGA.Unit_Test.Debug := True;
               Ada_Lib.Test.Debug := True;
               Ada_Lib.Trace.Trace_Tests := True;
               Ada_Lib.Unit_Test.Debug := True;
               Options.Debug := True;

            when 'A' =>
               Ada_Lib.Trace.Trace_Tests := True;

            when 'g' =>
               Ada_Lib.GNOGA.Unit_Test.Debug := True;

            when 'l' =>
               Ada_Lib.Unit_Test.Debug := True;

            when 'r' =>
               Debug := True;

           when 'p' =>
              Options.Debug := True;

            when 't' =>
               Ada_Lib.Test.Debug := True;

            when 'T' =>
               Ada_Lib.Trace.Debug_Trace := True;

            when others =>
               declare
                  Message        : constant String :=
                                    Quote ("unexpected trace option", Trace) &
                                    " for 'U'";

               begin
                  Log_Exception (Trace_Options or Debug, Message);
                  raise Failed with Message;
               end;

         end case;
      end loop;
      Log_Out (Debug or Trace_Options);
   end Trace_Parse;

   ----------------------------------------------------------------------------
   overriding
   procedure Update_Filter (
      Options                    : in out Ada_Lib_Unit_Test_Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, "options " &
         Image (Options'address) & " filter " &
         Image (Options.Filter'address) &
         Quote (" Suite", Options.Suite_Name) &
         Quote (" routine", Options.Routine));
      if Options.Suite_Name.Length > 0 then
         if Options.Routine.Length > 0 then
            Log_Here (Debug, Quote ("suite", Options.Suite_Name) & Quote (" routine", Options.Routine));
            Options.Filter.Set_Name (Options.Suite_Name.Coerce & " : " & Options.Routine.Coerce);
         else
            Log_Here (Debug, Quote ("suite", Options.Suite_Name) & " no routine");
            Options.Filter.Set_Name (Options.Suite_Name.Coerce);
         end if;
      end if;
      Log_Out (Debug);
   end Update_Filter;

begin
--Ada_Lib.Trace.Trace_Tests := True;
--Elaborate := True;
   Debug := Debug_Options.Debug_All;
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug or Elaborate or Trace_Options);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Ada_Lib.Options.Unit_Test;
