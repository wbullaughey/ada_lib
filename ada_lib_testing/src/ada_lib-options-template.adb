with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Help;
with Ada_Lib.Options_Interface;
with Ada_Lib.Runstring_Options;
--with Ada_Lib.Strings.Unlimited;
--with Ada_Lib.Template;
with Ada_Lib.Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Unit_Test; --.GNOGA;
--with Debug_Options;
--with AUnit;

pragma Elaborate_All (Ada_Lib.Command_Line_Iterator);

package body Ada_Lib.Options.Template is

-- use type  Ada_Lib.Strings.Unlimited.String_Type;

   Trace_Option                  : constant Character := 'T';
   Options_With_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options_Interface.
                                       Options_Type :=
                                          Ada_Lib.Options_Interface.Create_Options (
                                             Trace_Option);
   Options_Without_Parameters    : aliased constant
                                    Standard.Ada_Lib.Options_Interface.
                                       Options_Type := Standard.Ada_Lib.
                                          Options_Interface.Null_Options;

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Template_Options_Type;
     From                        : in     String := Here
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "from " & From);
      Template_Options_Constant := Options'unchecked_access;
      Ada_Lib.Runstring_Options.Options.Register (Ada_Lib.Runstring_Options.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Runstring_Options.Options.Register (Ada_Lib.Runstring_Options.Without_Parameters,
         Options_Without_Parameters);
      return Log_Out (Nested_Options_Type (Options).Initialize, Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options                    : in out Template_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.
                                             Abstract_Package.Abstract_Iterator_Type'class;
      Option                     : in     Ada_Lib.Options_Interface.
                                             Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      use Standard.Ada_Lib.Options_Interface;

      Has_On_Options             : constant Boolean :=
                                    Has_Option (Option,
                                       Options_With_Parameters,
                                       Options_Without_Parameters);
   begin
      Log_In (Trace_Options or Debug, Option.Image &
         " has options " & Has_On_Options'img &
         " Options address " & Image (Options'address));

      if Has_On_Options then
         case Option.Option is

            when 'T' => -- template trace options
               Options.Trace_Parse (Iterator);

            when Others =>
               Log_Exception (Trace_Options or Debug, " other option" &
                  Option.Image);
               raise Failed with "Has_Option incorrectly passed " & Option.Image;

         end case;
      else
         return Log_Out(False, Trace_Options or Debug,
            "other option" & Option.Image);
      end if;

      Log_Out (Trace_Options or Debug, " option" & Option.Image & " handled");
      return True;

   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Template_Options_Type;  -- root of derivation truee
      Help_Mode                  : in     Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      use Standard.Ada_Lib.Options_Interface;

      Component                  : constant String := "Ada_Lib.Template.Tests";

   begin
      Log_In (Debug, "mode " & Help_Mode'img);
      case Help_Mode is

         when Ada_Lib.Options.Program =>
            New_Line;
            Ada_Lib.Help.Add_Option (Trace_Option, "options", -- t
               "enables trace template unit tests", Component);

         when Ada_Lib.Options.Traces =>
            Put_Line (Component & " (-" & Trace_Option & ")");
            Put_Line ("      a               all");
            Put_Line ("      c               compile");
            Put_Line ("      e               evaluate");
            Put_Line ("      E               expand");
            Put_Line ("      l               load");
            Put_Line ("      t               test");
            New_Line;

      end case;
      Log_Out (Debug);
   end Program_Help;

--   ----------------------------------------------------------------------------
--   overriding
--   function Set_Options (
--      Options                    : in out Template_Options_Type
--   ) return Boolean is
--   ----------------------------------------------------------------------------
--
--   begin
--      Log_Here (Debug or Trace_Options);
--      return False;     -- terminal derivation
----    return Options_Type (Options).Set_Options;
--   end Set_Options;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Template_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      Log (Trace_Options or Debug, Here, Who & Quote (" Parameter", Parameter));
      for Index in Parameter'range  loop
         declare
            Trace    : constant Character := Parameter (Index);

         begin
            Log_Here (Trace_Options or Debug, Quote ("trace", Trace));
            case Trace is

               when 'a' =>
                  Debug := True;
                  Options.Compile := True;
                  Options.Evaluate := True;
                  Options.Expand := True;
                  Options.Load := True;
                  Options.Test := True;
                  Ada_Lib.Test.Debug := True;

               when 'o' =>
                  Debug := True;

               when 't' =>
                  Ada_Lib.Test.Debug := True;

               when 'T' =>
                  Options.Test := True;

               when others =>
                  Options.Bad_Option (Trace);

            end case;


         end;
      end loop;
   end Trace_Parse;

-- ----------------------------------------------------------------------------
-- overriding
-- procedure Update_Filter (
--    Options                    : in out Template_Options_Type) is
-- ----------------------------------------------------------------------------
--
-- begin
--    Log_In (Debug or Ada_Lib.Unit_Test.Debug, "options " &
--       Image (Options'address) & " filter " &
--       Image (Options.Filter'address) &
--       Quote (" Suite", Options.Suite_Name) &
--       Quote (" routine", Options.Routine));
--    if Options.Suite_Name.Length > 0 then
--       if Options.Routine.Length > 0 then
--          Log_Here (Debug, Quote ("suite", Options.Suite_Name) & Quote (" routine", Options.Routine));
--          Options.Filter.Set_Name (Options.Suite_Name.Coerce & " : " & Options.Routine.Coerce);
--       else
--          Log_Here (Debug, Quote ("suite", Options.Suite_Name) & " no routine");
--          Options.Filter.Set_Name (Options.Suite_Name.Coerce);
--       end if;
--    end if;
--    Log_Out (Debug);
-- end Update_Filter;

begin
-- Elaborate := True;
-- Debug := Debug_Options.Debug_All;
--Trace_Options := True;
--debug := True;
   Log_Here (Elaborate or Debug);
end Ada_Lib.Options.Template;
