with Ada.Text_IO; use Ada.Text_IO;
-- with Ada_Lib.Options.Help;
----with Ada_Lib.Database.Server;
--with Ada_Lib.GNOGA;
with Ada_Lib.Options.Help;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Options.Runstring;
--with Debug_Options;
with GNOGA.Ada_Lib;

--pragma Elaborate_All (Ada_Lib.Command_Line_Iterator);
package body Ada_Lib.Options.GNOGA is

   Trace_Option                     : constant Character := 'G';
   Options_With_Parameters          : aliased constant
                                       Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             Trace_Option);
   Options_Without_Parameters       : aliased constant
                                       Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Create_Options ('w');

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out GNOGA_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options);
      GNOGA_Options := Options'unchecked_access;
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);
      return Log_Out (Actual.Nested_Options_Type (Options).Initialize,
         Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options                    : in out GNOGA_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, " option" & Option.Image);
      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Ada_Lib.Options.Null_Options) then
         case Option.Kind is

         when Ada_Lib.Options.Plain =>
            case Option.Option is

               when 'G' =>   -- options for GNOGA
                  Options.Trace_Parse (Iterator);

               when 'w' =>   -- options for GNOGA
                  Options.HTTP_Port := Ada_Lib.Socket_IO.Port_Type (
                     Iterator.Get_Integer);

               when Others =>
                  raise Failed with Quote ("Has_Option incorrectly passed ",
                     Option.Option);

            end case;

         when Ada_Lib.Options.Nil_Option |
              Ada_Lib.Options.Modified =>
            raise Failed with "Has_Option incorrectly passed " & Option.Image;

         end case;
      else
         return Log_Out (False, Debug or Trace_Options); -- derived from Interface_Options_Type
      end if;

      return Log_Out (True, Debug or Trace_Options, " option" & Option.Image &
         " handled");
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in      GNOGA_Options_Type;
      Help_Mode                  : in      Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

--    use Ada_Lib.Options;

   begin
      Log_In (Debug or Trace_Options);
      case Help_Mode is

      when Ada_Lib.Options.Program =>
            Ada_Lib.Options.Help.Add_Option (Create_Option ('G'), "trace options",
               "GNOGA traces", "GNOGA library");
            Ada_Lib.Options.Help.Add_Option (Create_Option ('w'), "",
               "Web server port", "GNOGA library");

      when Ada_Lib.Options.Traces =>
         Put_Line ("Ada_Lib.Options.GNOGA library trace options (-" & Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      m               GNOGA.Ada_Lib");
         Put_Line ("      o               GNOGA options");
         New_Line;

      end case;

      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out GNOGA_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      for Index in Parameter'range  loop
         declare
            Trace    : constant Character := Parameter (Index);

         begin
            case Trace is

               when 'a' =>
                  Standard.GNOGA.Ada_Lib.Trace := True;
                  Debug := True;

               when 'm' =>
                  Standard.GNOGA.Ada_Lib.Trace := True;

               when 'o' =>
                  Debug := True;

               when others =>
                  Options.Bad_Trace_Option (Trace_Option, Trace);

            end case;


         end;
      end loop;
   end Trace_Parse;

begin
--   Debug := Debug_Options.Debug_All;
--Debug := True;
--Trace_Options := True;
   Log_Here (Elaborate);
end Ada_Lib.Options.GNOGA;
