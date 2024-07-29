with Ada_Lib.Options;
with Ada_Lib.Trace;

package Ada_Lib.Help is

   Failed                        : exception;

   procedure Add_Option (
      Option                     : in     Ada_Lib.Options.Option_Type;
      Parameter                  : in     String;
      Description                : in     String;
      Component                  : in     String := "";
      Source_Line                : in     String := Ada_Lib.Trace.Here
   ) with Pre => Description'length > 0;

   procedure Add_Option (
      Option                     : in     Character;
      Parameter                  : in     String;
      Description                : in     String;
      Component                  : in     String := "";
      Modifier                   : in     Character :=
                                             Ada_Lib.Options.Unmodified;
      Source_Line                : in     String := Ada_Lib.Trace.Here
   ) with Pre => Description'length > 0;

   procedure Display (
      Output_Line                : not null access procedure (
         Line                       : in     String));

   procedure Reset;

   Debug                         : Boolean := False;
   Modifier                      : constant Character := '@';
   Modifiers                     : constant String := "@";

end  Ada_Lib.Help;

