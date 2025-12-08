--with  Ada.Characters.Latin_1;
--with Ada.Finalization;
--with Ada.Tags;
--with Ada_Lib.Options.Actual;
with Ada_Lib.Trace; -- use Ada_Lib.Trace;
--with GNAT.Source_Info;

package Ada_Lib.Options.Create is

   function Create_Options (     -- create a single options with a character
     Option                     : in     Character;
     Modifier                   : in     Character;
     From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access;

   function Create_Options (     -- create multiple options from a string
     Source                     : in     String;
     Modifier                   : in     Character;
     From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access;

   function Create_Options (     -- create a single options
     Option                     : in     Character;
     Modifier                   : in     Character;
     From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access;

   function Create_Options (    -- create a single options with a character
     Source                     : in     String;
     Modifier                   : in     Character;
     From                       : in     String := Ada_Lib.Trace.Here
   ) return Root_Options_Access;

   function Has_Option (   -- tests if option is registered for a catagory
     Option                     : in     Root_Option_Type'class;
     Options_With_Parameters    : in     Options_Type;
     Options_Without_Parameters : in     Options_Type
   ) return Boolean;

   function Image (
     Options                    : in     Options_Type;
     Quote                      : in     Boolean := True
   ) return String;

end Ada_Lib.Options.Create;
