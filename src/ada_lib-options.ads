with Ada.Characters.Latin_1;
--with  Ada.Characters.Latin_1;
--with Ada.Finalization;
--with Ada.Tags;
with Ada_Lib.Trace; -- use Ada_Lib.Trace;
with GNAT.Source_Info;
with Interfaces;

package Ada_Lib.Options is

   Failed                        : exception;

   type Help_Mode_Type           is (Program, Traces);

   type Mode_Type                is (Driver_Suites, List_Suites, Print_Suites,
                                       Run_Tests);

   type Option_Kind_Type         is (Nil_Option, Plain, Modified);

   No_Option                     : constant Character :=
                                    Ada.Characters.Latin_1.NUL;
   Unmodified                    : constant Character :=
                                    Ada.Characters.Latin_1.NUL;

   type Option_Type              is tagged record
      Kind                       : Option_Kind_Type := Nil_Option;
      Modifier                   : Character := Unmodified;
      Option                     : Character;
   end record;

   function Create_Option (
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Option_Type;

   function Image (
      Option                     : in     Option_Type;
      Quote                      : in     Boolean := True
   ) return String;

   function Less (
      Left, Right                : in     Option_Type
   ) return Boolean;

   function Modified (
      Option                     : in     Option_Type
   ) return Boolean;

   function Modifier (
      Option                     : in     Option_Type
   ) return Character;

   type Options_Type             is array (Positive range <>) of Option_Type;
   type Options_Access           is access Options_Type;

   function Create_Options (     -- create a single options with a character
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Options_Type;

   function Create_Options (     -- create multiple options from a string
      Source                     : in     String;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Options_Access;

   function Create_Options (     -- create a single options
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Options_Access;

   function Create_Options (    -- create a single options with a character
      Source                     : in     String;
      Modifier                   : in     Character;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Options_Type;

   function Has_Option (   -- tests if option is registered for a catagory
      Option                     : in     Option_Type;
      Options_With_Parameters    : in     Options_Type;
      Options_Without_Parameters : in     Options_Type
   ) return Boolean;

   function Image (
      Options                    : in     Options_Type;
      Quote                      : in     Boolean := True
   ) return String;

   type Command_Line_Iterator_Interface
                                 is interface;

      procedure Advance (
         Iterator          : in out Command_Line_Iterator_Interface) is abstract;

      function At_End (
         Iterator          : in   Command_Line_Iterator_Interface
      ) return Boolean is abstract;

      procedure Dump_Iterator (
         Iterator                : in     Command_Line_Iterator_Interface;
         What                    : in     String;
         Where                   : in     String := Ada_Lib.Trace.Here
      ) is abstract;

      function Get_Argument (
         Iterator                : in     Command_Line_Iterator_Interface
      ) return String is abstract;

      function Get_Argument (
         Iterator                : in     Command_Line_Iterator_Interface;
         Index                   : in     Positive
      ) return String is abstract;

      function Get_Option (
         Iterator          : in   Command_Line_Iterator_Interface
      ) return Ada_Lib.Options.Option_Type'class is abstract;

      -- parameter of an option
      function Get_Parameter (
         Iterator          : in out Command_Line_Iterator_Interface
      ) return String is abstract;

      -- numeric parameter of an option
      -- raise Invalid_Number
      function Get_Integer (
         Iterator          : in out Command_Line_Iterator_Interface
      ) return Integer  is abstract;

      -- numeric parameter of an option
      -- raise Invalid_Number
      function Get_Float (
         Iterator          : in out Command_Line_Iterator_Interface
      ) return float is abstract;

      -- numeric parameter of an option
      -- raise Invalid_Number
      function Get_Unsigned (
         Iterator          : in out Command_Line_Iterator_Interface;
         Base              : in   Positive := 16
      ) return Interfaces.Unsigned_64 is abstract;


      function Is_Option (
         Iterator                : in   Command_Line_Iterator_Interface
      ) return Boolean is abstract;

   type Interface_Options_Type is limited interface;

   type Interface_Options_Class_Access
                                 is access all Interface_Options_Type'class;
   type Interface_Options_Constant_Class_Access
                                 is access constant Interface_Options_Type'class;

   procedure Bad_Option (
      Options                    : in     Interface_Options_Type;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

   procedure Bad_Option (
      Options                    : in     Interface_Options_Type;
      What                       : in     String;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

   procedure Bad_Option (
      Options                    : in     Interface_Options_Type;
      Option                     : in     Option_Type'class;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

-- function Has_Option (   added 2/22/24 to resolve issue with multple option lists
--    Options                    : in     Interface_Options_Type;
--    Option                     : in     Option_Type
-- ) return Boolean is abstract;

   procedure Bad_Trace_Option (
      Options                    : in     Interface_Options_Type;
      Trace_Option               : in     Character;
      What                       : in     Character;
      Modifier          : in     Character := Ada.Characters.Latin_1.Nul;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

   procedure Display_Help (            -- common for all programs that use GNOGA_Options
                              -- prints full help, aborts program
     Options                     : in     Interface_Options_Type;  -- only used for dispatch
     Message                     : in     String := "";   -- leave blank no error help
     Halt                        : in     Boolean := True) is abstract;

   -- direct decendent should return true
   -- indirect decentdent should return initialize of parent
   function Initialize (
     Options                     : in out Interface_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is abstract;

   function Process_Argument (  -- process one argument
     Options                     : in out Interface_Options_Type;
     Iterator                    : in out Command_Line_Iterator_Interface'class;
     Argument                    : in     String
   ) return Boolean is abstract;

   function Process_Option (  -- process one option
     Options                     : in out Interface_Options_Type;
     Iterator                    : in out Command_Line_Iterator_Interface'class;
     Option                      : in     Option_Type'class
   ) return Boolean is abstract;

   procedure Program_Help (      -- common for all programs that use GNOGA_Options
     Options                     : in     Interface_Options_Type;  -- only used for dispatch
     Help_Mode                   : in     Help_Mode_Type) is abstract;

   procedure Trace_Parse (
      Options                    : in out Interface_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class
   ) is abstract;

   procedure Update_Filter (
      Options                    : in out Interface_Options_Type) is abstract;

   function Verify_Initialized (
      Options                    : in     Interface_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is abstract;

   function Verify_Preinitialize (
      Options                    : in     Interface_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is abstract;

-- function Ada_Lib_Options
-- return Interface_Options_Constant_Class_Access;

-- function Get_Ada_Lib_Modifiable_Options (
--    From                       : in  String := Ada_Lib.Trace.Here
-- ) return Interface_Options_Class_Access
-- with pre => Have_Options;
--
-- function Get_Ada_Lib_Read_Only_Program_Options (
--    From                       : in  String := Ada_Lib.Trace.Here
-- ) return Interface_Options_Constant_Class_Access
-- with pre => Have_Options;
--
-- type Registration_Type        is abstract new Ada.Finalization.Controlled with null record;

   -- non class declarations

-- function Have_Options return Boolean;

   procedure Parsing_Failed;
   function Parsing_Failed return Boolean;

   Debug                         : Boolean := False;
   Null_Option                   : constant Option_Type := Option_Type'(
                                    Kind     => Nil_Option,
                                    Modifier => Unmodified,
                                    Option   => No_Option);
   Null_Options                  : constant Options_Type (1 .. 0) :=
                                    (others => Null_Option);
   Use_Options_Prefix            : constant Boolean := True;

end Ada_Lib.Options;
