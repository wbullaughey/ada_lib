with Ada.Characters.Latin_1;
--with  Ada.Characters.Latin_1;
--with Ada.Finalization;
--with Ada.Tags;
--with Ada_Lib.Trace; -- use Ada_Lib.Trace;
with GNAT.Source_Info;
with Interfaces;

package Ada_Lib.Options is

   Failed                        : exception;

   function Here
   return String renames GNAT.Source_Info.Source_Location;

   type Help_Mode_Type           is (Program, Traces);

   type Mode_Type                is (Driver_Suites, List_Suites, Print_Suites,
                                       Run_Tests);

   type Flag_Option_Kind_Type    is (Nil_Option, Plain, Modified);
   type SubFlag_Option_Kind_Type is (Plain, Modified);


   Not_Flag_Option               : constant Character :=
                                    Ada.Characters.Latin_1.NUL;
   Unmodified_Flag               : constant Character :=
                                    Ada.Characters.Latin_1.NUL;

   type Base_Flag_Option_Type;
   type Base_Flag_Option_Access  is access all Base_Flag_Option_Type;
   type Base_Flag_Option_Class_Access
                                 is access all Base_Flag_Option_Type'class;

   type Base_Options_Array       is array (Positive range <>) of
                                    Base_Flag_Option_Class_Access;

   type Flag_List_Type           is tagged private;
   type Flag_List_Class_Access   is access Flag_List_Type;


   function "&" (
      Left, Right                : in        Flag_List_Type
   ) return Flag_List_Type;

   procedure Create_Options (
      Flag                    :    out Flag_List_Type;
      Options                 : in     Base_Options_Array;
      From                    : in     String := Here);

   function Image (
      Flat                    : in        Flag_List_Type
   ) return String;

   procedure Iterate (
      Flags                      : in     Flag_List_Type;
      Callback                   : access procedure (
         Option                  : in     Base_Flag_Option_Type'class));

   function Length (
      Flags                      : in     Flag_List_Type
   ) return Natural;

   type Base_Flag_Option_Type    is abstract tagged record
     Kind                        : Flag_Option_Kind_Type := Nil_Option;
     Modifier                    : Character := Unmodified_flag;
     Option                      : Character;
   end record;

   procedure Create_Option (
      Flag                       :    out Base_Flag_Option_Type;
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Here);

     function Has_Option (   -- tests if option is registered for a catagory
        Option                     : in     Base_Flag_Option_Type;
        Options_With_Parameters    : in     Flag_List_Type'class;
        Options_Without_Parameters : in     Flag_List_Type'class
     ) return Boolean is abstract;

     function Image (
        Option                     : in     Base_Flag_Option_Type;
        Quote                      : in     Boolean := True
     ) return String is abstract;

     function Less (
        Left, Right                : in     Base_Flag_Option_Type
     ) return Boolean is abstract;

     function Modified (
        Option                     : in     Base_Flag_Option_Type
     ) return Boolean is abstract;

     function Modifier (
        Option                     : in     Base_Flag_Option_Type
     ) return Character is abstract;

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
        Where                   : in     String := Here
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
     ) return Base_Flag_Option_Type'class is abstract;

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

   type Abstract_Runtime_Options_Type
                     is limited interface;

   type Abstract_Runtime_Options_Class_Access
                     is access all Abstract_Runtime_Options_Type'class;
   type Abstract_Runtime_Options_Constant_Class_Access
                     is access constant Abstract_Runtime_Options_Type'class;

   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Abstract_Runtime_Options_Type;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Here) is abstract;

   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Abstract_Runtime_Options_Type;
      What                       : in     String;
      Message                    : in     String := "";
      Where                      : in     String := Here) is abstract;

   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Abstract_Runtime_Options_Type;
      Option                     : in     Base_Flag_Option_Type'class;
      Message                    : in     String := "";
      Where                      : in     String := Here) is abstract;

   procedure Bad_Trace_Option (  -- raises Failed exception
      Options                    : in     Abstract_Runtime_Options_Type;
      Trace_Option               : in     Character;
      What                       : in     Character;
      Modifier          : in     Character := Ada.Characters.Latin_1.Nul;
      Message                    : in     String := "";
      Where                      : in     String := Here) is abstract;

-- procedure Bad_Option (
--    Options                    : in     Abstract_Runtime_Options_Type;
--    What                       : in     Character;
--    Message                    : in     String := "";
--    Where                      : in     String := Here) is abstract;
--
-- procedure Bad_Option (
--    Options                    : in     Abstract_Runtime_Options_Type;
--    What                       : in     String;
--    Message                    : in     String := "";
--    Where                      : in     String := Here) is abstract;
--
-- procedure Bad_Option (
--    Options                    : in     Abstract_Runtime_Options_Type;
--    Option                     : in     Abstract_Runtime_Options_Type'class;
--    Message                    : in     String := "";
--    Where                      : in     String := Here) is abstract;

-- function Has_Option (   -- added 2/22/24 to resolve issue with multple option lists
--    Options                    : in     Abstract_Runtime_Options_Type;
--    Option                     : in     Abstract_Runtime_Options_Type
-- ) return Boolean is abstract;

-- procedure Bad_Trace_Option (
--    Options                    : in     Abstract_Runtime_Options_Type;
--    Trace_Option               : in     Character;
--    What                       : in     Character;
--    Modifier          : in     Character := Ada.Characters.Latin_1.Nul;
--    Message                    : in     String := "";
--    Where                      : in     String := Here) is abstract;
--
   procedure Display_Help (            -- common for all programs that use GNOGA_Options
                              -- prints full help, aborts program
     Options                     : in     Abstract_Runtime_Options_Type;  -- only used for dispatch
     Message                     : in     String := "";   -- leave blank no error help
     Halt                        : in     Boolean := True) is abstract;

   function Image (
     Options                     : in     Abstract_Runtime_Options_Type
   ) return String is abstract;

   -- direct decendent should return true
   -- indirect decentdent should return initialize of parent
   function Initialize (
     Options                     : in out Abstract_Runtime_Options_Type;
     From                        : in     String := Here
   ) return Boolean is abstract;

   function Process_Argument (  -- process one argument
     Options                     : in out Abstract_Runtime_Options_Type;
     Iterator                    : in out Command_Line_Iterator_Interface'class;
     Argument                    : in     String
   ) return Boolean is abstract;

   function Process_Option (  -- process one option
     Options                     : in out Abstract_Runtime_Options_Type;
     Iterator                    : in out Command_Line_Iterator_Interface'class;
     Option                      : in     Base_Flag_Option_Type'class
   ) return Boolean is abstract;

   procedure Program_Help (      -- common for all programs that use GNOGA_Options
     Options                     : in     Abstract_Runtime_Options_Type;  -- only used for dispatch
     Help_Mode                   : in     Help_Mode_Type) is abstract;

   procedure Trace_Parse (
      Options                    : in out Abstract_Runtime_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class
   ) is abstract;

   procedure Update_Filter (
      Options                    : in out Abstract_Runtime_Options_Type) is abstract;

   function Verify_Initialized (
      Options                    : in     Abstract_Runtime_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is abstract;

   function Verify_Preinitialize (
      Options                    : in     Abstract_Runtime_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is abstract;

--   function Create_Options (     -- create multiple options from a string
--      Source                     : in     String;
--      Modifier                   : in     Character;
--      From                       : in     String := Here
--   ) return Options_Access;
--
--   function Create_Options (     -- create a single options
--      Option                     : in     Character;
--      Modifier                   : in     Character;
--      From                       : in     String := Here
--   ) return Options_Access;
--
--   function Create_Options (    -- create a single options with a character
--      Source                     : in     String;
--      Modifier                   : in     Character;
--      From                       : in     String := Here
--   ) return Flag_List_Type;
--
--
--   function Image (
--      Options                    : in     Flag_List_Type;
--      Quote                      : in     Boolean := True
--   ) return String;
--

-- function Ada_Lib_Options
-- return Base_Flag_Option_Constant_Class_Access;

-- function Get_Ada_Lib_Modifiable_Options (
--    From                       : in  String := Here
-- ) return Base_Flag_Option_Class_Access
-- with pre => Have_Options;
--
-- function Get_Ada_Lib_Read_Only_Program_Options (
--    From                       : in  String := Here
-- ) return Base_Flag_Option_Constant_Class_Access
-- with pre => Have_Options;
--
---- type Registration_Type        is abstract new Ada.Finalization.Controlled with null record;
--
--   -- non class declarations
--
---- function Have_Options return Boolean;
--
   -- raises assert
   procedure Not_Implemented (
      Why                        : in     String := "";
      Here                       : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity);

  procedure Parsing_Failed;
  function Parsing_Failed return Boolean;

   Debug                         : Boolean := False;
   Debug_All                     : constant Boolean := False;
   Debug_Options                 : constant Boolean := False;
   Null_Flag_List                : constant Flag_List_Type;
   Use_Options_Prefix            : constant Boolean := True;

   package Ada_Lib_Environment is

      Debug                      : Boolean := False;
      Help_Test                  : Boolean := False;
      Unit_Testing               : constant Boolean := False;
--    := Ada_Lib_Environment.
--                         Parse_Environment_Variable (
--                            Ada_Lib_Environment.Unit_Test_Kind);

   end Ada_Lib_Environment;
   package Aunit is
      Debug                      : Boolean := False;
   end Aunit;

   package Help is
      Debug                      : Boolean := False;
   end Help;

   package GNOGA_Options is
      Debug                         : aliased Boolean := False;
      GNOGA_Ada_Lib_Debug           : aliased Boolean := False;
      GNOGA_Ada_Lib_Base_Debug      : aliased Boolean := False;
      Debug_Options                 : aliased Boolean := False;
   end GNOGA_Options;

private

   type Options_Array_Access     is access all Base_Options_Array;

   type Flag_List_Type           is tagged record
      Options                    : Options_Array_Access;
   end record;

   Null_Flag_List             : constant Flag_List_Type := (
      Options  => Null
   );

end Ada_Lib.Options;
