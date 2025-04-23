--with  Ada.Characters.Latin_1;
--with Ada.Finalization;
--with Ada.Tags;
with Ada_Lib.Trace; -- use Ada_Lib.Trace;
with GNAT.Source_Info;

package Ada_Lib.Options.Actual is

   function Have_Ada_Lib_Nested_Options
   return Boolean;

   function Have_Ada_Lib_Program_Options
   return Boolean;

   type Abstract_Options_Type is abstract limited new Interface_Options_Type
                                 with null record;

   type Abstract_Options_Class_Access
                                 is access all Abstract_Options_Type'class;
   type Abstract_Options_Constant_Class_Access
                                 is access constant Abstract_Options_Type'class;

   overriding
   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Abstract_Options_Type;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Abstract_Options_Type;
      What                       : in     String;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Abstract_Options_Type;
      Option                     : in     Ada_Lib.Options.Option_Type'class;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   procedure Bad_Trace_Option (  -- raises Failed exception
      Options                    : in     Abstract_Options_Type;
      Trace_Option               : in     Character;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   procedure Update_Filter (
      Options                    : in out Abstract_Options_Type) ;

-- generic
--    type Generic_Options_Type  is abstract limited new Interface_Options_Type
--                                  with private;
--
-- package Verification_Package is

   type Verification_Options_Type
         is abstract limited new Abstract_Options_Type with record
      Initialized             : Boolean := False;
   end record;

   type Verification_Options_Access        is access all Verification_Options_Type;
   type Verification_Options_Class_Access  is access all Verification_Options_Type'class;
   type Verification_Options_Constant_Class_Access
                              is access constant Verification_Options_Type'class;

-- function Get_Ada_Lib_Modifiable_Verification_Options (
--    From                       : in  String := Ada_Lib.Trace.Here
-- ) return Verification_Options_Class_Access
-- with pre => Have_Ada_Lib_Verification_Options;
--
-- function Get_Ada_Lib_Read_Only_Verification_Options (
--    From                       : in  String := Ada_Lib.Trace.Here
-- ) return Verification_Options_Constant_Class_Access
-- with pre => Have_Ada_Lib_Verification_Options;

   overriding
   function Initialize (
      Options                 : in out Verification_Options_Type;
      From                    : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean
   with Pre => Options.Verify_Preinitialize;

   overriding
   function Process_Argument (  -- process one argument
      Options                  : in out Verification_Options_Type;
      Iterator                 : in out Command_Line_Iterator_Interface'class;
      Argument                 : in     String
   ) return Boolean;

   overriding
   function Verify_Initialized (
      Options                 : in     Verification_Options_Type;
      From                    : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   overriding
   function Verify_Preinitialize (
      Options                 : in     Verification_Options_Type;
      From                    : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

-- end Verification_Package;

   -- type to application options
   type Program_Options_Type  is abstract limited new Verification_Options_Type with record
      Help_Test               : Boolean := False;  -- used to test help options
      In_Help                 : Boolean := False;
      Processed               : Boolean := False;
      Test_Driver             : Boolean := False;
      Verbose                 : Boolean := False;
   end record;

   type Program_Options_Access  is access all Program_Options_Type;
   type Program_Options_Class_Access
                                 is access all Program_Options_Type'class;
   type Program_Options_Constant_Class_Access
                                 is access constant Program_Options_Type'class;

   overriding
   procedure Display_Help (            -- common for all programs that use Ada_Lib.Options.GNOGA
                              -- prints full help, aborts program
     Options                     : in     Program_Options_Type;  -- only used for dispatch
     Message                     : in     String := "";   -- leave blank no error help
     Halt                        : in     Boolean := True);

-- package Program_Options_Package
--                               is new Verification_Package (
--                                     Abstract_Options_Type);

   overriding
   function Initialize (
     Options                     : in out Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize,
        post => Options.Verify_Initialized;

   procedure Post_Process (      -- final initialization
     Options                    : in out Program_Options_Type);

   -- needs to be overrident by type used to allocate options object
   function Process (
     Options                     : in out Program_Options_Type;
     Include_Options             : in     Boolean;
     Include_Non_Options         : in     Boolean;
     Option_Prefix               : in     Character := '-';
     Modifiers                   : in     String := ""
   ) return Boolean
   with Pre => Options.Verify_Preprocess,
        Post => Options.Verify_Postprocess;

   procedure Process (     -- process command line options
     Options                    : in out Program_Options_Type;
     Iterator                   : in out Command_Line_Iterator_Interface'class);

   overriding
   function Process_Option (  -- process one option
      Options                    : in out Program_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class;
      Option                     : in     Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;
-- with Pre => not Have_Ada_Lib_Program_Options;

   overriding
   procedure Program_Help (      -- common for all programs that use Ada_Lib.Options.GNOGA
      Options                    : in      Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Help_Mode_Type);

   procedure Set_Ada_Lib_Program_Options (
      Options                    : in     Program_Options_Class_Access
   ) with Pre => Options /= Null and then
                 not Have_Ada_Lib_Program_Options;

   overriding
   procedure Trace_Parse (
      Options                    : in out Program_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class);

   overriding
   function Verify_Initialized (
      Options                    : in     Program_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   overriding
   function Verify_Preinitialize (
      Options                    : in     Program_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   function Verify_Postprocess (
      Options                    : in     Program_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   function Verify_Preprocess (
      Options                    : in     Program_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

-- package Nested_Options_Package
--                               is new Verification_Package (
--                                     Abstract_Options_Type);
   -- type used for options nested in other options
   type Nested_Options_Type   is abstract limited new Verification_Options_Type
                                 with null record;

   type Nested_Options_Access is access all Nested_Options_Type;
   type Nested_Options_Class_Access is access all Nested_Options_Type'class;
   type Nested_Options_Constant_Class_Access is access constant Nested_Options_Type'class;

   overriding
   procedure Display_Help (            -- common for all programs that use Ada_Lib.Options.GNOGA
                              -- prints full help, aborts program
     Options                     : in     Nested_Options_Type;  -- only used for dispatch
     Message                     : in     String := "";   -- leave blank no error help
     Halt                        : in     Boolean := True);

   function Get_Ada_Lib_Modifiable_Nested_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Nested_Options_Class_Access
   with pre => Have_Ada_Lib_Nested_Options;

   function Get_Ada_Lib_Read_Only_Nested_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Nested_Options_Constant_Class_Access
   with pre => Have_Ada_Lib_Nested_Options;

   overriding
   procedure Program_Help (
      Options                    : in      Nested_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Ada_Lib.Options.Help_Mode_Type);

   overriding
   function Process_Option (
      Options                    : in out Nested_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class;
      Option                     : in     Option_Type'class
   ) return Boolean;

   procedure Set_Ada_Lib_Nested_Options (
      Options                    : in     Nested_Options_Class_Access
   ) with Pre => Options /= Null and then
                 not Have_Ada_Lib_Nested_Options;

end Ada_Lib.Options.Actual;
