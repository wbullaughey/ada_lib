with Ada_Lib.Options.Verification;
with Ada_Lib.Trace;

-- options for both main program and test case
package Ada_Lib.Options.Program is

   type Program_Options_Type  is limited new Verification.Verification_Options_Type with record
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
   procedure Display_Help (            -- common for all programs that use GNOGA_Options
                              -- prints full help, aborts program
     Options                     : in     Program_Options_Type;  -- only used for dispatch
     Message                     : in     String := "";   -- leave blank no error help
     Halt                        : in     Boolean := True);


   function Get_Modifiable_Program_Options (
      From                       : in  String := Options_Here
   ) return Program_Options_Class_Access;

   function Get_Read_Only_Program_Options (
      From                       : in  String := Options_Here
   ) return Program_Options_Constant_Class_Access;

   overriding
   function Image (
     Options                     : in     Program_Options_Type
   ) return String;

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
      Option                     : in     Base_Flag_Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;
-- with Pre => not Have_Ada_Lib_Program_Options;

   overriding
   procedure Program_Help (      -- common for all programs that use GNOGA_Options
      Options                    : in      Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Help_Mode_Type);

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



end Ada_Lib.Options.Program;
