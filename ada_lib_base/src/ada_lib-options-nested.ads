--with Ada_Lib.Options.Flags;
with Ada_Lib.Options.Verification;
with Ada_Lib.Trace;

package Ada_Lib.Options.Nested is

   function Have_Ada_Lib_Nested_Options (
      Report      : Boolean := True
   ) return Boolean;

   -- type used for options nested in other options
   type Nested_Options_Type   is limited new Verification.
                                 Verification_Options_Type with null record;

   type Nested_Options_Access is access all Nested_Options_Type;
   type Nested_Options_Class_Access is access all Nested_Options_Type'class;
   type Nested_Options_Constant_Class_Access is access constant Nested_Options_Type'class;

   overriding
   procedure Display_Help (            -- common for all programs that use GNOGA_Options
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
   function Image (
     Options                     : in     Nested_Options_Type
   ) return String;

   overriding
   procedure Program_Help (
      Options                    : in      Nested_Options_Type;  -- only used for dispatch
      Help_Mode                  : in      Ada_Lib.Options.Help_Mode_Type);

   overriding
   function Process_Option (
      Options                    : in out Nested_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class;
      Option                     : in     Base_Flag_Option_Type'class
   ) return Boolean;

   procedure Set_Ada_Lib_Nested_Options (
      Options                    : in     Nested_Options_Class_Access
   ) with Pre => Options /= Null and then
                 not Have_Ada_Lib_Nested_Options (False);

end Ada_Lib.Options.Nested;

