--with AUnit.Test_Filters;
--with Ada_Lib.Strings.Unlimited;
--with Ada_Lib.Unit_Test.Tests;

-- options for unit test of Ada_Lib
package Ada_Lib.Options.Template is

   Failure                       : exception;

   type Template_Options_Type       is limited new Ada_Lib.Options.Nested_Options_Type
                                    with record
      Debug                      : Boolean := False;
      Compile                    : Boolean := False;
      Evaluate                   : Boolean := False;
      Expand                     : Boolean := False;
      Load                       : Boolean := False;
      Test                       : Boolean := False;
   end record;

   type Template_Options_Constant_Class_Access is access constant Template_Options_Type'class;

   overriding
   function Initialize (
     Options                     : in out Template_Options_Type
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (
      Options                    : in out Template_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class;
      Option                     : in     Ada_Lib.Options_Interface.
                                             Option_Type'class
   ) return Boolean
   with pre => Options.Verify_Initialized;

   overriding
   procedure Trace_Parse (
      Options                    : in out Template_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type'class);

   Template_Options_Constant     : Template_Options_Constant_Class_Access := Null;

private

   overriding
   procedure Program_Help (
      Options                    : in     Template_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     Help_Mode_Type);

end Ada_Lib.Options.Template;
