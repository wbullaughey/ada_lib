with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Options;
with Ada_Lib.Options_Interface;
--with Ada_Lib.Socket_IO;
--with Ada_Lib.Unit_Test.Tests;

package Ada_Lib.GNOGA.Unit_Test.Options is

   Failed                        : exception;

   type GNOGA_Unit_Test_Options_Type
                                 is Limited new Ada_Lib.Options.
                                    Nested_Options_Type with null record;

   type GNOGA_Options_Constant_Class_Access
                                 is access constant GNOGA_Unit_Test_Options_Type'class;
   overriding
   function Initialize (
      Options                    : in out GNOGA_Unit_Test_Options_Type
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (
      Options                    : in out GNOGA_Unit_Test_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.
                                             Abstract_Package.Abstract_Iterator_Type'class;
      Option                     : in     Ada_Lib.Options_Interface.
                                             Option_Type'class
   ) return Boolean
   with pre => Options.Verify_Initialized;

   overriding
   procedure Program_Help (      -- common for all programs that use Ada_Lib.Options.GNOGA
      Options                    : in      GNOGA_Unit_Test_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     Ada_Lib.Options.Help_Mode_Type)
   with pre => Options.Verify_Initialized;

   Debug                         : aliased Boolean := False;
   Debug_Options                 : aliased Boolean := False;
-- GNOGA_Options                 : GNOGA_Options_Constant_Class_Access := Null;

private

   overriding
   procedure Trace_Parse (
      Options                    : in out GNOGA_Unit_Test_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class);

end Ada_Lib.GNOGA.Unit_Test.Options;
