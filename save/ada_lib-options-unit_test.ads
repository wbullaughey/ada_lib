--with Ada_Lib.Options.Aunit_Lib;
--with Ada_Lib.Options.Database;
--with Ada_Lib.Options.GNOGA;
with Ada_Lib.Command_Line_Iterator;
--with Ada_Lib.Trace;
--with Ada_Lib.GNOGA.Unit_Test;
with Ada_Lib.Strings.Unlimited;
with AUnit.Test_Filters;

-- provides options for all unit testing
package Ada_Lib.Options.Unit_Test is

   Failed                        : exception;

   type Suites_Type              is (Database_Server, Textbelt);

   type Suite_Set_Type           is array (Suites_Type) of Boolean;

-- type Mode_Type                is (Driver_Suites, List_Suites, Print_Suites, Run_Tests);
--
   -- base type for all unit test programs
-- type Unit_Test_Options_Type ( -- Type to be used by all unit tests which include ada_lib
--    Multi_Test                 : Boolean -- perform multiple tests in one execution of test program
--                                  ) is Limited new Program_Options_Type with record
--    Debug                      : Boolean := False;  -- debug unit test application
--    Debug_Options              : Boolean := False;  -- debug unit test application options
--    Exit_On_Done               : Boolean := False;  -- exit test application after all unit tests complete
--    Filter                     : aliased Standard.AUnit.Test_Filters.Name_Filter;
--    Mode                       : Mode_Type := Run_Tests;  -- run unit tests
--    Manual                     : Boolean := False;  -- GUI interactions must be performed manually
--    Random_Seed                : Integer;
--    Report_Random              : Boolean := False;
--    Routine                    : Ada_Lib.Strings.Unlimited.String_Type;
--    Set_Random_Seed            : Boolean := False;
--    Suite_Name                 : Ada_Lib.Strings.Unlimited.String_Type;
--    Suite_Set                  : Suite_Set_Type := (others => False);
-- end record;
--
-- type Unit_Test_Options_Class_Access
--                               is access all Unit_Test_Options_Type'class;
-- type Unit_Test_Options_Constant_Class_Access
--                               is access constant Unit_Test_Options_Type'class;

-- -- call this for unit tests that cannot run multiple tests at one time
-- procedure Check_Test_Suite_And_Routine (
--    Options                    : in     Unit_Test_Options_Type);
--
-- overriding
-- function Initialize (
--   Options                     : in out Unit_Test_Options_Type
-- ) return Boolean
-- with pre => Options.Verify_Preinitialize;

-- overriding
-- function Process_Option (  -- process one option
--   Options                     : in out Unit_Test_Options_Type;
--   Iterator                    : in out Ada_Lib.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class;
--    Option                     : in     Ada_Lib.Options_Interface.
--                                           Option_Type'class
-- ) return Boolean
-- with pre => Options.Verify_Initialized;

-- overriding
-- procedure Update_Filter (
--    Options                    : in out Unit_Test_Options_Type);
--
-- procedure Routine_Action (
--    Suite                      : in     String;
--    Routine                    : in     String;
--    Mode                       : in     Mode_Type);
--
-- procedure Suite_Action (
--    Suite                      : in     String;
--    First                      : in out Boolean;
--    Mode                       : in     Mode_Type);
--
-- Unit_Test_Options             : Unit_Test_Options_Constant_Class_Access
--                                  := Null;
--private

-- overriding
-- procedure Program_Help (
--    Options                    : in     Unit_Test_Options_Type;  -- only used for dispatch
--    Help_Mode                  : in     Ada_Lib.Options.Help_Mode_Type);
--
-- overriding
-- procedure Trace_Parse (
--    Options                    : in out Unit_Test_Options_Type;
--    Iterator                   : in out Ada_Lib.Command_Line_Iterator.
--                                  Abstract_Package.Abstract_Iterator_Type'class);

end Ada_Lib.Options.Unit_Test;
