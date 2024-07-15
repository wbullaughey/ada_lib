with Ada_Lib.Database.Connection;
with Ada_Lib.Options.Database;
with Ada_Lib.GNOGA.Unit_Test.Options;
with Ada_Lib.Options.Template;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
with AUnit.Ada_Lib.Options;
with AUnit.Test_Suites;

-- options for unit tests of Ada_Lib
package Ada_Lib.Options.AUnit.Ada_Lib_Tests is

   type DBDamon_Test_Suite       is new Standard.AUnit.Test_Suites.Test_Suite
                                    with null record;

   type DBDamon_Test_Access      is access DBDamon_Test_Suite;

   function New_Suite return DBDamon_Test_Access;

   type Non_DBDamon_Test_Suite   is new Standard.AUnit.Test_Suites.Test_Suite with null record;

   type Non_DBDamon_Test_Access  is access Non_DBDamon_Test_Suite;

   function Has_Database return Boolean;

   function New_Suite return Non_DBDamon_Test_Access;

   -- type used in application for unit testing;
   type Aunit_Options_Type       is new Program_Options_Type with record
      AUnit_Options              : Standard.AUnit.Ada_Lib.Options.AUnit_Options_Type;
      Database                   : Ada_Lib.Database.Connection.
                                    Abstract_Database_Class_Access := Null;
      Database_Options           : Ada_Lib.Options.Database.Database_Options_Type;
      GNOGA_Unit_Test_Options    : Ada_Lib.GNOGA.Unit_Test.Options.
                                    GNOGA_Unit_Test_Options_Type;
      Template                   : Ada_Lib.Options.Template.Template_Options_Type;
      Tester_Debug               : Boolean := False;
      Unit_Test                  : Ada_Lib.Options.Unit_Test.
                                    Unit_Test_Options_Type (True);
   end record;

   type Aunit_Options_Class_Access
                                 is access all Aunit_Options_Type'class;
   type Aunit_Options_Constant_Class_Access
                                 is access constant Aunit_Options_Type'class;

   Failure                       : exception;

   function Get_Modifiable_AUnit_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Aunit_Options_Class_Access
   with pre => AUnit_Lib_Options /= Null;

   overriding
   function Initialize (
     Options                     : in out Aunit_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   procedure Set_Options;

   AUnit_Lib_Options             : Aunit_Options_Constant_Class_Access := Null;
   Debug                         : aliased Boolean := False;

private

   overriding
   procedure Program_Help (
      Options                    : in     Aunit_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     Help_Mode_Type);

   overriding
   function Process_Option (
      Options                    : in out Aunit_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class;
      Option                     : in     Ada_Lib.Options_Interface.
                                             Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   procedure Register_Tests (
      Options                    : in     Aunit_Options_Type;
      Suite_Name                 : in     String;
      Test                       : in out Ada_Lib.Unit_Test.Test_Cases.
                                             Test_Case_Type'class);
   overriding
   procedure Trace_Parse (
      Options                    : in out Aunit_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.
                                             Abstract_Package.
                                                Abstract_Iterator_Type'class);

end Ada_Lib.Options.AUnit.Ada_Lib_Tests;
