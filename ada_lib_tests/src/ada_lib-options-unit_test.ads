with Ada_Lib.Options.Actual;
with Ada_Lib.Options.GNOGA;
with Ada_Lib.Strings.Unlimited;
with AUnit.Test_Filters.Ada_Lib;

--with Ada_Lib.Database.Connection;
--with GNAT.Source_Info;

-- provides options for all unit testing
package Ada_Lib.Options.Unit_Test is

   Failed                        : exception;

   Maximum_Random_Generators     : constant := 5;
   type Random_Generator_Number_Type
                                 is new Natural range 0 ..
                                    Maximum_Random_Generators;
   subtype Random_Generator_Index_Type
                                 is Random_Generator_Number_Type range 1 ..
                                    Maximum_Random_Generators;
   type Random_Seed_Mode_Type   is (Default_Seed, Random_Seed,
                                    Seed_Not_Set, Specified_Seed);

   type Random_Seeds_Type        is array (Random_Generator_Index_Type) of
                                    Integer;

   type Suites_Type              is (Database_Server, Textbelt);

   type Suite_Set_Type           is array (Suites_Type) of Boolean;

   Default_Random_Seed           : constant := 0;

   -- base type for all unit test programs which -- include ada_lib
   type Ada_Lib_Unit_Test_Options_Type (
      Multi_Test        : Boolean -- perform multiple tests in one
                                   -- execution of test program
                           ) is new Ada_Lib.Options.Actual.
                              Program_Options_Type with record
      Debug             : Boolean := False;  -- debug unit test application
      Debug_Options     : Boolean := False;  -- debug unit test options
      Exit_On_Done      : Boolean := False;  -- exit test application after
                                             -- all unit tests complete
      Filter            : aliased AUnit.Test_Filters.Ada_lib.Ada_Lib_Filter;
      GNOGA_Options     : Ada_Lib.Options.GNOGA.GNOGA_Options_Type;
      Mode              : Mode_Type := Run_Tests;  -- run unit tests
      Manual            : Boolean := False;  -- GUI interactions must be
                                             -- performed manually
      Number_Random_Generators
                        : Random_Generator_Number_Type := 0;
      Random_Seeds      : Random_Seeds_Type := (others => Default_Random_Seed);
      Random_Seed_Count : Random_Generator_Number_Type := 0;
      Random_Seed_Mode  : Random_Seed_Mode_Type := Seed_Not_Set;
      Report_Random     : Boolean := False;
      Routine           : Ada_Lib.Strings.Unlimited.String_Type;
      Suite_Name        : Ada_Lib.Strings.Unlimited.String_Type;
      Suite_Set         : Suite_Set_Type := (others => False);
   end record;

   type Ada_Lib_Unit_Test_Options_Class_Access
                                 is access all Ada_Lib_Unit_Test_Options_Type'class;
   type Ada_Lib_Unit_Test_Options_Constant_Class_Access
                                 is access constant Ada_Lib_Unit_Test_Options_Type'class;

   -- call this for unit tests that cannot run multiple tests at one time
   procedure Check_Test_Suite_And_Routine (
      Options                    : in     Ada_Lib_Unit_Test_Options_Type);

   overriding
   function Initialize (
     Options                     : in out Ada_Lib_Unit_Test_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   procedure Post_Process (      -- final initialization
     Options                    : in out Ada_Lib_Unit_Test_Options_Type);

   overriding
   function Process_Option (  -- process one option
     Options                     : in out Ada_Lib_Unit_Test_Options_Type;
     Iterator                    : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean
   with pre => Options.Verify_Initialized;

-- function Options (
--    From                       : in     String :=
--                                           Standard.GNAT.Source_Info.Source_Location
-- ) return Unit_Test_Options_Constant_Class_Access;

   overriding
   procedure Update_Filter (
      Options                    : in out Ada_Lib_Unit_Test_Options_Type);

   procedure Routine_Action (
      Suite                      : in     String;
      Routine                    : in     String;
      Mode                       : in     Mode_Type);

   procedure Suite_Action (
      Suite                      : in     String;
      First                      : in out Boolean;
      Mode                       : in     Mode_Type);

-- Ada_Lib_Unit_Test_Options_Constant_Class_Access    : Unit_Test_Options_Constant_Class_Access := Null;
private

   overriding
   procedure Program_Help (
      Options                    : in     Ada_Lib_Unit_Test_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     Ada_Lib.Options.Help_Mode_Type);

   overriding
   procedure Trace_Parse (
      Options              : in out Ada_Lib_Unit_Test_Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class);

end Ada_Lib.Options.Unit_Test;
