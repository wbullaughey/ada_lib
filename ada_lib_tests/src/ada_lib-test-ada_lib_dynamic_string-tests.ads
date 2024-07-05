--
--  Copyright (C) 2008, AdaCore
--
with Ada.Text_IO;
with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Ada_Lib.Unit_Test.Tests;
-- with Ada_Lib.Unit_Test.Test_Cases;


package Ada_Lib.Test.Ada_Lib_Dynamic_String.Tests is

   type Test_Type is new Ada_Lib.Unit_Test.Tests.Test_Case_Type with record
      File                    : Ada.Text_IO.File_Type;
   end record;

   type Test_Access is access Test_Type;

   procedure Basic_Operations (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   function Name (Test : Test_Type) return AUnit.Message_String;

   procedure Register_Tests (Test : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type
   ) with Pre => Test.Verify_Pre_Setup,
          Post => Test.Verify_Post_Setup;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (Test : in out Test_Type)
   with post => Verify_Set_Up (Test);

   procedure Text_IO_Operations(
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

end Ada_Lib.Test.Ada_Lib_Dynamic_String.Tests;
