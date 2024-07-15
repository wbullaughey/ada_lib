with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with Ada_Lib.Unit_Test;
with Ada_Lib.Options.AUnit.Ada_Lib_Tests;
with Ada_Lib.Template.Compile;
with Ada_Lib.Template.Parameters;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
with AUnit.Test_Cases;

package body Ada_Lib.Template.Tests is

   type Test_Type is new Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type with null record;
   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (Test : in out Test_Type);

   procedure Test_Simple_Template (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Simple_Variable_Template (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- procedure Test_Expression_Template (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);
--
-- procedure Test_If_Expression_Template (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Global_Path                   : constant String := "templates/";
   Suite_Name                    : constant String := "Template";

   ---------------------------------------------------------------
   procedure Tester (
      Path                       : in     String) is
   ---------------------------------------------------------------

      Full_Source_Path           : constant String := Global_Path & Path;
      Full_Parameter_Path        : constant String := Global_Path & Path & ".parameters";
      Template                   : Ada_Lib.Template.Compile.Template_Type;
      Raw                        : constant String := Ada_Lib.Template.Compile.Load (Full_Source_Path);
      Parameters                 : constant Ada_Lib.Template.Parameters.Parameter_Array :=
                                    Ada_Lib.Template.Parameters.Load (Full_Parameter_Path);
      Processed                  : constant String := Template.Compile (Raw, Parameters);

   begin
      Log_In (Trace_Test, Quote ("source", Full_Source_Path) &
         Quote ("parameters", Full_Parameter_Path) &
         Quote ("parameters", Raw));
      if Ada_Lib.Options.AUnit.Ada_Lib_Tests.AUnit_Lib_Options.Verbose then
         Put_Line ("---------------------------------------");
         Put_Line (Processed);
         Put_Line ("---------------------------------------");
      end if;
      Log_Out (Trace_Test);

   exception
      when AUnit.Assertions.Assertion_Error =>
         null;

      when Fault: Failed =>
         Trace_Message_Exception (Fault, "template failure in library");
         Assert (False, "library failed");

      when Fault: Not_Found =>
         Trace_Message_Exception (Fault, Quote ("Could not open template file", Full_Source_Path));
         Assert (False, Quote ("Could not open template file", Full_Source_Path));

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Tester;

   ---------------------------------------------------------------
   procedure Test_Simple_Template (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Path                       : constant String := "simple_template";

   begin
      Log_In (Trace_Test, Quote ("path", Path) &
         Quote ("current directory", Ada.Directories.Current_Directory));
      Tester (Path);
      Log_Out (Trace_Test);
   end Test_Simple_Template;

   ---------------------------------------------------------------
   procedure Test_Simple_Variable_Template (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Path                       : constant String := "simple_variable_template";

   begin
      Log_In (Trace_Test, Quote ("path", Path) &
         Quote ("current directory", Ada.Directories.Current_Directory));
      Tester (Path);
      Log_Out (Trace_Test);
   end Test_Simple_Variable_Template;

-- ---------------------------------------------------------------
-- procedure Test_Expression_Template (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
--    pragma Unreferenced (Test);
-- ---------------------------------------------------------------
--
--    Path                       : constant String := "expression_template";
--
-- begin
--    Log_In (Trace_Test, Quote ("path", Path) &
--       Quote ("current directory", Ada.Directories.Current_Directory));
--    Tester (Path);
--    Log_Out (Trace_Test);
-- end Test_Expression_Template;
--
-- ---------------------------------------------------------------
-- procedure Test_If_Expression_Template (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
--    pragma Unreferenced (Test);
-- ---------------------------------------------------------------
--
--    Path                       : constant String := "if_expression_template";
--
-- begin
--    Log_In (Trace_Test, Quote ("path", Path) &
--       Quote ("current directory", Ada.Directories.Current_Directory));
--    Tester (Path);
--    Log_Out (Trace_Test);
-- end Test_If_Expression_Template;

   ---------------------------------------------------------------
   overriding
   function Name (Test : Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (Test : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Simple_Template'access,
         Routine_Name   => AUnit.Format ("Test_Simple_Template")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Simple_Variable_Template'access,
         Routine_Name   => AUnit.Format ("Test_Simple_Variable_Template")));

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Expression_Template'access,
--       Routine_Name   => AUnit.Format ("Test_Expression_Template")));

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_If_Expression_Template'access,
--       Routine_Name   => AUnit.Format ("Test_If_Expression_Template")));

   end Register_Tests;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Suite;

end Ada_Lib.Template.Tests;
