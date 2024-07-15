with Ada.Characters.Latin_1;
with Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with Ada_Lib.Options.AUnit.Ada_Lib_Tests;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.OS;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
with AUnit.Test_Cases;


package body Ada_Lib.Text.Textbelt.Tests is

   use type Ada_Lib.Options.Mode_Type;

   type Test_Type is new Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type with null record;
   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   procedure Send_Text_Invalid_Number (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Send_Text_Valid_Number (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   overriding
   procedure Register_Tests (Test : in out Test_Type);

   procedure Test_Parse (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Phone_Number                  : constant String := "4846787757";
   Suite_Name                    : constant String := "Textbelt";

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

   use Ada_Lib.Options.Unit_Test;

      Options                 : Ada_Lib.Options.Unit_Test.
                                 Unit_Test_Options_Type'class renames
                                    Ada_Lib.Options.Unit_Test.
                                       Unit_Test_Options_Constant.all;
      Listing_Suites             : constant Boolean :=
                                    Options.Mode /= Ada_Lib.Options.Run_Tests;
      Star_Names                 : constant String :=
                                    (if Listing_Suites then "*" else "");
   begin
      if Listing_Suites or else
            Options.Suite_Set (Ada_Lib.Options.Unit_Test.Textbelt) then
         Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
            Routine        => Send_Text_Valid_Number'access,
            Routine_Name   => AUnit.Format ("Send_Text_Valid_Number" & Star_Names)));

         Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
            Routine        => Send_Text_Invalid_Number'access,
            Routine_Name   => AUnit.Format ("Send_Text_Invalid_Number" & Star_Names)));

         Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
            Routine        => Test_Parse'access,
            Routine_Name   => AUnit.Format ("Test_Parse" & Star_Names)));
      end if;
   end Register_Tests;

   ---------------------------------------------------------------
   procedure Send_Text_Invalid_Number(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Options                    : Ada_Lib.Options.AUnit.Ada_Lib_Tests.Aunit_Options_Type'class renames
                                       Ada_Lib.Options.AUnit.Ada_Lib_Tests.
                                          Aunit_Options_Constant_Class_Access (
                                             Ada_Lib.Options.Program_Options).all;
   begin
      Send ("9999999999", "hello", Options.Verbose);
      Assert (False, "send did not fail but should have");

   exception
      when Fault: Failed =>
         Trace_Message_Exception (Fault, "send failed as expected");

      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Send_Text_Invalid_Number;

   ---------------------------------------------------------------
   procedure Send_Text_Valid_Number(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Options                    : Ada_Lib.Options.AUnit.Ada_Lib_Tests.Aunit_Options_Type'class renames
                                       Ada_Lib.Options.AUnit.Ada_Lib_Tests.
                                          Aunit_Options_Constant_Class_Access (
                                             Ada_Lib.Options.Program_Options).all;
   begin
      Send (Phone_Number, "hello", Options.Verbose);

   exception
      when Fault: others =>
         Trace_Message_Exception (Fault, "error in library");
         Assert (False, "library failed");

   end Send_Text_Valid_Number;

-- ---------------------------------------------------------------
-- procedure Set_Up (Test : in out Test_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log (Debug, Here, Who);
-- end Set_Up;

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

-- ---------------------------------------------------------------
-- procedure Tear_Down (Test : in out Test_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log (Debug, Here, Who);
-- end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Parse (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

      type Item_Type            is record
         Name                    : Ada_Lib.OS.Temporary_File_Name;
         Result                  : Boolean;
      end record;

      Response_File_Names        : constant array (Positive range <>) of
                                    Item_Type := (
                                       (
                                          Name     => "GNAT-g7Rbzp" &
                                                      Ada.Characters.Latin_1.Nul,
                                          Result   => True
                                       ),
                                       (
                                          Name     => "GNAT-Nua1FP" &
                                                      Ada.Characters.Latin_1.Nul,
                                          Result   => True
                                       ),
                                       (
                                          Name     => "GNAT-6V29Rf" &
                                                      Ada.Characters.Latin_1.Nul,
                                          Result   => False
                                       )
                                    );
   begin
      Log_In (Debug);

      for Index in Response_File_Names'range loop
         declare
            Item                 : Item_Type renames Response_File_Names (Index);
            Quota                : Natural := 0;
            Response_File        : Ada.Text_IO.File_Type;
            Success              : Boolean := False;

         begin
            Ada.Text_IO.Open (Response_File, Ada.Text_IO.In_File,
               "tests/data/" & Item.Name);

            while not Ada.Text_IO.End_Of_File (Response_File) loop
               declare
                  Response       : constant String :=
                                    Ada.Text_IO.Get_Line (Response_File);

               begin
                  if Debug then
                     Log_Here ("expected result " & Item.Result'img);
                     Ada.Text_IO.Put_Line (Quote ("Response", Response));
                  end if;

                  Parse (Response, Success, Quota);
                  Log_Out (Debug, "success " & Success'img &
                     " quota " & Quota'img);

               exception
                  when Fault: Failed =>
                     Trace_Exception (Fault, Here);
                     Assert (False, "unexpected exception");
--
--                   if Item.Result then
--                      Assert (False, "success should not be set");
--                      Assert (Quota = 0, "zero quota");
--                   end if;
               end;

            end loop;

            Assert (Success = Item.Result, "success " & Success'img &
               " not expected value " & Item.Result'img &
               " for file " & Item.Name);
            Assert ((if Item.Result then
                  Quota > 0
               else
                  Quota = 0
               ),
               " quota value" & Quota'img & " wrong");

            Ada.Text_IO.Put_Line ("item" & Index'img &
               " success " & Success'img &
               " quota" & Quota'img);

         end;
      end loop;

-- exception
--    when Fault: others =>
--       Trace_Message_Exception (Fault, "error in library");
--       Assert (False, "library failed");

   end Test_Parse;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Ada_Lib.Text.Textbelt.Tests;
