with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with Ada_Lib.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
with Ada.Text_IO;
with Ada_Lib.Unit_Test.Test_Cases;
with AUnit.Test_Cases;

package body Ada_Lib.Test.Ada_Lib_Dynamic_String.Tests is

   type Test_Type is new Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type with record
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

   overriding
   procedure Tear_Down (Test : in out Test_Type)
   with post => Verify_Set_Up (Test);

   procedure Text_IO_Operations(
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   subtype Word_Type          is String (1 .. 10);
   type Words_Array           is array (1 .. 5) of Word_Type;

   subtype String_Type        is Ada_Lib.Strings.Unlimited.String_Type;

   Comment                    : constant String := "#";
   Debug              : constant Boolean := False;
   Extra_Word                 : constant String := "extra word";
   Path                       : constant String := "dynamic_string_test_case.txt";
   Quoted_Value               : constant String := "x#,;";
   Quotes                     : constant String := "'";
   Seperators                 : constant String := ",.";
   Terminators                : constant String := ";";
   Test_Seperators            : array (Words_Array'range) of character;
   Words                      : Words_Array := (others => Word_Type'length * ' ');

   ---------------------------------------------------------------
   procedure Basic_Operations(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Appendix                   : constant String := "xyz";
      Source                     : constant String := "abc";
      Padded                     : constant String := "  " & Source & " ";
      Coerce_Target              : constant String_Type := Coerce (Source);
      Append_Target              : String_Type := Coerce (Source);
      Padded_Target              : constant String_Type := Coerce (Padded);

   begin
      Put_Line ("Basic Operations");

      Assert (Source = Coerce_Target.Coerce, "convert to and from works");
      Assert (Source'length = Coerce_Target.Length, "length works");
      Append_Target.Append (Appendix);
      Assert (Source & Appendix = Append_Target.Coerce, "string append works");
      Append_Target.Append (Coerce_Target);
      Assert (Source & Appendix & Source = Append_Target.Coerce, "string class append works");
      Append_Target.Append ('x');
      Assert (Source & Appendix & Source & 'x' = Append_Target.Coerce, "character append works");
      Assert (Padded_Target.Trim (Ada.Strings.Both) = Source, "timming works");
   end Basic_Operations;

   ---------------------------------------------------------------
   function Name (Test : Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   procedure Register_Tests (Test : in out Test_Type) is
   ---------------------------------------------------------------

   begin
-- Log_Here ("enter");
      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Basic_Operations'access,
         Routine_Name   => AUnit.Format ("Basic_Operations")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Text_IO_Operations'access,
         Routine_Name   => AUnit.Format ("Text_IO_Operations")));

-- Log_Here ("exit");
   end Register_Tests;

   ---------------------------------------------------------------
   procedure Set_Up (Test : in out Test_Type) is
   ---------------------------------------------------------------

      Seperator               : character;

   begin
-- Log (Here, Who);
      Log (Debug, Here, Who);
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;

      Create (Test.File, Out_File, Path);

      for Index in Words'range loop
         if Index = 3 then    -- quoted value
            Overwrite  (Words (Index), 1,  Quoted_Value);
            Put (Test.File, "'" & Words (Index) & "'");
         else
            Overwrite  (Words (Index), 1, "abc" & Index'img);
            Put (Test.File, Words (Index));
         end if;
         Log (Debug, Here, Who & " word" & Index'img & " = '" & Words (Index) & "'");
         case Index is

            when 1 =>
               Seperator := ',';
               Put (Test.File, Seperator);


            when 2 =>
               Seperator := ',';
               Put_Line (Test.File, Seperator & Comment & " this is a comment following a word");

            when 3 =>
               Seperator := '.';
               New_Line (Test.File);
               Put (Test.File, Seperator); New_Line (Test.File);

            when 4 =>
               Seperator := '.';
               New_Line (Test.File);
               Put (Test.File, Seperator); New_Line (Test.File);
               New_Line (Test.File);

            when 5 =>
               Seperator := ';';
               New_Line (Test.File);
               Put_Line (Test.File, "# this is a commane on a line by its self");
               Put (Test.File, Seperator); New_Line (Test.File);

         end case;
         Test_Seperators (Index) := Seperator;
      end loop;
      Put_Line (Test.File, Extra_Word);
      Close (Test.File);
-- Log (Here, Who);
   end Set_Up;

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

   ---------------------------------------------------------------
   procedure Tear_Down (Test : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log (Debug, Here, Who);
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
   end Tear_Down;

   ---------------------------------------------------------------
   procedure Text_IO_Operations(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
   ---------------------------------------------------------------

      procedure Test_Case (
         Trim                    : in     Boolean) is


         Stream                     : Token_Stream_Class;
         Count                      : Natural := 0;

      begin
         Log_In (Debug, "test Word Operations with trim " & Trim'img);
         Stream.Open (Path, Seperators, Terminators, Quotes, Comment, Trim);

         while Stream.Have_More loop
            Count := Count + 1;
            declare
               Found                : constant String := Stream.Get;
               Seperator            : constant Character := Stream.Last_Seperator;
               Expected             : constant String := (if Trim then
                                          Ada.Strings.Fixed.Trim (Words (Count), Ada.Strings.Both)
                                       else
                                          Words (Count));

            begin
               Log (Debug or Expected /= Found, Here, Who & " expected '" & Expected & "' got '" & Found & "'");
               Assert (Expected = Found, "words match");

               Log (Debug or Test_Seperators (Count) /= Seperator,
                    Here, Who & " expected '" & Test_Seperators (Count) &
                     "' got '" & Seperator & "'");
               Assert (Test_Seperators (Count) = Seperator, "seperators match");
            end;
         end loop;
         Assert (Words'length = Count, "correct number of words");
         Stream.Clear_Terminator;
         Assert (Stream.Have_More, "more after terminator");
         Assert (Stream.Get = Extra_Word, "got word after terminator");
         Stream.Close;
      end Test_Case;

   begin
         Test_Case (False);
         Test_Case (True);
   end Text_IO_Operations;

   -------------------
begin
if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Ada_Lib.Test.Ada_Lib_Dynamic_String.Tests;
