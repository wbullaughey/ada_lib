with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Source_Info;

package body Ada_Lib_Environment is

-- type String_Constant_Access   is access constant String;
-- type Record_Type              is record
--    Variable                   : String_Constant_Access;
--    True_Value                 : String_Constant_Access;
--    Valse_Value                : String_Constant_Access;
-- end record;
--
-- type Array_Type               is array (Environment_Variable_Kind_Type) of Record_Type;

----------------------------------------------------------------------------
   function Parse_Environment_Variable (
      Kind  : in     Environment_Variable_Kind_Type
   ) return Boolean is
----------------------------------------------------------------------------

   -- False_Value : constant array (Environment_Variable_Kind_Type) of
   --                String_Constant_Access := (
   --                new String'("FALSE"),      -- help test
   --                new String'("HELP TEST")   -- Unit Test
   --             );
   -- True_Value : constant array (Environment_Variable_Kind_Type) of
   --                String_Constant_Access := (
   --                new String'("TRUE"),    -- Help Test
   --                new String'("execute")  -- Unit Test-- Help Test
   --             );                                     -- Unit Test
   -- Variable : constant array (Environment_Variable_Kind_Type) of
   --                String_Constant_Access := (
   --                new String'("BUILD_MODE"),
   --                new String'("UNIT_TEST")
   ----             );
   --
   --   Table    : constant Array_Type := (
   --      Help_Test_Kind => (
   --         Variable    =>

      Build_Mode  : constant String := Ada.Environment_Variables.Value (
                     "BUILD_MODE", "execute");
      Unit_Test   : constant String := Ada.Environment_Variables.Value (
                     "UNIT_TEST", "FALSE");

      -------------------------------------------------------------------
      procedure Bad_Value (
         Variable          : in     String;
         Value             : in     String;
         From              : in     String) is
      -------------------------------------------------------------------

      begin
         Put ("unexpected value: '" & Value & "' for " &
            Variable & " for kind " & Kind'img);
         if Debug then
            Put ("from " & From &
               "at " & GNAT.Source_Info.Source_Location);
         end if;
         New_Line;
         GNAT.OS_Lib.OS_Exit (-1);
      end Bad_Value;
      -------------------------------------------------------------------

         Result      : Boolean;

      begin
         if Debug then
            Put_Line ("kind " & Kind'img & " build mode '" & Build_Mode &
               "' unit test '" & Unit_Test & "' from " &
               GNAT.Source_Info.Source_Location);
         end if;

         case Kind is

            when Help_Test_Kind =>
               if Build_Mode = "help_test" then
                  if Unit_Test = "FALSE" then   -- unit_test is set but not used
                     Result :=  True;
                  elsif Unit_Test = "TRUE" then
                     Result :=  True;
                  else
                     Bad_Value ("UNIT_TEST", Unit_Test,
                        GNAT.Source_Info.Source_Location);
                  end if;
               elsif Build_Mode = "execute" then
                  Result :=  False;
               else
                  Bad_Value ("BUILD_MODE", Build_Mode,
                     GNAT.Source_Info.Source_Location);
               end if;

            when Unit_Test_Kind =>
               if Build_Mode = "execute" then
                  if Unit_Test = "TRUE" then
                     Result :=  True;
                  elsif Unit_Test = "FALSE" then
                     Result :=  False;
                  else
                     Bad_Value ("UNIT_TEST", Unit_Test,
                        GNAT.Source_Info.Source_Location);
                  end if;
               elsif Build_Mode = "HELP_MODE" then
                  Result :=  False;
               else
                  Bad_Value ("BUILD_MODE", Build_Mode,
                     GNAT.Source_Info.Source_Location);
               end if;
         end case;

         if Debug then
            Put_Line ("result " & Result'img & " for kind " & Kind'img &
               "' from " & GNAT.Source_Info.Source_Location);
         end if;
         return Result;

      exception
         when Fault : others =>
            Put_Line ("exception: " & Ada.Exceptions.Exception_Name (Fault));
            Put_Line ("exception message: " & Ada.Exceptions.Exception_Message (Fault));
            GNAT.OS_Lib.OS_Exit (-1);

   end Parse_Environment_Variable;

begin
--Debug := True;
   if Debug then
      Put_Line (GNAT.Source_Info.Source_Location);
   end if;

end Ada_Lib_Environment;

