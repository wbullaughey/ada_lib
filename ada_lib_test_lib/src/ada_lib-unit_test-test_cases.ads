with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada_Lib.Options.Actual;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Strings.Unlimited;
with AUnit.Test_Cases;

package Ada_Lib.Unit_Test.Test_Cases is

-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;

   Failed                        : exception;

   package Random_Number_Generator is
      new Ada.Numerics.Discrete_Random (Integer);

   type Random_Generators_Type   is array (Ada_Lib.Options.Unit_Test.
                                    Random_Generator_Index_Type) of
                                       Random_Number_Generator.Generator;

   package Root_Test is
      type Test_Type is abstract new AUnit.Test_Cases.Test_Case with private;

      function Did_Set_Up_Fail (
         Test                       : in     Test_Type
      ) return Boolean;

      overriding
      procedure Set_Up (
         Test                       : in out Test_Type);


      procedure Set_Up_Failed (
         Test                       : in out Test_Type;
         Here                       : in     String := Ada_Lib.Trace.Here);

      overriding
      procedure Tear_Down (
         Test                       : in out Test_Type);

      procedure Tear_Down_Failed (
         Test                       : in out Test_Type;
         Here                       : in     String := Ada_Lib.Trace.Here);

      function Verify_Set_Up (
         Test                       : in     Test_Type
      ) return Boolean;

      function Verify_Tear_Down (
         Test                       : in     Test_Type
      ) return Boolean;

   private

      type Test_Type is abstract new AUnit.Test_Cases.Test_Case with record
         Set_Up_Succeeded           : Boolean := False;
         Set_Up_Failed              : Boolean := False;
         Tear_Down_Failed           : Boolean := False;
         Torn_Down                  : Boolean := False;
      end record;

   end Root_Test;

   type Test_Case_Type is abstract new Root_Test.Test_Type with record
      Async_Failure_Message      : Ada_Lib.Strings.Unlimited.String_Type;
      Failure_From               : Ada_Lib.Strings.Unlimited.String_Type;
      Random_Generators          : Random_Generators_Type;
      Suite_Name                 : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   type Test_Case_Class_Access is access all Test_Case_Type'class;

   overriding
   procedure Add_Routine (
      Test                    : in out Test_Case_Type;
      Val                     : in     AUnit.Test_Cases.Routine_Spec
   ) with pre => Ada_Lib.Options.Actual.Have_Ada_Lib_Program_Options;

   overriding
   procedure Set_Up (
      Test                       : in out Test_Case_Type);

   procedure Set_Up_Exception (
      Test                       : in out Test_Case_Type;
      Fault                      : Ada.Exceptions.Exception_Occurrence;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who);

   procedure Set_Up_Message_Exception (
      Test                       : in out Test_Case_Type;
      Fault                      : Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who);

   procedure Set_Up_Failure (
      Test                       : in     Test_Case_Type;
      Condition                  : in     Boolean;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who;
      Message                    : in     String := "");

-- procedure Tear_Down (
--    Test                       : in out Test_Case_Type)
--
   procedure Tear_Down_Exception (
      Test                       : in out Test_Case_Type;
      Fault                      : Ada.Exceptions.Exception_Occurrence;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who;
      Message                    : in     String := "");

   procedure Tear_Down_Failure (
      Test                       : in out Test_Case_Type;
      Condition                  : in     Boolean;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who;
      Message                    : in     String := "");

-- function Was_There_An_Async_Failure
-- return Boolean;

   Debug                         : Boolean := False;

end Ada_Lib.Unit_Test.Test_Cases;
