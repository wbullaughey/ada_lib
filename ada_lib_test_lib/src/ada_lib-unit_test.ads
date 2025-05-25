with Ada.Exceptions;
with Ada_Lib.Options;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package Ada_Lib.Unit_Test is

   Failed                        : exception;

   type Suite_Action_Access      is access procedure (
      Suite                      : in     String;
      First                      : in out Boolean;
      Mode                       : in     Ada_Lib.Options.Mode_Type);

   type Routine_Action_Access    is access procedure (
      Suite                      : in     String;
      Routine                    : in     String;
      Mode                       : in     Ada_Lib.Options.Mode_Type);

   function Did_Fail return Boolean;

   procedure Exception_Assert (
      Fault                      : Ada.Exceptions.Exception_Occurrence;
      Here                       : in     String := Ada_Lib.Trace.Here;
      Who                        : in     String := Ada_Lib.Trace.Who);

   function Has_Test (
      Suite_Name                 : in     String;
      Routine_Name               : in     String
   ) return Boolean;

   procedure Iterate_Suites (
      Suite_Action               : in     Suite_Action_Access;
      Routine_Action             : in     Routine_Action_Access;
      Mode                       : in     Ada_Lib.Options.Mode_Type);

   procedure Routine (
      Suite_Name                 : in     String;
      Routine_Name               : in     String);

-- procedure Run_Tests;

   procedure Set_Failed (
      From                       : in     String := Ada_Lib.Trace.Here);

   procedure Suite (
      Name                       : in     String);

   function Test_Name (
      Suite_Name                 : in     String;
      Routine_Name               : in     String
   ) return String;

   Debug                         : Boolean := False;

end Ada_Lib.Unit_Test;
