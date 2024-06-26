with GNAT.Source_Info;

package Ada_Lib.AUnit_Lib is

   function Did_Fail return Boolean;

   procedure List_Suites;

   procedure Set_Failed (
      From                       : in     String := Ada_Lib.Trace.Here);

   procedure Suite (
      Name                       : in     String;
      From                       : in     String := GNAT.Source_Info.Source_Location);

   function Test_Name (
      Suite_Name                 : in     String;
      Routine_Name               : in     String
   ) return String;

   Debug                         : aliased Boolean := False;

end Ada_Lib.AUnit_Lib;
