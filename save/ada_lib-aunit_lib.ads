with GNAT.Source_Info;

package Ada_Lib.AUnit_Lib is

   procedure List_Suites;

   procedure Suite (
      Name                       : in     String;
      From                       : in     String := GNAT.Source_Info.Source_Location);

end Ada_Lib.AUnit_Lib;
