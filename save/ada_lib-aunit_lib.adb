with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.AUnit_Lib is

   type String_Access            is access constant String;

   type Suites_Type              is record
      From                       : String_Access;
      Suite                      : String_Access;
   end record;

   -----------------------------------------------------------
   function Less_Than (
      Left, Right                : in     Suites_Type
   ) return Boolean is
   -----------------------------------------------------------

   begin
      return Left.Suite.all < Right.Suite.all;
   end Less_Than;

   package Suites_Package        is new Ada.Containers.Ordered_Sets (
      Element_Type      => Suites_Type,
      "<"               => Less_Than,
      "="               => "=");

   Suites                        : Suites_Package.Set;

   -----------------------------------------------------------
   procedure List_Suites is
   -----------------------------------------------------------

   begin
      Put_Line ("test suites: ");
      for Element of Suites loop
         Put_Line ("  " & Element.Suite.all);
      end loop;
   end List_Suites;

   -----------------------------------------------------------
   procedure Suite (
      Name                       : in     String;
      From                       : in     String := GNAT.Source_Info.Source_Location) is
   -----------------------------------------------------------

   begin
      Suites_Package.Insert (Suites, Suites_Type'(
         From     => new String'(From),
         Suite    => new String'(Name)));
   end Suite;

end Ada_Lib.AUnit_Lib;
