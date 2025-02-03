with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Ada_Lib.Unit_Test.Tests;

-- package for testing server side socket io
package Ada_Lib.Socket_IO.Server.Tests is

   package Parent is

      type Parent_Test_Type   is abstract new Ada_Lib.Unit_Test.Tests.
                                 Test_Case_Type with null record;

      overriding
      function Name (
         Test                 : Parent_Test_Type
      ) return AUnit.Message_String;

   end Parent;

   -- package testing socket bind
   package Bind is
      First_Socket_Description      : aliased constant String :=
                                       "first socket";
      Second_Socket_Description     : aliased constant String :=
                                       "second socket";
      Test_Port                     : constant GNAT.Sockets.Port_Type := 12345;

      type Bind_Test_Type     is new Parent.Parent_Test_Type
                                 with null record;

   private

      overriding
      procedure Register_Tests (Test : in out Bind_Test_Type);

      procedure Test_Bind (
         Test                       : in out AUnit.Test_Cases.Test_Case'class);

   end Bind;

   -- package for testing server side library routines other than bind
   package Server is

      type Server_Test_Type   is new Parent.Parent_Test_Type
                                 with null record;
      overriding
      procedure Register_Tests (Test : in out Server_Test_Type);

   end Server;

   -- creates a test suite
   -- adds a test list for bind and server tests to the test suite
   function Suite
   return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "Socket_IO_Server";

end Ada_Lib.Socket_IO.Server.Tests;
