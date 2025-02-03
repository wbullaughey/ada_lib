--with Ada.Exceptions;
with AUnit.Assertions; use AUnit.Assertions;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;

package body Ada_Lib.Socket_IO.Server.Tests is

   ---------------------------------------------------------------
   function Suite
   return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite     : constant AUnit.Test_Suites.Access_Test_Suite :=
                        new AUnit.Test_Suites.Test_Suite;

      Bind_Test      : constant Ada_Lib.Unit_Test.Test_Cases.
                        Test_Case_Class_Access :=
                           Ada_Lib.Unit_Test.Test_Cases.
                              Test_Case_Class_Access'(new Bind.Bind_Test_Type);
      Server_Test    : constant Ada_Lib.Unit_Test.Test_Cases.
                        Test_Case_Class_Access :=
                           Ada_Lib.Unit_Test.Test_Cases.
                              Test_Case_Class_Access'(
                                 new Server.Server_Test_Type );

   begin
      Log_Here (Debug);
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Bind_Test);
      Test_Suite.Add_Test (Server_Test);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   package body Bind is

      ---------------------------------------------------------------
      overriding
      procedure Register_Tests (
         Test                       : in out Bind_Test_Type) is
      ---------------------------------------------------------------

      begin
         Log_In (Debug);

         Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
            Routine        => Test_Bind'access,
            Routine_Name   => AUnit.Format ("Test_Bind")));

         Log_Out (Debug);
      end Register_Tests;

      --------------------------------------------------------------
      procedure Test_Bind (
         Test                       : in out AUnit.Test_Cases.Test_Case'class) is
      pragma Unreferenced (Test);
      ---------------------------------------------------------------

      begin
         Log_In (Debug);
         begin
            declare     -- test 2 sockets trying to bind to same socket
               First_Socket      : Server_Socket_Type (
                                    First_Socket_Description'access, Test_Port);
               Second_Socket     : Server_Socket_Type (
                                       Second_Socket_Description'access,
                                       Test_Port);
               pragma Unreferenced (First_Socket, Second_Socket);

            begin
               Assert (False, "second socket declaration should fail");
            end;

         exception
            when Fault: others =>
               Trace_Message_Exception (Debug, Fault,
                  "expected second Bind failed");

         end;

         begin
            declare     -- test to make sure can reuse port
               First_Socket      : Server_Socket_Type (
                                    First_Socket_Description'access, Test_Port);
               pragma Unreferenced (First_Socket);
            begin
               Log_Here (Debug, "reuse port worked");
            end;

         exception
            when Fault: others =>
               Trace_Message_Exception (Debug, Fault, "reuse Bind failed");
               Assert (False, "reuse bind failed");

         end;
         Log_Out (Debug);

      end Test_Bind;
   end Bind;

   ---------------------------------------------------------------
   package body Parent is

      ---------------------------------------------------------------
      overriding
      function Name (
         Test     : Parent_Test_Type
      ) return AUnit.Message_String is
      pragma Unreferenced (Test);
      ---------------------------------------------------------------

      begin
         return AUnit.Format (Suite_Name);
      end Name;

   end Parent;

   ---------------------------------------------------------------
   package body Server is

      --------------------------------------------------
      overriding
      procedure Register_Tests (Test : in out Server.Server_Test_Type) is
      ---------------------------------------------------------------

      begin
         Log_In (Debug);
         Log_Out (Debug);
      end Register_Tests;

--    ---------------------------------------------------------------
--    overriding
--    procedure Set_Up (
--       Test                          : in out Server.Server_Test_Type) is
--    ---------------------------------------------------------------
--
--       Options           : Ada_Lib.Options.AUnit_Lib.Aunit_Options_Type'class
--                            renames Ada_Lib.Options.AUnit_Lib.
--                               Aunit_Options_Constant_Class_Access (
--                                  Ada_Lib.Options.Get_Ada_Lib_Read_Only_Options).all;
--    begin
--       Log (Debug, Here, Who & " enter which host " & Test.Which_Host'img);
--       Log_Out (Debug);
--
--    exception
--       when Fault: others =>
--          Trace_Message_Exception (Fault, Who, Here);
--          Test.Set_Up_Message_Exception (Fault, Here, Who, "could not open database");
--          Log (Debug, Here, Who & " kill");
--    end Set_Up;
--
--    ---------------------------------------------------------------
--    overriding
--    procedure Tear_Down (
--       Test                          : in out Server.Server_Test_Type) is
--    ---------------------------------------------------------------
--
-- --    Options                    : constant Runtime_Options.Options_Constant_Class_Access :=
-- --                                     Runtime_Options.Get_Options;
--       Server                     : constant Ada_Lib.Database.Server.Server_Access := Server_State.Get_Server;
--
--    begin
--       Log_In (Debug);
--       Log_Out (Debug);
--    end Tear_Down;

   end Server;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Trace_Options := True;
   Log_Here (Debug or Elaborate or Trace_Options);

end Ada_Lib.Socket_IO.Server.Tests;

