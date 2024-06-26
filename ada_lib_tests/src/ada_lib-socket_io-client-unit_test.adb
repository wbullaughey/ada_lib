--with Ada.Exceptions;
--with Ada.Numerics.Discrete_Random;
--with Ada.Real_Time;
--with Ada.Text_IO;use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
--with Ada_Lib.Options.Unit_Test;
--with Ada_Lib.OS;
--with Ada_Lib.Socket_IO.Client;
--with Ada_Lib.Socket_IO.Server;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Trace_Tasks;
with AUnit.Test_Cases;
--with Hex_IO;
--with Runtime_Options;
--with SYSTEM.ASSERTIONS;

package body Ada_Lib.Socket_IO.Client.Unit_Test is

-- use type Ada.Streams.Stream_Element_Array;
-- use type Ada.Streams.Stream_Element_Offset;
-- use type Ada_Lib.Socket_IO.Port_Type;

   procedure Test_Connect (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Socket_Client_Test_Type
   ) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Socket_Client_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Connect'access,
         Routine_Name   => AUnit.Format ("Test_Connect")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Socket_Client_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Ada_Lib.Unit_Test.Tests.Test_Case_Type (Test).Set_Up;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Set_Up;

   ---------------------------------------------------------------
   function Suite return Standard.AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant Standard.AUnit.Test_Suites.
                                    Access_Test_Suite :=
                                       new Standard.AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Socket_Client_Test_Access :=
                                    new Socket_Client_Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Suite;

   --------------------------------------------------------------
   procedure Test_Connect (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      IP_Address                 : constant IP_Address_Type := (151,101,193,164);
      IP_Address_Port            : constant := 80;
      URL_Address                : constant String := "nytimes.com";
      URL_Port                   : constant := 80;

   begin
--debug := true;
--trace := true;
      Log_In (Debug);
      declare
         Socket                  : Client_Socket_Type;

      begin
         Socket.Connect (URL_Address, URL_Port);
         Assert (Socket.Is_Connected, "could not connect to " & URL_Address &
            " port" & URL_Port'img);
      exception

         when Fault: others =>
            Trace_Exception (Fault);
--          Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

      end;
      declare
         Socket                  : Client_Socket_Type;

      begin
         Socket.Connect (IP_Address, IP_Address_Port);
         Assert (Socket.Is_Connected, "could not connect to " &
            Ada_Lib.Socket_IO.Image (IP_Address) &
            " port" & IP_Address_Port'img);
      exception

         when Fault: others =>
            Trace_Exception (Fault);
--          Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

      end;
--debug := false;
--trace := false;
      Log_Out (Debug);

   end Test_Connect;

begin
if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Trace_Options or Elaborate);
end Ada_Lib.Socket_IO.Client.Unit_Test;

