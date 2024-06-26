with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Text_IO;use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with Ada_Lib.Options.AUnit_Lib;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.OS;
with Ada_Lib.Socket_IO.Client;
with Ada_Lib.Socket_IO.Server;
--with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with GNAT.Source_Info;
with Hex_IO;
--with Runtime_Options;
with SYSTEM.ASSERTIONS;

package body Ada_Lib.Socket_IO.Stream_IO.Unit_Test is

   use type Ada.Streams.Stream_Element_Array;
   use type Ada.Streams.Stream_Element_Offset;
   use type Port_Type;

   Raise_Assert                     : Exception;

   type Length_Or_Answer_Type       is mod 2 ** 32;

   task type Client_Task_Type is

      entry Start (
         Test                    : in     Socket_Test_Access);

   end Client_Task_Type;

   type Client_Task_Access       is access Client_Task_Type;

-- type Length_Buffer_Type    is new Buffer_Type (1 .. 4);

   task type Server_Task_Type is

      entry Start (
         Test                    : in     Socket_Test_Access);

   end Server_Task_Type;

   type Server_Task_Access       is access Server_Task_Type;

   package Random_Number_Data_Generator is new Ada.Numerics.Discrete_Random (
      Data_Type);
   package Random_Number_Offset_Generator is new Ada.Numerics.Discrete_Random (
      Index_Type);

   procedure Socket_Open_Close (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Socket_Send_Receive (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Socket_Acknowledge_Send_Receive (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Send_Receive_Missmatch_Read_Length (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Socket_Timeout_Send_Receive (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Default_Client_Read_Timeout_Time
                                 : constant Duration := 0.2;
   Default_Server_Read_Timeout_Time
                                 : constant Duration := 0.2;
   Delay_Time                    : constant Duration := 0.5;   -- time to delay second part of write to test timeout
-- Limit_Length                  : constant := 128;
   Notify_Frequency              : constant := 10;
   Server_Name                   : constant String := "localhost";

   ---------------------------------------------------------------
   overriding
   function Name (Test : Socket_Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   procedure Record_Socket (
      Test                       : in out Socket_Test_Type;
      Socket                     : in out Socket_Type'class) is
   ---------------------------------------------------------------

   begin
      Test.Socket_Count := Test.Socket_Count + 1;
      Test.Sockets (Test.Socket_Count) := Socket'unchecked_access;
      Log_Here (Debug, "record socket " & Socket.Image &
         " count" & Test.Socket_Count'img);
   end Record_Socket;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (Test : in out Socket_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Socket_Open_Close'access,
         Routine_Name   => AUnit.Format ("Socket_Open_Close")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Socket_Send_Receive'access,
         Routine_Name   => AUnit.Format ("Socket_Send_Receive")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Socket_Acknowledge_Send_Receive'access,
         Routine_Name   => AUnit.Format ("Socket_Acknowledge_Send_Receive")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Send_Receive_Missmatch_Read_Length'access,
         Routine_Name   => AUnit.Format ("Send_Receive_Missmatch_Read_Length")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Socket_Timeout_Send_Receive'access,
         Routine_Name   => AUnit.Format ("Socket_Timeout_Send_Receive")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Server_Failure'access,
         Routine_Name   => AUnit.Format ("Server_Failure")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Socket_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Ada_Lib.Unit_Test.Tests.Test_Case_Type (Test).Set_Up;
      Test.Answer := Success;
      Test.Client_Completed := False;
      Test.Client_Delayed             := False;
      Test.Client_Failed              := False;
      Test.Client_Read_Timeout_Time   := Default_Client_Read_Timeout_Time;
      Test.Client_Started             := False;
      Test.Client_Write_Timeout_Time  := No_Timeout;
--    Test.Do_Acknowledgement         := False;
--    Test.Missmatch_Read_Length      := False;
      Test.Server_Completed := False;
      Test.Server_Failed    := False;
      Test.Server_Port                := Default_Port;
      Test.Server_Read_Timeout_Time   := Default_Server_Read_Timeout_Time;
                                          -- time out incase client fails
      Test.Server_Started             := False;
      Test.Server_Timedout            := False;
      Test.Server_Write_Timeout_Time  := No_Timeout;
      Test.Socket_Count := 0;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Set_Up;

   --------------------------------------------------------------
   procedure Send_Receive (
      Test                       : in out AUnit.Test_Cases.Test_Case'class;
      From                       : in     String := GNAT.Source_Info.Source_Location) is
   ---------------------------------------------------------------

      Client                     : constant Client_Task_Access :=
                                    new Client_Task_Type;
      Local_Test                 : Socket_Test_Type renames Socket_Test_Type (Test);

      Generator                  : Random_Number_Data_Generator.Generator;
      Send_Started               : Boolean := True;
      Server                     : constant Server_Task_Access :=
                                    new Server_Task_Type;
      Server_Read_Timeout_Time   : Duration renames
                                    Local_Test.Server_Read_Timeout_Time;

   begin
      Log_In (Debug,  --, "Missmatch_Read_Length " &
--          Local_Test.Missmatch_Read_Length'img &
         " Client Read Timeout " & Local_Test.Client_Read_Timeout_Time'img &
         " Client Write Timeout " & Local_Test.Client_Write_Timeout_Time'img &
         " Client_Delay_Write_Time " & Local_Test.Client_Delay_Write_Time'img &
         " Server Read Timeout " & Local_Test.Server_Read_Timeout_Time'img &
         " Server Write Timeout " & Local_Test.Server_Write_Timeout_Time'img &
            From);

      if Local_Test.Client_Failed then
         Log_Here (Debug, "client failed");
         Send_Started := False;
      end if;

      if Local_Test.Server_Failed then
         Log_Here (Debug, "Server failed");
         Send_Started := False;
      end if;

      if not Send_Started then
         Log_Exception (Debug);
         raise Raise_Assert with "server or client not started";
      end if;

      -- initialize data buffer
      for Index in Local_Test.Send_Data'range loop

         Local_Test.Send_Data (Index) :=
            Random_Number_Data_Generator.Random (Generator);
      end loop;

      Log_Here (Debug, "start server task");
      Server.Start (    -- reads from client and then writes to client
         Test              => Local_Test'unchecked_access);
      delay 0.1;  -- let server start
      if not Local_Test.Server_Started then
         Log_Exception (Debug);
         raise Raise_Assert with "server did not start";
      end if;

      Log_Here (Debug, "start client task");
      Client.Start (    -- writes to server and reads from server
         Test              => Local_Test'unchecked_access);

      delay 0.1;  -- let client start
      if not Local_Test.Client_Started then
         Log_Exception (Debug);
         raise Raise_Assert with "client did not start";
      end if;

      Log_Here (Debug, "wait for tasks to complete");

      while not (
            Local_Test.Client_Completed and then
            Local_Test.Server_Completed
         ) and then not (
            Local_Test.Client_Failed or else
            Local_Test.Server_Failed
         ) loop
         delay 0.2;
      end loop;

      Log_Here (Debug, "tasks completed" &
         (if Local_Test.Server_Timedout then " server timedout" else "") &
         (if Local_Test.Server_Failed then " server failed" else "") &
         (if Local_Test.Client_Failed then " client failed" else ""));

      if Local_Test.Answer /= Success then
         Log_Exception (Debug, "bad answer " & Local_Test.Answer'img);
         raise Raise_Assert with (if Local_Test.Server_Timedout then
               "server unexpectedly time out"
            else
               "server return answer false") & " at " & Here;
      end if;

      if Local_Test.Client_failed then
         Log_Exception (Debug);
         raise Raise_Assert with "Client failed";
      end if;
      if Local_Test.Server_failed then
         Log_Exception (Debug);
         raise Raise_Assert with "server failed";
      end if;

      if    Local_Test.Server_Write_Timeout_Time = No_Timeout and then
            Local_Test.Server_Timedout then
         Log_Exception (Debug);
         raise Raise_Assert with "server timed out";
      end if;

      if Local_Test.Send_Data = Local_Test.Received_Data then
         Local_Test.Answer := Success;
      else
         if Server_Read_Timeout_Time = 0.0 then
            Log_Here (Debug, "data missmach");
            Hex_IO.Dump_8 (Local_Test.Send_Data'address, Local_Test.Send_Data'size, 32, "sent data");
            Hex_IO.Dump_8 (Local_Test.Received_Data'address, Local_Test.Received_Data'size, 32, "received data");
            Local_Test.Answer := Bad_DAta;
         else
            Local_Test.Answer := Timeout_Answer;
            Log_Here (Debug, "Server_Read_Timeout");
         end if;
      end if;
      Log_Out (Debug);

   exception

      when Fault: others =>
put_Line (here);
--       Stop_Tasks;
         Log_Exception (Debug, Fault);
put_Line (here);
         raise;
--
   end Send_Receive;

   --------------------------------------------------------------
   procedure Server_Failure (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Answer                     : Buffer_Type (1 ..
                                    Buffer_Bytes (Length_Or_Answer_Type'size));
      Ack                        : Length_Or_Answer_Type;
      for Ack'address use Answer'address;
      Local_Test                 : Socket_Test_Type renames Socket_Test_Type (Test);
      Send_Buffer                : constant Data_Buffer_Type := (
                                    others => 123);
      Write_Length               : constant Index_Type := Send_Buffer'length;
      Length_Buffer              : Buffer_Type (1 .. 4);
      for Length_Buffer'address use Write_Length'address;

      OK_Repetition              : constant := 1;    -- repetition 1 ok
      No_Server_Repetition       : constant := 2;    -- server not started so
                                                   -- connect should fail
      Wrong_Port                 : constant := 3;

   begin
      Log_In (Debug);

      Local_Test.Client_Completed := True;   -- client not used

      for Repitition in OK_Repetition .. Wrong_Port loop
         declare
            Port                 : constant Port_Type :=
                                    Default_Port +
                                    Port_Type (Repitition);
         begin
            Put_Line ("doing repitition" & Repitition'img &
               " port" &
               " will fail on repetition" & No_Server_Repetition'img);
            Local_Test.Server_Port := (if Repitition = Wrong_Port then
                  Default_Port
               else
                  Port);
            Local_Test.Server_Started := False;
            declare
               Server               : Server_Task_Access;
               Socket               : constant Ada_Lib.Socket_IO.Client.
                                       Client_Socket_Access := new Ada_Lib.
                                          Socket_IO.Client.Client_Socket_Type;

            begin
               if Repitition /= No_Server_Repetition then
                  Server := new Server_Task_Type;
                  delay 0.1;  -- let server get initialized
                  Server.Start (Local_Test'unchecked_access);
                  delay 0.1;  -- let server get initialized

                  Log_Here (Debug, "started" & Repitition'img);
                  Assert (Local_Test.Server_Started, "server did not start");
                  delay 0.1;  -- let server socket get bound
               end if;

               Log_Here (Debug, "connect to server repetition" &
                  Repitition'img & " port" & Port'img);

               begin
                  Socket.Connect (Server_Name, Port);

               exception
                  when Fault: others =>
                     Trace_Message_Exception (Debug, Fault, (if Repitition = OK_Repetition then
                           "unexpected "
                        else
                           "") &
                     "exception");
                     Assert (Repitition /= OK_Repetition, "unexpected failed exception");
                     goto Loop_Repeat;
               end;

               if Repitition = No_Server_Repetition then
                  Assert (False, "exception for connect failure did not happen");
               end if;

               Log_Here (Debug, "write length" & Write_Length'img);
               Socket.Write (Length_Buffer);
               Log_Here (Debug, "write buffer" &
                  Natural'(Send_Buffer'size/8)'img);
               Socket.Write (Send_Buffer);
               -- read is done by server task
               Log_Here (Debug);
--             Socket.Close;

               while not (Local_Test.Server_Completed) loop
                  delay 0.2;
               end loop;

               Log_Here (Debug, "server task completed" &
                  (if Local_Test.Server_Timedout then " server timedout" else "") &
                  (if Local_Test.Server_Failed then " server failed" else ""));

               Assert (Local_Test.Answer = Success,
                  "Local_Test.Answer " & Local_Test.Answer'img);
               Log_Here (Debug);

            exception

               when Fault: AUNIT.ASSERTIONS.ASSERTION_ERROR =>
                  Trace_Exception (Debug, Fault, Here);

               when Fault: others =>
                  Trace_Exception (Debug, Fault, Here);
                  Assert (False, "unexpected exception " &
                     Ada.Exceptions.Exception_Name (Fault) & " " &
                     Ada.Exceptions.Exception_Message (Fault));

            end;
            Log_Here (Debug, "Repitition" & Repitition'img);
         end;
<< Loop_Repeat >>
      end loop;
      Log_Out (Debug);
   end Server_Failure;

   --------------------------------------------------------------
   procedure Socket_Open_Close (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Socket_Test_Type renames Socket_Test_Type (Test);

   begin
      Log_In (Debug);

      declare
         Socket                  : Stream_Socket_Access := new Stream_Socket_Type;

      begin
         Log_Here (Debug, "socket address " & Image (Socket'address));
         Socket.Set_Description ("test socket");
         Record_Socket (Local_Test, Socket.all);
--       delay 0.1;                 -- let tasks initialize
--       Socket.Close;
--       Log_Here (Debug);
--       delay 0.1;                 -- let tasks exit
      end;

      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Fault);

   end Socket_Open_Close;

   --------------------------------------------------------------
   procedure Socket_Send_Receive (
      Test                      : in out AUnit.Test_Cases.Test_Case'class) is
-- pragma Unreferenced (Test);
   ---------------------------------------------------------------

--    Local_Test                 : Socket_Test_Type renames Socket_Test_Type (Test);

   begin
      Log_In (Debug);
      Send_Receive (Test);
put_Line (here);
      Log_Out (Debug);

   exception

      when Fault: Raise_Assert =>
put_Line (here);
         Log_Exception (Debug, "Assert in Send_Receive");
         Assert (False, Ada.Exceptions.Exception_Message (Fault));

      when Fault: others =>
put_Line (here);
         Log_Exception (Debug, Fault);
put_Line (here);
         raise;

   end Socket_Send_Receive;

   --------------------------------------------------------------
   --
   procedure Send_Receive_Missmatch_Read_Length (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------
   -- total read and write lengths are equal
   -- individual read and writes differ

      Local_Test                 : Socket_Test_Type renames Socket_Test_Type (Test);

   begin
      Log_In (Debug);
      Put_Line ("long test ~45 seconds");
--    Send_Receive (Test,
--       Acknowledge                => True,
--       Answer            => Local_Test.Answer,
--       Missmatch_Read_Length      => True,
--       Notify                     => True,
--       Timeout                    => False,      -- do not expect a timeout
--       Client_Delay_Write_Time    => No_Timeout, -- continues write until completed
--       Server_Timeout_Time        => 0.0);

      Send_Receive (Test);
      Assert (Local_Test.Answer = Success,
         "Answers failed " & Local_Test.Answer'img);
      Log_Out (Debug);

   exception

      when Fault: others =>
         Log_Exception (Debug, Fault);
         raise;

   end Send_Receive_Missmatch_Read_Length;

   --------------------------------------------------------------
   -- expect timeout in server read
   procedure Socket_Timeout_Send_Receive (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Socket_Test_Type renames Socket_Test_Type (Test);

   begin
      Log_In (Debug);
      Put_Line ("long test ~45 seconds");
      Send_Receive (Test);
      Assert (Local_Test.Answer = Success,
         "Answers failed " & Local_Test.Answer'img);
      Log_Out (Debug);

   exception

      when Fault: others =>
         Log_Exception (Debug, Fault);
         raise;

   end Socket_Timeout_Send_Receive;

   --------------------------------------------------------------
   procedure Socket_Acknowledge_Send_Receive (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Socket_Test_Type renames Socket_Test_Type (Test);

   begin
      Log_In (Debug);
      Put_Line ("long test ~45 seconds");

      Send_Receive (Test);
      Assert (Local_Test.Answer = Success,
         "Answers failed " & Local_Test.Answer'img);
      Log_Out (Debug);

   exception

      when Fault: others =>
         Log_Exception (Debug, Fault);
         raise;

   end Socket_Acknowledge_Send_Receive;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Socket_Test_Access := new Socket_Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (Test : in out Socket_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "sockets" & Test.Socket_Count'img);

      for Index in 1 .. Test.Socket_Count loop
         Log_Here (Debug, "close " & Test.Sockets (Index).Image &
            " index" & Index'img);

         begin
--          GNAT.Sockets.Close_Socket (Test.Sockets (Index).GNAT_Socket);
            Test.Sockets (Index).Close;

         exception
            when Fault: others =>  -- unexpected, server may have failed
               Trace_Message_Exception (Fault, "socket " &
                  Test.Sockets (Index).Image, Here);
         end;
      end loop;
      Ada_Lib.Unit_Test.Tests.Test_Case_Type (Test).Tear_Down;
      Log_Out (Debug);
   end Tear_Down;

   ---------------------------------------------------------------
   task body Client_Task_Type is
   ---------------------------------------------------------------

      Count                      : Natural := 0;
      Local_Test                 : Socket_Test_Access := Null;

   begin
      Log_In (Debug);
      Ada_Lib.Trace_Tasks.Start ("client");

      accept Start (
         Test                    : in     Socket_Test_Access) do

         Log_Here (Debug, "started Answer " &
            Test.Answer'img & Test.Client_Read_Timeout_Time'img);
         Local_Test := Test;
      end Start;
      Local_Test.Client_Started := True;
      Local_Test.Client_Failed := False;

      declare
         Data_Left               : Index_Type := Data_Buffer_Type'length;
         Generator               : Random_Number_Offset_Generator.Generator;
         Now                     : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
         Offset                  : Duration;
         Options                 : Ada_Lib.Options.AUnit_Lib.Aunit_Options_Type'class renames
                                    Ada_Lib.Options.AUnit_Lib.
                                       Aunit_Options_Constant_Class_Access (
                                          Ada_Lib.Options.Program_Options).all;
         Seconds                 : Ada.Real_Time.Seconds_Count;
         Seed                    : Integer;
         Client_Socket           : Ada_Lib.Socket_IO.Client.Client_Socket_Access :=
                                    new Ada_Lib.Socket_IO.Client.Client_Socket_Type;
         Start_Offset            : Index_Type :=
                                    Local_Test.Send_Data'first;
         Time_Span               : Ada.Real_Time.Time_Span;
         Written                 : Index_Type := 0;

      begin
         Ada.Real_Time.Split (Now, Seconds, Time_Span);
         Offset := Ada.Real_Time.To_Duration (Time_Span);
         Seed := Integer (Offset * 1000000);
         Log_Here (Debug,
            "client socket " & Client_Socket.Image &
--          " response socket " & Response_Socket.Image &
            " data left" & Data_Left'img &
--          " Do_Acknowledgement " & Local_Test.Do_Acknowledgement'img &
            " delay time " & Delay_Time'img &
            " socket " & Image (Client_Socket'address) &
            " new seed " & Seed'img & " entered seed " &
               Options.Unit_Test.Random_Seed'img &
            " Test " & Image (Local_Test.all'address));
         if Options.Unit_Test.Report_Random then
            Put_Line ("random seed " & Seed'img);
         end if;
         if Options.Unit_Test.Set_Random_Seed then
            Random_Number_Offset_Generator.Reset (Generator,
               Options.Unit_Test.Random_Seed);
         else
            Random_Number_Offset_Generator.Reset (Generator, Seed);
         end if;

         Client_Socket.Connect (
            Connection_Timeout=> 1.0,
            Description      => "unit test client connect",
--          Expected_Read_Callback
--                            => Null,
            Port              => Local_Test.Server_Port,
            Server_Name       => Server_Name);

         Record_Socket (Local_Test.all, Client_Socket.all);
         Log_Here (Debug, "connected");

         while Data_Left > 0 and then not (
               Local_Test.Server_Failed or else Local_Test.Server_Timedout) loop
            declare
               Request_Buffer    : Length_Or_Answer_Type :=
                                    Length_Or_Answer_Type(Data_Left);
               Write_Buffer      : Buffer_Type (1 ..
                                    Buffer_Bytes (Request_Buffer'size));
               for Write_Buffer'address use Request_Buffer'address;
               Short_Length      : Index_Type;

            begin
               pragma Assert (Request_Buffer'size = Write_Buffer'size,
                  "buffer size missmatch at " & Here);
               Log_Here (Debug,
                  " left" & Data_Left'img);

               Short_Length := Random_Number_Offset_Generator.Random (
                                 Generator) mod (Data_Buffer_Type'length - 1) + 1;
                                 -- make sure never zero
               if Short_Length > Data_Left then
                  Short_Length := Data_Left;

               end if;
               Request_Buffer  := Length_Or_Answer_Type (Short_Length);

               Log_Here (Debug, "short length" &
                  " Request_Buffer" & Request_Buffer'img );

               declare
                  End_Offset     : constant  Index_Type :=
                                    Start_Offset + Index_Type (Request_Buffer) - 1;
--                                  (if Do_Timeout then 2 else 1);
               begin
                  Log_Here (Debug, "write length" & Request_Buffer'img &
                     " start " & Start_Offset'img & " end " & End_Offset'img &
                     " to client socket " & Client_Socket.Image);
                  Client_Socket.Write (Write_Buffer);     -- request length

--                if Local_Test.Do_Acknowledgement then  -- get ack for length
                     declare
                        Answer   : Buffer_Type (1 ..
                                    Buffer_Bytes (Length_Or_Answer_Type'size));
                        Ack      : Length_Or_Answer_Type;
                        for Ack'address use Answer'address;

                     begin
                        Client_Socket.Read (
                           Buffer         => Answer,
                           Timeout_Length => Local_Test.Client_Read_Timeout_Time);
                        Log_Here (Debug, "ack" & Ack'img);

                        if Ack /= Request_Buffer then
                           Local_Test.Answer := Wrong_Length;
                           Log_Here (Debug, "ack bad for lenth" & Ack'img);
                           exit;
                        end if;

                     exception
                        when Fault: Timeout =>  -- unexpected, server may have failed
                           Trace_Exception (Fault, Here);
                           Local_Test.Client_Failed := True;
                           Local_Test.Server_Failed := True;
                           exit;

                     end;
--                end if;

                  Client_Socket.Write (Local_Test.Send_Data (
                     Start_Offset .. End_Offset));
                                                   -- variable length records
                  Written := Written + Index_Type (Request_Buffer);

--                if Do_Timeout then
--                   Log_Here (Debug, "Delay_Time " & Delay_Time'img);
--                   Local_Test.Client_Delayed := True;
--                   delay Delay_Time;     -- delay sending rest of buffer
--                                         -- so read will timeout
--                   Log_Here (Debug, "second write");
--                   Client_Socket.Write (Local_Test.Send_Data (
--                      End_Offset + 1 .. End_Offset + 2));
--                   exit;    -- server should timeout and end test
--                end if;
--                if Local_Test.Do_Acknowledgement then
                     declare
                        Answer   : Buffer_Type (1 ..
                                    Buffer_Bytes (Length_Or_Answer_Type'size));
                        Ack      : Length_Or_Answer_Type;
                        for Ack'address use Answer'address;
                        Sum               : Length_Or_Answer_Type := 0;

                     begin
                        Log_Here (Debug, "Answer size" &
                           Answer'size'img & " ack" & Ack'size'img);
                        pragma Assert (Answer'size = Sum'size);

                        begin
                           Client_Socket.Read (Answer);

                        exception
                           when Fault: Timeout =>  -- unexpected, server may have failed
                              Trace_Exception (Fault, Here);
                              exit;

                        end;
                        for Index in Start_Offset .. End_Offset loop
                           Sum := Sum +
                              Length_Or_Answer_Type (Local_Test.Send_Data (Index));
                        end loop;

                        if Sum /= Ack then
                           Local_Test.Answer := Bad_Ack;
                        end if;

                        Log_Here (Debug, "answer " & Local_Test.Answer'img &
                           " sum" & Sum'img & " ack" & Ack'img);

                        if Ada_Lib.Options.Program_Options.Verbose and then
                              Count mod Notify_Frequency = 0 then
                           Put_Line (Count'img & " records received");
                        end if;
                     end;
--                end if;
                  Start_Offset := Start_Offset + Index_Type (Request_Buffer);
                  Data_Left := Data_Left - Index_Type (Request_Buffer);

                  Count := Count + 1;
                  if Count mod 10 = 0 and then Options.Verbose then
                     Put_LIne (Count'img & " records written");
                  end if;

                  Log_Here (Debug, "left" & Data_Left'img &
                     " start " & Start_Offset'img & " length" & Request_Buffer'img &
                     " written" & Written'img);
               end;
            end;
         end loop;
         Local_Test.Client_Completed := True;
--
--       Log_Here (Debug, "wait for server to complete");
--       while not Local_Test.Server_Completed loop
--          delay 0.1;
--       end loop;
--       Log_Here (Debug, "close server socket" & " socket " & Image (Client_Socket'address));
--       Client_Socket.Close;

      exception
         when Fault: others =>
            Trace_Exception (Fault, Here);
            Local_Test.Client_Failed := True;
            Put_Line ("Client task failed with " &
               Ada.Exceptions.Exception_Message (Fault));
--          Client_Socket.Close;
      end;

      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Debug);

   exception

      when SYSTEM.ASSERTIONS.ASSERT_FAILURE =>
         Put_Line ("pragma assert failed");
         Log_Out (Debug);
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

      when Fault: others =>
         Trace_Exception (Fault, Here);
         if Local_Test = Null then
            Put_Line ("exception in " & Who & " befor Local_Test initialized");
            Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
         else
            Local_Test.Client_Failed := True;
         end if;
         Log_Out (Debug);

   end Client_Task_Type;

   ---------------------------------------------------------------
   task body Server_Task_Type is
   ---------------------------------------------------------------

      Count                      : Natural := 0;
      Do_Timeout                 : Boolean;
      Local_Test                 : Socket_Test_Access := Null;

   begin
      Log_In (Debug);
      Ada_Lib.Trace_Tasks.Start ("server");

      accept Start (
         Test                    : in     Socket_Test_Access) do

         Log_Here (Debug, "started Answer " & Test.Answer'img);
         Do_Timeout := Test.Server_Read_Timeout_Time /= 0.0;
         Local_Test := Test;
      end Start;
      Local_Test.Server_Started := True;
      Local_Test.Server_Failed := False;

      declare
         Accepted_Socket         : constant Ada_Lib.Socket_IO.Server.
                                    Accepted_Socket_Access := new Ada_Lib.
                                       Socket_IO.Server.Accepted_Socket_Type;
         Data_Left               : Index_Type := Data_Buffer_Type'length;
         End_Offset              : Index_Type :=
                                    Local_Test.Received_Data'last;
         Generator               : Random_Number_Offset_Generator.Generator;
         Server_Socket           : constant Ada_Lib.Socket_IO.Server.
                                    Server_Socket_Access := new Ada_Lib.
                                       Socket_IO.Server.Server_Socket_Type (
                                          Local_Test.Server_Port);
         Start_Offset            : Index_Type :=
                                    Local_Test.Received_Data'first;

--         ---------------------------------------------------------------
--         procedure Close_Sockets is
--         ---------------------------------------------------------------
--
--         begin
--            Log_In (Debug, "server socket " & Server_Socket.Image &
--               " accepted socket " & Accepted_Socket.Image);
--            Server_Socket.Close;
--log_here;
--            declare
--               Last              : Index_Type;
--               Nudge             : Buffer_Type (1 .. 1) := (others => 0);
--
--            begin
--               Accepted_Socket.Set_Closed;
--               GNAT.Sockets.Send_Socket (Accepted_Socket.GNAT_Socket, Nudge, Last);
--            end;
--log_here;
--            Accepted_Socket.Close;
--            Log_Out (Debug);
--         end Close_Sockets;
--         ---------------------------------------------------------------

      begin
         Log_Here (Debug, "data left" & Data_Left'img &
            " unaccepted socket " & Accepted_Socket.Image & " no description expected " &
            " server socket " & Server_Socket.Image & " no description expected " &
--          " accepted socket " & Image (Accepted_Socket'address) &
--          " server socket " & Image (Server_Socket'address) &
--          " do ack " & Local_Test.Do_Acknowledgement'img &
            " do timeout " & Do_Timeout'img &
            " timeout duration " & Local_Test.Server_Read_Timeout_Time'img &
            " Test " & Image (Local_Test.all'address));

         Random_Number_Offset_Generator.Reset (Generator);
         Server_Socket.Accept_Socket (
            Accepted_Socket      => Accepted_Socket.all,
            Server_Description   => "unit test server",
            Accepted_Description => "unit test client accepted");
         Record_Socket (Local_Test.all, Server_Socket.all);
         Record_Socket (Local_Test.all, Accepted_Socket.all);
         Log_Here (Debug, "server socket " & Server_Socket.Image &
            " accepted socket " & Accepted_Socket.Image);

         while Data_Left > 0 and then
               not Local_Test.Client_Failed and then
               not Local_Test.Server_Failed loop
            -- keep reading until all data received
            declare
               Request_Buffer    : Length_Or_Answer_Type;
               Read_Buffer       : Buffer_Type (1 ..
                                    Buffer_Bytes (Request_Buffer'size));
               for Read_Buffer'address use Request_Buffer'address;

            begin
               pragma Assert (Request_Buffer'size = Read_Buffer'size,
                  "buffer size missmatch at " & Here);
               Accepted_Socket.Read (Read_Buffer,     -- read the length
                  Local_Test.Server_Read_Timeout_Time);

               Log_Here (Debug, "read length" & Request_Buffer'img);
--             if Local_Test.Do_Acknowledgement then  -- send ack for length
                  Accepted_Socket.Write (Read_Buffer);
--             end if;

               if Index_Type (Request_Buffer) > Data_Left then
                  Log_Here (Debug, "read length" & Request_Buffer'img &
                     " > data left" & Data_Left'img);
                  exit;
               end if;

               declare
                  Last              : Index_Type;
                  Received          : Index_Type;

               begin
                  End_Offset := Start_Offset + Index_Type (Request_Buffer) - 1;
                  Log_Here (Debug, "start " & Start_Offset'img &
                     " End_Offset" & End_Offset'img);

                  Accepted_Socket.Read (
                     Buffer            => Local_Test.Received_Data (
                                             Start_Offset .. End_Offset),
                     Last              => Last,    -- index in Received_Data
                     Timeout_Length    => Local_Test.
                                             Server_Read_Timeout_Time);

                  Received := Last - Start_Offset + 1;
                  Log_Here (Debug, "expected" & Request_Buffer'img &
                     " Received" & Received'img &
                     " last" & Last'img & " end offset" &
                     End_Offset'img);

                  Data_Left := Data_Left - Received;
                  Log_Here (Debug, "Received" & Received'img & " left" & Data_Left'img);

                  if Received = Index_Type (Request_Buffer) then
                     declare     -- got the full chunk
                        Answer   : Buffer_Type (1 ..
                                    Buffer_Bytes (Length_Or_Answer_Type'size));
                        Sum               : Length_Or_Answer_Type;
                        for Sum'address use Answer'address;

                     begin
                        Log_Here (Debug, "Answer size" &
                           Answer'size'img & " sum" & Sum'size'img);
                        pragma Assert (Answer'size = Sum'size);
                        Sum := 0;
                        for Index in Start_Offset .. End_Offset loop
                           Sum := Sum +
                              Length_Or_Answer_Type (Local_Test.Received_Data (Index));
                        end loop;
                        Log_Here (Debug, "Sum" & Sum'img);
                        Accepted_Socket.Write (Answer);
--                      Request_Buffer.Response_Socket.Write (Answer);
                     end;
                  else
                     if Ada_Lib.Options.Program_Options.Verbose and then
                           Count mod Notify_Frequency = 0 then
                        Put_Line ("records" & Count'img);
                     end if;
                  end if;
                  Count := Count + 1;
                  Start_Offset := Start_Offset + Index_Type (Request_Buffer);
                  Log_Here (Debug, "left" & Data_Left'img &
                     " Request_Buffer" & Request_Buffer'img &
                     " start " & Start_Offset'img & " received" & Received'img);
                  exception
                     when Fault: Timeout =>
                        Trace_Message_Exception (Debug, Fault,
                           "client delayed " & Local_Test.Client_Delayed'img);
                        Local_Test.Answer := Timeout_Answer;
                        Local_Test.Server_Timedout := True;
                        exit;                   -- expected
               end;
            end;
         end loop;

         Local_Test.Server_Completed := True;

         Log_Here (Debug, "wait for client to complete Client_Completed " &
            Local_Test.Client_Completed'img &
            " Test " & Image (Local_Test.all'address));
--       while not (Local_Test.Client_Completed or else
--                  Local_Test.Client_Failed) loop
--          delay 0.1;
--       end loop;
--       Log_Here (Debug, "close sockets");
--       Close_Sockets;
--       Log_Here (Debug, "server sockets closed");

      exception
         when Fault: Timeout =>
            if Do_Timeout then
               Local_Test.Server_Failed := False;
               Local_Test.Answer := Success;
            else
               Trace_Message_Exception (Fault, "timeout not expected");
               Put_Line ("Server task failed with unexpected timeout " &
                  Ada.Exceptions.Exception_Message (Fault));
               Local_Test.Server_Failed := True;
               Local_Test.Answer := Timeout_Answer;
            end if;
--          Log_Here (Debug, "close sockets");
--          Close_Sockets;

         when Fault: others =>
            Trace_Exception (Fault, Here);
            Local_Test.Server_Failed := True;
            Local_Test.Answer := Unexpected;
            Put_Line ("Server task failed with " &
               Ada.Exceptions.Exception_Message (Fault));
--          Log_Here (Debug, "close sockets");
--          Close_Sockets;
      end;

      Log_Here (Debug);
      Local_Test.Server_Completed := True;
      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Debug);

   exception
      when SYSTEM.ASSERTIONS.ASSERT_FAILURE =>
         Put_Line ("pragma assert failed");
         Log_Out (Debug);
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

      when Fault: others =>
         Local_Test.Server_Failed := True;
         Trace_Exception (Fault, Here);
         Log_Out (Debug);

   end Server_Task_Type;
   ---------------------------------------------------------------

begin
if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Trace_Options or Elaborate);
end Ada_Lib.Socket_IO.Stream_IO.Unit_Test;

