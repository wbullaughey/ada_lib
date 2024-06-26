-- package provides socket server to off load and on load
-- meta data via stream over socket

with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Text_IO; use  Ada.Text_IO;
-- with Ada_Lib.Socket_Stream_IO;
with Ada_Lib.Socket_Stream_Line_IO;
with GNAT.Sockets;
with Interrupt_Handler;
with Strings;
with Trace;

procedure Socket_Stream_Line_IO_Test is

   Traceing             : constant Boolean := True;

   task Server_Task;

   use type Ada.Streams.Stream_Element_Array;
   use type Ada.Streams.Stream_Element_Offset;

   subtype Line_Type          is Ada.Streams.Stream_Element_Array;
   type Line_Access        is access Ada.Streams.Stream_Element_Array;

   type Test_Type          is array (1 .. 4) of Line_Access;

   Accept_Port             : constant := 9000;
   CR                   : constant Ada.Streams.Stream_Element :=
                           Ada.Streams.Stream_Element (
                              Character'pos (Ada.Characters.Latin_1.CR));
   Dump                 : constant Boolean := True;
   LF                   : constant Ada.Streams.Stream_Element :=
                           Ada.Streams.Stream_Element (
                              Character'pos (Ada.Characters.Latin_1.LF));
   EOL                     : constant Line_Type := ( CR, LF );
   Tests                : constant array (Positive range <>) of Test_Type := (
       ( new Line_Type (1 .. 0), Null, Null, Null )
      ,( new Line_Type'(1 & 2 & 3), Null, Null, Null )
      ,( new Line_Type'(1 & 2 & 3), new Line_Type'(4 & 5 & 6), new Line_Type'(7 & 8 & 9), Null )
   );

   Stream                  : Ada_Lib.Socket_Stream_Line_IO.Stream_Type;

   ---------------------------------------------------------------------------
   procedure Control_C_Handler is
   ---------------------------------------------------------------------------

   begin
      Put_Line ("Control-C");
      Ada_Lib.Socket_Stream_Line_IO.Close (Stream);
-- Trace.Log (Trace.Here, "stream closed");
   end Control_C_Handler;

   ---------------------------------------------------------------------------
   procedure Process_Requst (
      Connect_Socket       : in   GNAT.Sockets.Socket_Type) is
   ---------------------------------------------------------------------------

      Selector          : GNAT.Sockets.Selector_Type;
      Socket_Set           : GNAT.Sockets.Socket_Set_Type;
      Stream               : aliased Ada_Lib.Socket_Stream_Line_IO.Stream_Type;

   begin
      Ada_Lib.Socket_Stream_Line_IO.Create (Stream, Connect_Socket, 5.0, 1.0, True);
      Trace.Log (Traceing, Trace.Here, "stream created");

      begin
         for Test in Tests'range loop
            for Line in Tests (Test)'range loop
               if Tests (Test) (Line) /= Null then
                  declare
                     Input    : Ada.Streams.Stream_Element_Array (1 .. 100);
                     Last     : Ada.Streams.Stream_Element_Offset;

                  begin
-- Trace.Log (Trace.Here, "");
                     Ada_Lib.Socket_Stream_Line_IO.Read (Stream, Input, Last);
-- Trace.Log (Trace.Here, "last" & last'img);

                     if Dump then
                        Put ("received:");
                        for I in 1 .. Last loop
                           Put (Input (I)'img);
                        end loop;
                        New_Line;
                     end if;
                     if Last /= Tests (Test) (Line).all'length then
                        Put_Line ("wrong length for" & Test'img & Line'img);
                        Put_Line ("expected" & Integer'image (Tests (Test) (Line).all'length));
                        Put_Line ("got" & Last'img);
                     elsif Input (1 .. Last) /= Tests (Test) (Line).all then
                        Put_Line ("wrong value for" & Test'img & Line'img);
                     else
                        Put_Line ("got line" & Test'img & Line'img);
                     end if;
                  end;
               end if;
            end loop;
         end loop;

         Put_Line ("all received");

      exception

         when Fault: Ada_Lib.Socket_Stream_Line_IO.Timeout =>
            Trace.Trace_Exception (Fault, Trace.Here,
               "Timeout on socket IO");

         when Fault: others =>
            Trace.Trace_Exception (Fault, Trace.Here,
               "process request failed");

      end;

      Ada_Lib.Socket_Stream_Line_IO.Close (Stream);
-- Trace.Log (Trace.Here, "stream closed");
   end Process_Requst;

   ---------------------------------------------------------------------------
   procedure Socket_Server is
   ---------------------------------------------------------------------------

      Accept_Socket           : GNAT.Sockets.Socket_Type;

   begin
      begin
         GNAT.Sockets.Create_Socket (Accept_Socket);
         Trace.Log (Traceing, Trace.Here, "socket accept created");

      exception
         when Fault: GNAT.Sockets.Socket_Error =>
            Trace.Trace_Exception (Fault, Trace.Here,
               "Could not create server socket");
         return;
      end;

      begin
         GNAT.Sockets.Bind_Socket (Accept_Socket, GNAT.Sockets.Sock_Addr_Type'(
            Addr  => GNAT.Sockets.Any_Inet_Addr,
            Family   => GNAT.Sockets.Family_Inet,
            Port  => Accept_Port
         ));
         Trace.Log (Traceing, Trace.Here, "socket bound");

      exception
         when Fault: GNAT.Sockets.Socket_Error =>
            Trace.Trace_Exception (Fault, Trace.Here,
               "Could not bind Socket_Stream_Line_IO socket");
         return;
      end;

      begin
         GNAT.Sockets.Listen_Socket (Accept_Socket, 1);  -- only accept one connectin at a time
         Trace.Log (Traceing, Trace.Here, "socket listening");

      exception
         when Fault: GNAT.Sockets.Socket_Error =>
            Trace.Trace_Exception (Fault, Trace.Here,
               "Could not listen on Socket_Stream_Line_IO socket");
         return;
      end;

      declare
         Connect_Socket : GNAT.Sockets.Socket_Type;
         Connected      : Boolean := False;

      begin
         declare
            Connect_Address   : GNAT.Sockets.Sock_Addr_Type;
         begin
            GNAT.Sockets.Accept_Socket (Accept_Socket, Connect_Socket, Connect_Address);
            Trace.Log (Traceing, Trace.Here, "socket accepted");
            Connected := True;

         exception
            when Fault: GNAT.Sockets.Socket_Error =>
               Trace.Trace_Exception (Fault, Trace.Here,
                  "Connect to Socket_Stream_Line_IO failed");
         end;

         if Connected then
            Process_Requst (Connect_Socket);
            GNAT.Sockets.Close_Socket (Connect_Socket);
            Trace.Log (Traceing, Trace.Here, "connect socket closed");
         end if;
      end;

      GNAT.Sockets.Close_Socket (Accept_Socket);
-- Trace.Log (Trace.Here, "socket server socket closed");
   end Socket_Server;

   ---------------------------------------------------------------------------
   task body Server_Task is

   begin
      Socket_Server;

   exception

      when Fault: others =>
         Trace.Trace_Exception (Fault, Trace.Here,
            "unexpected exception in Socket_Stream_Line_IO socket");

   end Server_Task;

    Server                 : GNAT.Sockets.Sock_Addr_Type := (
         Addr  => GNAT.Sockets.Addresses (
                  GNAT.Sockets.Get_Host_By_Name ("localhost"), 1),
         Family   => GNAT.Sockets.Family_Inet,
         Port  => Accept_Port);
   Socket                  : GNAT.Sockets.Socket_Type;
   Socket_Created          : Boolean := False;

begin
-- Interrupt_Handler.Enable_Control_C_Abort (Control_C_Handler'access);

   begin
      GNAT.Sockets.Create_Socket (Socket);
      Socket_Created := True;
      Trace.Log (Traceing, Trace.Here, "connect socket created");

   exception
      when Fault: GNAT.Sockets.Socket_Error =>
         Trace.Trace_Exception (Fault, Trace.Here,
            "Could not create sender socket");
      return;
   end;

   begin
      GNAT.Sockets.Connect_Socket (Socket, Server);

      Trace.Log (Traceing, Trace.Here, "socket connected");

   exception
      when Fault: GNAT.Sockets.Socket_Error =>
         Trace.Trace_Exception (Fault, Trace.Here,
            "Could not connect to server socket");
         GNAT.Sockets.Close_Socket (Socket);
      return;
   end;

   begin
      Ada_Lib.Socket_Stream_Line_IO.Create (Stream, Socket, 1.0, 1.0);
-- Trace.Log (Trace.Here, "");

   exception
      when Fault: GNAT.Sockets.Socket_Error =>
         Trace.Trace_Exception (Fault, Trace.Here,
            "Could not create sender stream");
         Ada_Lib.Socket_Stream_Line_IO.Close (Stream);
         GNAT.Sockets.Close_Socket (Socket);
      return;
   end;

   declare
      ---------------------------------------------------------------------------
      procedure Send (
         Test              : in   Test_Type;
         Index             : in   Positive;
         Buffer               : in   Line_Type) is
      ---------------------------------------------------------------------------

      begin
put_line ("send" & Index'img & Ada.Streams.Stream_Element_Offset'image (buffer'first) & Ada.Streams.Stream_Element_Offset'image (buffer'last));
         if Index > Test'last or else Test (Index) = Null then
            if Dump then
               Put( "send:");
               for I in Buffer'range loop
                  Put (Buffer (I)'img);
               end loop;
               New_Line;
            end if;

            Ada_Lib.Socket_Stream_Line_IO.Write (Stream, Buffer);
         else
            Send (Test, Index + 1, Buffer & EOL & Test (Index).all);
         end if;
      end Send;

   begin
      for Test in Tests'range loop
         Send (Tests (Test), 2, Tests (Test) (1).all);
         Put_Line ("test" & Test'img & " sent");
         delay 1.0;
      end loop;

      Put_Line ("all lines sent");
   exception
      when Fault: GNAT.Sockets.Socket_Error =>
         Trace.Trace_Exception (Fault, Trace.Here,
            "write to sender stream failed");
   end;

   Ada_Lib.Socket_Stream_Line_IO.Close (Stream);
   GNAT.Sockets.Close_Socket (Socket);
end Socket_Stream_Line_IO_Test;