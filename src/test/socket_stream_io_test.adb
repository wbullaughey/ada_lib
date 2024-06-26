-- part of test for Ada_Lib.Socket_Stream_IO package

with Ada.Calendar;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Socket_Stream_IO;
with GNAT.Sockets;
with Trace;

procedure Socket_Stream_IO_Test is

	use type Ada.Calendar.Time;

	type Test_Types is (
		Complete,
		Short_Command,
		Write_Fail
	);

	Accept_Port					: constant := 9100;
	Socket						: GNAT.Sockets.Socket_Type;
	Stream						: aliased Ada_Lib.Socket_Stream_IO.Stream_Type;
	Traceing					: constant Boolean := True;

begin
	for Test in Test_Types'range loop
		begin
			GNAT.Sockets.Create_Socket (Socket);
			Trace.Log (Traceing, Trace.Here, "socket created");				
				
		exception
			when Fault: GNAT.Sockets.Socket_Error =>
				Trace.Trace_Exception (Fault, Trace.Here,
					"Could not create Data_IO.Server socket");
			return;
		end;

		begin
			GNAT.Sockets.Connect_Socket (Socket, GNAT.Sockets.Sock_Addr_Type'(
				Family	=> GNAT.Sockets.Family_Inet,
				Addr	=> GNAT.Sockets.Addresses (
							GNAT.Sockets.Get_Host_By_Name ("localhost"), 1),
				Port	=> Accept_Port
			));
			Trace.Log (Traceing, Trace.Here, "socket connected");				
				
		exception
			when Fault: GNAT.Sockets.Socket_Error =>
				Trace.Trace_Exception (Fault, Trace.Here,
					"Could not create Data_IO.Server socket");
			return;
		end;

		begin
			Ada_Lib.Socket_Stream_IO.Create (Stream, Socket, 10.0, 2.0);

		exception
			when Fault: others =>
				Trace.Trace_Exception (Fault, Trace.Here,
					"Socket_Stream_IO.Create failed");
		end;

		begin
			case Test is

				when Complete =>
					String'Write (Stream'access, "t 1");
					String'Write (Stream'access, "end");

				when Short_Command =>
					String'Write (Stream'access, "t2");
					delay 10.0;

				when Write_Fail =>
					String'Write (Stream'access, "t 3");

					declare
						Start_Time	: constant Ada.Calendar.Time :=
										Ada.Calendar.Clock;

					begin
						loop
							String'Write (Stream'access, "fill up the socket buffer");
						end loop;
						
					exception
						when Fault: Ada_Lib.Socket_Stream_IO.Timeout =>
							Put_Line ("expected timeout after " &
								Trace.Image (Ada.Calendar.Clock - Start_Time));

					end;

			end case;

		exception
			when Fault: others =>
				Trace.Trace_Exception (Fault, Trace.Here,
					"stream write failed");
		end;

		begin
			GNAT.Sockets.Close_Socket (Socket);

		exception
			when Fault: others =>
				Trace.Trace_Exception (Fault, Trace.Here,
					"socket close failed");
		end;
	end loop;
end Socket_Stream_IO_Test;
