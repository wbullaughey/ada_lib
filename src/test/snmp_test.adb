with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.SNMP;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Strings;

procedure SNMP_Test is

	Name						: constant String :=
									".1.3.6.1.4.1.21728.3.2.1.8";
	Session						: Ada_Lib.SNMP.Session_Type;
	Value						: Natural := 0;

begin
	Log (Here, Who);
	Ada_Lib.SNMP.Open (Session, "np-08a", Ada_Lib.SNMP.Version_1);

	for Tries in 1 .. 3 loop
		declare
			Set_Value			: constant String := Strings.Trim (Value'img);
		begin
			Put_Line ("set value " & Set_Value);
			Ada_Lib.SNMP.Set (Session, Name, Ada_Lib.SNMP.Integer_Kind, Set_Value);

			declare
				Get_Value		: constant String := Ada_Lib.SNMP.Get (Session, Name);
		
			begin
				Put_Line ("get value: '" & Get_Value & "'");
				if Get_Value /= Set_Value then
					Put_Line ("get returned different value from set " & Set_Value);
				end if;
			end;
		end;

		Value := 1 - Value;
	end loop;

	Ada_Lib.SNMP.Close (Session);

exception
	when Fault: others =>
		Put_Line ("unexpected exception: " &
			Ada.Exceptions.Exception_Name (Fault));
		Put_Line (Ada.Exceptions.Exception_Message (Fault));
		Ada_Lib.SNMP.Diagnose_Error ("default exception handler", Session);
		Ada_Lib.SNMP.Close (Session);
end SNMP_Test;
