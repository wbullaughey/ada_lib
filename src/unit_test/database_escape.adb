with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use  Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Source_Info;

with Ada_Lib.Database.Poster; use Ada_Lib.Database.Poster;

procedure Database_Escape is

	type Tests_Type				is array (Positive range <>) of Unbounded_String;
	Ok							: Boolean := True;
	Tests						: constant Tests_Type := (
		To_Unbounded_String ("abc"),
		To_Unbounded_String ("a&c"),
		To_Unbounded_String ("a&&d"),
		To_Unbounded_String ("abc" &
			Ada.Characters.Latin_1.LF & "xyz"),
		To_Unbounded_String ("abc" &
			Ada.Characters.Latin_1.CR & 
			Ada.Characters.Latin_1.LF & "xyz"));

begin
	for Index in Tests'range loop
		declare
			Source				: constant String := To_String (Tests (Index));
			Escaped				: constant String := Escape (Source);
			Result				: constant String := DeEscape (Escaped);

		begin
			Put_Line ("Test" & Index'img & " source '" & Source & "'");
			Put_Line ("escaped '" & Escaped & "'");
			Put_Line ("descaped '" & Result & "'");

			if Source /= Result then
				Put_Line ("Test" & Index'img & " failed");
				Ok := False;
			end if;
		end;
	end loop;

	if Ok then
		Put_Line ("all tests passed");
	end if;

	exception
		when Fault: others =>
			Put_Line (Ada.Exceptions.Exception_Name (Fault) &
				" caught at " &GNAT.Source_Info.Source_Location);
			Put_Line (Ada.Exceptions.Exception_Message (Fault));
			GNAT.OS_Lib.OS_Exit (0);
						
end Database_Escape;