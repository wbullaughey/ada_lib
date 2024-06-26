with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Exit is

	use type Ada.Command_Line.Exit_Status;

	procedure OK is

	begin
		Ada.Command_Line.Set_Exit_Status (0);
	end OK;

	procedure Failed is

	begin
		Ada.Command_Line.Set_Exit_Status (-1);
	end Failed;

	procedure Fault (
		Fault					: in	 Ada.Exceptions.Exception_Occurrence;
		Here					: in	 String) is

	begin
		Put_Line ("test exit report");
		Put_Line ("exception name: " & Ada.Exceptions.Exception_Name (Fault) & " caught at " & Here);
		Put_Line ("exception message: " & Ada.Exceptions.Exception_Message (Fault));
		Ada.Command_Line.Set_Exit_Status (-2);
	end Fault;

end Test_Exit;