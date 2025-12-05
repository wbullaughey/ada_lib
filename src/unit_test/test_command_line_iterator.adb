-- driver program that executes test program with various cases

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Command_Line_Iterator_Test_Helper; use Command_Line_Iterator_Test_Helper;
with GNAT.OS_Lib;
with Strings;

procedure Test_Command_Line_Iterator is

	Null_ID						: Ada.Exceptions.Exception_Id renames
									Ada.Exceptions.Null_ID;

	type Case_Type				is record
		Command_Line			: Strings.Constant_String_Access;
		Exception_ID			: Ada.Exceptions.Exception_Id;
	end record;

	use type Ada.Exceptions.Exception_Id;

	Tests						: constant array (Positive range <>) of 
									Case_Type := (
		(1 =>
			(
				Command_Line 	=> new String'("-echo abc"),
				Exception_ID	=> Null_ID
			)
		)
	);

begin
	for Index in Tests'range loop
		declare
			Test				: Case_Type renames Tests (Index);
			Result				: constant Integer :=
									GNAT.OS_Lib.Spawn ("Command_Line_Iterator_Test",
										GNAT.OS_Lib.Argument_String_To_List (
											Test.Command_Line.all).all);

		begin
			Put_Line ("result " & Result'img & " for '" & Test.Command_Line.all & "'");

			if Result > 0 then	-- returned exception
				declare
					Id			: constant Ada.Exceptions.Exception_Id :=
									Exception_ID (Result);

				begin
					if ID /= Test.Exception_ID then
						Put_Line ("test raise unexpected exception " &
							Ada.Exceptions.Exception_Name (Exception_ID (Result)));

					end if;
				end;
			elsif Result < 0 then
				Put_Line ("unexpected exception from test");
			end if;
		end;
	end loop;
end Test_Command_Line_Iterator;