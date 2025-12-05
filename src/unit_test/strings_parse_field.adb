with Ada.Text_IO;use Ada.Text_IO;
with Strings;

procedure Strings_Parse_Field is

	type Fields_Array			is array (1 .. 5) of Strings.Constant_String_Access;

	type Test_Type				is record
		Pattern					: Strings.Constant_String_Access;
		Number_Fields			: Natural;
		Fields					: Fields_Array;
	end record;

	Tests						: array (Positive range <>) of Test_Type := (
		( new String'(""), 1, 
			( new String'(""), Null, Null, Null, Null ) ),
		( new String'("a"), 1, 
			( new String'("a"), Null, Null, Null, Null ) ),
		( new String'(",,"), 3, 
			( new String'(""), new String'(""), new String'(""), Null, Null ) ),
		( new String'("a,b,c"), 3, 
			( new String'("a"), new String'("b"), new String'("c"), Null, Null ) ),
		( new String'("a,,bc"),  3, 
			( new String'("a"), new String'(""), new String'("bc"), Null, Null ) )
	);

begin
	for Index in Tests'range loop
		declare
			Test				: Test_Type renames Tests (Index);

		begin
			for Field in 1 .. Test.Number_Fields + 1 loop
				declare
					Data		: constant String := Strings.Parse_Field (Test.Pattern.all, ',', Field);

				begin
					Put_Line (Index'img & Field'img & " = '" & Data & "'");
			
					if Field > Test.Number_Fields then
						if Data'length /= 0 then
							Put_Line ("data returned for non existent field '" & Data & "'");
						end if;
					elsif Data /= Test.Fields (Field).all then
						Put_Line ("missmatch. expected '" & Test.Fields (Field).all & "'");
					end if;

				end;
			end loop;
		end;
	end loop;
		
end Strings_Parse_Field;