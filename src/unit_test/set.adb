with Ada.Text_IO; use  Ada.Text_IO;

with Ada_Lib.Set;

procedure Test_Set is

	type Data_Type	is new Integer range 0 .. 10;
	 
	package Set	is new Ada_Lib.Set (Data_Type);


begin
	Put_Line ("1-3,5,7-9" & Set.Image ((
		1 .. 3  => True,
		5		=> True,
		7 .. 9	=> True,
		others => False)));
end Test_Set;