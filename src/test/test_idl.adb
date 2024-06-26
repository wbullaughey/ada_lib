with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.IDL;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Interfaces;
with Strings.Bounded; use Strings.Bounded;

procedure Test_IDL is

	Failed						: exception;

	use type Ada_Lib.Idl.Double;
	use type Interfaces.Integer_8;
	use type Interfaces.Integer_16;
	use type Interfaces.Integer_32;
	use type Interfaces.Integer_64;

	type Field_Kind_Type		is (
		Boolean_Kind,
		Bounded,
		Double,
		Int_8, Hex_8,
		Int_16, Hex_16,
		Int_32, Hex_32,
		Int_64, Hex_64,
		Standard_Integer,
		Text_Kind,
		Empty
	);

	type Field_Type (
		Kind					: Field_Kind_Type := Empty) is record

		case Kind is

			when Boolean_Kind =>
				Bool			: Boolean;

			when Bounded =>
				Bounded			: Ada_Lib.IDL.Default_String_Type;

			when Double =>
				Double			: Ada_Lib.IDL.Double;

			when Empty =>
				null;

			when Int_8 | Hex_8 =>
				Int_8			: Interfaces.Integer_8;

			when Int_16 | Hex_16 =>
				Int_16			: Interfaces.Integer_16;

			when Int_32 | Hex_32 =>
				Int_32			: Interfaces.Integer_32;

			when Int_64 | Hex_64 =>
				Int_64			: Interfaces.Integer_64;

			when Standard_Integer =>
				Standard_Integer	: Integer;

			when Text_Kind =>
				Text			: Strings.Constant_String_Access;

		end case;
	end record;

	type Test_Array				is array (1 .. 5) of Field_Type;

	Tests						: constant array (Positive range <>) of
									Test_Array := (
		(
			(
				Kind		=> Boolean_Kind,
				Bool		=> True
			),
			(
				Kind		=> Boolean_Kind,
				Bool		=> False
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Bounded,
				Bounded		=> Strings.Bounded.Bounded (
									Ada_Lib.IDL.Largest_Default_String, "")
			),
			(
				Kind		=> Bounded,
				Bounded		=> Strings.Bounded.Bounded (
									Ada_Lib.IDL.Largest_Default_String, "a&b;c")
			),
			(
				Kind		=> Bounded,
				Bounded		=> Strings.Bounded.Bounded (
									Ada_Lib.IDL.Largest_Default_String, "X==Y")
			),
			(
				Kind		=> Bounded,
				Bounded		=> Strings.Bounded.Bounded (
									Ada_Lib.IDL.Largest_Default_String, "X==")
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Double,
				Double		=> 1.1
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Int_8,
				Int_8		=> 123
			),
			(
				Kind		=> Int_8,
				Int_8		=> -123
			),
			(
				Kind		=> Hex_8,
				Int_8		=> 16#12#
			),
			(
				Kind		=> Hex_8,
				Int_8		=> -127
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Int_16,
				Int_16		=> 123
			),
			(
				Kind		=> Int_16,
				Int_16		=> -123
			),
			(
				Kind		=> Hex_16,
				Int_16		=> 16#1234#
			),
			(
				Kind		=> Hex_16,
				Int_16		=> -127
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Int_32,
				Int_32		=> 123
			),
			(
				Kind		=> Int_32,
				Int_32		=> -123
			),
			(
				Kind		=> Hex_32,
				Int_32		=> 16#12345678#
			),
			(
				Kind		=> Hex_32,
				Int_32		=> -127
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Int_64,
				Int_64		=> 123
			),
			(
				Kind		=> Int_64,
				Int_64		=> -123
			),
			(
				Kind		=> Hex_64,
				Int_64		=> 16#1234567812345678#
			),
			(
				Kind		=> Hex_64,
				Int_64		=> -127
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Standard_Integer,
				Standard_Integer
							=> 99
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Text_Kind,
				Text		=> new String'("abc")
			),
			(
				Kind		=> Text_Kind,
				Text		=> new String'("xyz")
			),
			others => (Kind => Empty)
		),
		(
			(
				Kind		=> Text_Kind,
				Text		=> new String'("a&b;c")
			),
			(
				Kind		=> Text_Kind,
				Text		=> new String'("x==z")
			),
			(
				Kind		=> Text_Kind,
				Text		=> new String'("x==")
			),
			others => (Kind => Empty)
		)
	);

	Trace						: constant Boolean := True;

begin
	for Test_Number in Tests'range loop
		if Trace then
			Put (Here, "test" & Test_Number'img);
		end if;

		declare
			Test				: Test_Array renames Tests (Test_Number);
			Write_Buffer		: Ada_Lib.IDL.Write_Buffer;

		begin
			-- encode the test
			for Field_Number in Test'range loop
				declare
					Field		: Field_Type renames Test (Field_Number);

				begin
					if Trace then
						Put (Here, "pushd field" & Field_Number'img &
							" kind " & Field.Kind'img);
					end if;

					case Field.Kind is

						when Boolean_Kind =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Bool);

						when Bounded =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Bounded);
							
						when Double =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Double);

						when Empty =>
							null;

						when Int_8 | Hex_8 =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Int_8, Field.Kind = Hex_8);
							
						when Int_16 | Hex_16 =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Int_16, Field.Kind = Hex_16);
							
						when Int_32 | Hex_32 =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Int_32, Field.Kind = Hex_32);
							
						when Int_64 | Hex_64 =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Int_64, Field.Kind = Hex_64);
							
						when Standard_Integer =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Standard_Integer);

						when Text_Kind =>
							Ada_Lib.IDL.Push (Write_Buffer, Field.Text.all);

					end case;
				end;
			end loop;

			if Trace then
				Put (Here, "start decode" & Test_Number'img);
			end if;

			-- decode the test
			declare
				Read_Buffer		: Ada_Lib.IDL.Read_Buffer;
				Empty_Tested	: Boolean := False;

			begin
				Ada_Lib.IDL.Initialize (Read_Buffer,
					Ada_Lib.IDL.To_String (Write_Buffer));

				for Field_Number in Test'range loop
					declare
						Field		: Field_Type renames Test (Field_Number);

					begin
						if Trace then
							Put (Here, "pop field" & Field_Number'img &
								" kind " & Field.Kind'img);
						end if;

						case Field.Kind is

							when Boolean_Kind =>
								declare
									Popped	: Boolean := False;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped);

									if Popped = Field.Bool then
										Put (Here, Popped'img & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped " & Popped'img & 
											"' did not match '" & Field.Bool'img & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Bounded =>
								declare
									Popped	: Ada_Lib.IDL.Default_String_Type;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped);

									if Popped = Field.Bounded then
										Put (Here, Strings.Bounded.To_String (Popped) & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped '" & Strings.Bounded.To_String (Popped) & 
											"' did not match '" & 
												Strings.Bounded.To_String (Field.Bounded) & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Double =>
								declare
									Popped	: Ada_Lib.IDL.Double := 0.0;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped);

									if Popped = Field.Double then
										Put (Here, Popped'img & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped " & Popped'img & 
											"' did not match '" & Field.Double'img & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Empty =>
								if not Empty_Tested then
									Empty_Tested := True;
									declare
										Value	: Interfaces.Integer_32 := 0;

									begin
										Ada_Lib.IDL.Pop (Read_Buffer, Value, False);

									exception
										when Ada_Lib.IDL.No_Value =>
										
										if Trace then
											Put (Here, "got expected No_Value exception");
										end if;
									end;
								end if;

							when Int_8 | Hex_8 =>
								declare
									Popped	: Interfaces.Integer_8 := 0;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped, Field.Kind = Hex_8);

									if Popped = Field.Int_8 then
										Put (Here, Popped'img & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped " & Popped'img & 
											"' did not match '" & Field.Int_8'img & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Int_16 | Hex_16 =>
								declare
									Popped	: Interfaces.Integer_16 := 0;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped, Field.Kind = Hex_16);

									if Popped = Field.Int_16 then
										Put (Here, Popped'img & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped " & Popped'img & 
											"' did not match '" & Field.Int_16'img & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Int_32 | Hex_32 =>
								declare
									Popped	: Interfaces.Integer_32 := 0;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped, Field.Kind = Hex_32);

									if Popped = Field.Int_32 then
										Put (Here, Popped'img & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped " & Popped'img & 
											"' did not match '" & Field.Int_32'img & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Int_64 | Hex_64 =>
								declare
									Popped	: Interfaces.Integer_64 := 0;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped, Field.Kind = Hex_64);

									if Popped = Field.Int_64 then
										Put (Here, Popped'img & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped " & Popped'img & 
											"' did not match '" & Field.Int_64'img & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Standard_Integer =>
								declare
									Popped	: Integer := 0;

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Popped);

									if Popped = Field.Standard_Integer then
										Put (Here, Popped'img & " matched");
									else
										Ada.Exceptions.Raise_Exception (Failed'identity,
											"popped " & Popped'img & 
											"' did not match '" & Field.Standard_Integer'img & 
											"' for test" & Test_Number'img &
											" field" & Field_Number'img);
									end if;
								end;

							when Text_Kind =>
								declare
									Text	: Bounded_Type (10);

								begin
									Ada_Lib.IDL.Pop (Read_Buffer, Text);

									declare
										Popped	: constant String :=
													To_String (Text);
				
									begin
										if Popped = Field.Text.all then
											Put (Here, "'" & Popped & "' matched");
										else
											Ada.Exceptions.Raise_Exception (Failed'identity,
												"popped '" & Popped & 
												"' did not match '" & Field.Text.all & 
												"' for test" & Test_Number'img &
												" field" & Field_Number'img);
										end if;
									end;
								end;
						end case;
					end;
				end loop;
			end;
		end;
	end loop;

	Put_Line ("test completed successfully");

exception
	when Fault: Failed =>
		Put (Here, Ada.Exceptions.Exception_Message (Fault));

	when Fault: others =>
		Put (Here, Fault);

end Test_IDL;

