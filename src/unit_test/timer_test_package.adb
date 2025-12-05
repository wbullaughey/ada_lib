with Ada.Calendar;
with Ada.Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada_Lib.Timer;
with Ada_Lib.Time;
with Ada_Lib.Trace; use Ada_Lib.Trace;

pragma Elaborate_All (Ada_Lib.Timer);

package body Timer_Test_Package is

--	Elaborate_All (Ada_Lib.Timer);
	use type Ada.Calendar.Time;

	subtype Time_Type			is Ada.Calendar.Time;

	function Now return Time_Type	
								renames Ada.Calendar.Clock;

	---------------------------------------------------------------------------
	procedure Dynamic_Test is
	---------------------------------------------------------------------------

		type Event_Type			is new Ada_Lib.Timer.Event_Type with record
			Count				: Positive;
			Event_Time			: Time_Type;
		end record;
	
		type Event_Access		is access all Event_Type;
	
		procedure Callback (
			Event				: in out Event_Type);
	
		procedure Free			is new Ada.Unchecked_Deallocation (
			Object	=> Event_Type,
			Name	=> Event_Access);
		
		protected Outstanding	is
		
			procedure Increment;
			procedure Decrement;
			function Count
			return Natural;
			
		private
			Value				: Natural := 0;
		end Outstanding;
	
		---------------------------------------------------------------------------
		procedure Callback (
			Event				: in out Event_Type) is
		---------------------------------------------------------------------------
	
			Pointer				: Event_Access := Event_Access'(Event'unchecked_access);
	
		begin
			Put_Line (Event.Count'img & Outstanding.Count'img & 
				" time offset " & Ada_Lib.Time.Image (Ada_Lib.Time.Now - Event.Event_Time, True));
			Outstanding.Decrement;
			Free (Pointer);
		end Callback;
	
		---------------------------------------------------------------------------
		protected body Outstanding		is
		
			-----------------------------------------------------------------------
			procedure Increment is
			-----------------------------------------------------------------------
	
			begin
				Value := Value + 1;
			end Increment;
	
			-----------------------------------------------------------------------
			procedure Decrement is
			-----------------------------------------------------------------------
	
			begin
				Value := Value - 1;
			end Decrement;
	
			-----------------------------------------------------------------------
			function Count
			return Natural is
			-----------------------------------------------------------------------
				
			begin
				return Value;
			end Count;
			
		end Outstanding;
		---------------------------------------------------------------------------
	
		Counter					: Natural := 0;
		Generator				: Ada.Numerics.Float_Random.Generator;

	begin
		loop
			if Outstanding.Count < 100 then
				Counter := Counter + 1;

				declare
					Event				: constant Event_Access := new Event_Type;
					Offset				: constant Duration := 
											Duration (Ada.Numerics.Float_Random.Random (
												Generator)) * 10.0;

				begin
					Put_Line ("set" & Counter'img & Offset'img);
					Event.Count := Counter;
					Outstanding.Increment;
					Event.Event_Time := Ada_Lib.Time.Now + Offset;
					Event.Set (Offset);
				end;
			else
				delay 0.1;
			end if;
		end loop;

	exception
		when Fault: others =>
			Put_Line ("unexpected exception " & Ada.Exceptions.Exception_Name (Fault) &
				" in dynamic test");
			Put_Line (Ada.Exceptions.Exception_Message (Fault));

	end Dynamic_Test;

	---------------------------------------------------------------------------
	procedure Static_Test is
	---------------------------------------------------------------------------

		type Event_Type			is new Ada_Lib.Timer.Event_Type with record
			Event_Time			: Time_Type;
		end record;
	
		procedure Callback (
			Event				: in out Event_Type);

		type Time_Array			is array (1 .. 100) of Time_Type;

		type Cycle_Event_Type	is new Ada_Lib.Timer.Repeating_Type with record
			Count				: Natural := 0;
			Times				: Time_Array;
		end record;
	
		procedure Callback (
			Event				: in out Cycle_Event_Type);

		type Table_Entry_Type	is record
			Event				: Event_Type;
			Offset				: Duration;
			Schedule_Time		: Time_Type;
		end record;

		Single_Event_Count		: Natural := 0;

		---------------------------------------------------------------------------
		procedure Callback (
			Event				: in out Event_Type) is
		---------------------------------------------------------------------------

		begin
			Event.Event_Time := Now;
			Single_Event_Count := Single_Event_Count + 1;
		end Callback;



		---------------------------------------------------------------------------
		procedure Callback (
			Event				: in out Cycle_Event_Type) is
		---------------------------------------------------------------------------

		begin
			if Event.Count < Event.Times'last then
				Event.Count := Event.Count + 1;
				Event.Times (Event.Count) := Now;
			end if;
		end Callback;
		---------------------------------------------------------------------------

		Initialize_Event		: Event_Type;
		Table					: array (Positive range <>) of Table_Entry_Type := (
			(
				Event				=> Initialize_Event,
				Offset				=> 10.0,
				Schedule_Time		=> Ada_Lib.Timer.Null_Time
			),
			(
				Event				=> Initialize_Event,
				Offset				=> 0.5,
				Schedule_Time		 => Ada_Lib.Timer.Null_Time
			),
			(
				Event				=> Initialize_Event,
				Offset				=> 6.0,
				Schedule_Time		 => Ada_Lib.Timer.Null_Time
			),
			(
				Event				=> Initialize_Event,
				Offset				=> 3.0,
				Schedule_Time		 => Ada_Lib.Timer.Null_Time
			),
			(
				Event				=> Initialize_Event,
				Offset				=> 0.25,
				Schedule_Time		 => Ada_Lib.Timer.Null_Time
			),
			(
				Event				=> Initialize_Event,
				Offset				=> 9.0,
				Schedule_Time		 => Ada_Lib.Timer.Null_Time
			),
			(
				Event				=> Initialize_Event,
				Offset				=> 11.0,
				Schedule_Time		 => Ada_Lib.Timer.Null_Time
			)
		);
		Cycle_Event				: array (1 .. 3) of Cycle_Event_Type;

	begin
		for Index in Table'range loop
			declare
				Cell			: Table_Entry_Type renames Table (Index);

			begin
				Cell.Schedule_Time := Cell.Offset + Now;
				Cell.Event.Set (Cell.Offset);
			end;
		end loop;

		for Index in Cycle_Event'range loop
			Cycle_Event (Index).Set (Duration (Index) * 0.133);
		end loop;

		while Single_Event_Count < Table'Length loop
			delay 0.1;
		end loop;

		Put_Line ("single events completed, cancel dynamic events");

		for Index in Cycle_Event'range loop
			Cycle_Event (Index).Cancel;
		end loop;

		delay 1.0;

		Put_Line ("all events completed");

		Put_Line ("single event delta");
		for Index in Table'range loop
			declare
				Cell			: Table_Entry_Type renames Table (Index);
				Offset			: constant Duration := abs (Cell.Schedule_Time - Cell.Event.Event_Time);

			begin
				if Offset > 0.1 then
					Put_Line ("Large offset for" & Index'img & " = " & Image (Offset, True));
				end if;
			end;
		end loop;

		for Event_Index in Cycle_Event'range loop
			declare
				Event			: Cycle_Event_Type renames Cycle_Event (Event_Index);

			begin
				Put_Line ("cycle offset for " & Image (Event.Offset, True));

				for Instance_Index in Time_Array'first + 1 .. Time_Array'last loop
					declare
						First	: Time_Type renames 
										Event.Times (Instance_Index - 1); 
						Second	: Time_Type renames 
										Event.Times (Instance_Index);
						Offset		: constant Duration := Second - First - Event.Offset;
						 
					begin
						if Offset > 0.1 then
							Put_Line (" step" & Instance_Index'img & " greater then 0.1: " &
								Image (Offset, True));
						end if;
					end; 
				end loop;
			end;
		end loop;

	exception
		when Fault: others =>
			Put_Line ("unexpected exception " & Ada.Exceptions.Exception_Name (Fault) &
				" in static test");
			Put_Line (Ada.Exceptions.Exception_Message (Fault));

	end Static_Test;

	---------------------------------------------------------------------------
	procedure Test is
	---------------------------------------------------------------------------

	begin
		Static_Test;
		Dynamic_Test;
	end Test;

end Timer_Test_Package;