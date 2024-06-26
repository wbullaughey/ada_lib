with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Exit;
with Test_Resource_Helper;
with GNAT.Source_Info;

procedure Test_Resource is

	function Here
	return String renames GNAT.Source_Info.Source_Location;
	use type Test_Resource_Helper.Resource_Type;

	package Stream_IO			renames Ada.Streams.Stream_IO;

	Failed						: exception;

	First						: constant Test_Resource_Helper.Resource_Type := 100;
	Last						: constant Test_Resource_Helper.Resource_Type := 900;
	Max_Resources				: constant Positive := Positive (Last - First + 1);

	---------------------------------------------------------------------------
	procedure Test_Block is
	---------------------------------------------------------------------------

		Block_Count				: constant := 10;
		Blocks					: array (1 .. Max_Resources / Block_Count) of
									Test_Resource_Helper.Allocated_Resource_Type;
		Resources				: Test_Resource_Helper.Allocateable_Resource_Type :=
									Test_Resource_Helper.Create (First, Last);
		Remaining_Resources		: constant Natural := 
									Max_Resources - Block_Count * Blocks'length;

	begin
		for Index in Blocks'range loop
			Test_Resource_Helper.Allocate (Resources, Block_Count, Blocks (Index));
		end loop; 
			
		if Test_Resource_Helper.Count (Resources) /= Remaining_Resources then 
			Put_Line ("incorrect available resources after block allocation");
			Put_Line ("expected" & Remaining_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Resources) 'img);
			raise Failed;
		end if;

		Put_Line ("available count after block allocation correct");

		for Even in Boolean'range loop
			for Index in Blocks'range loop
				if Even = (Index mod 2 = 0) then
					Test_Resource_Helper.Release (Resources, Blocks (Index));
				end if;
			end loop; 
		end loop; 
		
		if Test_Resource_Helper.Count (Resources) /= Max_Resources then
			Put_Line ("incorrect available resources after all returned blocks");
			Put_Line ("expected" & Max_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Resources) 'img);
			raise Failed;
		end if;

		Put_Line ("available count correct after returning blocks");
		Put_Line ("block test completed");
	end Test_Block;

	---------------------------------------------------------------------------
	procedure Test_Compare is
	---------------------------------------------------------------------------

		Allocated_1				: Test_Resource_Helper.Allocated_Resource_Type;
		Allocated_2				: Test_Resource_Helper.Allocated_Resource_Type;
		Allocated_Copy			: Test_Resource_Helper.Allocated_Resource_Type;
		Resources_1				: Test_Resource_Helper.Allocateable_Resource_Type :=
									Test_Resource_Helper.Create (First, Last);
		Resources_2				: Test_Resource_Helper.Allocateable_Resource_Type :=
									Test_Resource_Helper.Create (First, Last);

	begin
		Test_Resource_Helper.Allocate (Resources_1, 33, Allocated_1);
		Test_Resource_Helper.Allocate (Resources_2, 33, Allocated_2);
		
		if not Test_Resource_Helper.Compare (Allocated_1, Allocated_2) then
			Put_Line ("compare just allocated resources failed");
			raise Failed;
		end if;

		Test_Resource_Helper.Release (Resources_2, Allocated_2);

		Test_Resource_Helper.Allocate (Resources_2, 33, Allocated_2);
		
		if not Test_Resource_Helper.Compare (Allocated_1, Allocated_2) then
			Put_Line ("compare reallocated resources failed");
			raise Failed;
		end if;

		Test_Resource_Helper.Release (Resources_2, Allocated_2);

		Test_Resource_Helper.Allocate (Resources_2, 32, Allocated_2);
		
		if Test_Resource_Helper.Compare (Allocated_1, Allocated_2) then
			Put_Line ("compare smaller resource incorrectly compared");
			raise Failed;
		end if;

		Allocated_Copy := Allocated_1;

		if not Test_Resource_Helper.Compare (Allocated_1, Allocated_Copy) then
			Put_Line ("compare copied resource failed");
			raise Failed;
		end if;


		Put_Line ("compare test completed");
	end Test_Compare;

	---------------------------------------------------------------------------
	procedure Test_Map_Iterator is
	---------------------------------------------------------------------------

		Allocated				: Test_Resource_Helper.Allocated_Resource_Type;
		Block_Count				: constant := 10;
		Blocks					: array (1 .. Max_Resources / Block_Count) of
									Test_Resource_Helper.Allocated_Resource_Type;
		Resources				: Test_Resource_Helper.Allocateable_Resource_Type :=
									Test_Resource_Helper.Create (First, Last);
		Released_Resources		: Natural := 0;

	begin
		-- allocate all blocks
		for Index in Blocks'range loop
			Test_Resource_Helper.Allocate (Resources, Block_Count, Blocks (Index));
		end loop; 
			
		-- release odd blocks to create gaps
		for Index in Blocks'range loop
			if Index mod 2 = 1 then
				Test_Resource_Helper.Release (Resources, Blocks (Index));
				Released_Resources := Released_Resources + Block_Count;
			end if;
		end loop; 
		
		-- allocate released blocks to one allocated
		Test_Resource_Helper.Allocate (Resources, Released_Resources, Allocated);

		if Test_Resource_Helper.Count (Allocated) /= Released_Resources then 
			Put_Line ("incorrect available resources after block allocation");
			Put_Line ("expected" & Released_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Allocated) 'img);
			raise Failed;
		end if;

		-- iterate over allocated resources
		declare
			Counter				: Natural := 0;
			Expected_Resource	: Test_Resource_Helper.Resource_Type := First;
			Iterator			: Test_Resource_Helper.Map_Iterator_Type;

		begin
			loop
				declare
					Exists		: Boolean;

				begin
					Test_Resource_Helper.Next (Allocated, Iterator, Exists);

					if not Exists then
						exit;
					end if;

					Counter := Counter + 1;

					declare
						Resource	: constant Test_Resource_Helper.Resource_Type :=
										Test_Resource_Helper.Index (Allocated, Iterator);

					begin
						if Resource /= Expected_Resource then
							Put_Line ("unexpected resource from iterator");
							Put_Line ("expected" & Expected_Resource'img);
							Put_Line ("actual" & Resource'img);
							raise Failed;
						end if;

						if Resource mod Block_Count = Block_Count - 1 then
							Expected_Resource := Expected_Resource + Block_Count + 1;
						else
							Expected_Resource := Expected_Resource + 1;	
						end if;
					end;
				end;
			end loop;

			if Counter /= Released_Resources then
				Put_Line ("iterator returned wrong number of blocks");
				Put_Line ("expected" & Released_Resources'img);
				Put_Line ("actual" & Counter'img);
				raise Failed;
			end if;
				
		end;

		Put_Line ("map iterator test completed");
	end Test_Map_Iterator;

	---------------------------------------------------------------------------
	procedure Test_List_Iterator is
	---------------------------------------------------------------------------

		Allocated				: Test_Resource_Helper.Allocated_List_Type;
		Block_Count				: constant := 10;
		Blocks					: array (1 .. Max_Resources / Block_Count) of
									Test_Resource_Helper.Allocated_Resource_Type;
		Resources				: Test_Resource_Helper.Allocateable_Resource_Type :=
									Test_Resource_Helper.Create (First, Last);
		Released_Resources		: Natural := 0;

	begin
		-- allocate all blocks
		for Index in Blocks'range loop
			Test_Resource_Helper.Allocate (Resources, Block_Count, Blocks (Index));
		end loop; 
			
		-- release odd blocks to create gaps
		for Index in Blocks'range loop
			if Index mod 2 = 1 then
				Test_Resource_Helper.Release (Resources, Blocks (Index));
				Released_Resources := Released_Resources + Block_Count;
			end if;
		end loop; 
		
		-- allocate released blocks to one allocated
		Test_Resource_Helper.Allocate (Resources, Released_Resources, Allocated);

		if Test_Resource_Helper.Count (Allocated) /= Released_Resources then 
			Put_Line ("incorrect available resources after block allocation");
			Put_Line ("expected" & Released_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Allocated) 'img);
			raise Failed;
		end if;

		-- iterate over allocated resources
		declare
			Counter				: Natural := 0;
			Expected_Resource	: Test_Resource_Helper.Resource_Type := First;
			Iterator			: Test_Resource_Helper.List_Iterator_Type;

		begin
			loop
				declare
					Exists		: Boolean;

				begin
					Test_Resource_Helper.Next (Allocated, Iterator, Exists);

					if not Exists then
						exit;
					end if;

					Counter := Counter + 1;

					declare
						Resource	: constant Test_Resource_Helper.Resource_Type :=
										Test_Resource_Helper.Index (Allocated, Iterator);

					begin
						if Resource /= Expected_Resource then
							Put_Line ("unexpected resource from iterator");
							Put_Line ("expected" & Expected_Resource'img);
							Put_Line ("actual" & Resource'img);
							raise Failed;
						end if;

						if Resource mod Block_Count = Block_Count - 1 then
							Expected_Resource := Expected_Resource + Block_Count + 1;
						else
							Expected_Resource := Expected_Resource + 1;	
						end if;
					end;
				end;
			end loop;

			if Counter /= Released_Resources then
				Put_Line ("iterator returned wrong number of blocks");
				Put_Line ("expected" & Released_Resources'img);
				Put_Line ("actual" & Counter'img);
				raise Failed;
			end if;
				
		end;

		Put_Line ("list iterator test completed");
	end Test_List_Iterator;

	---------------------------------------------------------------------------
	procedure Test_List is
	---------------------------------------------------------------------------

		Block_Count				: constant := 10;
		Lists					: array (1 .. Max_Resources / Block_Count) of
									Test_Resource_Helper.Allocated_List_Type;
		Resources				: Test_Resource_Helper.Allocateable_Resource_Type :=
									Test_Resource_Helper.Create (First, Last);
		Remaining_Resources		: constant Natural := 
									Max_Resources - Block_Count * Lists'length;

	begin
		for Index in Lists'range loop
			Test_Resource_Helper.Allocate (Resources, Block_Count, Lists (Index));
		end loop; 
			
		if Test_Resource_Helper.Count (Resources) /= Remaining_Resources then 
			Put_Line ("incorrect available resources after list allocation");
			Put_Line ("expected" & Remaining_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Resources) 'img);
			raise Failed;
		end if;

		Put_Line ("available count after list allocation correct");

		for Even in Boolean'range loop
			declare
				Released		: Natural := 0;

			begin
				for Index in Lists'range loop
					if Even = (Index mod 2 = 0) then
						Test_Resource_Helper.Release (Resources, Lists (Index));
						Released := Released + Block_Count;
					end if;
				end loop;
				
				if not Even then
					-- allocate all remaining
					declare
						Remaining	: Test_Resource_Helper.Allocated_List_Type;
						
					begin
						Test_Resource_Helper.Allocate (Resources, 
							Test_Resource_Helper.Count (Resources), Remaining);

						Put_Line ("remaining resources: " &
							Test_Resource_Helper.Image (Remaining));

						if Test_Resource_Helper.Count (Resources) /= 0 then
							Put_Line ("not all resorces were allocated to all list"); 
							raise Failed;
						end if;

						if Test_Resource_Helper.Count (Remaining) /= 
								Released + Remaining_Resources then
							Put_Line ("all list count wrong." &
								" got" & Test_Resource_Helper.Count (Remaining)'img &
								" expected" & Natural'image (Released + Remaining_Resources));
						end if;

						Test_Resource_Helper.Release (Resources, Remaining);
					end;						
				end if;
			end;
		end loop; 
		
		if Test_Resource_Helper.Count (Resources) /= Max_Resources then
			Put_Line ("incorrect available resources after all returned blocks");
			Put_Line ("expected" & Max_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Resources) 'img);
			raise Failed;
		end if;

		Put_Line ("available count correct after returning Lists");
		Put_Line ("list test completed");
	end Test_List;

	---------------------------------------------------------------------------
	procedure Test_Load is
	---------------------------------------------------------------------------

		File					: Stream_IO.File_Type;

		-----------------------------------------------------------------------
		function Open_Stream (
			Name				: in	 String
		) return Stream_IO.Stream_Access is
		-----------------------------------------------------------------------

		begin
			Stream_IO.Open (File, Stream_IO.In_File, Name);
			return Stream_IO.Stream (File);
		end Open_Stream;
		-----------------------------------------------------------------------

		Allocated				: Test_Resource_Helper.Allocated_Resource_Type;
		Block_Count				: constant := 10;
		List					: Test_Resource_Helper.Allocated_List_Type;
		Resources				: Test_Resource_Helper.Allocateable_Resource_Type;

	begin
		Test_Resource_Helper.Load (Resources, Open_Stream ("resources"));
		Stream_IO.Close (File);

		Test_Resource_Helper.Load (Allocated, Open_Stream ("allocated"));
		Stream_IO.Close (File);
		Test_Resource_Helper.Release (Resources, Allocated);

		Test_Resource_Helper.Load (List, Open_Stream ("list"));
		Stream_IO.Close (File);
		Test_Resource_Helper.Release (Resources, List);

		if Test_Resource_Helper.Count (Resources) /= Natural (Last - First + 1) then
			Put_Line ("wrong number of blocks after load resource");
			Put_Line ("expected" & Test_Resource_Helper.Resource_Type'image (Last - First + 1));
			Put_Line ("got" & Test_Resource_Helper.Count (Resources)'img);
		end if;

		Put_Line ("load test completed");
	end Test_Load;

	---------------------------------------------------------------------------
	procedure Test_Single is
	---------------------------------------------------------------------------
	
		Resources					: Test_Resource_Helper.Allocateable_Resource_Type :=
										Test_Resource_Helper.Create (First, Last);
	begin
		if Test_Resource_Helper.Count (Resources) /= Max_Resources then
			Put_Line ("incorrect available resources after create");
			Put_Line ("expected" & Max_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Resources) 'img);
			raise Failed;
		end if;

		Put_Line ("initial available count correct");

		-- test allocating one at a time until all gone
		declare
			Expected				: Test_Resource_Helper.Resource_Type := First;
			Resource				: Test_Resource_Helper.Resource_Type;

		begin
			loop
				begin
					Test_Resource_Helper.Allocate (Resources, Resource);
				
				exception
					when Test_Resource_Helper.Resource_Package.Empty =>
						-- all resources have been allocated
						-- check that last one allocated was last one created.
						if Resource = Last then
							exit;
						else
							Put_Line ("unexpected resource allocated");
							Put_Line ("expected" & Last'img);
							Put_Line ("actual" & Resource'img);
							raise Failed;
						end if;
				end;

				if Resource /= Expected then	
					Put_Line ("unexpected resource allocated for last");
					Put_Line ("expected" & Expected'img);
					Put_Line ("actual" & Resource'img);
					raise Failed;
				end if;

				Expected := Expected + 1;
			end loop;
		end;

		if Test_Resource_Helper.Count (Resources) /= 0 then
			Put_Line ("non-zero resources" & 
				Test_Resource_Helper.Count (Resources)'img &
				" available after all allocated indvidually");
			raise Failed;
		end if;

		Put_Line ("correct number resource allocated individually");

		for Resource in First .. Last loop
			Test_Resource_Helper.Release (Resources, Resource);
		end loop;		
		
		if Test_Resource_Helper.Count (Resources) /= Max_Resources then
			Put_Line ("incorrect available resources after all returned individually");
			Put_Line ("expected" & Max_Resources'img);
			Put_Line ("actual" & Test_Resource_Helper.Count (Resources) 'img);
			raise Failed;
		end if;

		Put_Line ("available count correct after returning individually");
		Put_Line ("single test completed");
	end Test_Single;

	---------------------------------------------------------------------------
	procedure Test_Store is
	---------------------------------------------------------------------------

		File					: Stream_IO.File_Type;

		-----------------------------------------------------------------------
		function Create_Stream (
			Name				: in	 String
		) return Stream_IO.Stream_Access is
		-----------------------------------------------------------------------

		begin
			Stream_IO.Create (File, Stream_IO.Out_File, Name);
			return Stream_IO.Stream (File);
		end Create_Stream;
		-----------------------------------------------------------------------

		Allocated				: Test_Resource_Helper.Allocated_Resource_Type;
		Block_Count				: constant := 10;
		List					: Test_Resource_Helper.Allocated_List_Type;
		Resources				: Test_Resource_Helper.Allocateable_Resource_Type :=
									Test_Resource_Helper.Create (First, Last);

	begin
		Test_Resource_Helper.Allocate (Resources, Block_Count, Allocated);
		Test_Resource_Helper.Allocate (Resources, Block_Count, List);

		Test_Resource_Helper.Store (Resources, Create_Stream ("resources"));
		Stream_IO.flush (File);
		Stream_IO.Close (File);
		Test_Resource_Helper.Store (Allocated, Create_Stream ("allocated"));
		Stream_IO.flush (File);
		Stream_IO.Close (File);

		Test_Resource_Helper.Store (List, Create_Stream ("list"));
		Stream_IO.flush (File);
		Stream_IO.Close (File);
		Put_Line ("store test completed");
	end Test_Store;

	---------------------------------------------------------------------------

begin
	Test_Single;
	Test_Block;
	Test_List;
	Test_Map_Iterator;
	Test_List_Iterator;
	Test_Compare;
	Test_Store;
	Test_Load;
	Put_Line ("resource test completed");
	Test_Exit.OK;

exception

	when Failed =>
		Test_Exit.Failed;

	when Fault: others =>
		Put_Line (Ada.Exceptions.Exception_Name (Fault));
		Put_Line (Ada.Exceptions.Exception_Message (Fault));
		Test_Exit.Failed;

end Test_Resource;