with Ada.Streams;
with Ada_Lib.Resource.Iterator;
-- with Ada_Lib.Set;			-- required for Elaborate_All
with Ada_Lib.Smart_Pointer;	-- required for Elaborate_All

pragma Elaborate_All (
	Ada_Lib.Resource,
	Ada_Lib.Resource.Iterator,
--	Ada_Lib.Set,
	Ada_Lib.Smart_Pointer);

package Test_Resource_Helper is

	type Resource_Type			is range 1 .. 1000;

	package Resource_Package	is new Ada_Lib.Resource (
		Index_Type			=> Resource_Type,
		Maximum_List_Length	=> 10000);

	package Iterator_Package	is new Resource_Package.Iterator;

	subtype Allocated_List_Type	is Resource_Package.Allocated_List_Type;

	subtype Allocateable_Resource_Type
								is Resource_Package.Allocatable_Type;

	subtype Allocated_Resource_Type
								is Resource_Package.Allocated_Type;

	subtype List_Iterator_Type	is Iterator_Package.List_Iterator_Type;

	subtype Map_Iterator_Type	is Iterator_Package.Map_Iterator_Type;

	procedure Allocate (
		Resource				: in	 Allocateable_Resource_Type;
		Count					: in	 Positive;
		Allocated				:	 out Allocated_Resource_Type'class;
		From_Where				: in	 String
	) renames Resource_Package.Allocate;

	procedure Allocate (
		Resource				: in	 Allocateable_Resource_Type;
		Count					: in	 Positive;
		Allocated				:	 out Allocated_List_Type;
		From_Where				: in	 String
	) renames Resource_Package.Allocate;

	procedure Allocate (
		Resource				: in	 Allocateable_Resource_Type;
		Allocated				:	 out Resource_Type
	) renames Resource_Package.Allocate;

	function Compare (
		Resource_1				: in	 Resource_Package.Resource_Type'class;
		Resource_2				: in	 Resource_Package.Resource_Type'class
	) return Boolean renames Resource_Package.Compare;

	function Count (
		Resource				: in	 Resource_Package.Resource_Type'class
	) return Natural renames Resource_Package.Count;

	function Count (
		Resource				: in	 Allocated_List_Type'class
	) return Natural renames Resource_Package.Count;

	function Create (
		First					: Resource_Type;
		Last					: Resource_Type
	) return Allocateable_Resource_Type renames Resource_Package.Create;

	function Image (
		Resource				: in	 Allocated_List_Type'class
	) return String renames Resource_Package.Image;

	function Index (
		List					: in	 Resource_Package.Allocated_List_Type'class;
		Iterator				: in 	 Iterator_Package.List_Iterator_Type
	) return Resource_Type renames Iterator_Package.Index;
		
	function Index (
		Resource_Pointer		: in	 Resource_Package.Resource_Type'class;
		Iterator				: in 	 Iterator_Package.Map_Iterator_Type
	) return Resource_Type renames Iterator_Package.Index;
		
	procedure Load (
		Resource				: in out Resource_Package.Allocatable_Type;
		Stream 					: access Ada.Streams.Root_Stream_Type'Class
	) renames Resource_Package.Load;

	procedure Load (
		Resource				: in out Resource_Package.Allocated_Type;
		Stream 					: access Ada.Streams.Root_Stream_Type'Class
	) renames Resource_Package.Load;

	procedure Load (
		List					: in out Resource_Package.Allocated_List_Type;
		Stream 					: access Ada.Streams.Root_Stream_Type'Class
	) renames Resource_Package.Load;

	procedure Next (
		List					: in	 Resource_Package.Allocated_List_Type'class;
		Iterator				: in out Iterator_Package.List_Iterator_Type;
		Exists					:	 out Boolean
	) renames Iterator_Package.Next;

	procedure Next (
		Resource_Pointer		: in	 Resource_Package.Resource_Type'class;
		Iterator				: in out Iterator_Package.Map_Iterator_Type;
		Exists					:	 out Boolean
	) renames Iterator_Package.Next;

	procedure Release (
		Resource				: in	 Allocateable_Resource_Type;
		Released				: in out Allocated_Resource_Type'class
	) renames Resource_Package.Release;

	procedure Release (
		Resource				: in	 Allocateable_Resource_Type;
		Released				: in out Allocated_List_Type'class
	) renames Resource_Package.Release;

	procedure Release (
		Resource				: in	 Allocateable_Resource_Type;
		Released				: in	 Resource_Type
	) renames Resource_Package.Release;

	procedure Store (
		Resource				: in	 Resource_Package.Allocatable_Type;
		Stream 					: access Ada.Streams.Root_Stream_Type'Class
	) renames Resource_Package.Store;

	procedure Store (
		Resource				: in	 Resource_Package.Allocated_Type;
		Stream 					: access Ada.Streams.Root_Stream_Type'Class
	) renames Resource_Package.Store;

	procedure Store (
		List					: in	 Resource_Package.Allocated_List_Type;
		Stream 					: access Ada.Streams.Root_Stream_Type'Class
	) renames Resource_Package.Store;

end Test_Resource_Helper;