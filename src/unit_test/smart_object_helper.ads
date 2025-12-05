with Ada_Lib.Smart_Object;

pragma Elaborate_All (Ada_Lib.Smart_Object);

package Smart_Object_Helper is


	package Object is
		type Object_Type		is new Ada_Lib.Smart_Object.Contents_Type with private;
	
	private
	 
		type Object_Type		is new Ada_Lib.Smart_Object.Contents_Type with record
			Value				: Integer;
		end record;
	end Object;

	package Pointer is new Ada_Lib.Smart_Object.Pointer (Object.Object_Type);

end Smart_Object_Helper;