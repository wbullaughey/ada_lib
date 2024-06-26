with Ada_Lib.Smart_Pointer;

pragma Elaborate_All (Ada_Lib.Smart_Pointer);

package Ada_Lib.Pmart_Pointer_Object is

   type Object_Type        is tagged record
      Field             : Integer;
   end record;

   type Object_Access         is access all Object_Type'class;

   package Pointer            is new Ada_Lib.Smart_Pointer.Pointer (
      Base_Type   => Object_Type,
      Base_Access => Object_Access);

end Ada_Lib.Pmart_Pointer_Object;