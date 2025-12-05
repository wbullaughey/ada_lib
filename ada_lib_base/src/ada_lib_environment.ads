
package Ada_Lib_Environment is

   type Environment_Variable_Kind_Type is (Help_Test_Kind, Unit_Test_Kind);

   function Parse_Environment_Variable (
      Kind  : in     Environment_Variable_Kind_Type
   ) return Boolean;

   Debug                : Boolean := False;

end Ada_Lib_Environment;
