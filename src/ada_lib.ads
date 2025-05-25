with Ada_Lib_Environment;
pragma Elaborate (Ada_Lib_Environment);

package Ada_Lib is

   Bits_Per_Byte     : constant := 8;
   Exception_Occured : Boolean := False;
   Help_Test         : constant Boolean := Ada_Lib_Environment.
                        Parse_Environment_Variable (
-- use for verifying help menu items
                           Ada_Lib_Environment.Help_Test_Kind);
   Unit_Testing      : constant Boolean := Ada_Lib_Environment.
                        Parse_Environment_Variable (
                           Ada_Lib_Environment.Unit_Test_Kind);
-- program built to do unit testing

end Ada_Lib;
