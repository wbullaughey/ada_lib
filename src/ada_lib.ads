with Build_Mode;

package Ada_Lib is

   Bits_Per_Byte                 : constant := 8;
   Exception_Occured             : Boolean := False;
   Help_Test                     : Boolean := Build_Mode.Help_Test;
   Unit_Testing                  : Boolean := False;

end Ada_Lib;
