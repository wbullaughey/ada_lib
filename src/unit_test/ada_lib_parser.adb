with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Parser;
with Strings;

procedure Ada_Lib.Parser is

   type Key_Type        is (key_1);

   Keys              : constant String  := "subscribe";

   package Parser is new Ada_Lib.Parser.Name_Value (
      Comment_Seperator => '#',
--    Field_Seperators  => ';',
      Key_Seperator     => ':',
      Ignore_Spaces     => True,
      Key_Type       => Key_Type,
      Keys           => Keys,
      Value_Seperators  => ";");

   Tests             : constant array (Positive range <>) of
                        Strings.Constant_String_Access := (
      new String'("subscribe: error_message,exception_message,exception_message"),
      new String'("subscribe: command,echo,status")
   );

begin
   for Index in Tests'range loop
      declare
         Test        : Strings.Constant_String_Access renames
                        Tests (Index);

      begin
         Parser.Initialize (Test.all);

         while not Parser.At_End loop
            if Parser.Is_Key then
               declare
                  Key   : constant Key_Type := Parser.Get_Key;

               begin
                  Put_Line ("got key " & Key'img);
               end;
            else
               declare
                  Value : constant String := Parser.Get_Value;

               begin
                  Put_Line ("value '" & Value & "'");
                  declare
                     Iterator : Ada_Lib.Parser.Iterator_Type :=
                                 Ada_Lib.Parser.Initialize (
                                    Value                => Value,
                                    Seperators              => ",",
                                    Ignore_Multiple_Seperators => True,
                                    Comment_Seperator       => '#',
                                    Trim_Spaces             => True);
                  begin
                     while not Ada_Lib.Parser.At_End (Iterator) loop
                        declare
                           Name        : constant String := Ada_Lib.Parser.Get_Value (Iterator);

                        begin
                           put_line ("name '" & name & "'");
                           Ada_Lib.Parser.Next (Iterator);
                        end;
                     end loop;
                  end;
               end;
            end if;
         end loop;
      end;
   end loop;
end Ada_Lib.Parser;
