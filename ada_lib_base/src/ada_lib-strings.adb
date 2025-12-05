with Ada.Characters.Handling;
with Ada.Strings.Maps;
with Ada_Lib.Substiture_For_Non_Alpha;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.Strings is

   -------------------------------------------------------------------
   function Is_Decimal (
      Source                     : in     String
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      for Index in Source'range loop
         if not Ada.Characters.Handling.Is_Decimal_Digit (Source (Index)) then
            return false;
         end if;
      end loop;

      return True;
   end Is_Decimal;

   -------------------------------------------------------------------
   function Optional (
      Source               : in   String_Constant_Access
   ) return String is
   -------------------------------------------------------------------

   begin
      if Source = Null then
         return "NULL";
      else
         return Source.all;
      end if;
   end Optional;

   -------------------------------------------------------------------
   function Optional (
      Source               : in   String_Access
   ) return String is
   -------------------------------------------------------------------

   begin
      if Source = Null then
         return "NULL";
      else
         return Source.all;
      end if;
   end Optional;

   -------------------------------------------------------------------
   function Pad (
      Source               : in   String;
      Length               : in   Positive;
      Side              : in   Ada.Strings.Trim_End := Right
   ) return String is
   -------------------------------------------------------------------

   begin
      if Source'length >= Length then
         return Source;
      end if;

      declare
         Padding           : constant String (Source'length + 1 .. Length) :=
                           ( others => ' ');

      begin
         case Side is

            when Both =>
               declare
                  Middle   : constant Natural :=
                           Padding'first + Padding'length / 2 - 1;

               begin
                  return Padding (Padding'first .. Middle) &
                     Source &
                     Padding (Middle + 1 .. Padding'last);
               end;

            when Left =>
               return Padding & Source;

            when Right =>
               return Source & Padding;

         end case;
      end;
   end Pad;

   -------------------------------------------------------------------
   function Parse_Field (
      Source               : in   String;
      Seperator            : in   Character;
      Index                : in   Positive
   ) return String is
   -------------------------------------------------------------------

      Pattern              : constant String := ( 1 => Seperator );
      Start                : Natural := Source'first;

   begin
      Log_In (Debug, Quote ("source", Source) &
         Quote (" seperator", Seperator) &
         " index" & Index'img);

      for Count in 1 .. Index loop
         declare
            Seperator_Index      : Natural := Ada_Lib.Strings.Index (
                                    Source (Start .. Source'last), Pattern);

         begin
            Log_Here (Debug, "seperator_index " & Seperator_Index'img);
            if Seperator_index = 0 then
               Seperator_index := Source'last + 1;
            end if;

            if Count = Index then
               if Seperator_index = Start then
                  Log_Here (Debug);
                  return "";
               end if;

               Log_Out (Debug, Quote ("returned", Source (Start .. Seperator_index - 1)));
               return Source (Start .. Seperator_index - 1);
            end if;

            Start := Seperator_index + 1;

            if Start > Source'last then
               Log_Here (Debug);
               exit;
            end if;
         end;
         Log_Here (Debug, "Start" & Start'img);
      end loop;

      Log_Out (Debug);
      return "";
   end Parse_Field;

   -------------------------------------------------------------------
   function Quote_A_String (
      Source                     : in     String;
      Quote                      : in     Character := '"'
   ) return String is
   -------------------------------------------------------------------

   begin
      return Quote & Ada_Lib.Substiture_For_Non_Alpha.Substitute (Source) & Quote;
   end Quote_A_String;

   -------------------------------------------------------------------
   -- coppy source to target and pad with padding
   procedure Set (
      Source               : in   String;
      Target               :   out String;
      Padding              : in   Character := ' ';
      Truncate          : in   Boolean := True) is
   -------------------------------------------------------------------

      Use_Length           : constant Natural :=
                           Natural'min (Source'length, Target'length);

   begin
      if Source'length > Target'length and then not Truncate then
         raise Length_Error;
      end if;

      Target (Target'first .. Target'first + Use_Length - 1) :=
         Source (Source'first .. Source'first + Use_Length - 1);

      for Index in Target'first + Use_Length .. Target'last loop
         Target (Index) := Padding;
      end loop;
   end Set;

   -------------------------------------------------------------------
   procedure String_Access_Read (
      Stream               : access Ada.Streams.Root_Stream_Type'class;
      Value             :   out String_Access) is
   -------------------------------------------------------------------

      Length               : Integer;

   begin
      Integer'Read (Stream, Length);
      pragma Assert (Length < 100_000);

      if Length >= 0 then
         Value := new String (1 .. Length);

         if Length > 0 then
            String'Read (Stream, Value.all);
         end if;
   else
      Value := Null;
   end if;

   end String_Access_Read;

   -------------------------------------------------------------------
   procedure String_Access_Write (
      Stream               : access Ada.Streams.Root_Stream_Type'class;
      Value             : in   String_Access) is
   -------------------------------------------------------------------

   begin
      if Value = Null then
         Integer'Write (Stream, -1);
      else
         Natural'Write (Stream, Value'length);

         if Value'length > 0 then
            String'Write (Stream, Value.all);
         end if;
      end if;

   end String_Access_Write;

   -------------------------------------------------------------------
   procedure Trim (
      Source               : in   String;
      Last              : in out Natural) is
   -------------------------------------------------------------------

   begin
      while Source (Last) = ' ' loop
         Last := Last - 1;
         if Last < Source'first then
            exit;
         end if;
      end loop;
   end Trim;

--  -------------------------------------------------------------------
--  function Up_Shift (
--    Source               : in   String;
--    Mapping              : in   Ada.Strings.Maps.Character_Mapping
--  ) return String is
--  -------------------------------------------------------------------
--
--  begin
--    return Ada.Strings.Fixed.Translate (Source,
--       Ada.Strings.Maps.Constants.Upper_Case_Map);
--  end Up_Shift;

   procedure Make is begin null; end Make;

end Ada_Lib.Strings;
