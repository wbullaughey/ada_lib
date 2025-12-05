with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Unchecked_Deallocation;

package Ada_Lib.Strings is

   Length_Error            : exception;

   type String_Access         is access String;

   type String_Access_All     is access all String;

   type String_Constant_Access   is access constant String;

   type Constant_String_Array is array (Positive range <>) of String_Constant_Access;

   Both                 : constant Ada.Strings.Trim_End :=
                           Ada.Strings.Both;

   Left                 : constant Ada.Strings.Trim_End :=
                           Ada.Strings.Left;

   Right                : constant Ada.Strings.Trim_End :=
                           Ada.Strings.Right;
   procedure String_Access_Read (
      Stream               : access Ada.Streams.Root_Stream_Type'class;
      Value             :   out String_Access);

   for String_Access'Read use    String_Access_Read;

   procedure String_Access_Write (
      Stream               : access Ada.Streams.Root_Stream_Type'class;
      Value             : in   String_Access);

   for String_Access'Write use String_Access_Write;

   function Down_Shift (
      Source               : in   String;
      Mapping              : in   Ada.Strings.Maps.Character_Mapping :=
                           Ada.Strings.Maps.Constants.Lower_Case_Map
   ) return String renames Ada.Strings.Fixed.Translate;

   procedure Free          is new Ada.Unchecked_Deallocation (
      Object   => String,
      Name  => String_Access);

   procedure Free_All          is new Ada.Unchecked_Deallocation (
      Object   => String,
      Name  => String_Access_All);

   function Index (
      Source               : in   String;
      Pattern              : in   String;
      Going             : in   Ada.Strings.Direction :=
                           Ada.Strings.Forward;
        Mapping            : in   Ada.Strings.Maps.Character_Mapping :=
                                    Ada.Strings.Maps.Identity
   ) return Natural renames Ada.Strings.Fixed.Index;

   function Is_Decimal (
      Source                     : in     String
   ) return Boolean;

   function Optional (
      Source               : in   String_Constant_Access
   ) return String;

   function Optional (
      Source               : in   String_Access
   ) return String;

   function Pad (
      Source               : in   String;
      Length               : in   Positive;
      Side                 : in   Ada.Strings.Trim_End := Right
   ) return String;

   function Parse_Field (
      Source               : in   String;
      Seperator            : in   Character;
      Index                : in   Positive
   ) return String;

   function Quote_A_String (
      Source                     : in     String;
      Quote                      : in     Character := '"'
   ) return String;

   -- coppy source to target and pad with padding
   procedure Set (
      Source               : in   String;
      Target               :   out String;
      Padding              : in   Character := ' ';
      Truncate          : in   Boolean := True);

   function Trim (
      Source               : in   String;
      Side                 : in   Ada.Strings.Trim_End :=
                                 Ada.Strings.Both
   ) return String renames Ada.Strings.Fixed.Trim;

   procedure Trim (
      Source               : in   String;
      Last              : in out Natural);

   function Up_Shift (
      Source               : in   String;
      Mapping              : in   Ada.Strings.Maps.Character_Mapping :=
                           Ada.Strings.Maps.Constants.Upper_Case_Map
   ) return String renames Ada.Strings.Fixed.Translate;

   procedure Make;               -- dummy used to force recomplation

   Debug                   : Boolean := False;

end Ada_Lib.Strings;
