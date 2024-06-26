with Ada.Characters.Latin_1;
with Ada.Strings.Maps;
with Ada_Lib.Strings.Unlimited;

package Ada_Lib.Parser is

   Underflow                  : exception;

   type Iterator_Type            is tagged private;

   Debug                      : aliased Boolean := False;
   No_Seperator               : constant Character :=
                                    Ada.Characters.Latin_1.NUL;
   function At_End (
      Iterator                : in   Iterator_Type
   ) return Boolean;

   function Get_Number (
      Iterator                   : in out Iterator_Type;
      Do_Next                    : in     Boolean := False
   ) return Integer;

   function Get_Original (     -- returns original unparsed value
      Iterator             : in   Iterator_Type
   ) return String;

   function Get_Parsed (     -- returns start of buffer to current
      Iterator             : in   Iterator_Type
   ) return String;

   function Get_Remainder (
      Iterator             : in   Iterator_Type
   ) return String;

   function Get_Seperator (
      Iterator             : in   Iterator_Type
   ) return Character;
   -- returns No_Seperator if no seperator after current key

   function Get_Seperators (
      Iterator             : in   Iterator_Type
   ) return String;
   -- returns 0 length string if no seperator

   function Get_Value (
      Iterator                   : in out Iterator_Type;
      Do_Next                    : in     Boolean := False;
      Allow_Null                 : in     Boolean := False
   ) return String;

   function Initialize (
      Value                      : in     String;
      Seperators                 : in     String := " ";
      Ignore_Multiple_Seperators : in     Boolean := True;
      Comment_Seperator          : in     Character := No_Seperator;
      Trim_Spaces                : in     Boolean := True;
      Quotes                     : in     String := ""
   ) return Iterator_Type;

   procedure Initialize (
      Iterator                   :    out Iterator_Type;
      Value                      : in     String;
      Seperators                 : in     String := " ";
      Ignore_Multiple_Seperators : in     Boolean := True;
      Comment_Seperator          : in     Character := No_Seperator;
      Trim_Spaces                : in     Boolean := True;
      Quotes                     : in     String := "");

   function Is_Quoted (       -- returns true if last Get_Value was a quoted string
      Iterator             : in   Iterator_Type
   ) return Boolean;

   procedure Next (
      Iterator             : in out Iterator_Type);


   generic

      Comment_Seperator       : in   Character;

      Key_Seperator           : in   Character;

      Ignore_Spaces           : in   Boolean := False;

      type Key_Type           is ( <> );

      Keys                 : in   String;

      Value_Seperators        : in   String;

   package Name_Value is

      Failed                  : exception;

      function At_End
      return Boolean;

      function Is_Key
      return Boolean;

      function Get_Key
      return Key_Type;

      function Get_Value
      return String;

      function Get_Value_Required (
         Value_Required       : in Boolean
      ) return String;

      procedure Initialize (
         Value             : in   String);

      function Key_Name (
         Key               : in     Key_Type
      ) return String;

      function Remainder
      return String;

      function Seperator
      return Character;

   end Name_Value;

private

   package Buffer_Package  renames Ada_Lib.Strings.Unlimited;

   subtype Buffer_Type     is Buffer_Package.String_Type;

   type State_Type is (Normal, Quote, Seperator);

   type Iterator_Type            is tagged record
      Buffer                     : Buffer_Type;
      Current_Seperators         : Buffer_Type;
      Current_Value              : Buffer_Type;
      Ignore_Multiple_Seperators : Boolean;
      Current                    : Natural;
      Last_Start                 : Natural;
      Length                     : Natural;
      Quoted                     : Boolean := False;
      Quotes                     : Ada.Strings.Maps.Character_Set;
      Seperators                 : Ada.Strings.Maps.Character_Set;
      State                      : State_Type := Normal;
      Trim                       : Boolean;
   end record;

end Ada_Lib.Parser;


