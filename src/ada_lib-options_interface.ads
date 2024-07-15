with Ada.Characters.Latin_1;
limited with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Trace; -- use Ada_Lib.Trace;
with GNAT.Source_Info;

package Ada_Lib.Options_Interface is

   Failed                        : exception;

   type Option_Kind_Type         is (Nil_Option, Plain, Modified);

   No_Option                     : constant Character :=
                                    Ada.Characters.Latin_1.NUL;
   Unmodified                    : constant Character :=
                                    Ada.Characters.Latin_1.NUL;

   type Option_Type              is tagged record
      Kind                       : Option_Kind_Type := Nil_Option;
      Modifier                   : Character := Unmodified;
      Option                     : Character;
   end record;

   function Create_Option (
      Option                     : in     Character;
      Modifier                   : in     Character := Unmodified
   ) return Option_Type;

   function Image (
      Option                     : in     Option_Type;
      Quote                      : in     Boolean := True
   ) return String;

   function Less (
      Left, Right                : in     Option_Type
   ) return Boolean;

   function Modified (
      Option                     : in     Option_Type
   ) return Boolean;

   function Modifier (
      Option                     : in     Option_Type
   ) return Character;

   type Options_Type             is array (Positive range <>) of Option_Type;
   type Options_Access           is access Options_Type;

   function Create_Options (     -- create a single options
      Option                     : in     Character;
      Modifier                   : in     Character := Unmodified
   ) return Options_Type;

   function Create_Options (
      Source                     : in     String;
      Modifier                   : in     Character := Unmodified
   ) return Options_Access;

   function Create_Options (     -- create a single options
      Option                     : in     Character;
      Modifier                   : in     Character := Unmodified
   ) return Options_Access;

   function Create_Options (
      Source                     : in     String;
      Modifier                   : in     Character := Unmodified
   ) return Options_Type;

   function Has_Option (   -- tests if option is registered for a catagory
      Option                     : in     Option_Type;
      Options_With_Parameters    : in     Options_Type;
      Options_Without_Parameters : in     Options_Type
   ) return Boolean;

   function Image (
      Options                    : in     Options_Type;
      Quote                      : in     Boolean := True
   ) return String;

   type Help_Mode_Type is (Program, Traces);

   type Interface_Options_Type is limited interface;

   type Interface_Options_Class_Access
                                 is access all Interface_Options_Type'class;
   type Interface_Options_Constant_Class_Access
                                 is access constant Interface_Options_Type'class;

   procedure Bad_Option (
      Options                    : in     Interface_Options_Type;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

   procedure Bad_Option (
      Options                    : in     Interface_Options_Type;
      What                       : in     String;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

   procedure Bad_Option (
      Options                    : in     Interface_Options_Type;
      Option                     : in     Option_Type'class;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

-- function Has_Option (   added 2/22/24 to resolve issue with multple option lists
--    Options                    : in     Interface_Options_Type;
--    Option                     : in     Option_Type
-- ) return Boolean is abstract;

   procedure Bad_Trace_Option (
      Options                    : in     Interface_Options_Type;
      Trace_Option               : in     Character;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here) is abstract;

   procedure Program_Help (      -- common for all programs that use Ada_Lib.Options.GNOGA
     Options                     : in     Interface_Options_Type;  -- only used for dispatch
     Help_Mode                   : in     Help_Mode_Type) is abstract;

   -- direct decendent should return true
   -- indirect decentdent should return initialize of parent
   function Initialize (
     Options                     : in out Interface_Options_Type;
     From                        : in     String
   ) return Boolean is abstract;

   function Process_Argument (  -- process one argument
     Options                     : in out Interface_Options_Type;
     Iterator                    : in out Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type'class;
     Argument                    : in     String
   ) return Boolean is abstract;

   function Process_Option (  -- process one option
     Options                    : in out Interface_Options_Type;
     Iterator                   : in out Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type'class;
     Option                     : in     Option_Type'class
   ) return Boolean is abstract;

   procedure Trace_Parse (
      Options                    : in out Interface_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.
                                       Abstract_Package.
                                          Abstract_Iterator_Type'class) is abstract;

   procedure Update_Filter (
      Options                    : in out Interface_Options_Type) is abstract;

   function Verify_Initialized (
      Options                    : in     Interface_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is abstract;

   function Verify_Preinitialize (
      Options                    : in     Interface_Options_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is abstract;

   generic
      type Generic_Options_Type  is abstract limited new Interface_Options_Type
                                    with private;

   package Verification_Package is

      type Options_Type          is abstract limited new Generic_Options_Type
                                    with record
         Initialized             : Boolean := False;
      end record;

      type Options_Access        is access all Options_Type;
      type Options_Class_Access  is access all Options_Type'class;
      type Options_Constant_Class_Access
                                 is access constant Options_Type'class;

      overriding
      function Initialize (
         Options                 : in out Options_Type;
         From                        : in     String := Ada_Lib.Trace.Here
      ) return Boolean
      with Pre => Options.Verify_Preinitialize;

      overriding
      function Process_Argument (  -- process one argument
        Options                     : in out Options_Type;
        Iterator                    : in out Ada_Lib.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class;
        Argument                    : in     String
      ) return Boolean;

      overriding
      function Verify_Initialized (
         Options                 : in     Options_Type;
         From                    : in     String := GNAT.Source_Info.Source_Location
      ) return Boolean;

      overriding
      function Verify_Preinitialize (
         Options                 : in     Options_Type;
         From                    : in     String := GNAT.Source_Info.Source_Location
      ) return Boolean;

   end Verification_Package;

   function Ada_Lib_Options
   return Interface_Options_Constant_Class_Access;

   function Get_Modifiable_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Interface_Options_Class_Access;

   procedure Set_Ada_Lib_Options (
      Options                    : in     Interface_Options_Class_Access);

   Debug                         : aliased Boolean := False;
-- Options_Prefix                : constant Character := '#';
   Null_Option                   : constant Option_Type := Option_Type'(
                                    Kind     => Nil_Option,
                                    Modifier => Unmodified,
                                    Option   => No_Option);
   Null_Options                  : constant Options_Type (1 .. 0) :=
                                    (others => Null_Option);
   Read_Only_Options             : Interface_Options_Constant_Class_Access := Null;

end Ada_Lib.Options_Interface;
