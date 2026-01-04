with Ada_Lib.Trace;

package Ada_Lib.Options.Verification is

   type Verification_Options_Type is abstract
         limited new Abstract_Runtime_Options_Type with record
      Initialized             : Boolean := False;
   end record;

   type Verification_Options_Access        is access all Verification_Options_Type;
   type Verification_Options_Class_Access  is access all Verification_Options_Type'class;
   type Verification_Options_Constant_Class_Access
                              is access constant Verification_Options_Type'class;

   overriding
   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Verification_Options_Type;
      What                       : in     Character;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Verification_Options_Type;
      What                       : in     String;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   procedure Bad_Option (        -- raises Failed exception
      Options                    : in     Verification_Options_Type;
      Option                     : in     Base_Flag_Option_Type'class;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   procedure Bad_Trace_Option (  -- raises Failed exception
      Options                    : in     Verification_Options_Type;
      Trace_Option               : in     Character;
      What                       : in     Character;
      Modifier          : in     Character := Ada.Characters.Latin_1.Nul;
      Message                    : in     String := "";
      Where                      : in     String := Ada_Lib.Trace.Here);

   overriding
   function Initialize (
      Options                 : in out Verification_Options_Type;
      From                    : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean
   with Pre => Options.Verify_Preinitialize;

   overriding
   function Process_Argument (  -- process one argument
      Options                  : in out Verification_Options_Type;
      Iterator                 : in out Command_Line_Iterator_Interface'class;
      Argument                 : in     String
   ) return Boolean;

   overriding
   procedure Trace_Parse (
      Options                    : in out Verification_Options_Type;
      Iterator                   : in out Command_Line_Iterator_Interface'class);

   overriding
   procedure Update_Filter (
      Options                    : in out Verification_Options_Type);

   overriding
   function Verify_Initialized (
      Options                 : in     Verification_Options_Type;
      From                    : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   overriding
   function Verify_Preinitialize (
      Options                 : in     Verification_Options_Type;
      From                    : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   function Was_Initialized (
      Options                 : in     Verification_Options_Type
   ) return Boolean;

   -- type to application options
end Ada_Lib.Options.Verification;

