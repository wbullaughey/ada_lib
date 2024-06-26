with Ada.Containers.Doubly_Linked_Lists;
with Ada_Lib.Options_Interface;
with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace;

package Ada_Lib.Runstring_Options is

   Duplicate_Options       : exception;
   Failed                  : exception;

   type Kind_Type             is (With_Parameters, Without_Parameters);

   type Registration_Type;
   type Registration_Constant_Access
                              is access constant Registration_Type;

   type Element_Type          is record
      From                    : Ada_Lib.Strings.Unlimited.String_Type;
      Kind                    : Kind_Type;
      Option                  : Ada_Lib.Options_Interface.Option_Type;
   end record;

   type Element_Access        is access all Element_Type;
   type Element_Constant_Access
                              is access constant Element_Type;

   overriding
   function "=" (
      Left, Right             : in     Element_Type
   ) return Boolean;

   package Registrations_Package is new
                              Ada.Containers.Doubly_Linked_Lists (
      Element_Type   => Element_Type,
      "="            => "=");

   subtype Registrations_Type    is Registrations_Package.List;

   protected type Registration_Type is

      function All_Options(
         Quote                   : in     Boolean := True
      ) return String;

      function All_Registered(
         Quote                   : in     Boolean := True
      ) return String;

      function Has_Parameter (
         Option                  : in     Ada_Lib.Options_Interface.
                                             Option_Type
      ) return Boolean;

      function Is_Registered (   -- tests if option was registered for the whole program
         Option                  : in     Ada_Lib.Options_Interface.
                                             Option_Type
      ) return Boolean;

      procedure Register (
         Kind                    : in     Kind_Type;
         Options                 : in     Ada_Lib.Options_Interface.
                                             Options_Type;
         From                    : in     String := Ada_Lib.Trace.Here);

      function Registration (
         Option                  : in     Ada_Lib.Options_Interface.
                                             Option_Type
      ) return String;

      procedure Reset;                 -- clears sets if need different iterator sets

   private
      Registrations              : aliased Registrations_Type;

   end Registration_Type;

-- function Get_Options (
--    Kind                       : in     Kind_Type
-- ) return Registration_Constant_Access;

   Debug                         : Boolean := False;
   Options                       : aliased Registration_Type;

end Ada_Lib.Runstring_Options;
