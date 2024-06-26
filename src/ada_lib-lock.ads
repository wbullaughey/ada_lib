with Ada_Lib.Lock_Interface;
with Ada.Task_Identification;
with Ada_Lib.Strings;
with GNAT.Source_Info;

package Ada_Lib.Lock is
Pragma Elaborate_Body;

   Not_Locked                    : exception;

   type Lock_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is limited new Ada_Lib.Lock_Interface.Lock_Interface with private;

   type Lock_Access              is access all Lock_Type;
   type Lock_Class_Access        is access all Lock_Type'class;

   overriding
   function New_Lock (              -- returns true if wasn't locked
      Object                     : in out Lock_Type;
      From                       : in     String :=
                                             GNAT.Source_Info.Source_Location
   ) return Boolean;

   function Get_Task_Id (
      Object                     : in     Lock_Type
   ) return Ada.Task_Identification.Task_Id;

   overriding
   function Has_Lock (                 -- uses lock object
      Object                     : in     Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   overriding
   function Is_Locked (                -- used value on Object
      Object                     : in     Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   overriding
   procedure Lock (
      Object                     : in out Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location);

   overriding
   procedure Unlock (
      Object                     : in out Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location);

   Debug                         : aliased Boolean := False;

private

   protected type Protected_Lock is

      function Get_Task_Id return Ada.Task_Identification.Task_Id;

      function Has_Lock (
         From                    : in     String
      ) return Boolean;

      procedure Unlock (
         From                    : in     String);

      entry Wait_Lock (
         Current_Task_ID         : in     Ada.Task_Identification.Task_Id;
         From                    : in     String);

   private
      Locked                     : Boolean := False;
      Task_ID                    : Ada.Task_Identification.Task_Id :=
                                    Ada.Task_Identification.Null_Task_Id;
   end Protected_Lock;

   type Lock_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is limited new Ada_Lib.Lock_Interface.Lock_Interface with record
      Lock_Object                : Protected_Lock;
      Locked                     : Boolean := False;
   end record;

end Ada_Lib.Lock;

