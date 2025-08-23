with Ada_Lib.Lock_Interface;
--with Ada_Lib.Trace;
--with Ada.Task_Identification;
with Ada_Lib.Strings;
with GNAT.Source_Info;

package Ada_Lib.Lock is
Pragma Elaborate_Body;

   Already_Locked          : exception;
   Not_Locked              : exception;

   type Lock_Type (
      Description          : Ada_Lib.Strings.String_Constant_Access
   ) is limited new Ada_Lib.Lock_Interface.Lock_Interface with private;

   type Lock_Access        is access all Lock_Type;
   type Lock_Class_Access  is access all Lock_Type'class;

   overriding
   function Is_Locked (                -- used value on Lock
      Lock                 : in     Lock_Type;
      From                 : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   overriding
   function Lock (
      Lock                 : in out Lock_Type;
      Timeout              : in     Duration := 0.0;
      From                 : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   -- raises exception if object already locked
   overriding
   procedure Lock (
      Lock                 : in out Lock_Type;
      From                 : in     String := GNAT.Source_Info.Source_Location);

   overriding
   function Try_Lock (              -- returns true if wasn't locked
      Lock                 : in out Lock_Type;
      From                 : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean;

   overriding
   procedure Unlock (
      Lock               : in out Lock_Type;
      From                 : in     String := GNAT.Source_Info.Source_Location);

   Debug                   : aliased Boolean := False;

private

   protected type Protected_Lock_Type is

      function Is_Locked return Boolean;

      entry Lock;

      procedure Try_Lock (
         Got_Lock       :   out Boolean);

      procedure Unlock;

   private

      Locked : Boolean := False;

   end Protected_Lock_Type;

   type Protected_Lock_Access is access all Protected_Lock_Type;

   -- Task to enforce timeout on the lock
   task type Timeout_Task_Type is

      entry Start_Timeout (
         Protected_Lock : in out Protected_Lock_Type;
         Timeout        : in     Duration);

   end Timeout_Task_Type;

   type Lock_Type (
      Description       : Ada_Lib.Strings.String_Constant_Access
   ) is limited new Ada_Lib.Lock_Interface.Lock_Interface with record
      Protected_Lock    : Protected_Lock_Type;
      Timer             : Timeout_Task_Type;
   end record;

end Ada_Lib.Lock;

