--with Ada.Text_IO; use  Ada.Text_IO;

package body Ada_Lib.Lock is

   -------------------------------------------------------------------
   overriding
   function New_Lock (
      Object                     : in out Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      if Object.Locked then
         return False;
      else
         Object.Locked := True;
         return True;
      end if;
   end New_Lock;

   -------------------------------------------------------------------
   function Get_Task_Id (
      Object                     : in     Lock_Type
   ) return Ada.Task_Identification.Task_Id is
   -------------------------------------------------------------------

   begin
      return Object.Lock_Object.Get_Task_Id;
   end Get_Task_Id;

   -------------------------------------------------------------------
   overriding
   function Has_Lock (
      Object                     : in     Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   -------------------------------------------------------------------

      Result                     : constant Boolean :=
                                    Object.Lock_Object.Has_Lock (From);
   begin
--put_Line (GNAT.Source_Info.Source_Location & ":"&Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) & " result " & result'img);
      return Result;
   end Has_Lock;

   -------------------------------------------------------------------
   overriding
   function Is_Locked (                -- used value on Object
      Object                     : in     Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   -------------------------------------------------------------------

   begin
--put_Line (GNAT.Source_Info.Source_Location & ":"&Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) & " locked " & object.locked'img);
      return Object.Locked;
   end Is_Locked;

   -------------------------------------------------------------------
   overriding
   procedure Lock (
      Object                     : in out Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location) is
   -------------------------------------------------------------------

      Current_Task               : constant Ada.Task_Identification.Task_Id :=
                                       Ada.Task_Identification.Current_Task;

   begin
--put_Line (GNAT.Source_Info.Source_Location & ":"&Ada.Task_Identification.Image (Current_Task) & " locked " & object.locked'img);
      if not Object.Locked then      -- wait if locked by some other task
         Object.Lock_Object.Wait_Lock (Current_Task, From);
         Object.Locked := True;
      end if;
--put_Line (GNAT.Source_Info.Source_Location & ":"&Ada.Task_Identification.Image (Current_Task) & " locked " & object.locked'img);
   end Lock;

   -------------------------------------------------------------------
   overriding
   procedure Unlock (
      Object                     : in out Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location) is
   -------------------------------------------------------------------

   begin
--put_Line (GNAT.Source_Info.Source_Location & ":"&Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) & " locked " & object.locked'img);
      Object.Lock_Object.Unlock (From);
      Object.Locked := False;
   end Unlock;

   protected body Protected_Lock is
   -------------------------------------------------------------------

      ---------------------------------------------------------------
      function Get_Task_Id return Ada.Task_Identification.Task_Id is
      ---------------------------------------------------------------

      begin
         return Ada.Task_Identification.Current_Task;
      end Get_Task_Id;

      ---------------------------------------------------------------
      function Has_Lock (
         From                    : in     String := Standard.Ada_Lib.Trace.Here
      ) return Boolean is
      pragma Unreferenced (From);
      ---------------------------------------------------------------

      begin
--put_Line (GNAT.Source_Info.Source_Location & ":"&Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) & " locked " & locked'img);
         return Locked;
      end Has_Lock;

      ---------------------------------------------------------------
      procedure Unlock (
         From                    : in     String) is
      pragma Unreferenced (From);
      ---------------------------------------------------------------

      begin
--put_Line (GNAT.Source_Info.Source_Location & ":"&Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) & " locked " & locked'img);
         if not Locked then
            raise Not_Locked with "not locked at " &
               GNAT.Source_Info.Source_Location;
         end if;
         Locked := False;
      end Unlock;

      ---------------------------------------------------------------
      entry Wait_Lock (
         Current_Task_ID         : in     Ada.Task_Identification.Task_Id;
         From                    : in     String
      ) when not Locked is
      pragma Unreferenced (From);
      ---------------------------------------------------------------

      begin
--put_Line (GNAT.Source_Info.Source_Location & " locked " & locked'img);
         Locked := True;
         Task_ID := Current_Task_ID;
      end Wait_Lock;

   end Protected_Lock;

end Ada_Lib.Lock;
