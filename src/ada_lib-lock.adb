--with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.Lock is

   -------------------------------------------------------------------
   overriding
   function Is_Locked (                -- used value on Lock
      Lock                    : in     Lock_Type;
      From                    : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      return Log_Here (Lock.Protected_Lock.Is_Locked, Debug,
         "called from " & From);
   end Is_Locked;

   -------------------------------------------------------------------
   overriding
   function Lock (
      Lock           : in out Lock_Type;
      Timeout        : in     Duration := 0.0;
      From           : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      Log_In (Debug, "timeout " & Timeout'img & " from " & From);
      Lock.Timer.Start_Timeout(Lock.Protected_Lock, Timeout);

      select
         Lock.Protected_Lock.Lock; -- set the lock
         return Log_Out (True, Debug);
      or
         delay Timeout;
         return Log_Out (False, Debug);
      end select;
   end Lock;

   -------------------------------------------------------------------
   -- raises exception if object already locked
   overriding
   procedure Lock (
      Lock                 : in out Lock_Type;
      From                 : in     String := GNAT.Source_Info.Source_Location) is
   -------------------------------------------------------------------

   begin
      Log_In (Debug, "from " & From);
      if not Lock.Lock (0.0) then
         raise Already_Locked with "from " & From;
      end if;
      Log_Out (Debug);
   end Lock;

   -------------------------------------------------------------------
   overriding
   function Try_Lock (
      Lock                     : in out Lock_Type;
      From                       : in     String := GNAT.Source_Info.Source_Location
   ) return Boolean is
   -------------------------------------------------------------------

      Got_Lock                   : Boolean;

   begin
      Log_In (Debug, "from " & From);
      Lock.Protected_Lock.Try_Lock (Got_Lock);
      return Log_Out (Got_Lock, Debug);
   end Try_Lock;

-------------------------------------
   overriding
   procedure Unlock (
      Lock               : in out Lock_Type;
      From                 : in     String := GNAT.Source_Info.Source_Location) is
   -------------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);
      Lock.Protected_Lock.Unlock;
   end Unlock;

   -------------------------------------------------------------------
   protected body Protected_Lock_Type is

      -------------------------------------------------------------------
      entry Lock when not Locked is
      -------------------------------------------------------------------

      begin
         Locked := True;
      end Lock;

      -------------------------------------------------------------------
      procedure Try_Lock (
         Got_Lock       :   out Boolean) is
      -------------------------------------------------------------------

      begin
         if Locked then
            Got_Lock := False;
         else
            Locked := True;
            Got_Lock := True;
         end if;
      end Try_Lock;

      -------------------------------------------------------------------
      procedure Unlock is
      -------------------------------------------------------------------

      begin
         Locked := False;
      end Unlock;

      -------------------------------------------------------------------
      function Is_Locked return Boolean is
      -------------------------------------------------------------------
      begin
         return Locked;
      end Is_Locked;

   end Protected_Lock_Type;

   -------------------------------------------------------------------
   task body Timeout_Task_Type is

   begin
      Log_In (Debug);
      loop
         select
            accept Start_Timeout (
               Protected_Lock : in out Protected_Lock_Type;
               Timeout        : in     Duration) do

               Log_Here (Debug, "timeout " & Timeout'img);
               delay Timeout;

               if Protected_Lock.Is_Locked then
                  Log_Here (Debug);
               end if;
            end Start_Timeout;
         or
            terminate;
         end select;
      end loop;
   end Timeout_Task_Type;

end Ada_Lib.Lock;
