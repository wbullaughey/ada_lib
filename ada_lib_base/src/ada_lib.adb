with Ada.Task_Identification;

package body Ada_Lib is

   use type Ada.Calendar.Time;

   --------------------------------------------------------------------
   -- return task identification for calling task
   function Current_Task
   return String is
   --------------------------------------------------------------------

   begin
      return Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task);
   end Current_Task;

end Ada_Lib;
