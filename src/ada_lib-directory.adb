with Ada.Exceptions;
with Ada_Lib.OS.Environment;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.Directory is

   ---------------------------------------------------------------
   procedure Delete (
      Name                       : in     String;
      Allow                      : in     Allow_Type := Default_Allow;
      Must_Exist                 : in     Boolean := False) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("Name", Name) & " must exist " & Must_Exist'img);

      if Exists (Name) then
         declare
            File_Kind            : constant Ada.Directories.File_Kind := Kind (Name);

         begin
            Log_Here (Debug, "kind " & File_Kind'img);
            if not Allow (File_Kind) then
               raise Failed with Quote ("Name", Name) & " kind " & File_Kind'img &
                  " not allowed to be deleted";
            end if;

            case File_Kind is

               when  Ada.Directories.Ordinary_File |
                     Ada.Directories.Special_File =>
                  Ada.Directories.Delete_File (Name);

               when Ada.Directories.Directory =>
                  Ada.Directories.Delete_Tree (Name);

            end case;
         end;
      else
         if Must_Exist then
            raise Failed with Quote ("Name", Name) & " does not exist " & Here;
         end if;
      end if;

      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, Who, Here);
         raise Failed with Quote ("Name", Name) & " could not be delete. Exceptin: " &
            Ada.Exceptions.Exception_Name (Fault);

   end Delete;

   ---------------------------------------------------------------
   function Exists (
      Name                       : in     String
   ) return Boolean is
   ---------------------------------------------------------------

      Result                     : constant Boolean := Ada.Directories.Exists (Name);

   begin
      Log_In (Debug, Quote ("Name", Name) & " result " & Result'img);
      return Result;
   end Exists;

   ---------------------------------------------------------------
   function Full_Name (
      Name                       : in     String
   ) return String is
   ---------------------------------------------------------------

   begin
      return Ada.Directories.Full_Name (
         (if Name (Name'first) = '~' then
               Ada_Lib.OS.Environment.Get ("HOME") &
                  (if Name'length > 1 then
                        Name (Name'first + 1 .. Name'last)
                     else
                        "")
            else
               Name));
   end Full_Name;

   ---------------------------------------------------------------
   function Kind (
      Name                       : in     String
   ) return Ada.Directories.File_Kind is
   ---------------------------------------------------------------

   begin
      return Ada.Directories.Kind (Name);
   end Kind;

begin
   Log_Here (Elaborate);
--Debug := True;
end Ada_Lib.Directory;
