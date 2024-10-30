with Ada.Exceptions;
with GNAT.OS_Lib;
with GNAT.Source_Info;

package Ada_Lib.OS is

   Failed                        : exception;

   type Priority_Type            is range -20 .. 19;

   Default_Priority              : constant Priority_Type := 0;
   PRIO_PROCESS                  : constant := 0;
   Temp_File_Length              : constant Integer := 12;


-- subtype Exit_Code_Type        is Integer range -128 .. 127;

   type Exit_Code_Type           is (  -- macos errors
      No_Error,
      Application_Error,               -- error codes are probably wrong
      Application_Exception,
      ENOENT,
      Unassigned);
   for Exit_Code_Type'size use 8;

   subtype File_Descriptor       is GNAT.OS_Lib.File_Descriptor;
   type Process_Id_Type          is new Integer;
   subtype Temporary_File_Name   is String (1 .. Temp_File_Length);

   procedure Exception_Halt (
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String := "";
      From                       : in     String := GNAT.Source_Info.Source_Location);

   function Get_Environment (
      Name                       : in     String
   ) return String;

   function Get_Host_Name
   return String;

   function Get_User
   return String;

   procedure Immediate_Halt (
      Exit_Code                  : in     Exit_Code_Type);

   procedure Close_File (
      File                       : in     File_Descriptor) renames
                                             GNAT.OS_Lib.Close;

   procedure Create_Scratch_File (
      File                       :    out File_Descriptor;
      Name                       :    out Temporary_File_Name) renames
                                       GNAT.OS_Lib.Create_Temp_File;

   procedure Set_Priority (
      Priority                   : in     Priority_Type);

   function Self
   return Process_Id_Type;

   function Self
   return String;

   subtype Argument_Array     is GNAT.OS_Lib.String_List;

   Trace        : aliased Boolean := False;
end Ada_Lib.OS;
